open System
open System.Text
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Web
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.WebSockets
open System.Net.WebSockets
open Microsoft.AspNetCore.Http
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Application
open Aardvark.Dom
open Aardvark.Dom.Remote
open Aardvark.Rendering


#nowarn "3511"

type Marker = class end

let aardvarkJs = 
    let ass = typeof<Marker>.Assembly
    match ass.GetManifestResourceNames() |> Array.tryFind (fun n -> n.EndsWith "aardvark-dom.js") with
    | Some res ->
        use s = ass.GetManifestResourceStream res
        use r = new StreamReader(s)
        r.ReadToEnd()
    | None ->
        ""

let mainPage =
    String.concat "\n" [
        "<html>"
        "   <head>"
        "       <script src='./aardvark-dom.js'></script>"
        "   </head>"
        "   <body>"
        "   </body>"
        "</html>"
    ]

type Message =
    | Text of string
    | Binary of byte[]

type Context =
    {
        Connection : string
        Receive : unit -> Task<Message>
        Execute : string -> Task
        Shutdown : unit -> unit
    }

let js (str : string) : HttpHandler =
    let bytes = Encoding.UTF8.GetBytes str
    fun (_ : HttpFunc) (ctx : HttpContext) ->
        ctx.SetContentType "text/javascript"
        ctx.WriteBytesAsync bytes

let private sha = System.Security.Cryptography.SHA1.Create()

let webApp (run : IServer -> Aardvark.Rendering.IRuntime -> Context -> Task<unit>) =
    
    let app = new Aardvark.Application.Slim.OpenGlApplication()
    let sockets = Dict<string, WebSocket -> Task<unit>>()
    let resources = Dict<string, string * byte[]>()
    let resourceIds = Dict<Guid, string>()


    let server =
        {
            new IServer with
                //member x.RegisterChannel(action : IChannel -> Task<unit>, js : string) =
                    
                member x.RegisterWebSocket(action : WebSocket -> Task<unit>) =
                    lock sockets (fun () ->
                        let id = Guid.NewGuid() |> string
                        let mutable connections = System.Collections.Generic.List()
                        sockets.[id] <- fun w -> 
                            connections.Add w
                            action w
                        let url = sprintf "registered/%s" id
                        url, 
                        { new System.IDisposable with 
                            member x.Dispose() = 
                                lock sockets (fun () -> sockets.Remove id |> ignore) 
                                for c in connections do
                                    if c.State = WebSocketState.Open || c.State = WebSocketState.Connecting then
                                        try c.CloseAsync(WebSocketCloseStatus.NormalClosure, "close", Unchecked.defaultof<_>).Wait()
                                        with _ -> ()
                                connections.Clear()
                        }
                    )

                member x.RegisterResource(uniqueId : Guid, mime : string, content : byte[]) =
                    let hash = System.Convert.ToBase64String (sha.ComputeHash content)
                    lock resources (fun () ->
                        match resourceIds.TryGetValue uniqueId with
                        | (true, url) -> 
                            url
                        | _ -> 
                            let id = Guid.NewGuid() |> string

                            let ext =
                                match mime with
                                | "text/css" -> ".css"
                                | "application/javascript" -> ".js"
                                | "application/wasm" -> ".wasm"
                                | _ -> ""

                            let name = sprintf "%s%s" id ext
                            let url = sprintf "/registered/%s" name
                            resources.[name] <- (mime, content)
                            resourceIds.[uniqueId] <- url
                            url
                    )
        }

    choose [
        routeStartsWithf "/registered/%s" (fun id next ctx -> 
            match ctx.WebSockets.IsWebSocketRequest with
            | true ->
                task {
                    match lock sockets (fun () -> sockets.TryGetValue id) with
                    | (true, handler) ->
                        let! (ws : WebSocket) = ctx.WebSockets.AcceptWebSocketAsync()
                        do! handler ws
                        return! next ctx
                    | _ ->
                        ctx.Response.StatusCode <- 404
                        return! next ctx
                            
                }
            | false ->
                match lock resources (fun () -> resources.TryGetValue id) with
                | (true, (mime, content)) -> 
                    ctx.SetContentType mime
                    ctx.WriteBytesAsync content
                | _ ->
                    ctx.Response.StatusCode <- 404
                    next ctx
        )
        route "/aardvark-dom.js" >=> js aardvarkJs
        route "/ping"   >=> text "pong"
        route "/"       >=> htmlString mainPage
        route "/socket" >=> (fun next ctx -> 
            match ctx.WebSockets.IsWebSocketRequest with
            | true ->
                task {
                    let! ws = ctx.WebSockets.AcceptWebSocketAsync()
                    let conn = ctx.Connection.Id

                    let execute (code : string) =
                        let data = 
                            StringBuilder()
                                .Append("{")
                                .Append("\"command\": \"execute\", ")
                                .AppendFormat("\"code\": \"{0}\"", HttpUtility.JavaScriptStringEncode code)
                                .Append("}")
                                .ToString()
                                |> Encoding.UTF8.GetBytes
                        ws.SendAsync(ArraySegment data, WebSocketMessageType.Text, true, CancellationToken.None)
                    
                    let rec receive () =
                        task {
                            let! typ, data = ws.ReceiveMessage()
                            match typ with
                            | WebSocketMessageType.Text -> 
                                return Message.Text (Encoding.UTF8.GetString data)
                            | WebSocketMessageType.Binary ->
                                return Message.Binary data
                            | _ ->
                                return! receive()
                        }
                    
                    try
                        do! run server app.Runtime {
                            Connection = conn
                            Receive = receive
                            Execute = execute
                            Shutdown = ws.Dispose
                        }
                    with _ -> ()

                    return! next ctx
                } 
            | false ->
                ctx.Response.StatusCode <- 400
                next ctx
        )
    ]



[<AutoOpen>]
module private ThreadingHelpers =
    open System.Threading.Tasks

    type AsyncSignal(isSet : bool) =
        static let finished = Task.FromResult()
        let mutable isSet = isSet
        let mutable tcs : option<TaskCompletionSource<unit>> = None

        member x.Pulse() =
            lock x (fun () ->
                if not isSet then
                    isSet <- true
                    match tcs with
                    | Some t -> 
                        t.SetResult()
                        tcs <- None
                    | None -> 
                        ()
                    tcs <- None
            )

        member x.Wait() =
            lock x (fun () ->
                if isSet then
                    isSet <- false
                    finished
                else
                    match tcs with
                    | Some tcs -> 
                        tcs.Task
                    | None -> 
                        let t = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
                        tcs <- Some t
                        t.Task
            )

module Updater =
    let run (state : UpdateState<int64>) (b : RemoteHtmlBackend) (action : string -> Task) (updater : Updater<int64>) =
        let mutable running = true
        let signal = AsyncSignal(true)
        let sub = updater.AddMarkingCallback signal.Pulse

        task {
            while running do
                do! signal.Wait()
                if running then
                    do! Async.SwitchToThreadPool()
                    updater.Update(state, b)
                    let code = b.GetCode().Trim() 
                    if code <> "" then
                        do! action code

        }

let myView (server : IServer) (runtime : IRuntime) (ctx : Context)=
    let content = cval 0
    let text = cval ""
    let click (evt : MouseEvent) =
        transact (fun () -> 
            content.Value <- int (round evt.ClientX)
            text.Value <- System.Text.Json.JsonSerializer.Serialize(evt, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
        )

    let down = cset[]
    let pos = cval V2d.Zero

    let view = 
        body {
            Require ["https://cdnjs.cloudflare.com/ajax/libs/bootstrap/5.2.0/css/bootstrap.min.css"]

            Style [ 
                Background "#36363A"
                Color "white"
                FontFamily "monospace"
            ]

            h1 {
                Id "foo"
                Class "bar"
                Att.OnClick(click, true)
                content |> AVal.map string

            }

            ul {
                OnMouseEnter(fun e -> printfn "enter 0")
                OnMouseLeave(fun e -> printfn "leave 0")
                Att.OnClick((fun e -> printfn "capture 0"; true), true)
                Att.OnClick((fun e -> printfn "bubble 0"; true), false)
                li { "Hans" }
                li {
                    
                    OnMouseEnter(fun e -> printfn "enter 1")
                    OnMouseLeave(fun e -> printfn "leave 1")
                    Att.OnClick((fun e -> printfn "capture 1"; true), true)
                    Att.OnClick((fun e -> printfn "bubble 1"; false), false)
                    ul {
                        li { 
                            "Sepp" 
                            OnMouseEnter(fun e -> printfn "enter 2")
                            OnMouseLeave(fun e -> printfn "leave 2")
                            Att.OnClick((fun e -> printfn "capture 2"; true), true)
                            Att.OnClick((fun e -> printfn "bubble 2"; true), false)
                        }
                        li { "Franz" }
                    }
                }
            }

            
            let rand = RandomSystem()
            let markerColor = cval (rand.UniformC3f().ToC4b())
            let marker = cval (Ray3d(V3d.NaN, V3d.Zero))

            pre {
                
                Style [
                    Background "#202124"
                    Color "#00C800"
                    BorderRadius "20px"
                    Padding "15px"
                    PaddingLeft "20px"
                ]
                
                down |> ASet.isEmpty |> AVal.map (function
                    | true -> None
                    | false -> Some (Style [FontWeight "bold"])
                )

                OnBoot "console.log('hi there');"
                OnShutdown "console.log('bye');"

                

                OnContextMenu(click, useCapture = true)
                Att.OnClick(click, true)

                Att.OnPointerDown (fun e ->
                    transact (fun () -> down.Add e.PointerId |> ignore)
                    //printfn "down\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                )
                
                Att.OnPointerUp (fun e ->
                    transact (fun () -> down.Remove e.PointerId |> ignore)
                    //printfn "up\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                )

                down |> ASet.isEmpty |> AVal.map (function
                    | true -> []
                    | false -> 
                        [
                            Att.OnPointerMove (fun e ->
                                printfn "move %d: %f,%f" e.PointerId e.ClientX e.ClientY
                                transact (fun () -> pos.Value <- V2d(e.ClientX, e.ClientY))
                            )
                        ]
                )


                text
            }
            
            down |> ASet.isEmpty |> AVal.map (function  
                | true -> []
                | false ->
                    [h3 { pos |> AVal.map string }; h4 { "foo"} ]    
            )

            h5 { "hello" }

            down |> ASet.sort |> AList.map (fun d ->
                pre {
                    
                    OnShutdown("__THIS__.shutdown();")

                    Attribute(
                        "boot", 
                        AttributeValue.Execute(
                            [|
                            
                                fun (c : IChannel) ->
                                    task {
                                    
                                        let sw = System.Diagnostics.Stopwatch.StartNew()

                                        let mutable t : System.Threading.Timer = null 
                                        let tick(_) =
                                            try c.Send(ChannelMessage.Text (string sw.Elapsed.TotalSeconds)).Result
                                            with _ -> ()

                                        t <- new System.Threading.Timer(TimerCallback(tick), null, 1000, 1000)
                                    
                                        let rec run () =
                                            task {
                                                let! msg = c.Receive()
                                                match msg with
                                                | ChannelMessage.Close -> 
                                                    return ()
                                                | ChannelMessage.Binary a -> 
                                                    printfn "%A" a
                                                    return! run()
                                                | ChannelMessage.Text a ->
                                                    printfn "%A" a
                                                    return! run()
                                            }

                                        do! run()
                                        printfn "CLOSE"
                                        t.Dispose()
                                    }
                                
                            |], 
                            fun n ->
                                let n = n.[0]
                                [
                                    $"{n}.onmessage = (e) => {{"
                                    $"  console.log(\"got\", e.data); "
                                    $"  {n}.send(e.data);"
                                    $"  let t = parseFloat(e.data);"
                                    $"  if(t >= 10.0) {n}.close();"
                                    $"}};"
                                    $"{n}.onclose = (e) => {{"
                                    $"  console.log(\"close\"); "
                                    $"}};"
                                    $"__THIS__.shutdown = () => {{ console.warn(\"shutdown\"); }}";
                                ]
                        )
                    )


                    $"pointer{d}"
                }
            )

            h5 { marker |> AVal.map (fun r -> if Vec.AnyNaN r.Origin then "no marker" else sprintf "%.2f %.2f %.2f" r.Origin.X r.Origin.Y r.Origin.Z) }
            
            let rotationTrafo (active : aval<bool>) (time : aval<System.DateTime>) =
                let seconds = 
                    let sw = System.Diagnostics.Stopwatch.StartNew()
                    time |> AVal.map (fun t ->
                        sw.Elapsed.TotalSeconds
                    )

                AVal.integrate Trafo3d.Identity time [
                    active |> AVal.map (function
                        | true ->
                            seconds |> AVal.step (fun t dt v ->
                                if dt > 0.1 then
                                    printfn "%.3f" dt
                                v * Trafo3d.RotationZ(dt)
                            )
                        | false ->
                            AdaptiveFunc.Identity
                    )
                ]

            renderControl  {
                // HTML attributes
                Style [Width "100%"; Height "600px"; Background "black"] 
                Att.OnMouseEnter(fun e ->
                    printfn "enter rc"
                )
                Att.OnMouseLeave(fun e ->
                    printfn "leave rc"
                    transact (fun _ -> marker.Value <- Ray3d(V3d.NaN, V3d.Zero))
                )
                Attribute("data-samples", AttributeValue.String "4")

                // react to resize of the RenderControl
                RenderControl.OnResize(fun s ->
                    Log.warn "resize: %A" s
                )
                RenderControl.OnRendered(fun s ->
                    () // some code here
                )

                // get the changeable size/time for the control
                let! size = RenderControl.ViewportSize
                let! time = RenderControl.Time

                // apply a camera
                let view = CameraView.lookAt (V3d(3,4,5)) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> AVal.constant
                let proj = size |> AVal.map (fun s -> Frustum.perspective 80.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                Sg.View view
                Sg.Proj proj
                    
                // set the cursor to "crosshair" for the entire control and to "Hand" whenever a scene-element is hovered
                Att.Style [Css.Cursor "crosshair"]
                Sg.Cursor Aardvark.Application.Cursor.None
                
                // setup transformation and shaders
                let rotActive = cval true
                Sg.Scale 3.0
                Sg.Trafo (rotationTrafo rotActive time)
                Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }  
                
             

                // scene      
                sg {
                    // whenever something is hovered udpate the marker-arrow
                    Sg.OnPointerEnter(fun e ->
                        printfn "enter group"
                        transact (fun () -> 
                            markerColor.Value <- C4b.Red
                            rotActive.Value <- false
                        )
                    )
                    Sg.OnPointerLeave(fun e ->
                        printfn "leave group"
                        transact (fun () -> 
                            markerColor.Value <- C4b.Gray
                            rotActive.Value <- true
                        )
                    )
                    Sg.OnPointerMove(fun e ->
                        let m = e.ModelTrafo
                        transact (fun () -> 
                            marker.Value <- Ray3d(e.Position, e.Normal)
                        )
                    )

                    // render a teapot and a sphere
                    sg {
                        Translate(0.5, 0.0, 0.0)
                        Primitives.Teapot(C4b.Green)
                    }

                    sg {
                        Primitives.WireSphere(V3d(-0.5, 0.0, 0.25), 0.25)
                    }
                    
                    sg {
                        Scale 0.5
                        Translate(0.0, 0.5, 0.0)
                        Primitives.Tetrahedron()
                    }
                }

                // render the arrow
                sg {
                    Sg.NoEvents
                    let len = 0.1
                    let h = 0.03
                    let radius = 0.005
                    Primitives.Cone(marker |> AVal.map (fun r -> Cone3d(r.GetPointOnRay len, -r.Direction * h, Constant.PiQuarter / 2.0)), markerColor)
                    Primitives.Cylinder(marker |> AVal.map (fun r -> Cylinder3d(r.Origin, r.GetPointOnRay (len - h), radius)), markerColor)
                }

            }


        }
        
    task {
        let checks =
            [
                $"let supported = {{}};"
                for t in RemoteHtmlBackend.ImageTransfers do
                    if t.IsSupported runtime then
                        let code = t.ClientCheck |> String.concat "\n"
                        $"supported[\"{t.GetType().AssemblyQualifiedName}\"] ="
                        $"(function() {{"
                        $"{code}"
                        $"}})();"
                $"aardvark.send(aardvark.stringify(supported));"
            ]
        do! ctx.Execute (String.concat "\n" checks)
        let! msg = ctx.Receive()

        let transfer = 
            match msg with
            | Message.Text str -> 
                let doc = System.Text.Json.JsonDocument.Parse(str)
                RemoteHtmlBackend.ImageTransfers |> List.pick (fun t ->
                    let name = t.GetType().AssemblyQualifiedName
                    match doc.RootElement.TryGetProperty name with
                    | (true, p) -> 
                        if p.GetBoolean() then
                            Some t
                        else
                            None
                    | _ -> None
                )
            | _ ->  
                failwithf "unexpected initial message: %A" msg

        Log.line "using %s" (transfer.GetType().Name)
        let backend = RemoteHtmlBackend(runtime, server, transfer)
        let state = { token = AdaptiveToken.Top; runtime = runtime }

    

        let reader =
            task {
                while true do
                    let! msg = ctx.Receive()
                    match msg with
                    | Message.Text json ->
                        try
                            let msg = System.Text.Json.JsonDocument.Parse(json).RootElement
                            let id = msg.GetProperty("source").GetInt64()
                            let typ = msg.GetProperty("type").GetString()
                            let data = msg.GetProperty "data"
                            backend.RunCallback(id, typ, data)
                        with _ ->
                            ()
                    | _ ->
                        ()
            }

        let u = Updater.Body(runtime, view, backend :> IHtmlBackend<_>)
        return! 
            u |> Updater.run state backend (fun code ->
                ctx.Execute code
            )
    }




[<Struct>]
type HMap(store : HashMap<Symbol, obj>) =
    static member Empty = HMap HashMap.empty

    member x.Add(key : TypedSymbol<'a>, value : 'a) =
        HMap(HashMap.add key.Symbol (box value) store)

    member x.Remove(key : TypedSymbol<'a>) =
        HMap(HashMap.remove key.Symbol store)

    member x.TryRemove(key : TypedSymbol<'a>) =
        match HashMap.tryRemove key.Symbol store with
        | Some(value, rest) ->
            Some(value :?> 'a, HMap rest)
        | _ ->
            None

    member x.TryFind(key : TypedSymbol<'a>) =
        match HashMap.tryFind key.Symbol store with
        | Some r -> Some (r :?> 'a)
        | None -> None

module HMap =
    let empty = HMap.Empty

    let add (key : TypedSymbol<'a>) (value : 'a) (map : HMap) =
        map.Add(key, value)
        
    let remove (key : TypedSymbol<'a>) (map : HMap) =
        map.Remove(key)

    let tryRemove (key : TypedSymbol<'a>) (map : HMap) =
        map.TryRemove(key)

    let tryFind (key : TypedSymbol<'a>) (map : HMap) =
        map.TryFind(key)

[<EntryPoint>]
let main _ =
  
    Aardvark.Init()

    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseSockets()
                    .Configure(fun app -> app.UseWebSockets().UseGiraffe (webApp myView))
                    .ConfigureServices(fun s -> s.AddGiraffe() |> ignore)
                    |> ignore)
        .Build()
        .Run()
    0