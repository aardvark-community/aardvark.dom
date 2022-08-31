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
open Aardvark.Dom
open Aardvark.Dom.Remote

#nowarn "3511"

type Marker = class end

[<AbstractClass; Sealed; Extension>]
type WebSocketExtensions private() =
    [<Extension>]
    static member ReceiveMessage(socket : WebSocket, ct : CancellationToken) =
        task {
            let mutable buffer = Array.zeroCreate 4096
            let! res = socket.ReceiveAsync(ArraySegment buffer, ct)
            let typ = res.MessageType
            if res.EndOfMessage then
                if res.Count < buffer.Length then Array.Resize(&buffer, res.Count)
                return typ, buffer
            else
                let mutable size = res.Count
                let mutable fin = false
                while not fin do
                    let rem = buffer.Length - size
                    if rem < 2048 then 
                        let cap = buffer.Length * 2
                        Array.Resize(&buffer, cap)
                    
                    let! res = socket.ReceiveAsync(ArraySegment(buffer, size, buffer.Length - size), ct)
                    size <- size + res.Count
                    if res.EndOfMessage then    
                        fin <- true

                if size < buffer.Length then Array.Resize(&buffer, size)
                return typ, buffer
        }
    
    [<Extension>]
    static member ReceiveMessage(socket : WebSocket) =
        socket.ReceiveMessage(CancellationToken.None)

    [<Extension>]
    static member Send(socket : WebSocket, message : string) =
        let bytes = Encoding.UTF8.GetBytes message
        socket.SendAsync(ArraySegment bytes, WebSocketMessageType.Text, true, CancellationToken.None)

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

let webApp (run : IServer -> Aardvark.Rendering.IRuntime -> Context -> Task<unit>) =
    
    let app = new Aardvark.Rendering.Vulkan.HeadlessVulkanApplication()
    let sockets = Dict<string, WebSocket -> Task<unit>>()
    let resources = Dict<string, string * byte[]>()
    let resourceIds = Dict<string * string, string>()

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

                member x.RegisterResource(mime : string, content : string) =
                    lock resources (fun () ->
                        match resourceIds.TryGetValue ((mime, content)) with
                        | (true, url) -> 
                            url
                        | _ -> 
                            let id = Guid.NewGuid() |> string

                            let ext =
                                match mime with
                                | "text/css" -> ".css"
                                | "application/javascript" -> ".js"
                                | _ -> ""

                            let name = sprintf "%s%s" id ext
                            let url = sprintf "/registered/%s" name
                            resources.[name] <- (mime, System.Text.Encoding.UTF8.GetBytes content)
                            resourceIds.[(mime,content)] <- url
                            url
                    )
        }

    choose [
        routeStartsWithf "/registered/%s" (fun id next ctx -> 
            printfn "%A" id
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
        //route "/registered" >=> (fun next ctx -> 
        //    match ctx.Request.Query.TryGetValue "id" with
        //    | (true, id) ->
        //        match ctx.WebSockets.IsWebSocketRequest with
        //        | true ->
        //            task {
        //                match lock sockets (fun () -> sockets.TryGetValue id) with
        //                | (true, handler) ->
        //                    let! (ws : WebSocket) = ctx.WebSockets.AcceptWebSocketAsync()
        //                    do! handler ws
        //                    return! next ctx
        //                | _ ->
        //                    ctx.Response.StatusCode <- 404
        //                    return! next ctx
                            
        //            }
        //        | false ->
        //            match lock resources (fun () -> resources.TryGetValue id) with
        //            | (true, (mime, content)) -> 
        //                ctx.SetContentType mime
        //                ctx.WriteBytesAsync content
        //            | _ ->
        //                ctx.Response.StatusCode <- 400
        //                next ctx
        //    | _ ->
        //        ctx.Response.StatusCode <- 400
        //        next ctx
        //)
        
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


open Aardvark.Rendering

type JpegRenderTarget(runtime : IRuntime, getTask : IFramebufferSignature -> IRenderTask, size : V2i, samples : int, quality : int) =
    inherit AdaptiveObject()

    let mutable clearTask : option<IRenderTask> = None
    let mutable task : option<IRenderTask> = None
    let mutable size = size
    let mutable samples = samples
    let mutable quality = quality
    let mutable signature : option<IFramebufferSignature> = None

    let mutable fbo : option<IFramebuffer * IRenderbuffer * IRenderbuffer> = None
    
    let getFramebufferSignature (samples : int) =
        match signature with
        | Some s when s.Samples = samples -> s
        | _ ->
            match signature with
            | Some s -> s.Dispose()
            | None -> ()

            let s = runtime.CreateFramebufferSignature([DefaultSemantic.Colors, TextureFormat.Rgba8; DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8], samples)
            signature <- Some s
            s

    let getFramebuffer (size : V2i) (samples : int) =
        match fbo with
        | Some (f,_,_) when f.Size = size && f.Signature.Samples = samples ->   
            f
        | _ ->
            match fbo with
            | Some (f, c, d) ->
                runtime.DeleteFramebuffer f
                runtime.DeleteRenderbuffer c
                runtime.DeleteRenderbuffer d
            | None ->
                ()
            let s = getFramebufferSignature samples
            let c = runtime.CreateRenderbuffer(size, TextureFormat.Rgba8, samples)
            let d = runtime.CreateRenderbuffer(size, TextureFormat.Depth24Stencil8, samples)
            let f = 
                runtime.CreateFramebuffer(s, [
                    DefaultSemantic.Colors, c :> IFramebufferOutput
                    DefaultSemantic.DepthStencil, d :> IFramebufferOutput
                ])

            fbo <- Some (f, c, d)
            f

    member x.Size
        with get() = size
        and set v = 
            if v <> size then
                size <- v
                x.MarkOutdated()
                
    member x.Samples
        with get() = samples
        and set v =
            if v <> samples then
                samples <- v
                x.MarkOutdated()
                
    member x.Quality
        with get() = quality
        and set v = 
            if v <> quality then
                quality <- v
                x.MarkOutdated()

    member x.Dispose() =
        match fbo with
        | Some (f, c, d) ->
            runtime.DeleteFramebuffer f
            runtime.DeleteRenderbuffer c
            runtime.DeleteRenderbuffer d
            fbo <- None
        | None ->
            ()

        match signature with
        | Some s -> 
            s.Dispose()
            signature <- None
        | None -> ()
        
        match clearTask with
        | Some t -> 
            t.Dispose()
            clearTask <- None
        | None ->
            ()

        match task with
        | Some t -> 
            t.Dispose()
            task <- None
        | None ->
            ()

    member x.Run(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            use __ = runtime.ContextLock
            let fbo = getFramebuffer size samples

            let clearTask = 
                match clearTask with
                | Some c -> c
                | None -> 
                    let t = runtime.CompileClear(fbo.Signature, clear { color C4f.Black; depth 1.0; stencil 0 })
                    clearTask <- Some t
                    t

            let task = 
                match task with
                | Some t -> t
                | None ->
                    let t = getTask fbo.Signature
                    task <- Some t
                    t

            clearTask.Run(token, RenderToken.Empty, fbo)
            task.Run(token, RenderToken.Empty, fbo)

            fbo.DownloadJpegColor(1.0, quality)
        )

    interface IDisposable with
        member x.Dispose() = x.Dispose()

let renderJpeg (runtime : IRuntime) (getRenderTask : IFramebufferSignature -> aval<V2i> -> aval<DateTime> -> Aardvark.Rendering.IRenderTask) =
    Attribute(
        "boot", 
        AttributeValue.Execute(
            [|     
                fun (c : IChannel) ->
                    task {     
                        let tryReadInfo (msg : ChannelMessage) =
                            match msg with
                            | ChannelMessage.Text a ->
                                try 
                                    let json = System.Text.Json.JsonDocument.Parse a
                                    let e = json.RootElement
                                    match e.TryGetProperty "cmd" with
                                    | (true, prop) ->
                                        match prop.GetString() with
                                        | "requestimage" ->
                                            let mutable s = V2i.II
                                            let mutable samples = 1
                                            let mutable quality = 80

                                            match e.TryGetProperty "width" with
                                            | (true, p) -> s.X <- max 1 (p.GetInt32())
                                            | _ -> ()
                                            
                                            match e.TryGetProperty "height" with
                                            | (true, p) -> s.Y <- max 1 (p.GetInt32())
                                            | _ -> ()

                                            match e.TryGetProperty "samples" with
                                            | (true, p) -> samples <- p.GetInt32()
                                            | _ -> ()

                                            match e.TryGetProperty "quality" with
                                            | (true, p) -> quality <- p.GetInt32()
                                            | _ -> ()

                                            Some (s, samples, quality)

                                        | _ ->
                                            None
                                    | _ ->
                                        None
                                with _ ->
                                    None
                            | _ ->
                                None

                        let mutable info = None
                        while Option.isNone info do
                            let! msg = c.Receive()
                            info <- tryReadInfo msg
                        let (size, samples, quality) = info.Value
                        
                        let t0 = DateTime.Now
                        let dt = System.Diagnostics.Stopwatch.StartNew()
                        let time = cval t0
                        let size = cval size

                        let rt s = getRenderTask s size time
                        let render = new JpegRenderTarget(runtime, rt, size.Value, samples, quality)

                        let mutable running = true

                        let renderDirty = MVar.create ()
                        let sub = render.AddMarkingCallback (MVar.put renderDirty)
                        let renderThread =
                            startThread <| fun () ->
                                let sw = System.Diagnostics.Stopwatch.StartNew()
                                let mm = new MultimediaTimer.Trigger(1)
                                while running do
                                    MVar.take renderDirty
                                    if running then
                                        printfn "render"
                                        let data = render.Run(AdaptiveToken.Top)
                                        c.Send(ChannelMessage.Binary data).Result
                                        while sw.Elapsed.TotalMilliseconds < 16.66666666666 do
                                            mm.Wait()
                                    
                                        transact (fun () -> time.Value <- t0 + dt.Elapsed)
                                        sw.Restart()

                        while running do
                            let! msg = c.Receive()
                            match msg with
                            | ChannelMessage.Close -> 
                                running <- false
                                MVar.put renderDirty ()
                            | msg -> 
                                match tryReadInfo msg with
                                | Some (newSize, samples, quality) ->
                                    transact (fun () ->
                                        size.Value <- newSize
                                        render.Size <- newSize
                                        render.Samples <- samples
                                        render.Quality <- quality
                                        //render.MarkOutdated()
                                    )
                                | None ->
                                    ()
                                    
                        sub.Dispose()
                        renderThread.Join()
                        render.Dispose()

                    }
                                
            |], 
            fun n ->
                let n = n.[0]
                [
                    $"{n}.binaryType = \"blob\";"
                    $"const img = document.createElement(\"img\");"
                    $"img.setAttribute(\"draggable\", \"false\");"
                    $"img.style.userSelect = \"none\";"
                    $"img.style.pointerEvents = \"none\";"
                    $"var requestImage = function() {{"
                    $"    const r = __THIS__.getBoundingClientRect();"
                    $"    const sam = parseInt(__THIS__.getAttribute(\"data-samples\") || 4);"
                    $"    const q = parseInt(__THIS__.getAttribute(\"data-quality\") || 80);"
                    $"    {n}.send(JSON.stringify({{ cmd: \"requestimage\", width: r.width, height: r.height, samples: sam, quality: q }}));"
                    $"}};" 
                    $"let unsub = (() => {{}});"
                    $"var start = function() {{"
                    $"      requestImage();"
                    $"      unsub = aardvark.onResize(__THIS__, () => {{ requestImage(); }});"
                    $"}};"
                    $"if({n}.readyState == 1) {{ start(); }}"
                    $"else {{ {n}.onopen = () => {{ start() }}; }}"
                    $"{n}.onmessage = function(e) {{"
                    $"    if(e.data instanceof Blob) {{"
                    $"        let url = URL.createObjectURL(e.data)"
                    $"        let o = img.src;"
                    $"        img.src = url;"
                    $"        if(o) {{ URL.revokeObjectURL(o); }}"
                    $"        requestImage();"
                    $"    }}"
                    $"}};"
                    $"{n}.onerror = function(e) {{"
                    $"  unsub();"
                    $"}}"
                    $"{n}.onclose = function(e) {{"
                    $"  unsub();"
                    $"}}"
                    $"__THIS__.appendChild(img);"
                    
                ]
        )
    )


open Aardvark.SceneGraph

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
                OnClick(click, true)
                content |> AVal.map string

            }

            ul {
                OnMouseEnter(fun e -> printfn "enter 0")
                OnMouseLeave(fun e -> printfn "leave 0")
                OnClick((fun e -> printfn "capture 0"; true), true)
                OnClick((fun e -> printfn "bubble 0"; true), false)
                li { "Hans" }
                li {
                    
                    OnMouseEnter(fun e -> printfn "enter 1")
                    OnMouseLeave(fun e -> printfn "leave 1")
                    OnClick((fun e -> printfn "capture 1"; true), true)
                    OnClick((fun e -> printfn "bubble 1"; false), false)
                    ul {
                        li { 
                            "Sepp" 
                            OnMouseEnter(fun e -> printfn "enter 2")
                            OnMouseLeave(fun e -> printfn "leave 2")
                            OnClick((fun e -> printfn "capture 2"; true), true)
                            OnClick((fun e -> printfn "bubble 2"; true), false)
                        }
                        li { "Franz" }
                    }
                }
            }


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
                OnClick(click, true)

                OnPointerDown (fun e ->
                    transact (fun () -> down.Add e.PointerId |> ignore)
                    //printfn "down\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                )
                
                OnPointerUp (fun e ->
                    transact (fun () -> down.Remove e.PointerId |> ignore)
                    //printfn "up\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                )

                down |> ASet.isEmpty |> AVal.map (function
                    | true -> []
                    | false -> 
                        [
                            OnPointerMove (fun e ->
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


            div {
                Style [Width "100%"; Height "600px"; Background "red"]

                renderJpeg runtime (fun signature size time ->
                    let sw = System.Diagnostics.Stopwatch.StartNew()
                    let r = AVal.constant Trafo3d.Identity //time |> AVal.map (fun t -> Trafo3d.RotationZ(sw.Elapsed.TotalSeconds))

                    let view = CameraView.lookAt (V3d(4,5,6)) V3d.Zero V3d.OOI |> CameraView.viewTrafo
                    let proj = size |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)

                    Sg.box' C4b.Red Box3d.Unit
                    |> Sg.trafo r
                    |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        do! DefaultSurfaces.simpleLighting
                    }
                    |> Sg.viewTrafo' view
                    |> Sg.projTrafo proj
                    |> Sg.compile runtime signature
                )
            }

            //DomNode.RenderControl(
            //    AttributeMap.ofList [Style [Width "200px"; Height "200px"; Background "red"]], 
            //    fun _ -> { View = AVal.constant Trafo3d.Identity; Proj = AVal.constant Trafo3d.Identity; Scene = Applicator([], ASet.empty) }
            //)

            // DomNode.Element(
            //     "h1", AttributeMap.ofList [ OnClick click ],
            //     AList.ofList [
            //         DomNode.Text(content |> AVal.map string)
            //     ]
            // )
        }
        
    let backend = RemoteHtmlBackend(server)
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

    let u = Updater.Body(view, backend :> IHtmlBackend<_>)
    u |> Updater.run state backend (fun code ->
        ctx.Execute code
    )







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