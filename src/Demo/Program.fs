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
        "<head>"
        "<script src='./aardvark-dom.js'></script>"
        "</head>"
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

let webApp (run : Context -> Task<unit>) =
    choose [
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
                        do! run {
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


// let myApp =
//     webApp (fun ctx ->
//         ignore (
//             task {
//                 while true do
//                     match! ctx.Receive() with
//                     | Text data ->
//                         printfn "received: %A" data
//                     | Binary data ->    
//                         printfn "received: %A" data
//             }
//         )

//         task {
//             let sw = System.Diagnostics.Stopwatch.StartNew()
//             let mutable index = 0
//             while true do
//                 do! Task.Delay 1000
//                 do! ctx.Execute(
//                     String.concat "\n" [
//                         $"let a = document.createElement(\"h1\");"
//                         $"a.innerHTML = \"%.0f{sw.Elapsed.TotalMilliseconds}ms\";"
//                         $"document.body.appendChild(a);"
//                         $"a.addEventListener(\"mousedown\", (e) => {{ aardvark.send(\"hello from {index}\"); e.preventDefault() }}, true);"
//                     ]
//                 )
//                 index <- index + 1
//         }
//     )

open Aardvark.Dom
open FSharp.Data.Adaptive
open Aardvark.Base


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
                        let t = TaskCompletionSource<unit>()
                        tcs <- Some t
                        t.Task
            )

module Updater =

    let run (state : UpdateState) (action : string -> Task) (updater : Updater) =
        let mutable running = true
        let signal = AsyncSignal(true)
        let sub = updater.AddMarkingCallback signal.Pulse

        task {
            let b = CodeBuilder()
            while running do
                do! signal.Wait()
                if running then
                    do! Async.SwitchToThreadPool()
                    b.Clear()
                    updater.Update(state, b)
                    let code = b.ToString().Trim() 
                    if code <> "" then
                        do! action code

        }


let myView (ctx : Context)=
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

                

                OnContextMenu(click, preventDefault = true, useCapture = true, stopPropagation = true)
                OnClick(click, true)

                OnPointerDown (fun e ->
                    transact (fun () -> down.Add e.PointerId |> ignore)
                    printfn "down\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                )
                
                OnPointerUp (fun e ->
                    transact (fun () -> down.Remove e.PointerId |> ignore)
                    printfn "up\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
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
                    $"pointer{d}"
                }
            )

            // DomNode.Element(
            //     "h1", AttributeMap.ofList [ OnClick click ],
            //     AList.ofList [
            //         DomNode.Text(content |> AVal.map string)
            //     ]
            // )
        }
        
    let state = { eventHandlers = System.Collections.Generic.Dictionary(); token = AdaptiveToken.Top }
    let reader =
        task {
            while true do
                let! msg = ctx.Receive()
                match msg with
                | Message.Text json ->
                    try
                        let msg = System.Text.Json.JsonDocument.Parse(json).RootElement
                        let id = msg.GetProperty("source").GetInt64()
                        match state.eventHandlers.TryGetValue id with
                        | (true, handlers) ->
                            let typ = msg.GetProperty("type").GetString()
                            match handlers.TryGetValue typ with
                            | (true, handler) ->
                                handler (msg.GetProperty "data")
                            | _ ->
                                ()
                        | _ -> 
                            ()
                    with _ ->
                        ()
                | _ ->
                    ()
        }

    let u = Updater.Body(view)
    u |> Updater.run state (fun code ->
        ctx.Execute code
    )







[<EntryPoint>]
let main _ =
  

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