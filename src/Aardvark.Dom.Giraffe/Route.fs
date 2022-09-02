namespace Aardvark.Dom.Remote

open System
open System.Text
open System.Web
open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open Microsoft.AspNetCore.Http
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Dom
open Aardvark.Dom.Remote
open FSharp.Data.Adaptive
open Giraffe

module DomNode =

    [<AutoOpen>]
    module private Utilities = 
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

        type Context =
            {
                Connection : string
                Receive : unit -> Task<ChannelMessage>
                Execute : string -> Task
                Shutdown : unit -> unit
            }

        let js (str : string) : HttpHandler =
            let bytes = Encoding.UTF8.GetBytes str
            fun (_ : HttpFunc) (ctx : HttpContext) ->
                ctx.SetContentType "text/javascript"
                ctx.WriteBytesAsync bytes


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

        let runUpdater (state : UpdateState<int64>) (b : RemoteHtmlBackend) (action : string -> Task) (updater : Updater<int64>) =
            let mutable running = true
            let signal = AsyncSignal(true)
            let sub = updater.AddMarkingCallback signal.Pulse

            task {
                while running do
                    do! signal.Wait()
                    if running then
                        do! Async.SwitchToThreadPool()
                        try
                            updater.Update(state, b)
                            let code = b.GetCode().Trim() 
                            if code <> "" then
                                do! action code
                        with e ->
                            Log.warn "update failed: %A" e
                sub.Dispose()

            }

        let runApp (server : IServer) (runtime : IRuntime) (view : DomNode) (ctx : Context) =
   
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
                    | ChannelMessage.Text str -> 
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
                            | ChannelMessage.Text json ->
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
                    u |> runUpdater state backend (fun code ->
                        ctx.Execute code
                    )
            }

    let toRoute (runtime : IRuntime) (view : IRuntime -> DomNode) =
    
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
            route "/aardvark-dom.js" >=> js RemoteHtmlBackend.AardvarkDomJavascript
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
                                    return ChannelMessage.Text (Encoding.UTF8.GetString data)
                                | WebSocketMessageType.Binary ->
                                    return ChannelMessage.Binary data
                                | _ ->
                                    return! receive()
                            }
                    
                        try
                            do! runApp server runtime (view runtime) {
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




