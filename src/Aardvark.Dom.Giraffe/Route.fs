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
                "       <script>aardvark.connect(aardvark.connectWebSocket, aardvark.relativePath(\"ws\", \"/socket\"));</script>"
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
                ExecuteWithCallback : string -> (System.Text.Json.JsonElement -> unit)-> Task
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
                    match tcs with
                    | Some t -> 
                        t.SetResult()
                        tcs <- None
                        isSet <- false
                    | None -> 
                        isSet <- true
                )
            
            member x.Poll() =
                lock x (fun () ->
                    if isSet then
                        isSet <- false
                        true
                    else
                        false
                )
            
            member x.Wait(ct : CancellationToken) =
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

                            if ct.CanBeCanceled then
                                Async.StartAsTask(Async.AwaitTask(t.Task), cancellationToken = ct)
                            else
                                t.Task
                )
                
            member x.Wait() = x.Wait(CancellationToken.None)
            
     
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

                            // done

                        with e ->
                            Log.warn "update failed: %A" e
                sub.Dispose()

            }

            
        let runApp (server : IServer) (runtime : IRuntime) (view : DomContext -> DomNode * IDisposable) (ctx : Context) =
   
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

                let appCtx = 
                    { 
                        Runtime = runtime
                        Execute = fun code cb ->
                            match cb with
                            | None -> ctx.Execute code |> ignore
                            | Some cb -> ctx.ExecuteWithCallback code cb |> ignore
                    }

                let dom, disp = view appCtx
                use _ = disp
                let u = Updater.Body(runtime,dom, backend :> IHtmlBackend<_>)
                return! 
                    u |> runUpdater state backend (fun code ->
                        ctx.Execute code
                    )

                
            }

    let private toChannel (socket : WebSocket) =
        let e = Event<unit>()
        { new IChannel with
            member x.OnClose = e.Publish
            member x.Send(msg) =
                match msg with
                | ChannelMessage.Binary a -> task { do! socket.Send a }
                | ChannelMessage.Text a -> task { do! socket.Send a }
                | ChannelMessage.Close -> 
                    e.Trigger()
                    task { do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "close", Unchecked.defaultof<_>) }
            member x.Receive() =
                task {
                    let! (typ, content) = socket.ReceiveMessage()
                    match typ with
                    | WebSocketMessageType.Binary -> return ChannelMessage.Binary content
                    | WebSocketMessageType.Text -> return ChannelMessage.Text (Encoding.UTF8.GetString content)
                    | WebSocketMessageType.Close -> 
                        e.Trigger()
                        return ChannelMessage.Close
                    | _ -> return ChannelMessage.Text "bad"
                }
        }

    let toRoute (runtime : IRuntime) (view : DomContext -> DomNode * IDisposable) =
    
        let sockets = Dict<string, WebSocket -> Task<unit>>()
        let resources = Dict<string, string * byte[]>()
        let resourceIds = Dict<Guid, string>()


        let server =
            {
                new IServer with
                    //member x.RegisterChannel(action : IChannel -> Task<unit>, js : string) =
                    
                    member x.RegisterWebSocket(action : IChannel -> Task<unit>) =
                        lock sockets (fun () ->
                            let id = Guid.NewGuid() |> string
                            let mutable connections = System.Collections.Generic.List()
                            sockets.[id] <- fun w -> 
                                connections.Add w
                                action (toChannel w)
                            let url = sprintf "registered/%s" id
                            url, 
                            { new System.IDisposable with 
                                member x.Dispose() = 
                                    lock sockets (fun () -> sockets.Remove id |> ignore) 
                                    for c in connections do
                                        if c.State = WebSocketState.Open || c.State = WebSocketState.Connecting then
                                            try c.CloseAsync(WebSocketCloseStatus.NormalClosure, "close", Unchecked.defaultof<_>).Wait()
                                            with _ -> ()
                                        c.Dispose()
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

                        let mutable messageId = 0
                        let newId() = Interlocked.Increment &messageId
                        let callbackTable = System.Collections.Concurrent.ConcurrentDictionary<int, System.Text.Json.JsonElement -> unit>()
                        
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
                    
                        let receive () =
                            task {
                                let mutable result = None
                                while Option.isNone result do
                                    let! typ, data = ws.ReceiveMessage()
                                    match typ with
                                    | WebSocketMessageType.Text -> 
                                        if data.[0] = uint8 '#' then
                                            task {
                                                let id = System.Int32.Parse(Encoding.UTF8.GetString(data.[1 .. 8]), System.Globalization.NumberStyles.HexNumber)
                                                let json = Encoding.UTF8.GetString(data, 9, data.Length - 9)

                                                match callbackTable.TryRemove id with
                                                | (true, cb) -> 
                                                    let data = 
                                                        try System.Text.Json.JsonDocument.Parse(json).RootElement
                                                        with _ -> System.Text.Json.JsonElement()
                                                    cb data
                                                | _ ->
                                                    ()
                                            } |> ignore

                                        else
                                            result <- Some (ChannelMessage.Text (Encoding.UTF8.GetString data))
                                    | WebSocketMessageType.Binary ->
                                        result <- Some( ChannelMessage.Binary data)
                                    | _ ->
                                        ()
                                return Option.get result
                            }
                    

                        let run (code : string) (callback : System.Text.Json.JsonElement -> unit) = 
                            let mid = newId()
                            let midStr = sprintf "%08X" mid
                            callbackTable.[mid] <- callback
                            let body =
                                String.concat "\n" [
                                    $"(function() {{"
                                    $"  function run() {{"
                                    $"      {code}"
                                    $"  }}"
                                    $"  let res = run();"
                                    $"  if(!res) res = null;"
                                    $"  aardvark.send('#{midStr}' + aardvark.stringify(res));"
                                    $"}})();"
                                ]
                            execute body
                                

                                
                        try
                            do! runApp server runtime view {
                                Connection = conn
                                Receive = receive
                                Execute = execute
                                ExecuteWithCallback = run
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




