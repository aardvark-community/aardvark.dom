namespace Aardvark.Dom.Remote

open System.Web
open System.Text
open System.Net.WebSockets
open System.Text.Json
open System.Collections.Generic
open FSharp.Data.Adaptive
open Aardvark.Base
open System.Reflection
open System.Threading.Tasks
open Aardvark.Dom

type CodeBuilder(variables : System.Collections.Generic.Dictionary<int64, string>) =
    let builder = System.Text.StringBuilder()

    new() = CodeBuilder(System.Collections.Generic.Dictionary())

    member x.Variables = variables

    member x.AppendLine (code : string) =
        builder.AppendLine code |> ignore

    override x.ToString() =
        builder.ToString()

    member x.Clear() =
        builder.Clear() |> ignore
        variables.Clear()
    
    member x.GetOrCreateVar(id : int64, def : bool) =
        match variables.TryGetValue id with
        | (true, v) -> 
            v
        | _ ->
            if def then builder.AppendLine $"let n{id} = aardvark.get({id});" |> ignore
            let n = $"n{id}"
            variables.[id] <- n
            n

    
    member x.GetOrCreateVar(id : int64) =
        x.GetOrCreateVar(id, true)


type IServer =
    abstract RegisterWebSocket : (System.Net.WebSockets.WebSocket -> Task<unit>) -> string * System.IDisposable
    abstract RegisterResource : mime : string * content : string -> string


type RemoteHtmlBackend(server : IServer, eventListeners : Dict<int64, Dict<string, System.Text.Json.JsonElement -> bool>>, currentId : ref<int>, parentIds : Dict<int64, int64>, childIds : Dict<int64, DictSet<int64>>, code : CodeBuilder) =
    static let bstr (b : bool) =
        match b with
        | true -> "true"
        | false -> "false"


    let newId() =
        System.Threading.Interlocked.Increment(&currentId.contents)

    let addChild(parent : int64) (child : int64) =
        parentIds.[child] <- parent
        let s = childIds.GetOrCreate(parent, fun _ -> DictSet())
        s.Add child |> ignore

    let rec destroyRefs (node : int64) =
        match childIds.TryRemove node with
        | (true, cs) -> cs |> Seq.iter destroyRefs
        | _ -> ()
        parentIds.Remove node |> ignore

    new(server : IServer) = RemoteHtmlBackend(server, Dict(), ref 0, Dict(), Dict(), CodeBuilder())

    interface IHtmlBackend<int64> with

        member x.Root = 0L

        member x.Delay(action : IHtmlBackend<int64> -> unit) =
            let inner = RemoteHtmlBackend(server, eventListeners, currentId, parentIds, childIds, CodeBuilder(System.Collections.Generic.Dictionary code.Variables))
            action inner
            inner.GetCode() :> obj

        member x.Run(thing : obj) =
            code.AppendLine (thing :?> string)

        member x.SetupRenderer(element : int64, scene : SceneHandler) =
            let x = x :> IHtmlBackend<int64>
            let var = code.GetOrCreateVar element

            let required = Set.ofList [ "https" ]

            let url =
                server.RegisterResource(
                    "application/javascript",
                    String.concat "\n" [
                        "aardvark.setupRenderer = function(node, url) {"
                        "   var ws = new WebSocket(url);"
                        "   ws.onopen = function() {"
                        "       let r = node.getBoundingClientRect();"
                        "       ws.send(JSON.stringify({ X: r.width, Y: r.height }));"
                        "   };"
                        "   ws.onerror = function() {"
                        "       console.error(\"connection error\");"
                        "   };"
                        "   ws.onmessage = function(e) {"
                        "       console.warn(e.data);"
                        "   }"
                        "};"
                            
                    ]
                )

            let path, dispose = 
                server.RegisterWebSocket (fun socket ->
                    task {
                        let! (typ, content) = socket.ReceiveMessage()
                        match typ with
                        | WebSocketMessageType.Text ->
                            let json = Encoding.UTF8.GetString content
                            printfn "%A" json
                        | _ ->
                            ()
                        do! socket.Send "nice, thanks!"

                        return ()
                    }
                )

            x.Require(Set.add url required, fun _ ->
                code.AppendLine $"aardvark.setupRenderer({var}, aardvark.relativePath(\"ws\", \"{path}\"));"
            )
            
        member x.DestroyRenderer(element : int64, scene : SceneHandler) =
            ()

        member x.Require(urls : Set<string>, action : IHtmlBackend<int64> -> unit) =
            if Set.isEmpty urls then
                action x
            else
                let str = urls |> Seq.map (sprintf "\"%s\"") |> String.concat ", "
                code.AppendLine $"aardvark.require([{str}], () => {{"
                action x
                code.AppendLine "});"

        member x.Execute(this : option<int64>, channels : array<IChannel -> Task<unit>>, js : array<string> -> list<string>) =
            //let bootCode arr = 
            //    match this with
            //    | Some this ->
            //        let var = code.GetOrCreateVar this
            //        js arr |> List.map (fun str -> str.Replace("__THIS__", var) |> sprintf "{ %s }") |> String.concat "\n" 
            //    | None ->
            //        js arr |> List.map (fun str -> str |> sprintf "{ %s }") |> String.concat "\n" 
                   
            let urls = 
                channels |> Array.map (fun action -> 
                    server.RegisterWebSocket(fun socket ->
                        let channel =
                            { new IChannel with
                                member x.Send(msg) =
                                    match msg with
                                    | ChannelMessage.Binary a -> task { do! socket.Send a }
                                    | ChannelMessage.Text a -> task { do! socket.Send a }
                                    | ChannelMessage.Close -> task { do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "close", Unchecked.defaultof<_>) }
                                member x.Receive() =
                                    task {
                                        let! (typ, content) = socket.ReceiveMessage()
                                        match typ with
                                        | WebSocketMessageType.Binary -> return ChannelMessage.Binary content
                                        | WebSocketMessageType.Text -> return ChannelMessage.Text (Encoding.UTF8.GetString content)
                                        | WebSocketMessageType.Close -> return ChannelMessage.Close
                                        | _ -> return ChannelMessage.Text "bad"
                                    }
                            }
                        action channel
                    )
                )
                
            let newName() = 
                "__channel" + string(System.Guid.NewGuid()).Replace("-", "")

            let names = urls |> Array.map (fun _ -> newName())

            let prefix = 
                (names, urls) ||> Array.map2 (fun n (u,_) -> 
                    $"const {n} = new WebSocket(aardvark.relativePath(\"ws\", \"{u}\"));"
                )   
                |> Array.toList

            let realCode = 
                let js = prefix @ js names
                match this with
                | Some this ->
                    let var = code.GetOrCreateVar this
                    js |> List.map (fun str -> str.Replace("__THIS__", var)) |> String.concat "\n" |> sprintf "{ %s }"
                | None ->
                    js |> String.concat "\n" |> sprintf "{ %s }"
                   
            code.AppendLine(realCode)
            { new System.IDisposable with
                member x.Dispose() =
                    urls |> Array.iter (fun (_, d) -> d.Dispose())
            }

        member x.CreateElement(tag : string) =
            let id = newId()
            let var = code.GetOrCreateVar(id, false)
            code.AppendLine $"let {var} = aardvark.createElement(\"{tag}\", {id});"
            id

        member x.CreateTextElement() =
            let id = newId()
            let var = code.GetOrCreateVar(id, false)
            code.AppendLine $"let {var} = aardvark.createTextNode({id});"
            id

        member x.RemoveAttribute(node : int64, name : string) =
            let var = code.GetOrCreateVar node
            let attName =
                match name with
                | "classList" -> "class"
                | "className" -> "class"
                | n -> n.ToLower()
            code.AppendLine $"{var}.{name} = null; {var}.removeAttribute(\"{attName}\");"
            
        member x.SetAttribute(node : int64, name : string, value : string) =
            let var = code.GetOrCreateVar node
            code.AppendLine $"{var}.{name} = \"{HttpUtility.JavaScriptStringEncode(value)}\";"
            
        member x.SetAttribute(node : int64, name : string, value : Set<string>) =
            let value = String.concat " " value
            let var = code.GetOrCreateVar node

            let attName =
                match name with
                | "classList" -> "className"
                | n -> n

            code.AppendLine $"{var}.{attName} = \"{HttpUtility.JavaScriptStringEncode(value)}\";"
            
        member x.InsertFirst(parent : int64, node : int64) =
            addChild parent node
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.insertFirst({parent}, {c});"
            
        member x.AppendChild(parent : int64, node : int64) =
            addChild parent node
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.appendChild({parent}, {c});"
            
        member x.InsertAfter(ref : int64, node : int64) =
            match parentIds.TryGetValue ref with
            | (true, p) -> addChild p node
            | _ -> ()
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.insertAfter({ref}, {c});"
            
        member x.InsertBefore(ref : int64, node : int64) =
            match parentIds.TryGetValue ref with
            | (true, p) -> addChild p node
            | _ -> ()
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.insertBefore({ref}, {c});"

        member x.Remove(node : int64) =
            destroyRefs node
            let var = code.GetOrCreateVar node
            code.AppendLine $"aardvark.delete({node}, {var});"
            eventListeners.Remove node |> ignore

        member x.SetListener(element, name, capture, pointerCapture, evtType, callback) =
            let var = code.GetOrCreateVar element
          
            let body =
                String.concat "" [
                    "if(e.bubbles) e.stopImmediatePropagation();"
                    "e.preventDefault();"
                    if pointerCapture then
                        match name.ToLower() with
                        | "pointerdown" -> $"{var}.setPointerCapture(e.pointerId);"
                        | "pointerup" -> $"{var}.releasePointerCapture(e.pointerId);"
                        | _ -> ()
                    $"aardvark.trigger((e.bubbles ? aardvark.getEventTargetId(e) : {element}), \"{name}\", e);"
                ]
            // aardvark.setListener = function (node, type, action, capture) {
            code.AppendLine $"aardvark.setListener({var}, \"{name}\", ((e) => {{ {body} }}), false);"

            // TODO: perfromance concerns here
            let m = evtType.GetMethod("TryParse", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            if isNull m then failwithf "non-parsable event type: %A" evtType
            let t = typedefof<option<_>>.MakeGenericType [| evtType |]
            let pValue = t.GetProperty("Value")
            let l = eventListeners.GetOrCreate(element, fun _ -> Dict())

            let triggerName =
                if capture then name + "_capture"
                else name + "_bubble"


            l.[triggerName] <- fun json ->  
                // parse json
                let e = m.Invoke(null, [| json :> obj |])
                if isNull e then true
                else
                    let e = pValue.GetValue e :?> Event
                    callback e


        member x.RemoveListener(element, name, capture) =
            let triggerName =
                if capture then name + "_capture"
                else name + "_bubble"

            let otherListener =
                if capture then name + "_bubble"
                else name + "_capture"

            match eventListeners.TryGetValue element with
            | (true, l) -> 
                if l.Remove(triggerName) then
                    let var = code.GetOrCreateVar element
                    if not (l.Contains otherListener) then 
                        code.AppendLine $"aardvark.removeListener({var}, \"{name}\", false);"
                    if l.Count = 0 then eventListeners.Remove element |> ignore
            | _ ->
                ()

    
    member x.RunCallback(srcId : int64, name : string, data : System.Text.Json.JsonElement) =

        let rec runCapture (captures : list<JsonElement -> bool>) (nodeId : int64) =
            match parentIds.TryGetValue nodeId with
            | (true, pid) ->
                match eventListeners.TryGetValue nodeId with
                | (true, evts) ->
                    match evts.TryGetValue $"{name}_capture" with
                    | (true, cb) -> runCapture (cb :: captures) pid
                    | _ -> runCapture captures pid
                | _ ->
                    runCapture captures pid
            | _ ->
                match eventListeners.TryGetValue nodeId with
                | (true, evts) ->
                    let cont = 
                        match evts.TryGetValue $"{name}_capture" with
                        | (true, cb) -> cb data
                        | _ -> true
                    
                    if cont then
                        captures |> List.forall (fun cb -> cb data)
                    else
                        false
                | _ ->
                    captures |> List.forall (fun cb -> cb data)

        let rec runBubble (nodeId : int64) =
            
            let cont =
                match eventListeners.TryGetValue nodeId with
                | (true, evts) ->
                    match evts.TryGetValue $"{name}_bubble" with
                    | (true, cb) -> cb data
                    | _ -> true
                | _ ->
                    true

            if cont then
                match parentIds.TryGetValue nodeId with
                | (true, pid) -> runBubble pid
                | _ -> cont
            else
                false

        let bubbles = 
            match data.TryGetProperty "bubbles" with
            | (true, prop) ->
                try prop.GetBoolean()
                with _ -> false
            | _ ->
                false

        if bubbles then
            if runCapture [] srcId then
                runBubble srcId |> ignore
        else
            match eventListeners.TryGetValue srcId with
            | (true, evts) ->
                match evts.TryGetValue $"{name}_capture" with
                | (true, cb) -> cb data |> ignore
                | _ -> ()

                match evts.TryGetValue $"{name}_bubble" with
                | (true, cb) -> cb data |> ignore
                | _ -> ()
            | _ ->
                ()

    member x.GetCode() =
        try code.ToString()
        finally code.Clear()
