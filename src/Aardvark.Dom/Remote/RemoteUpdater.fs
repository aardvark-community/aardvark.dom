namespace Aardvark.Dom.Remote

open System.Web
open System.Text
open System.Net.WebSockets
open System.Text.Json
open System.Collections.Generic
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open System.Reflection
open System.Threading.Tasks
open Aardvark.Dom
open System.Threading

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

    let subscribedListeners = Dict<int64 * string, ref<int>>()

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

            let disp =
                x.Execute(Some element, 
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
                        
                                let t0 = System.DateTime.Now
                                let dt = System.Diagnostics.Stopwatch.StartNew()
                                let size = cval size

                                let render = new JpegRenderTarget(scene.Runtime, scene.FramebufferSignature, scene.RenderTask, size.Value, quality)

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
                                                let data = render.Run(AdaptiveToken.Top)
                                                c.Send(ChannelMessage.Binary data).Result
                                                while sw.Elapsed.TotalMilliseconds < 16.66666666666 do
                                                    mm.Wait()
                                    
                                                transact (fun () -> scene.Time.Value <- t0 + dt.Elapsed)
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

            ()
            
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
            if name.Contains "-" then
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.setAttribute(\"{name}\", \"{HttpUtility.JavaScriptStringEncode(value)}\");"
            else
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
            let refCount = subscribedListeners.GetOrCreate((element, name), fun _ -> ref 0)
            if refCount.Value = 0 then
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

            refCount.Value <- refCount.Value + 1
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
            match subscribedListeners.TryGetValue((element, name)) with
            | (true, refCount) ->
                let triggerName =
                    if capture then name + "_capture"
                    else name + "_bubble"

                match eventListeners.TryGetValue element with
                | (true, l) -> 
                    if l.Remove(triggerName) then
                        let var = code.GetOrCreateVar element
                        if refCount.Value = 1 then 
                            code.AppendLine $"aardvark.removeListener({var}, \"{name}\", false);"
                            subscribedListeners.Remove((element,name)) |> ignore
                        if l.Count = 0 then eventListeners.Remove element |> ignore
                | _ ->
                    ()

                refCount.Value <- refCount.Value - 1
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
