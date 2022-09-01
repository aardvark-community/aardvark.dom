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

type internal RemoteEventCallbacks() =
    let mutable capture : option<System.Text.Json.JsonElement -> bool> = None
    let mutable bubble : option<System.Text.Json.JsonElement -> bool> = None
    
    member x.Capture
        with get() = capture
        and set v = capture <- v
        
    member x.Bubble
        with get() = bubble
        and set v = bubble <- v

    member x.Item
        with get(cap : bool) =
            if cap then capture
            else bubble
        and set (cap : bool) value =
            if cap then capture <- value
            else bubble <- value

    member x.IsEmpty =
        Option.isNone capture && Option.isNone bubble
        
    member x.IsNonEmpty =
        Option.isSome capture || Option.isSome bubble

[<AbstractClass>]
type TransferImageRenderer() =
    inherit AdaptiveObject()
    abstract RenderFrame : token : AdaptiveToken -> ChannelMessage
    abstract Destroy : unit -> unit

    member x.Run(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            x.RenderFrame(token)
        )

    member private x.Dispose(disposing : bool) =
        if disposing then System.GC.SuppressFinalize x
        x.Destroy()

    member x.Dispose() = x.Dispose true
    override x.Finalize() = x.Dispose false
    interface System.IDisposable with
        member x.Dispose() = x.Dispose()

type IImageTransfer =
    abstract IsSupported : runtime : IRuntime -> bool
    abstract CreateRenderer : signature : IFramebufferSignature * scene : IRenderTask * size : aval<V2i> * quality : aval<int> -> TransferImageRenderer
    abstract Boot : channelName : string -> list<string>
    abstract Shutdown : channelName : string -> list<string>
    abstract ClientCode : messageName : string -> list<string>
    abstract ClientCheck : list<string> 

type RemoteHtmlBackend private(runtime : IRuntime, server : IServer, imageTransfer : IImageTransfer, eventListeners : Dict<int64, Dict<string, RemoteEventCallbacks>>, currentId : ref<int>, parentIds : Dict<int64, int64>, childIds : Dict<int64, DictSet<int64>>, code : CodeBuilder) =
    static let toSortedTransferList(m) =
        m
        |> HashMap.toValueList
        |> List.sortByDescending fst
        |> List.map snd


    static let mutable imageTransferTable = HashMap.empty
    static let mutable imageTransfers = []

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

    static member ImageTransfers = imageTransfers

    static member RegisterImageTransfer<'t when 't :> IImageTransfer and 't : (new : unit -> 't)>(priority : int) =
        imageTransferTable <- HashMap.add typeof<'t> (priority, new 't() :> IImageTransfer) imageTransferTable
        imageTransfers <- toSortedTransferList imageTransferTable

    new(runtime : IRuntime, server : IServer, imageTransfer : IImageTransfer) = RemoteHtmlBackend(runtime, server, imageTransfer, Dict(), ref 0, Dict(), Dict(), CodeBuilder())


    interface IHtmlBackend<int64> with

        member x.Root = 0L

        member x.Delay(action : IHtmlBackend<int64> -> unit) =
            let inner = RemoteHtmlBackend(runtime, server, imageTransfer, eventListeners, currentId, parentIds, childIds, CodeBuilder(System.Collections.Generic.Dictionary code.Variables))
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
                                                    let mutable quality = 80

                                                    match e.TryGetProperty "width" with
                                                    | (true, p) -> s.X <- max 1 (p.GetInt32())
                                                    | _ -> ()
                                            
                                                    match e.TryGetProperty "height" with
                                                    | (true, p) -> s.Y <- max 1 (p.GetInt32())
                                                    | _ -> ()

                                                    match e.TryGetProperty "quality" with
                                                    | (true, p) -> quality <- p.GetInt32()
                                                    | _ -> ()
                                                    
                                                    Some (s, quality)

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
                                let (size, quality) = info.Value
                        
                                let size = cval size
                                let quality = cval quality
                                let t0 = System.DateTime.Now
                                let dt = System.Diagnostics.Stopwatch.StartNew()
                              
                                let render = imageTransfer.CreateRenderer(scene.FramebufferSignature, scene.RenderTask, size, quality)

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
                                                c.Send(data).Result
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
                                        | Some (newSize, newQuality) ->
                                            transact (fun () ->
                                                size.Value <- newSize
                                                quality.Value <- newQuality
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
                            String.concat "\n" (imageTransfer.Boot n)
                            $"var requestImage = function() {{"
                            $"    const r = __THIS__.getBoundingClientRect();"
                            $"    const q = parseInt(__THIS__.getAttribute(\"data-quality\") || 80);"
                            $"    {n}.send(JSON.stringify({{ cmd: \"requestimage\", width: r.width, height: r.height, quality: q }}));"
                            $"}};" 
                            $"let unsub = (() => {{}});"
                            $"var start = function() {{"
                            $"      requestImage();"
                            $"      unsub = aardvark.onResize(__THIS__, () => {{ requestImage(); }});"
                            $"}};"
                            $"if({n}.readyState == 1) {{ start(); }}"
                            $"else {{ {n}.onopen = () => {{ start() }}; }}"
                            $"{n}.onmessage = function(e) {{"
                            String.concat "\n" (imageTransfer.ClientCode "e.data")
                            $"    requestImage();"
                            $"}};"
                            $"{n}.onerror = function(e) {{"
                            $"  unsub();"
                            String.concat "\n" (imageTransfer.Shutdown n)
                            $"}}"
                            $"{n}.onclose = function(e) {{"
                            $"  unsub();"
                            String.concat "\n" (imageTransfer.Shutdown n)
                            $"}}"
                    
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
            let listeners = eventListeners.GetOrCreate(element, fun _ -> Dict()).GetOrCreate(name, fun _ -> RemoteEventCallbacks())
            if listeners.IsEmpty then
                let var = code.GetOrCreateVar element
          
                let body =
                    String.concat "" [
                        "if(e.bubbles) e.stopImmediatePropagation();"
                        "e.preventDefault();"
                        if pointerCapture then
                            match name.ToLower() with
                            | "pointerdown" -> $"e.target.setPointerCapture(e.pointerId);"
                            | "pointerup" -> $"e.target.releasePointerCapture(e.pointerId);"
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


            let callback json =
                // parse json
                let e = m.Invoke(null, [| json :> obj |])
                if isNull e then true
                else
                    let e = pValue.GetValue e :?> Event
                    callback e

            listeners.[capture] <- Some callback

        member x.RemoveListener(element, name, capture) =   
            match eventListeners.TryGetValue element with
            | (true, elementListeners) ->
                match elementListeners.TryGetValue name with
                | (true, listeners) ->
                    listeners.[capture] <- None
                    if listeners.IsEmpty then
                        let var = code.GetOrCreateVar element
                        code.AppendLine $"aardvark.removeListener({var}, \"{name}\", false);"
                        eventListeners.Remove element |> ignore
                | _ ->
                    ()
            | _ ->
                ()

    
    member x.RunCallback(srcId : int64, name : string, data : System.Text.Json.JsonElement) =
        let bubbles = 
            match data.TryGetProperty "bubbles" with
            | (true, prop) ->
                try prop.GetBoolean()
                with _ -> false
            | _ ->
                false

        let rec getPath (acc : list<RemoteEventCallbacks>) (nodeId : int64) =
            let newAcc =
                match eventListeners.TryGetValue nodeId with
                | (true, l) ->
                    match l.TryGetValue name with
                    | (true, evt) -> evt :: acc
                    | _ -> acc
                | _ ->
                    acc

            match parentIds.TryGetValue nodeId with
            | (true, pid) -> getPath newAcc pid
            | _ -> newAcc
            
        let rec run (bubble : list<System.Text.Json.JsonElement -> bool>) (data : System.Text.Json.JsonElement) (p : list<RemoteEventCallbacks>) =
            match p with
            | [] -> 
                bubble |> List.forall (fun cb -> cb data)
            | h :: rest ->
                let cont = 
                    match h.Capture with
                    | Some cb -> cb data
                    | None -> true

                if cont then
                    let newBubble =
                        if bubbles then
                            match h.Bubble with
                            | Some b -> b :: bubble
                            | None -> bubble
                        else
                            match h.Bubble with
                            | Some b -> [b]
                            | None -> []

                    run newBubble data rest
                else
                    false

        getPath [] srcId
        |> run [] data 
        |> ignore


    member x.GetCode() =
        try code.ToString()
        finally code.Clear()
