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
open System.Runtime.CompilerServices
open System

[<AutoOpen>]
module RuntimeThreading =

    let private startRenderThread (runtime : IRuntime) (action : unit -> unit) =
        match runtime with
        | :? Aardvark.Rendering.GL.Runtime as runtime ->
            startThread <| fun () ->
                use __ = runtime.Context.ResourceLock
                action()
        | _ ->
            startThread action

    type private AsyncResult<'a> =
        | Ok of 'a
        | Error of exn
        | Empty

    module private AsyncResult =
        let inline isEmpty (r : AsyncResult<'a>) =
            match r with
            | Empty -> true
            | _ -> false

    type RenderThread private (runtime : IRuntime) =
        [<ThreadStatic; DefaultValue>]
        static val mutable private IsRenderThread_ : bool

        static let renderThreads = ConditionalWeakTable<IRuntime, RenderThread>()

        let queue = new System.Collections.Concurrent.BlockingCollection<unit -> unit>()

        let thread = 
            startRenderThread runtime <| fun () ->
                RenderThread.IsRenderThread_ <- true
                for action in queue.GetConsumingEnumerable() do
                    try action()
                    with e -> Log.warn "bad: %A" e

        static member IsRenderThread = RenderThread.IsRenderThread_

        static member Get(runtime : IRuntime) =
            lock renderThreads (fun () ->
                match renderThreads.TryGetValue runtime with
                | (true, t) -> t
                | _ ->
                    let t = new RenderThread(runtime)
                    renderThreads.Add(runtime, t)
                    t
            )

        member x.Run(action : unit -> 'a) =
            if RenderThread.IsRenderThread_ then
                action()
            else
                if queue.IsAddingCompleted then raise <| System.ObjectDisposedException "RenderThread"
                let result = ref Empty
                queue.Add <| fun () ->
                    let value = 
                        try Ok (action())
                        with e -> Error e

                    lock result (fun () ->
                        result := value
                        Monitor.PulseAll result
                    )

                lock result (fun () ->
                    while AsyncResult.isEmpty result.Value do
                        Monitor.Wait result |> ignore
                )
                match result.Value with
                | Ok v -> v
                | Error e -> raise e
                | Empty -> failwith "impossible"

        member private x.Dispose(disposing : bool) =    
            if disposing then System.GC.SuppressFinalize x
            queue.CompleteAdding()

        member x.Dispose() = x.Dispose true
        override x.Finalize() = x.Dispose false

    type IRuntime with
        member x.RenderThread =
            RenderThread.Get(x)

        member x.RunRender(action : unit -> 'a) =
            let t = RenderThread.Get(x)
            t.Run(action)



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
    abstract RegisterResource : id : Guid * mime : string * content : byte[] -> string


type internal RemoveEventHandler = { Callback : option<System.Text.Json.JsonElement -> bool>; PointerCapture : bool; PreventDefault : bool } with
    member x.IsEmpty = Option.isNone x.Callback
    static member Empty = { Callback = None; PointerCapture = false; PreventDefault = false }

type internal RemoteEventCallbacks() =
    let mutable capture = RemoveEventHandler.Empty
    let mutable bubble = RemoveEventHandler.Empty

    member x.PointerCapture = capture.PointerCapture || bubble.PointerCapture
    member x.PreventDefault = capture.PreventDefault || bubble.PreventDefault

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
        capture.IsEmpty && bubble.IsEmpty
        
    member x.IsNonEmpty =
        not capture.IsEmpty || not bubble.IsEmpty

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

type Dependency =
    | Javascript of string
    | Css of string
    | Wasm of byte[]

type IImageTransfer =   
    abstract Requirements : list<System.Guid * string * byte[]>
    abstract IsSupported : runtime : IRuntime -> bool
    abstract CreateRenderer : signature : IFramebufferSignature * scene : IRenderTask * size : aval<V2i> * requestData : amap<string, string> -> TransferImageRenderer
    abstract Boot : channelName : string -> list<string>
    abstract Shutdown : channelName : string -> list<string>
    abstract ClientCode : messageName : string -> list<string>
    abstract ClientCheck : list<string> 

module private EventParser =
    open Aardvark.Base.IL
    open Microsoft.FSharp.Reflection
    let private cache = System.Collections.Concurrent.ConcurrentDictionary<Type, System.Text.Json.JsonElement -> option<Event>>()

    let private cilSupported =  
        try
            let test : int -> int =
                cil { 
                    do! IL.ldarg 0
                    do! IL.ldconst 10
                    do! IL.add
                    do! IL.ret
                }
            test 1 = 11
        with _ ->
            false

    let getParser (typ : Type) =
        cache.GetOrAdd(typ, System.Func<_,_>(fun (typ : Type) ->
            let mTryParse = typ.GetMethod("TryParse", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public, [| typeof<System.Text.Json.JsonElement> |])
            if isNull mTryParse || not mTryParse.ReturnType.IsGenericType || mTryParse.ReturnType.GetGenericTypeDefinition() <> typedefof<option<_>> then
                Log.warn "%s lacks a proper `TryParse : JsonElement -> option<SelfType>` method and cannot be parsed" typ.FullName
                fun _ -> None
            else
                if cilSupported then
                    let cases = FSharpType.GetUnionCases(mTryParse.ReturnType, true)
                    let value = 
                        let cSome = cases |> Array.find (fun c -> c.Name = "Some")
                        cSome.GetFields().[0]

                    let mCreateRes = 
                        let t = typeof<option<Event>>
                        let c = FSharpType.GetUnionCases(t) |> Array.find (fun c -> c.Name = "Some")
                        FSharpValue.PreComputeUnionConstructorInfo c
                    let res = Local(mTryParse.ReturnType)
                    let lNull = Label()
                    cil {
                        do! IL.ldarg 0
                        do! IL.call mTryParse
                        do! IL.stloc res

                        do! IL.ldloc res
                        do! IL.jmp JumpCondition.False lNull

                        do! IL.ldloc res
                        do! IL.call value.GetMethod
                        do! IL.call mCreateRes
                        do! IL.ret

                        do! IL.mark lNull
                        do! IL.ldnull
                        do! IL.ret
                    }
                else
                    let cSome = FSharpType.GetUnionCases(mTryParse.ReturnType, true) |> Array.find (fun c -> c.Name = "Some")
                    let readSome = FSharpValue.PreComputeUnionReader cSome
                    fun (data : System.Text.Json.JsonElement) ->
                        let res = mTryParse.Invoke(null, [|data|])
                        if isNull res then 
                            None
                        else
                            let values = readSome res
                            Some (values.[0] :?> Event)
        ))
        
            //let m = evtType.GetMethod("TryParse", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            //if isNull m then failwithf "non-parsable event type: %A" evtType
            //let t = typedefof<option<_>>.MakeGenericType [| evtType |]
            //let pValue = t.GetProperty("Value")


module private Color =
    open System.Text.RegularExpressions 

    let private namedColors =
        let props = typeof<C4b>.GetProperties(BindingFlags.NonPublic ||| BindingFlags.Static)
        props |> Array.map (fun p ->
            p.Name.ToLower(), (p.GetValue null :?> C4b)
        )
        |> Dict.ofArray

    let private rgbaRx = Regex @"^rgba?[ t]*\(([0-9]+)[ t]*,[ t]*([0-9]+)[ t]*,[ t]*([0-9]+)([ t]*,[ t]*([0-9\.]+)[ t]*)?\)$"
    let private hexRx = Regex @"^#([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})$"

    let tryParse (str : string) =
        let str = str.Trim().ToLower()
        match namedColors.TryGetValue str with
        | (true, c) -> Some c
        | _ ->
            let m = rgbaRx.Match str
            if m.Success then
                let r = m.Groups.[1].Value |> byte
                let g = m.Groups.[2].Value |> byte
                let b = m.Groups.[3].Value |> byte
                let a =
                    if m.Groups.[4].Success then float m.Groups.[5].Value |> clamp 0.0 1.0
                    else 1.0

                let color = C4b(r, g, b, byte (255.0 * a))
                Some color
            else
                let m = hexRx.Match str 
                if m.Success then
                    let r = System.Int32.Parse(m.Groups.[1].Value, Globalization.NumberStyles.HexNumber) |> byte
                    let g = System.Int32.Parse(m.Groups.[2].Value, Globalization.NumberStyles.HexNumber) |> byte
                    let b = System.Int32.Parse(m.Groups.[3].Value, Globalization.NumberStyles.HexNumber) |> byte
                    Some (C4b(r, g, b, 255uy))
                else
                    None


type RemoteHtmlBackend private(runtime : IRuntime, server : IServer, imageTransfer : IImageTransfer, eventListeners : Dict<int64, Dict<string, RemoteEventCallbacks>>, currentId : ref<int>, parentIds : Dict<int64, int64>, childIds : Dict<int64, DictSet<int64>>, code : CodeBuilder) =
    static let toSortedTransferList(m) =
        m
        |> HashMap.toValueList
        |> List.sortByDescending fst
        |> List.map snd


    static let mutable imageTransferTable = HashMap.empty
    static let mutable imageTransfers = []

    static let aardvarkDom =
        let ass = typeof<RemoteHtmlBackend>.Assembly
        let resourceName = $"{ass.GetName().Name}.aardvark-dom.js"
        use s = ass.GetManifestResourceStream resourceName
        use r = new System.IO.StreamReader(s)
        r.ReadToEnd()

    let renderThread = runtime.RenderThread

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

    static member AardvarkDomJavascript = aardvarkDom

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

            let cloudLock = obj()
            let mutable framesInCloud = 0

            let urls = 
                match imageTransfer.Requirements with
                | [] -> Set.empty
                | refs -> refs |> List.map (fun (guid, mime, data) -> server.RegisterResource(guid, mime, data)) |> Set.ofList

            let mutable disp = Unchecked.defaultof<_>

            x.Require(urls, fun x ->
                
                disp <-
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
                                                        let mutable data = HashMap.empty
                                                        let mutable bg = None

                                                        match e.TryGetProperty "width" with
                                                        | (true, p) -> s.X <- max 1 (p.GetInt32())
                                                        | _ -> ()
                                            
                                                        match e.TryGetProperty "height" with
                                                        | (true, p) -> s.Y <- max 1 (p.GetInt32())
                                                        | _ -> ()
                                                        
                                                        match e.TryGetProperty "background" with
                                                        | (true, p) ->
                                                            let str = p.GetString()
                                                            match Color.tryParse str with
                                                            | Some c -> bg <- Some c
                                                            | None -> ()
                                                        | _ -> ()

                                                        match e.TryGetProperty "data" with
                                                        | (true, p) ->
                                                            try
                                                                for k in p.EnumerateObject() do
                                                                    data <- HashMap.add k.Name (k.Value.GetString()) data
                                                            with _ ->
                                                                ()
                                                        | _ -> 
                                                            ()
                                                    
                                                        Some (s, bg, data)

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
                                    let (size, bg, requestData) = info.Value
                                
                                    let background = 
                                        match bg with
                                        | Some bg when bg.A >= 255uy -> bg.ToC4f()
                                        | _ -> C4f.Black

                                    transact (fun () -> scene.ClearColor.Value <- background)

                                    let size = cval size
                                    let request = cmap requestData
                                    let t0 = System.DateTime.Now
                                    let dt = System.Diagnostics.Stopwatch.StartNew()
                              
                                    let render = imageTransfer.CreateRenderer(scene.FramebufferSignature, scene.RenderTask, size, request)

                                    let mutable running = true

                                    let renderDirty = MVar.create ()
                                    let sub = render.AddMarkingCallback (fun () -> MVar.put renderDirty ())
                                    let renderThread =
                                        startThread <| fun () ->
                                            let sw = System.Diagnostics.Stopwatch.StartNew()
                                            let mm = new MultimediaTimer.Trigger(1)
                                            while running do
                                                MVar.take renderDirty
                                                if running then
                                                    let didWait = 
                                                        lock cloudLock (fun () ->
                                                            let mutable didWait = false
                                                            while framesInCloud > 3 do
                                                                didWait <- true
                                                                Log.warn "waiting: %A" framesInCloud
                                                                Monitor.Wait cloudLock |> ignore

                                                            framesInCloud <- framesInCloud + 1
                                                            didWait
                                                        )

                                                    if didWait then
                                                        Log.warn "resume"

                                                    let data = renderThread.Run (fun () -> render.Run(AdaptiveToken.Top))
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
                                            | Some (newSize, bg, newRequestData) ->
                                                transact (fun () ->
                                                    match bg with
                                                    | Some bg when bg.A >= 255uy -> scene.ClearColor.Value <- bg.ToC4f()
                                                    | _ -> scene.ClearColor.Value <- C4f.Black
                                                    size.Value <- newSize
                                                    request.Value <- newRequestData
                                                )
                                            
                                                lock cloudLock (fun () ->
                                                    framesInCloud <- framesInCloud - 1
                                                    Monitor.PulseAll cloudLock
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
                                $"    const data = aardvark.getDataAttributeDict(__THIS__);"
                                $"    const bg = __THIS__.computedStyleMap().get(\"background-color\").toString();"
                                $"    {n}.send(JSON.stringify({{ cmd: \"requestimage\", width: r.width, height: r.height, background: bg, data: data }}));"
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
                    $"const {n} = aardvark.newSocket(\"{u}\");"
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
             
        member x.SetAttribute(node : int64, name : string, value : bool) =
            let value = if value then "true" else "false"
            if name.Contains "-" then
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.setAttribute(\"{name}\", \"{value}\");"
            else
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.{name} = {value};"
            
        member x.SetAttribute(node : int64, name : string, value : int) =
            if name.Contains "-" then
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.setAttribute(\"{name}\", \"{value}\");"
            else
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.{name} = {value};"
            
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

        member x.SetListener(element, name, capture, preventDefault, pointerCapture, evtType, callback) =
            let listeners = eventListeners.GetOrCreate(element, fun _ -> Dict()).GetOrCreate(name, fun _ -> RemoteEventCallbacks())


            if listeners.IsEmpty then
                let var = code.GetOrCreateVar element
                let body =
                    String.concat "" [
                        $"const flags = aardvark.getListenerFlags({var}, \"{name}\");"
                        "if(e.bubbles) e.stopImmediatePropagation();"
                        "if(flags.preventDefault) { e.preventDefault(); }"
                            
                        match name.ToLower() with
                        | "pointerdown" -> $"if(flags.pointerCapture) {{ {var}.setPointerCapture(e.pointerId); }}"
                        | "pointerup" -> $"if(flags.pointerCapture) {{ {var}.releasePointerCapture(e.pointerId); }}"
                        | _ -> ()
                        $"aardvark.trigger({var}, {element}, \"{name}\", e);"
                    ]

                code.AppendLine $"aardvark.setListener({var}, \"{name}\", ((e) => {{ {body} }}), false);"

            let oldPointerCapture = listeners.PointerCapture
            let oldPreventDefault = listeners.PreventDefault

            let tryParse = EventParser.getParser evtType
            let callback json =
                match tryParse json with
                | Some e -> callback e
                | None -> true

            listeners.[capture] <- { Callback = Some callback; PreventDefault = preventDefault; PointerCapture = pointerCapture }

            if listeners.PreventDefault <> oldPreventDefault || listeners.PointerCapture <> oldPointerCapture then
                let var = code.GetOrCreateVar element
                let inline bstr a = if a then "true" else "false"
                code.AppendLine $"aardvark.setListenerFlags({var}, \"{name}\", {bstr listeners.PointerCapture}, {bstr listeners.PreventDefault});"


        member x.RemoveListener(element, name, capture) =   
            match eventListeners.TryGetValue element with
            | (true, elementListeners) ->
                match elementListeners.TryGetValue name with
                | (true, listeners) ->
                    
                    let oldPointerCapture = listeners.PointerCapture
                    let oldPreventDefault = listeners.PreventDefault

                    listeners.[capture] <- RemoveEventHandler.Empty
                    if listeners.IsEmpty then
                        let var = code.GetOrCreateVar element
                        code.AppendLine $"aardvark.removeListener({var}, \"{name}\", false);"
                        if elementListeners.Remove name && elementListeners.Count = 0 then
                            eventListeners.Remove element |> ignore
                    else
                        if listeners.PreventDefault <> oldPreventDefault || listeners.PointerCapture <> oldPointerCapture then
                            let var = code.GetOrCreateVar element
                            let inline bstr a = if a then "true" else "false"
                            code.AppendLine $"aardvark.setListenerFlags({var}, \"{name}\", {bstr listeners.PointerCapture}, {bstr listeners.PreventDefault});"
                        
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
                    match h.Capture.Callback with
                    | Some cb -> cb data
                    | None -> true

                if cont then
                    let newBubble =
                        if bubbles then
                            match h.Bubble.Callback with
                            | Some b -> b :: bubble
                            | None -> bubble
                        else
                            match h.Bubble.Callback with
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
