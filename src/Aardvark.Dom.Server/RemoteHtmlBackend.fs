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
    abstract Requirements : list<System.Guid * string * byte[]>
    abstract IsSupported : runtime : IRuntime -> bool
    abstract CreateRenderer : signature : IFramebufferSignature * scene : IRenderTask * size : aval<V2i> * requestData : amap<string, string> -> TransferImageRenderer
    abstract Boot : channelName : string -> list<string>
    abstract Shutdown : channelName : string -> list<string>
    abstract ClientCode : messageName : string -> list<string>
    abstract ClientCheck : list<string> 

    
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

type RemoteHtmlBackend private(runtime : IRuntime, server : IServer, imageTransfer : IImageTransfer, renderThread : RenderThread) =
    inherit AbstractRemoteHtmlBackend(server)
    
    static let toSortedTransferList(m) =
        m
        |> HashMap.toValueList
        |> List.sortByDescending fst
        |> List.map snd


    static let mutable imageTransferTable = HashMap.empty
    static let mutable imageTransfers = []

    new(runtime : IRuntime, server : IServer, imageTransfer : IImageTransfer) = 
        RemoteHtmlBackend(runtime, server, imageTransfer, runtime.RenderThread)

    static member ImageTransfers = imageTransfers

    static member RegisterImageTransfer<'t when 't :> IImageTransfer and 't : (new : unit -> 't)>(priority : int) =
        imageTransferTable <- HashMap.add typeof<'t> (priority, new 't() :> IImageTransfer) imageTransferTable
        imageTransfers <- toSortedTransferList imageTransferTable
        
    override x.Clone() =
        RemoteHtmlBackend(runtime, server, imageTransfer, renderThread)
        
    override x.SetupRenderer(element : int64, scene : SceneHandler) =
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
                            $"    const bg = window.getComputedStyle(__THIS__).backgroundColor.toString();"
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

    override x.DestroyRenderer(element : int64, scene : SceneHandler) =
        ()