namespace Aardvark.Dom.Remote.SharedMemory

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering
open System.Runtime.InteropServices
open Aardvark.Dom
open Aardvark.Dom.Remote
open Microsoft.FSharp.NativeInterop

#nowarn "9"

type private SharedMemoryRenderTarget(runtime : IRuntime, signature : IFramebufferSignature, task : IRenderTask, size : V2i) =
    inherit AdaptiveObject()

    static let randomString() =
        let str = System.Convert.ToBase64String(Guid.NewGuid().ToByteArray())
        str.TrimEnd('=').Replace("/", "-")

    static let getMappingSize (size : V2i) =
        let s = int64 size.X * int64 size.Y * 4L
        if s < 32768L then 32768L
        else Fun.NextPowerOfTwo s

    static let newSharedMemory (desiredMapSize : int64) =
        let name = randomString()
        SharedMemory.create name desiredMapSize



    let mutable clearTask : option<IRenderTask> = None
    let mutable size = size
    let mutable fbo : option<IFramebuffer * IRenderbuffer * IRenderbuffer> = None
    
    let mutable shmSize = size
    let mutable shm = newSharedMemory (getMappingSize size)

    let mutable downloader : option<IDownloader> = None //renderThread.Run (fun () -> RawDownloader.create runtime signature.Samples)
    
    let getFramebuffer (size : V2i) =
        match fbo with
        | Some (f,_,_) when f.Size = size  ->   
            f
        | _ ->
            match fbo with
            | Some (f, c, d) ->
                runtime.DeleteFramebuffer f
                runtime.DeleteRenderbuffer c
                runtime.DeleteRenderbuffer d
            | None ->
                ()
            let s = signature
            let c = runtime.CreateRenderbuffer(size, TextureFormat.Rgba8, signature.Samples)
            let d = runtime.CreateRenderbuffer(size, TextureFormat.Depth24Stencil8, signature.Samples)
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

    member x.Dispose() =
        shm.Dispose()
        shmSize <- V2i.Zero
        match downloader with
        | Some d -> 
            d.Dispose()
            downloader <- None
        | None -> ()
        match fbo with
        | Some (f, c, d) ->
            runtime.DeleteFramebuffer f
            runtime.DeleteRenderbuffer c
            runtime.DeleteRenderbuffer d
            fbo <- None
        | None ->
            ()

        match clearTask with
        | Some t -> 
            t.Dispose()
            clearTask <- None
        | None ->
            ()


    member x.Run(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
        
            if shmSize <> size then
                shmSize <- size
                let mapSize = getMappingSize size
                if mapSize <> shm.Size then
                    shm.Dispose()
                    shm <- newSharedMemory mapSize

            let fbo = getFramebuffer size

            let downloader =
                match downloader with
                | Some d -> d
                | None ->
                    let d = RawDownloader.create runtime signature.Samples
                    downloader <- Some d
                    d

            let clearTask = 
                match clearTask with
                | Some c -> c
                | None -> 
                    let t = runtime.CompileClear(fbo.Signature, clear { color C4f.Black; depth 1.0; stencil 0 })
                    clearTask <- Some t
                    t

            
            clearTask.Run(token, RenderToken.Empty, fbo)
            task.Run(token, RenderToken.Empty, fbo)

            downloader.Download(fbo, shm.Pointer)

            (shm.Name, size, shm.Size)
        )

    interface IDisposable with
        member x.Dispose() = x.Dispose()

type SharedMemoryTransfer() =

    let supported =
        lazy (
            try 
                let shm = SharedMemory.create "test" (1 <<< 20)
                shm.Dispose()
            with _ ->
                ()
        )

    [<OnAardvarkInit>]
    static member Init() = RemoteHtmlBackend.RegisterImageTransfer<SharedMemoryTransfer>(100)

    interface IImageTransfer with
        member x.Requirements = []

        member x.ClientCheck = ["return (aardvark.openMapping != undefined);"]

        member x.IsSupported(runtime : IRuntime) = supported.Value
        member x.CreateRenderer(signature : IFramebufferSignature, scene : IRenderTask, size : aval<V2i>, _quality : aval<int>) =
            let runtime = signature.Runtime :?> IRuntime
            
            let target = new SharedMemoryRenderTarget(runtime, signature, scene, AVal.force size)

            { new TransferImageRenderer() with
                member x.RenderFrame(token) =
                    let s = size.GetValue token
                    transact (fun () ->
                        target.Size <- s
                    )
                    let (name, size, length) = target.Run(token)
                    let json = $"{{ \"name\": \"{name}\", \"length\": {length}, \"size\": {{ \"width\": {size.X}, \"height\": {size.Y} }} }}"
                    ChannelMessage.Text json

                member x.Destroy() =
                    target.Dispose()
            }

        member x.Boot(_channelName) =
            [
                $"const canvas = document.createElement(\"canvas\");"
                $"canvas.style.userSelect = \"none\";"
                $"canvas.style.pointerEvents = \"none\";"
                $"canvas.style.transform = \"scaleY(-1)\";"
                $"let ctx = canvas.getContext(\"2d\");"
                $"let mappedMemory = null;"
                $"let framebuffer = {{ width: 0, height: 0, buffer : null, length: 0 }};"
                $"__THIS__.appendChild(canvas);"
            ]

        member x.Shutdown(_channelName) =
            [
                $"if(mappedMemory) {{ mappedMemory.close(); mappedMemory = null; }}"
                $"framebuffer.buffer = null;"
                $"canvas.remove();"
            ]

        member x.ClientCode(message) =
            [
                $"let msg = JSON.parse({message});"
                $"if(mappedMemory) {{"
                $"    if(mappedMemory.name != msg.name) {{"
                $"      mappedMemory.close();"
                $"      console.log(\"new mapping\", msg.name, msg.length);"
                $"      mappedMemory = aardvark.openMapping(msg.name, msg.length);"
                $"    }}"
                $"}}"
                $"else {{"
                $"    console.log(\"new mapping\", msg.name, msg.length);"
                $"    mappedMemory = aardvark.openMapping(msg.name, msg.length);"
                $"}}"
                
                $"if(framebuffer.width != msg.size.width || framebuffer.height != msg.size.height) {{"
                $"    const length = 4 * msg.size.width * msg.size.height;"
                $"    console.log(\"new framebuffer\", msg.size.width, msg.size.height, length);"
                $"    framebuffer.width = msg.size.width;"
                $"    framebuffer.height = msg.size.height;"
                $"    framebuffer.buffer = new Uint8ClampedArray(length);"
                $"    framebuffer.length = length;"
                $"}}"

                $"canvas.width = msg.size.width;"
                $"canvas.height = msg.size.height;"
                $"framebuffer.buffer.set(new Uint8ClampedArray(mappedMemory.buffer, 0, framebuffer.length));"
                $"ctx.putImageData(new ImageData(framebuffer.buffer, msg.size.width, msg.size.height), 0, 0);"
            ]
