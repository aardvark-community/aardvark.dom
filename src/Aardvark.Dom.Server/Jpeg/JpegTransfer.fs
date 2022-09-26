namespace Aardvark.Dom.Remote.Jpeg

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Dom
open Aardvark.Dom.Remote

type private JpegRenderTarget(runtime : IRuntime, signature :IFramebufferSignature, task : IRenderTask, size : aval<V2i>, quality : aval<int>) =
    inherit AdaptiveObject()

    //let mutable clearTask : option<IRenderTask> = None
    let mutable fbo : option<IFramebuffer * IRenderbuffer * IRenderbuffer> = None
    
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
   
    member x.Quality
        with get() = quality

    member x.Dispose() =
        match fbo with
        | Some (f, c, d) ->
            runtime.DeleteFramebuffer f
            runtime.DeleteRenderbuffer c
            runtime.DeleteRenderbuffer d
            fbo <- None
        | None ->
            ()

        //match clearTask with
        //| Some t -> 
        //    t.Dispose()
        //    clearTask <- None
        //| None ->
        //    ()

    

    member x.Run(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            let size = size.GetValue token
            let quality = quality.GetValue token
            let fbo = getFramebuffer size

            //let clearTask = 
            //    match clearTask with
            //    | Some c -> c
            //    | None -> 
            //        let color = background |> AVal.map (fun c ->  c.ToC4f())

            //        let values =
            //            color |> AVal.map (fun color ->
            //                ClearValues.empty
            //                |> ClearValues.color color
            //                |> ClearValues.depth 1.0
            //                |> ClearValues.stencil 0
            //            )

            //        let t = runtime.CompileClear(fbo.Signature, values)
            //        clearTask <- Some t
            //        t


            //clearTask.Run(token, RenderToken.Empty, fbo)
            task.Run(token, RenderToken.Empty, fbo)

            fbo.DownloadJpegColor(1.0, quality)
        )

    interface IDisposable with
        member x.Dispose() = x.Dispose()
   
type JpegTransfer() =

    let supported =
        lazy (
            try
                let t = new TJCompressor()
                t.Dispose()
                true
            with _ ->
                false
        )

    [<OnAardvarkInit>]
    static member Init() = RemoteHtmlBackend.RegisterImageTransfer<JpegTransfer>(0)

    interface IImageTransfer with
        member x.Requirements = []
        member x.ClientCheck = ["return (URL.createObjectURL != undefined);"]
        member x.IsSupported(runtime : IRuntime) = supported.Value

        member x.CreateRenderer(signature : IFramebufferSignature, scene : IRenderTask, size : aval<V2i>, requestData : amap<string, string>) =
            let runtime = signature.Runtime :?> IRuntime
            
            let quality =
                requestData |> AMap.tryFind "quality" |> AVal.map (function
                    | None -> 80
                    | Some q ->
                        match System.Int32.TryParse q with
                        | (true, q) ->
                            clamp 1 100 q
                        | _ ->
                            80
                )

            let target = new JpegRenderTarget(runtime, signature, scene, size, quality)

            { new TransferImageRenderer() with
                member x.RenderFrame(token) =
                    target.Run(token) |> ChannelMessage.Binary
                member x.Destroy() =
                    target.Dispose()
            }

        member x.Boot(channelName) =
            [
                $"{channelName}.binaryType = \"blob\";"
                $"const img = document.createElement(\"img\");"
                $"img.setAttribute(\"draggable\", \"false\");"
                $"img.style.userSelect = \"none\";"
                $"img.style.pointerEvents = \"none\";"
                $"__THIS__.appendChild(img);"
            ]

        member x.Shutdown(channelName) =
            [
                $"img.remove();"
            ]

        member x.ClientCode message =
            [
                $"    if({message} instanceof Blob) {{"
                $"        let url = URL.createObjectURL({message})"
                $"        let o = img.src;"
                $"        img.src = url;"
                $"        if(o) {{ URL.revokeObjectURL(o); }}"
                $"    }}"
            ]
