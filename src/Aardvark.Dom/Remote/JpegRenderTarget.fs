namespace Aardvark.Dom.Remote

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering

type JpegRenderTarget(runtime : IRuntime, signature :IFramebufferSignature, task : IRenderTask, size : V2i, quality : int) =
    inherit AdaptiveObject()

    let mutable clearTask : option<IRenderTask> = None
    let mutable size = size
    let mutable quality = quality
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
        and set v = 
            if v <> size then
                size <- v
                x.MarkOutdated()
   
    member x.Quality
        with get() = quality
        and set v = 
            if v <> quality then
                quality <- v
                x.MarkOutdated()

    member x.Dispose() =
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
            use __ = runtime.ContextLock
            let fbo = getFramebuffer size

            let clearTask = 
                match clearTask with
                | Some c -> c
                | None -> 
                    let t = runtime.CompileClear(fbo.Signature, clear { color C4f.Black; depth 1.0; stencil 0 })
                    clearTask <- Some t
                    t


            clearTask.Run(token, RenderToken.Empty, fbo)
            task.Run(token, RenderToken.Empty, fbo)

            fbo.DownloadJpegColor(1.0, quality)
        )

    interface IDisposable with
        member x.Dispose() = x.Dispose()
