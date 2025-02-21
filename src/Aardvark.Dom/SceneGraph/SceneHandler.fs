namespace Aardvark.Dom

open System.Runtime.InteropServices
open FShade.Imperative
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.Dom
open Microsoft.FSharp.NativeInterop

#nowarn "9"

[<ReflectedDefinition>]
module internal Normal32 =
    let private sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let private clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )

    let decode (v : int) : V3d =
        if v = 0 then
            V3d.Zero
        else
            let e = V2d(float (uint32 v >>> 16) / 65535.0, float (v &&& 0xFFFF) / 65535.0) * 2.0 - V2d.II
            let v = V3d(e, 1.0 - abs e.X - abs e.Y)
            if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
            else v |> Vec.normalize

    let encode (v : V3d) : int =
        if v.X = 0.0 && v.Y = 0.0 && v.Z = 0.0 then
            0
        else
            let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
            let p = 
                if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
                else clamp p
        
            let x0 = floor ((p.X * 0.5 + 0.5) * 65535.0) |> int
            let y0 = floor ((p.Y * 0.5 + 0.5) * 65535.0) |> int
            
            let mutable bestDot = 0.0
            let mutable best = 0

            for dx in 0 .. 1 do
                for dy in 0 .. 1 do
                    let e = (((x0 + dx) <<< 16) ||| (y0 + dy))
                    let vv = decode e
                    let d = Vec.dot vv v
                    if d > bestDot then
                        bestDot <- d
                        best <- e

            best
        
module internal PickShader =
    open FShade 

    type UniformScope with
        member x.PickId : int = uniform?PickId
        member x.Level : int = uniform?Level
        member x.Selected : int = uniform?Selected

        member x.Levels : int = uniform?Levels
        member x.Distance : float = uniform?Distance
        
        member x.LevelSizes : Arr<16 N, V2i> = uniform?LevelSizes
        member x.LevelPixelSizes : Arr<16 N, V2i> = uniform?LevelPixelSizes

    type Vertex =
        {
            [<Color>] c : V4d
            [<Position>] pos : V4d
            [<Semantic("ViewSpaceNormal")>] vn : V3d
            [<Semantic("PickViewPosition")>] pvp : V3d
            [<Depth>] d : float
            [<FragCoord>] fc : V4d
        }
    
    type Fragment =
        {
            [<Color>] c : V4d
            [<Semantic("PickId")>] id : V4i
        }
        
    let pickVertex (v : Effects.Vertex) =
        vertex {
            let vn = uniform.ModelViewTrafoInv.Transposed * V4d(v.n, 0.0) |> Vec.xyz |> Vec.normalize
            
            return {
                c = v.c
                pos = v.pos
                pvp = uniform.ModelViewTrafo * v.pos |> Vec.xyz
                vn = vn
                d = 0.0
                fc = V4d.Zero
            }
        }
        
    [<GLSLIntrinsic("gl_FragCoord")>]
    let fragCoord() : V4d = onlyInShaderCode "fragcoord"
    
    let pickIdBefore(v : Vertex) =
        fragment {
            let d = fragCoord().Z
            return { v with d = d }
        }
        
    let pickIdWithRealPosition(v : Vertex) =
        fragment {
            let n32 = Normal32.encode (Vec.normalize v.vn) |> int
            let len = Vec.length v.pvp
            let dir = Normal32.encode (v.pvp / len) |> int
            return { c = v.c; id = V4i(-uniform.PickId, n32, dir, Bitwise.FloatBitsToInt len) }
        }
        
    let pickId(v : Vertex) =
        fragment {
            let n32 = Normal32.encode (Vec.normalize v.vn) |> int
            let d = (2.0 * v.d - 1.0) 
            return { c = v.c; id = V4i(uniform.PickId, n32, Bitwise.FloatBitsToInt d, Bitwise.FloatBitsToInt 0.0) }
        }
        
    let pickIdNoNormal(v : Vertex) =
        fragment {
            let n32 = 0
            let d = (2.0 * v.d - 1.0) 
            return { c = v.c; id = V4i(uniform.PickId, n32, Bitwise.FloatBitsToInt d, Bitwise.FloatBitsToInt 0.0) }
        }

    let vertexPickEffect = Effect.ofFunction pickVertex
    let pickEffectBefore = Effect.ofFunction pickIdBefore
    let pickEffect = Effect.ofFunction pickId
    let pickEffectWithRealPosition = Effect.ofFunction pickIdWithRealPosition
    let pickEffectNoNormal = Effect.ofFunction pickIdNoNormal

    let pickSampler =
        intSampler2d {
            texture uniform?PickTexture
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }
        
    let binary (v : Vertex) =
        fragment {
            let mutable r = 0.0
            let px = V2i v.fc.XY
            let s = pickSampler.Size
            if uniform.Selected >= 0 && px.X < s.X && px.Y < s.Y then
                let id = pickSampler.[px].X
                if id = uniform.Selected then 
                    r <- 1.0
            return V4d(r, r, r, r)
        }
        
    let quadTree =
        sampler2d {
            texture uniform?QuadTreeTexture
            filter Filter.MinMagMipPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }
        
    [<ReflectedDefinition>]
    let enqueue (value : V4d) (heap : ref<Arr<_, V4d>>) (cnt : int) =
        let mutable i = cnt
        let value = value
        let mutable run = true

        while run && i > 0 do
            let p = (i-1)/2
            let pv = (!heap).[p]
            if pv.X > value.X then
                (!heap).[i] <- pv
                i <- p
            else
                run <- false

        (!heap).[i] <- value
        
    [<ReflectedDefinition>]
    let dequeue (queue : ref<Arr<_, V4d>>) (cnt : int) =
        if cnt <= 1 then
            (!queue).[0]
        else
            let mutable cnt = cnt
            let minValue = (!queue).[0]
            let value = (!queue).[cnt - 1]
            let mutable run = true
            let mutable i = 0
            cnt <- cnt - 1
            while run && i < cnt do
                let i0 = 2*i+1
                let i1 = i0 + 1

                if i1 < cnt then
                    let v0 = (!queue).[i0]
                    let v1 = (!queue).[i1]

                    if v0.X < value.X then
                        if v1.X < value.X then
                            if v0.X < v1.X then
                                (!queue).[i] <- v0
                                i <- i0
                            else
                                (!queue).[i] <- v1
                                i <- i1
                        else
                            (!queue).[i] <- v0
                            i <- i0
                    elif v1.X < value.X then
                        (!queue).[i] <- v1
                        i <- i1
                    else
                        run <- false
                            
                elif i0 < cnt then
                    let v0 = (!queue).[i0]
                    if v0.X < value.X then
                        (!queue).[i] <- v0
                        i <- i0
                    else
                        run <- false
                else
                    run <- false
                    
            (!queue).[i] <- value

            minValue


    [<ReflectedDefinition; Inline>]
    let sqr (a : float) = a*a

    [<ReflectedDefinition>]
    let minDistSq (px : V2i) (o : V2i) (s : V2i) : float =
        if s.X <= 1 && s.Y <= 1 then
            let d = V2d px - V2d o
            Vec.dot d d
        else
            let min = o
            let max = o + s - V2i.II


            let lx = px.X < min.X
            let ly = px.Y < min.Y
            let hx = px.X > max.X
            let hy = px.Y > max.Y

            let ix = not lx && not hx
            let iy = not ly && not hy

            if ix then
                if iy then
                    0.0
                else
                    if hy then sqr (float (px.Y - max.Y))
                    else sqr (float (min.Y - px.Y))

            elif iy then
                if hx then sqr (float (px.X - max.X))
                else sqr (float (min.X - px.X))

            elif hx then
                if hy then 
                    let d = (V2d px - V2d max)
                    Vec.dot d d
                else 
                    let d = (V2d px - V2d(max.X, min.Y))
                    Vec.dot d d
            else
                if hy then 
                    let d = (V2d px - V2d(min.X, max.Y))
                    Vec.dot d d
                else
                    let d = (V2d px - V2d min)
                    Vec.dot d d
                

            //if ix && iy then
            //    0.0
            //elif ix then
            //    if hy then sqr (float (px.Y - max.Y))
            //    else sqr (float (min.Y - px.Y))
            //elif iy then
            //    if hx then sqr (float (px.X - max.X))
            //    else sqr (float (min.X - px.X))
            //elif hx then
            //    if hy then 
            //        let d = (V2d px - V2d max)
            //        Vec.dot d d
            //    else 
            //        let d = (V2d px - V2d(max.X, min.Y))
            //        Vec.dot d d
            //else
            //    if hy then 
            //        let d = (V2d px - V2d(min.X, max.Y))
            //        Vec.dot d d
            //    else
            //        let d = (V2d px - V2d min)
            //        Vec.dot d d
            

                
            



    let outline (v : Vertex) =
        fragment {
            let px = V2i v.fc.XY
            let distanceSq = sqr uniform.Distance
            let mutable radiusSq = sqr (uniform.Distance + 1.0) 

            let queue : Arr<64 N, V4d> = Unchecked.defaultof<_>
            let mutable queueCount = 0

            //let treeSize = quadTree.Size
            let maxLevel = uniform.Levels - 1

            enqueue (V4d(0.0, float maxLevel, 0.0, 0.0) ) &&queue queueCount
            queueCount <- queueCount + 1


            let mutable iter = 0
            while queueCount > 0 do
                let e = dequeue &&queue queueCount
                queueCount <- queueCount - 1


                iter <- iter + 1

                if e.X > radiusSq then
                    queueCount <- 0
                else
                    let level = int e.Y
                    let offset = V2i e.ZW
                    let levelPixelSize = uniform.LevelPixelSizes.[level]
                    let value = quadTree.Read(offset / levelPixelSize, level).X

                    if value >= 1.0 then 
                        radiusSq <- e.X 

                    elif value > 0.0 then
                        if levelPixelSize.X <= 4 && levelPixelSize.Y <= 4 then
                            for x in 0 .. levelPixelSize.X - 1 do
                                for y in 0 .. levelPixelSize.Y - 1 do
                                    let pp = offset + V2i(x,y)
                                    let value = quadTree.Read(pp, 0).X
                                    if value >= 1.0 then
                                        let d = V2d pp - V2d px
                                        let dSq = Vec.dot d d
                                        if dSq < radiusSq then
                                            radiusSq <- dSq
                                
                        else
                            if level > 0 then
                                // has non-empty children
                                let h = levelPixelSize / 2

                                let i0 = offset
                                let i1 = offset + V2i(h.X, 0)
                                let i2 = offset + V2i(0, h.Y)
                                let i3 = offset + h

                                let c0 = V4d(minDistSq px i0 h, float level - 1.0, V2d i0)
                                enqueue c0 &&queue queueCount; queueCount <- queueCount + 1
                          
                                let c1 = V4d(minDistSq px i1 h, float level - 1.0, V2d i1)
                                enqueue c1 &&queue queueCount; queueCount <- queueCount + 1
                          
                                let c2 = V4d(minDistSq px i2 h, float level - 1.0, V2d i2)
                                enqueue c2 &&queue queueCount; queueCount <- queueCount + 1
                         
                                let c3 = V4d(minDistSq px i3 h, float level - 1.0, V2d i3)
                                enqueue c3 &&queue queueCount; queueCount <- queueCount + 1


            //return V4d(float iter / 10.0, 0.0, 0.0, 1.0)
            //return quadTree.[px]

            let mutable a = 0.5

            if radiusSq >= distanceSq then a <- 0.0
            elif radiusSq <= 0 then a <- 0.1


            //let t = sqrt radiusSq / uniform.Distance
            //let a = (1.0 - abs (2.0 * t - 1.0)) ** 1.0

            //let a = 1.0 - radius / uniform.Distance
            return V4d(0.05, 0.5, 0.85, a)
                

        }
    //let selectionSampler =
    //    sampler2d {
    //        texture uniform?SelectionTexture
    //        addressU WrapMode.Clamp
    //        addressV WrapMode.Clamp
    //    }

    //let downsampleSelectionBuffer (v : Vertex) =
    //    fragment {
    //        let px = v.fc.XY
    //        let si = selectionSampler.GetSize uniform.Level
    //        let so = selectionSampler.GetSize (uniform.Level+1)
            
    //        let iMin = V2d si * (px - V2d.Half) / V2d so |> floor |> V2i
    //        let iMax = V2d si * (px + V2d.Half) / V2d so |> ceil |> V2i
            
    //        let mutable cnt = 0
    //        for x in iMin.X .. iMax.X do
    //            for y in iMin.Y .. iMax.Y do
    //                let v = selectionSampler.Read(V2i(x,y), uniform.Level).X
    //                if v > 0.0 then cnt <- cnt + 1
            
            
    //        if cnt > 0 then
    //            return V4d.IIII
    //        else
    //            return V4d.Zero
    //    }

module private PickBuffer =
    open Aardvark.SceneGraph
    
    let compileQuadTree (outputSignature : IFramebufferSignature) (selected : aval<int>) (pickTexture : IBackendTexture) =
        let runtime = pickTexture.Runtime :?> IRuntime
        use __ = runtime.ContextLock
        let srcSize = pickTexture.Size.XY

        let treeSize = 
            //let a = Fun.NextPowerOfTwo (max srcSize.X srcSize.Y)
            V2i(Fun.NextPowerOfTwo srcSize.X, Fun.NextPowerOfTwo srcSize.Y)
        let levels = 1 + int (floor (log2 (float (max treeSize.X treeSize.Y))))


        let selectionTexture = runtime.CreateTexture2D(treeSize, TextureFormat.R32f, levels)


        let signature = runtime.CreateFramebufferSignature([ DefaultSemantic.Colors, TextureFormat.R32f ])
        
        let createSelection =
            let render =
                Sg.fullScreenQuad
                |> Sg.depthTest' DepthTest.None
                |> Sg.shader { do! PickShader.binary }
                |> Sg.uniform "Selected" selected
                |> Sg.texture' "PickTexture" pickTexture
                |> Sg.compile runtime signature

            render

        let fbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, selectionTexture.[TextureAspect.Color, 0, 0] :> IFramebufferOutput])
            
        createSelection.Update(AdaptiveToken.Top, RenderToken.Empty)

        let levelSizes =
            let mutable s = treeSize
            Array.init 16 (fun _i ->
                let mine = s
                s <- V2i(max 1 (s.X / 2), max 1 (s.Y / 2))
                mine
            )

        let levelPixelSizes =
            levelSizes |> Array.map (fun s ->
                treeSize / s
            )


        let renderOutline =
            Sg.fullScreenQuad
            |> Sg.depthTest' DepthTest.None
            |> Sg.shader { do! PickShader.outline }
            |> Sg.texture' "QuadTreeTexture" selectionTexture
            |> Sg.uniform' "Distance" 10.0
            |> Sg.uniform' "Levels" levels
            |> Sg.uniform' "LevelSizes" levelSizes
            |> Sg.uniform' "LevelPixelSizes" levelPixelSizes
            |> Sg.blendMode' BlendMode.Blend
            |> Sg.compile runtime outputSignature

        let run () =
            createSelection.Run(fbo)
            runtime.GenerateMipMaps selectionTexture
  
        let release() =
            createSelection.Dispose()
            signature.Dispose()
            runtime.DeleteFramebuffer fbo
            runtime.DeleteTexture selectionTexture
            
        renderOutline, run, release
        
    

    
[<AutoOpen>]
module internal BlitExtensions =
    let private pickBuffer = Symbol.Create "PickId"
    let private pickViewPosition = Symbol.Create "PickViewPosition"
    //module private GL =
    //    open OpenTK.Graphics.OpenGL4
    //    open Aardvark.Rendering.GL
    //    let blit (src : Framebuffer) (dst : Framebuffer) =
    //        let drawBuffer =
    //            if dst.Handle = 0 then DrawBufferMode.BackLeft
    //            else DrawBufferMode.ColorAttachment0
            
    //        GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, src.Handle)
    //        GL.BindFramebuffer(FramebufferTarget.DrawFramebuffer, dst.Handle)
            
    //        GL.DrawBuffer(drawBuffer)
    //        GL.ReadBuffer(ReadBufferMode.ColorAttachment0)
    //        GL.BlitFramebuffer(
    //            0, 0, src.Size.X, src.Size.Y,
    //            0, 0, dst.Size.X, dst.Size.Y,
    //            ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit ||| ClearBufferMask.StencilBufferBit,
    //            BlitFramebufferFilter.Nearest
    //        )
    //        GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, dst.Handle)
            
    //    let readPixel (src : Framebuffer) (px : V2i) =
    //        use __ = src.Context.ResourceLock
    //        GL.BindFramebuffer(FramebufferTarget.Framebuffer, src.Handle)
    //        let res = [| V4i.Zero |]
    //        let (KeyValue(slot, _)) = src.Signature.Layout.ColorAttachments |> Seq.find (fun (KeyValue(id, sem)) -> sem.Name = pickBuffer)
    //        GL.ReadBuffer(unbox (int ReadBufferMode.ColorAttachment0 + slot))
    //        GL.BindBuffer(BufferTarget.PixelPackBuffer, 0)
    //        GL.BindBuffer(BufferTarget.PixelUnpackBuffer, 0)
    //        GL.ReadPixels(px.X, src.Size.Y - 1 - px.Y, 1, 1, PixelFormat.RgbaInteger, PixelType.Int, res)
    //        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
    //        res.[0]
        
    //module Vulkan =
    //    open Aardvark.Rendering.Vulkan
    //    let blit (src : Framebuffer) (dst : Framebuffer) =
    //        let device = src.Device
            
    //        device.perform {
    //            for (KeyValue(name, srcView)) in src.Attachments do
    //                match dst.Attachments.TryGetValue name with
    //                | (true, dstView) ->
    //                    let ap = if name = DefaultSemantic.DepthStencil then TextureAspect.DepthStencil else TextureAspect.Color
    //                    let lSrc = srcView.Image.Layout
    //                    let lDst = dstView.Image.Layout
    //                    do! Command.TransformLayout(srcView.Image, VkImageLayout.TransferSrcOptimal)
    //                    do! Command.TransformLayout(dstView.Image, VkImageLayout.TransferDstOptimal)
    //                    do! Command.Copy(srcView.Image.[ap, 0, *], dstView.Image.[ap, 0, *])
    //                    do! Command.TransformLayout(srcView.Image, lSrc)
    //                    do! Command.TransformLayout(dstView.Image, lDst)
    //                | _ ->
    //                    ()
    //        }
                      
    //    let readPixel (src : Framebuffer) (px : V2i) =
    //        let att = src.Attachments.[pickBuffer].Image :> IBackendTexture
    //        let rt = src.Device.Runtime
    //        let dst = PixImage<int>(Col.Format.RGBA, V2i.II)
    //        rt.Download(att.[TextureAspect.Color, 0, 0], dst, px, V2i.II)
    //        let r = V4i dst.Volume.Data
    //        r
            
    type IFramebufferRuntime with
        member x.BlitFramebuffer(src : IFramebuffer, dst : IFramebuffer) =
            x.Copy(src, dst)
                
        // member x.ReadPixel(src : IFramebuffer, pixel : V2i) : V4i =
        //     let img = x.ReadPixels(src, pickBuffer, pixel, V2i.II) :?> PixImage<float32>
        //     use ptr = fixed img.Data
        //     let iptr = NativePtr.ofNativeInt<int> (NativePtr.toNativeInt ptr)
        //     V4i(NativePtr.get iptr 0, NativePtr.get iptr 1, NativePtr.get iptr 2, NativePtr.get iptr 3)
        //     //V4i img.Volume.Data
        
        member x.ReadPickInfo(src : IFramebuffer, projTrafo : Trafo3d, pixel : V2i) =
            let img = x.ReadPixels(src, pickBuffer, pixel, V2i.II) :?> PixImage<int>
            use ptr = fixed img.Data
            let fptr = NativePtr.ofNativeInt<float32> (NativePtr.toNativeInt ptr)
            let iptr = NativePtr.ofNativeInt<int> (NativePtr.toNativeInt ptr)
            
            let i0 = int img.Volume.Origin
            let i1 = i0 + int img.Volume.DZ
            let i2 = i1 + int img.Volume.DZ
            let i3 = i2 + int img.Volume.DZ
            // let pickIdWithRealPosition(v : Vertex) =
            //     fragment {
            //         let n32 = Normal32.encode (Vec.normalize v.vn) |> int
            //         let len = Vec.length v.pvp
            //         let dir = Normal32.encode (v.pvp / len) |> int
            //         return { c = v.c; id = V4d(Bitwise.IntBitsToFloat -uniform.PickId, Bitwise.IntBitsToFloat n32, Bitwise.IntBitsToFloat dir, len) }
            //     }
            //     
            // let pickId(v : Vertex) =
            //     fragment {
            //         let n32 = Normal32.encode (Vec.normalize v.vn) |> int
            //         let d = (2.0 * v.d - 1.0) 
            //         return { c = v.c; id = V4d(Bitwise.IntBitsToFloat uniform.PickId, Bitwise.IntBitsToFloat n32, d, 0.0) }
            //     }
            //     
            // let pickIdNoNormal(v : Vertex) =
            //     fragment {
            //         let n32 = 0
            //         let d = (2.0 * v.d - 1.0) 
            //         return { c = v.c; id = V4d(Bitwise.IntBitsToFloat uniform.PickId, Bitwise.IntBitsToFloat n32, d, 0.0) }
            //     }
            let id = NativePtr.get iptr i0
            if id > 0 then
                let normal = Normal32.decode (NativePtr.get iptr i1)
                let depth = NativePtr.get fptr i2 |> float
                
                let tc = (V2d pixel + V2d.Half) / V2d src.Size
                let ndc = V3d(2.0 * tc.X - 1.0, 1.0 - 2.0 * tc.Y, float depth)
                let viewPos = projTrafo.Backward.TransformPosProj ndc
                Some (id, viewPos, normal)
            elif id < 0 then
                let normal = Normal32.decode (NativePtr.get iptr i1)
                let direction = Normal32.decode (NativePtr.get iptr i2)
                let distance = NativePtr.get fptr i3
                let viewPos = direction * float distance
                Some (-id, viewPos, normal)
            else
                None
                
            
            
            
            
        
        
type private SceneHandlerFramebuffers =
    {
        PickableFramebuffer         : IFramebuffer
        NonPickableFramebuffer      : IFramebuffer
        PickBuffer                 : IRenderbuffer
        PickTextureResolved         : IBackendTexture
        PickFramebufferResolved     : IFramebuffer
        Disposables                 : list<System.IDisposable>

    }


type private Stats(maxCnt : int) =
    let mutable sum = 0.0
    let mutable sumSq = 0.0
    let elements = System.Collections.Generic.Queue<float>(maxCnt + 1)

    member x.Add(value : float) =
        sum <- sum + value
        sumSq <- sumSq + value * value
        elements.Enqueue(value)

        if elements.Count > maxCnt then
            let o = elements.Dequeue()
            sum <- sum - o
            sumSq <- sumSq - o * o

    member x.Average = 
        let c = elements.Count
        if c <= 0 then 0.0
        else sum / float c

    member x.StdDev = 
        let c = elements.Count
        if c > 1 then
            let avg = sum / float c
            sqrt (max 0.0 ((sumSq - avg * sum) / float (c - 1)))
        else
            0.0


type SceneHandler(signature : IFramebufferSignature, trigger : RenderControlEvent -> unit, setCursor : option<string> -> unit, scene : ISceneNode, view : aval<Trafo3d>, proj : aval<Trafo3d>, fboSize : cval<V2i>, time : cval<System.DateTime>) =
    static let pickBuffer = Symbol.Create "PickId"
    
    let runtime = signature.Runtime :?> IRuntime

    let mutable currentId = 1
    let pickIds = Dict<TraversalState, int>()
    let scopes = Dict<int, TraversalState>()

    let mutable pickTexture : option<IBackendTexture * IFramebuffer> = None
    //let mutable attachments = []
    let mutable fbos : option<SceneHandlerFramebuffers> = None
    let mutable viewportSize = V2i.Zero
    let hoverId = cval -1
    let lastOver : cval<option<TraversalState>> = cval None
    let lastFocus : cval<option<TraversalState>> = cval None
    
    let cursor = 
        lastOver |> AVal.bind (function
            | Some l -> l.Cursor
            | None -> AVal.constant None
        )

    let cursorSub =
        cursor.AddCallback setCursor


    let clearColor = cval C4f.Black

    let getId(scope : TraversalState) =
        pickIds.GetOrCreate(scope, fun s -> 
            let i = currentId
            currentId <- i + 1
            scopes.[i] <- s
            i
        )

    let mutable lastMousePosition = None
     
    let read (projTrafo : Trafo3d) (pixel : V2i) =
        match pickTexture with
        | Some (pickTexture, pickFbo) when pixel.AllGreaterOrEqual 0 && pixel.AllSmaller pickTexture.Size.XY ->
            match runtime.ReadPickInfo(pickFbo, projTrafo, pixel) with
            | Some (id, viewPos, normal) ->
                transact (fun () -> hoverId.Value <- id)
                match scopes.TryGetValue id with
                | true, scope ->
                    let depth = projTrafo.TransformPosProj(viewPos).Z
                    Some (scope, float depth, viewPos, normal)
                | _ ->
                    None
                
            | None ->
                transact (fun () -> hoverId.Value <- 0)
                None
            //
            // let value = runtime.ReadPixel(pickFbo, pixel)
            //
            // let id = value.X
            // transact (fun () -> hoverId.Value <- id)
            // if id > 0 then
            //     match scopes.TryGetValue id with
            //     | true, scope ->
            //         let n = value.Y |> Normal32.decode
            //         let depth = MemoryMarshal.Cast<int, float32>(System.Span<int> [|value.Z|]).[0]
            //         let tc = (V2d pixel + V2d.Half) / V2d pickTexture.Size.XY
            //         let ndc = V3d(2.0 * tc.X - 1.0, 1.0 - 2.0 * tc.Y, float depth)
            //         let viewPos = projTrafo.Backward.TransformPosProj ndc
            //         
            //         Some (scope, float depth, viewPos, n)
            //     | _ ->
            //         None
            // elif id < 0 then
            //     let id = -id
            //     match scopes.TryGetValue id with
            //     | true, scope ->
            //         let n = value.Y |> Normal32.decode
            //         let vd = value.Z |> Normal32.decode
            //         let vl = MemoryMarshal.Cast<int, float32>(System.Span<int> [|value.W|]).[0] |> float
            //         
            //         let viewPos = vd * vl
            //         let depth = projTrafo.TransformPosProj(viewPos).Z
            //         Some (scope, float depth, viewPos, n)
            //     | _ ->
            //         None
            // else
            //     None
        | _ ->
            None
             

    let mutable renderTask, pickObjects, dispose =
        let runtime = signature.Runtime :?> IRuntime
        
        let pickId = signature.ColorAttachmentSlots

        let colorAttachments =
            Map.add pickId { Name = pickBuffer; Format = TextureFormat.Rgba32i } signature.ColorAttachments

        let newSignature =
            runtime.CreateFramebufferSignature(colorAttachments, signature.DepthStencilAttachment, signature.Samples, signature.LayerCount, signature.PerLayerUniforms)

        let render, pick = scene.GetObjects(TraversalState.empty runtime)
        

        let rec wrapObject (t : TraversalState) (o : IRenderObject) =
            if t.PixelPick then
                match o with
                | :? RenderObject as o ->
                    let pickId = getId(t)
                    match o.Surface with
                    | Surface.Effect eff ->
                        let newShaders =
                            lazy (
                                let hasAllInputs (effect : FShade.Effect) =
                                    let m = Effect.link newSignature o.Mode false effect

                                    let vertex = 
                                        m.Entries |> List.find (fun e -> 
                                            e.decorations |> List.exists (function 
                                                | EntryDecoration.Stages (ShaderStageDescription.Graphics { self = FShade.ShaderStage.Vertex }) -> true 
                                                | _ -> false
                                            )
                                        )
                                
                                    let hasVertexInputs = 
                                        vertex.inputs |> List.forall (fun p -> 
                                            Option.isSome (o.VertexAttributes.TryGetAttribute (Symbol.Create p.paramSemantic)) ||
                                            Option.isSome (o.InstanceAttributes.TryGetAttribute (Symbol.Create p.paramSemantic))
                                        )
                                    
                                    //let hasUniforms = 
                                    //    m.entries |> List.collect (fun e -> e.uniforms) |> List.forall (fun u -> 
                                    //        let has = 
                                    //            u.uniformName = "PickId" ||
                                    //            Option.isSome (Uniforms.tryGetDerivedUniform u.uniformName o.Uniforms) ||
                                    //            Option.isSome (o.Uniforms.TryGetUniform(Ag.Scope.Root, u.uniformName))
                                    //        if not has then Log.warn "missing: %A" u
                                    //        has
                                    //    )
                                    hasVertexInputs

                                let hasPickPositions =
                                    Map.containsKey "PickViewPosition" eff.Outputs
                                    
                                let newShader =
                                    if hasPickPositions then
                                        FShade.Effect.compose [PickShader.vertexPickEffect; PickShader.pickEffectBefore; eff; PickShader.pickEffectWithRealPosition]
                                    else
                                        let withNormal = FShade.Effect.compose [PickShader.vertexPickEffect; PickShader.pickEffectBefore; eff; PickShader.pickEffect]
                                        
                                        if hasAllInputs withNormal then
                                            withNormal
                                        else
                                            FShade.Effect.compose [PickShader.pickEffectBefore; eff; PickShader.pickEffectNoNormal]
                                newShader.Shaders
                            )
                            
                        let newEffect = FShade.Effect("ipick_" + eff.Id, newShaders, [])
                            
                        let r = RenderObject.Clone o
                        
                        let newBlendState =
                            let newModes = 
                                o.BlendState.AttachmentMode |> AVal.map (fun map ->
                                    Map.add pickBuffer BlendMode.None map    
                                )
                            let newWrites =
                                o.BlendState.AttachmentWriteMask |> AVal.map (fun map ->
                                    Map.add pickBuffer ColorMask.All map
                                )
                            
                            {
                                Mode = o.BlendState.Mode
                                AttachmentMode = newModes
                                AttachmentWriteMask = newWrites
                                ConstantColor = o.BlendState.ConstantColor
                                ColorWriteMask = o.BlendState.ColorWriteMask
                            }
                        
                        
                        r.Uniforms <- UniformProvider.union o.Uniforms (UniformProvider.ofList ["PickId", AVal.constant pickId :> IAdaptiveValue])
                        r.Surface <- Surface.Effect newEffect
                        r.BlendState <- newBlendState
                        r :> IRenderObject, true
                    | s ->
                        Log.warn "cannot change surface: %A" s
                        o :> IRenderObject, true
              
                | :? MultiRenderObject as o ->
                    let res = o.Children |> List.map (wrapObject t)
                    if res |> List.forall snd then
                        let n = MultiRenderObject(List.map fst res)
                        n :> IRenderObject, true
                    else
                        o :> IRenderObject, false
                | o ->
                    Log.warn "cannot wrap object: %A" o
                    o, false
            else
                o, false
            

        let objs = 
            render
            |> ASet.map (fun o ->
                match RenderObject.traversalStates.TryGetValue o with
                | true, t ->
                    wrapObject t o
                | _ ->
                    o, false
            )

        let pickable = objs |> ASet.choose (fun (o, p) -> if p then Some o else None)
        let nonPickable = objs |> ASet.choose (fun (o, p) -> if not p then Some o else None)

        let renderPickable = runtime.CompileRender(newSignature, pickable)
        let renderNonPickable = runtime.CompileRender(signature, nonPickable)

   
        let getFramebuffers (size : V2i) =
            match fbos with
            | Some o when o.PickableFramebuffer.Size = size ->
                o
            | _ ->
                match fbos with
                | Some o ->
                    runtime.DeleteFramebuffer o.PickableFramebuffer
                    runtime.DeleteFramebuffer o.NonPickableFramebuffer
                    runtime.DeleteFramebuffer o.PickFramebufferResolved
                    //for f in o.PickLevelFramebuffers do runtime.DeleteFramebuffer f
                    for t in o.Disposables do t.Dispose()
                | None ->
                    ()
                    
                let semantics =
                    let res = newSignature.ColorAttachments |> Map.toList |> List.map (fun (_slot, a) -> a.Name, a.Format) |> Map.ofList
                    match newSignature.DepthStencilAttachment with
                    | Some ds -> Map.add DefaultSemantic.DepthStencil ds res
                    | None -> res
                    
                let buffers, outputs =
                    let buffers =
                        semantics |> Map.map (fun _ a ->
                            runtime.CreateRenderbuffer(size, a, newSignature.Samples)    
                        )
                    let outputs = buffers |> Map.map (fun _ b -> b :> IFramebufferOutput)
                    buffers, outputs
                    // if newSignature.Samples > 1 then
                    //     let buffers =
                    //         semantics |> Map.map (fun _ a ->
                    //             runtime.CreateRenderbuffer(size, a, newSignature.Samples)    
                    //         )
                    //     let outputs = buffers |> Map.map (fun _ b -> b :> IFramebufferOutput)
                    //     Map.empty, outputs
                    // else
                    //     let textures =
                    //         semantics |> Map.map (fun _ a ->
                    //             if newSignature.LayerCount > 1 then runtime.CreateTexture2DArray(size, a, 1, newSignature.Samples, newSignature.LayerCount)
                    //             else runtime.CreateTexture2D(size, a, 1, newSignature.Samples)
                    //         )
                    //     let outputs =
                    //         textures |> Map.map (fun _ t -> t.[TextureAspect.Color, 0, *] :> IFramebufferOutput)
                    //     textures, outputs
                let nf = runtime.CreateFramebuffer(signature, outputs)
                let pf = runtime.CreateFramebuffer(newSignature, outputs)
                
                let pickResolvedTex, pickResolved =
                    let tex = runtime.CreateTexture2D(size, TextureFormat.Rgba32i, 1, 1)
                    let s = runtime.CreateFramebufferSignature([pickBuffer, TextureFormat.Rgba32i])
                    //let fbo = runtime.CreateFramebuffer(s, [pickBuffer, tex.[TextureAspect.Color, 0, *] :> IFramebufferOutput])
                    
                    //let levelFbos =
                    //    Array.init levels (fun l ->
                    //        runtime.CreateFramebuffer(s, [pickBuffer, tex.[TextureAspect.Color, l, *] :> IFramebufferOutput])
                    //    )

                    let fbo = runtime.CreateFramebuffer(s, [pickBuffer, tex.[TextureAspect.Color, 0, 0] :> IFramebufferOutput])
                    
                    (tex, fbo)
                
                //let sTex, rSel, dSel = PickBuffer.compileQuadTree signature hoverId pickResolvedTex

                let result =
                    {
                        PickableFramebuffer         = pf
                        NonPickableFramebuffer      = nf
                        PickFramebufferResolved     = pickResolved
                        PickBuffer                  = buffers.[pickBuffer]
                        PickTextureResolved         = pickResolvedTex
                        Disposables                 = pickResolvedTex :: (buffers |> Map.toList |> List.map (fun (_, b) -> b :> System.IDisposable))
                        //RenderOutline               = sTex
                        //CreateSelection             = rSel
                        //DisposeSelection            = dSel
                    }
                
                
                //attachments <- textures |> Map.toList |> List.map snd
                fbos <- Some result
                result
                
               
        let clear = 
            let clearValues =
                clearColor
                |> AVal.map (fun color ->
                    ClearValues.empty
                    |> ClearValues.colors (Map.ofList [DefaultSemantic.Colors, ClearColor.op_Implicit color; pickBuffer, ClearColor.op_Implicit V4i.Zero])
                    |> ClearValues.depth 1.0
                    |> ClearValues.stencil 0
                )
            runtime.CompileClear (newSignature, clearValues)
        
        let mutable idx = 0
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let frameTimeWatch = System.Diagnostics.Stopwatch()
        let mutable frameTimeStats = Stats(30)

        let getInfo (size : V2i) =
            {
                Signature = signature
                Size = size
                FrameIndex = idx
                Time = sw.MicroTime
                FrameTime = MicroTime(System.TimeSpan(int64 frameTimeStats.Average))
            }
      

        let task = 
            RenderTask.custom (fun (t, _rt, o) ->
                frameTimeWatch.Restart()
                let size = o.framebuffer.Size
                let evtInfo = getInfo size

                if o.framebuffer.Size <> fboSize.Value then
                    transact (fun () -> fboSize.Value <- o.framebuffer.Size)
                    trigger (RenderControlEvent.Resize evtInfo)
               
                let s = fboSize.GetValue t
                let rt = RenderToken.Empty
                let outputInfo = getFramebuffers s

             
                trigger (RenderControlEvent.PreRender evtInfo)

                clear.Run(t, rt, outputInfo.PickableFramebuffer)
                renderPickable.Run(t, rt, outputInfo.PickableFramebuffer)
                renderNonPickable.Run(t, rt, outputInfo.NonPickableFramebuffer)
                let pickBuffer = outputInfo.PickBuffer
                if pickBuffer.Samples > 1 then runtime.ResolveMultisamples(pickBuffer, outputInfo.PickTextureResolved)
                else runtime.Copy(pickBuffer, outputInfo.PickTextureResolved.[TextureAspect.Color, 0, *])
                
                pickTexture <- Some (outputInfo.PickTextureResolved, outputInfo.PickFramebufferResolved)
                viewportSize <- outputInfo.PickTextureResolved.Size.XY
           
                trigger (RenderControlEvent.PostRender evtInfo)


                runtime.BlitFramebuffer(outputInfo.NonPickableFramebuffer, o.framebuffer)
                idx <- idx + 1
                frameTimeWatch.Stop()
                frameTimeStats.Add (float frameTimeWatch.Elapsed.Ticks)
                
            )

        let dispose() =
            clear.Dispose()
            renderPickable.Dispose()
            renderNonPickable.Dispose()
            task.Dispose()
            newSignature.Dispose()
            match fbos with
            | Some o ->
                runtime.DeleteFramebuffer o.PickableFramebuffer
                runtime.DeleteFramebuffer o.NonPickableFramebuffer
                for t in o.Disposables do t.Dispose()
                fbos <- None
            | None ->
                ()

        task, pick, dispose
    
    
    
    let mutable bvh =
        let transformedPickObject =
            pickObjects |> ASet.mapA (fun o ->
                o.Trafo |> AVal.map (fun t -> o.Intersectable, o.State, t)    
            )
            
        let reader = transformedPickObject.GetReader()
        let mutable tree = Aardvark.Dom.BvhTree3d.empty
        AVal.custom (fun token ->
            let ops = reader.GetChanges token
            
            for op in ops do
                match op with
                | Add _ -> ()
                | Rem(_, (_, s, _)) -> tree <- Aardvark.Dom.BvhTree3d.remove s tree

            for op in ops do
                match op with
                | Add(_, (i, s, t)) ->
                    let bb = i.BoundingBox.Transformed t
                    tree <- Aardvark.Dom.BvhTree3d.add s bb (i, t) tree
                | Rem _ ->
                    ()
            tree
        )
   

    let capturedScopes = Dict<int, TraversalState>()
    let mutable lastMouseInfo = None
    let mutable lastRealMouseInfo = None
    
    member private x.Dispose(disposing : bool) =
        printfn "SceneHandler died: %A" disposing
        if disposing then System.GC.SuppressFinalize x
        pickTexture <- None
        lastMouseInfo <- None
        lastRealMouseInfo <- None
        capturedScopes.Clear()
        dispose()
        renderTask <- RenderTask.empty
        pickObjects <- ASet.empty
        bvh <- AVal.constant BvhTree3d.empty
        cursorSub.Dispose()

    member x.Dispose() = x.Dispose true
    override x.Finalize() = x.Dispose false
    interface System.IDisposable with
        member x.Dispose() = x.Dispose()


    member x.Time = time
    member x.ClearColor = clearColor
    member x.Runtime = runtime
    member x.FramebufferSignature = signature

    member x.Cursor = cursor

    member x.RenderTask = renderTask
  

    member x.Read(pixel : V2i, pointerId : int, kind : SceneEventKind, ?evt : PointerEvent) =

        let capturedScope =
            match capturedScopes.TryGetValue pointerId with
            | true, s -> Some s
            | _ -> None

        let result = 
            if pixel.AllGreaterOrEqual 0 && pixel.AllSmaller viewportSize then
                let v = AVal.force view
                let p = AVal.force proj
                lastMousePosition <- Some pixel
                let pixelResult = read p pixel

                let s =
                    match pickTexture with
                    | Some(t,_) -> t.Size.XY
                    | None -> V2i.II

                let vp = v * p
                let ndc = V2d(2.0 * float pixel.X / float s.X - 1.0, 1.0 - 2.0 * float pixel.Y / float s.Y)
                let ray =
                    let p0 = vp.Backward.TransformPosProj(V3d(ndc, -1.0))
                    let d = vp.Backward.TransformPosProj(V3d(ndc, 0.0)) - p0 |> Vec.normalize
                    Ray3d(p0, d)
                
            
            
                let tryIntersect (skipPickThrough : bool) (tmin : float) (tmax : float) (state : TraversalState) (i : IIntersectable, trafo : Trafo3d) =
                    if AVal.force state.Active then
                        let mutable r = 0.0
                        let mutable n = V3d.Zero
                        let mutable hit = V3d.Zero
                        let skip =
                            if skipPickThrough then state.PickThrough
                            else false
                        let localRay = ray.Transformed(trafo.Backward)
                        if not skip && i.Intersects(localRay, tmin, tmax, &r, &hit, &n) then
                            let worldPoint = trafo.Forward.TransformPos hit
                            let depth = vp.Forward.TransformPosProj(worldPoint).Z
                            let vn = Vec.normalize (v.Backward.TransposedTransformDir (trafo.Backward.TransposedTransformDir n))
                            Some (r, (state, depth, v.Forward.TransformPos worldPoint, vn))
                        else
                            None
                    else
                        None
            
                let tryGetIntersection (skipPickThrough : bool) (tmin : float) (tmax : float) (bvh : BvhTree3d<TraversalState, _>) =
                    match bvh.GetClosestHit(ray, tmin, tmax, tryIntersect skipPickThrough tmin tmax) with
                    | Some (_, (bvhScope, bvhDepth, bvhViewPos, bvhViewNormal)) ->
                        match pixelResult with
                        | Some (scope, depth, viewPos, viewNormal) ->
                            if depth < bvhDepth then
                                Some (true, depth, scope, viewPos, viewNormal)
                            else
                                Some (false, bvhDepth, bvhScope, bvhViewPos, bvhViewNormal)
                        | None ->
                            Some (false, bvhDepth, bvhScope, bvhViewPos, bvhViewNormal)
                    | None ->
                        match pixelResult with
                        | Some (scope, depth, viewPos, normal) ->
                            Some (true, depth, scope, viewPos, normal)
                        | None ->
                            None

                
                
                
                
                // limit t if pixel-result found
                let mutable t = System.Double.PositiveInfinity
                match pixelResult with
                | Some (_, _, viewPos, _) ->
                    let world = v.Backward.TransformPos viewPos
                    t <- Vec.dot ray.Direction (world - ray.Origin)
                | None ->
                    ()
                
                let bvh = AVal.force bvh
                match tryGetIntersection false 0.0 t bvh with
                | Some (isPixelPick, _depth, scope, viewPos, viewNormal) ->
                    if scope.PickThrough then
                        if isPixelPick then
                            // TODO: any way to realize that??
                            Log.warn "cannot pick-through pixel-picked objects"
                            Some (scope, viewPos, viewNormal, None)
                        else
                            match tryGetIntersection true 0.0 t bvh with
                            | Some (_isPixel, _nDepth, nScope, nViewPos, nViewNormal) ->
                                
                                let rec hasEventHandler (kind : SceneEventKind) (scope : TraversalState) =
                                    HashMap.containsKey kind (AMap.force scope.EventHandlers) ||
                                    (Option.isSome scope.Parent && hasEventHandler kind (Option.get scope.Parent))
                                
                                if hasEventHandler kind scope then
                                    Some (scope, nViewPos, nViewNormal, Some nScope)
                                else
                                    Some (nScope, nViewPos, nViewNormal, None)
                                    
                            | None ->
                                Some (scope, viewPos, viewNormal, None)
                    else
                        Some (scope, viewPos, viewNormal, None)
                | None ->
                    None
            else
            
                None

        let capturedResult = 
            match capturedScope with
            | None -> 
                match result with
                | Some (state, depth, normal, nextScope) ->
                    let target =
                        match nextScope with
                        | Some n -> n
                        | None -> state
                    Some (state, target :> obj, depth, normal)
                | None ->
                    None
            | Some c ->
                match result with
                | Some (target, viewPos, normal, nextScope) -> Some (c, target :> obj, viewPos, normal)
                | None -> Some (c, null, V3d(0.0, 0.0, -1000000000.0), V3d.Zero)

        lastMouseInfo <- Some (pixel, capturedResult)
        lastRealMouseInfo <- Some (pixel, result, evt)

        capturedResult

    member x.DispatchPointerEvent(target : obj, evt : ScenePointerEvent) =
        if isNull target then
            match evt.Kind with
            | SceneEventKind.Click when Option.isSome lastFocus.Value -> 
                let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, evt.ViewTrafo, evt.ProjTrafo, evt.Pixel, evt.ViewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                let evt = ScenePointerEvent(x, lastFocus.Value.Value, null, evt.Kind, loc, evt.Original)
                TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
            | _ -> ()
                

            if Option.isSome lastOver.Value then
                let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, evt.ViewTrafo, evt.ProjTrafo, evt.Pixel, evt.ViewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                let evt = ScenePointerEvent(x, lastOver.Value.Value, null, evt.Kind, loc, evt.Original)
                TraversalState.handleMove lastOver evt None

            true
        else
            let view = evt.ViewTrafo
            let proj = evt.ProjTrafo
            let best = target :?> TraversalState
            let viewPos = evt.ViewPosition
            let viewNormal = evt.ViewNormal
            let model = TraversalState.modelTrafo best
            let loc = SceneEventLocation(model, view, proj, evt.Pixel, evt.ViewportSize, viewPos, viewNormal)
            let evt = ScenePointerEvent(x, best, target, evt.Kind, loc, evt.Original)

            TraversalState.handleMove lastOver evt (Some best)

            let rec findFocusable (state : TraversalState) =
                if state.CanFocus then
                    Some state
                else
                    match state.Parent with
                    | Some p -> findFocusable p
                    | None -> None


            match evt.Kind with
            | SceneEventKind.Click ->
                match findFocusable best with
                | Some f -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt (Some f)
                | None -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
            | _ ->
                ()

            TraversalState.handleEvent true evt best
    
    member x.HandlePointerEvent(kind : SceneEventKind, original : PointerEvent) : bool =
        let s = viewportSize
        if s.AllGreater 0 then
            let view = AVal.force view 
            let proj = AVal.force proj
            let scrollDelta = V2d.Zero

            let pixel = original.ClientPosition - V2i original.ClientRect.Min

            match x.Read(pixel, original.PointerId, kind, original) with
            | Some (best, target, viewPos, viewNormal) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, viewPos, viewNormal)
                let evt = ScenePointerEvent(x, best, target, kind, loc, original)

                TraversalState.handleMove lastOver evt (Some best)

                let rec findFocusable (state : TraversalState) =
                    if state.CanFocus then
                        Some state
                    else
                        match state.Parent with
                        | Some p -> findFocusable p
                        | None -> None


                match kind with
                | SceneEventKind.Click ->
                    match findFocusable best with
                    | Some f -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt (Some f)
                    | None -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
                | _ ->
                    ()

                TraversalState.handleEvent true evt best
            | None ->
                match kind with
                | SceneEventKind.Click when Option.isSome lastFocus.Value -> 
                    let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d pixel, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                    let evt = ScenePointerEvent(x, lastFocus.Value.Value, null, kind, loc, original)
                    TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
                | _ -> ()
                    

                if Option.isSome lastOver.Value then
                    let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d pixel, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                    let evt = ScenePointerEvent(x, lastOver.Value.Value, null, kind, loc, original)
                    TraversalState.handleMove lastOver evt None

                true
        else
            true
        
    member x.HandleTapEvent(kind : SceneEventKind, original : TapEvent) : bool =
        let s = viewportSize
        if s.AllGreater 0 then
            let view = AVal.force view 
            let proj = AVal.force proj

            let pixel = original.ClientPosition - V2i original.ClientRect.Min

            match x.Read(pixel, original.PointerId, kind) with
            | Some (best, target, depth, viewNormal) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, depth, viewNormal)
                let evt = SceneTapEvent(x, best, target, kind, loc, original)
                TraversalState.handleEvent true evt best
            | None ->
                true
        else
            true
        
    member x.HandleWheelEvent(kind : SceneEventKind, original : WheelEvent) : bool =
        let s = viewportSize
        if s.AllGreater 0 then
            let view = AVal.force view 
            let proj = AVal.force proj

            let pixel = original.ClientPosition - V2i original.ClientRect.Min

            match x.Read(pixel, -1, kind) with
            | Some (best, target, depth, viewNormal) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, depth, viewNormal)
                let evt = SceneWheelEvent(x, best, target, kind, loc, original)
                TraversalState.handleEvent true evt best
            | None ->
                true
        else
            true
        
    member x.HandleKeyEvent(kind : SceneEventKind, original : KeyboardEvent) : bool =
        let s = viewportSize

        match lastFocus.Value with
        | Some best ->
            let model = TraversalState.modelTrafo best
            let view = AVal.force view 
            let proj = AVal.force proj
            let evtLocation =
                match lastMouseInfo with
                | Some (px, Some (_, _, depth, viewNormal)) ->  SceneEventLocation(model, view, proj, V2d px, s, depth, viewNormal)
                | _ ->
                    match lastMousePosition with
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                
            let evt = SceneKeyboardEvent(x, best, best, kind, evtLocation, original)
            TraversalState.handleEvent true evt best

        | None ->
            true
                
    member x.HandleInputEvent(kind : SceneEventKind, original : InputEvent) : bool =
        let s = viewportSize

        match lastFocus.Value with
        | Some best ->
            let model = TraversalState.modelTrafo best
            let view = AVal.force view 
            let proj = AVal.force proj
            let evtLocation =
                match lastMouseInfo with
                | Some (px, Some (_, _, depth, viewNormal)) ->  SceneEventLocation(model, view, proj, V2d px, s, depth, viewNormal)
                | _ ->
                    match lastMousePosition with
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                
            let evt = SceneInputEvent(x, best, best, kind, evtLocation, original)
            TraversalState.handleEvent true evt best

        | None ->
            true

    member x.SetFocus(newTarget : option<TraversalState>) =
        if lastFocus.Value <> newTarget then
            let view = AVal.force view 
            let proj = AVal.force proj

            let model =
                match newTarget with
                | Some t -> TraversalState.modelTrafo t
                | None -> AVal.constant Trafo3d.Identity


            let evtLocation =
                match lastMouseInfo with
                | Some (px, Some (_, _, depth, viewNormal)) ->  SceneEventLocation(model, view, proj, V2d px, viewportSize, depth, viewNormal)
                | _ ->
                    match lastMousePosition with
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, viewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, viewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero)

            let target =
                match newTarget with
                | Some s -> s :> obj
                | None -> null

            let original = Event(None, "", 0.0, true, "focusout", Box2d.Invalid)
            let evt = PlainSceneEvent(x, target, target, SceneEventKind.FocusLeave, evtLocation, original)
            TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt newTarget
                
    interface IEventHandler with
        member x.Read(pixel : V2i, kind : SceneEventKind) =
            let s = viewportSize
            let view = AVal.force view 
            let proj = AVal.force proj
            match x.Read(pixel, -1, kind) with
            | Some (best,_,depth,viewNormal) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, depth, viewNormal)
                Some loc
            | None ->
                None
            
        member x.Size = viewportSize
            
        member x.SetFocus(dst : option<obj>) =
            match dst with
            | Some (:? TraversalState as s) -> x.SetFocus(Some s)
            | _ -> x.SetFocus None
            
        member x.DispatchPointerEvent(target : obj, e : ScenePointerEvent) =
            x.DispatchPointerEvent(target, e)
            
        member x.HandlePointerEvent(kind : SceneEventKind, original : PointerEvent) =
            x.HandlePointerEvent(kind, original)
              
        member x.HandleKeyEvent(kind : SceneEventKind, original : KeyboardEvent) =
            x.HandleKeyEvent(kind, original)
   
        member x.HandleInputEvent(kind : SceneEventKind, original : InputEvent) =
            x.HandleInputEvent(kind, original)
   
        member x.HandleTapEvent(kind : SceneEventKind, original : TapEvent) =
            x.HandleTapEvent(kind, original)
            
        member x.HandleWheelEvent(kind : SceneEventKind, original : WheelEvent) =
            x.HandleWheelEvent(kind, original)
   
        member x.HasPointerCapture(state : obj, pointerId : int) =
            match capturedScopes.TryGetValue pointerId with
            | (true, s) -> s :> obj = state
            | _ -> false

        member x.SetPointerCapture(state : obj, pointerId : int) =
            let state = state :?> TraversalState
            capturedScopes.[pointerId] <- state
            
        member x.ReleasePointerCapture(state : obj, pointerId : int) =
            capturedScopes.Remove pointerId |> ignore
            match lastRealMouseInfo with
            | Some (px, scope, originalEvt) ->
                let view = AVal.force view 
                let proj = AVal.force proj
                let loc, target = 
                    match scope with
                    | Some (scope, depth, viewNormal, _) -> SceneEventLocation(TraversalState.modelTrafo scope, view, proj, V2d px, viewportSize, depth, viewNormal), Some scope
                    | None -> SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d px, viewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero), None

                let eventTarget =
                    match target with
                    | Some t -> t :> obj
                    | None -> null

                let original = 
                    match originalEvt with
                    | Some e -> e
                    | None -> PointerEvent(None, "", 0.0, true, "pointermove", Box2d.Invalid, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, false, false, false, false, Button.None, Buttons.None, -1, 0.0, 0.0, 0.0, 0.0, 0.0, PointerType.Mouse)

                let evt = ScenePointerEvent(x, eventTarget, eventTarget, SceneEventKind.PointerMove, loc, original)
                TraversalState.handleMove lastOver evt target
                match target with
                | Some t -> TraversalState.handleEvent true evt t |> ignore
                | None -> ()
            | None ->
                ()

            
        member x.Cursor = x.Cursor