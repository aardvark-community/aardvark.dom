namespace Aardvark.Dom

open System.Runtime.InteropServices
open FShade.Imperative
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.Dom

[<ReflectedDefinition>]
module internal Normal32 =
    let private sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let private clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )

    let decode (v : uint32) : V3d =
        if v = 0u then
            V3d.Zero
        else
            let e = V2d(float (v >>> 16) / 65535.0, float (v &&& 0xFFFFu) / 65535.0) * 2.0 - V2d.II
            let v = V3d(e, 1.0 - abs e.X - abs e.Y)
            if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
            else v |> Vec.normalize

    let encode (v : V3d) : uint32 =
        if v.X = 0.0 && v.Y = 0.0 && v.Z = 0.0 then
            0u
        else
            let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
            let p = 
                if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
                else clamp p
        
            let x0 = floor ((p.X * 0.5 + 0.5) * 65535.0) |> int
            let y0 = floor ((p.Y * 0.5 + 0.5) * 65535.0) |> int

            let mutable bestDot = 0.0
            let mutable best = 0u

            for dx in 0 .. 1 do
                for dy in 0 .. 1 do
                    let e = uint32 (((x0 + dx) <<< 16) ||| (y0 + dy))
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
                vn = vn
                fc = V4d.Zero
            }
        }
        
    [<GLSLIntrinsic("gl_FragCoord")>]
    let fragCoord() : V4d = onlyInShaderCode "fragcoord"
    
    let pickId(v : Vertex) =
        fragment {
            let n32 = Normal32.encode (Vec.normalize v.vn) |> int
            let d = (2.0 * fragCoord().Z - 1.0) |> Bitwise.FloatBitsToInt
            return { c = v.c; id = V4i(uniform.PickId, n32, d, 0) }
        }
        
    let pickIdNoNormal(v : Vertex) =
        fragment {
            let n32 = Normal32.encode V3d.Zero |> int
            let d = (2.0 * fragCoord().Z - 1.0) |> Bitwise.FloatBitsToInt
            return { c = v.c; id = V4i(uniform.PickId, n32, d, 0) }
        }

    let vertexPickEffect = Effect.ofFunction pickVertex
    let pickEffect = Effect.ofFunction pickId
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
            Array.init 16 (fun i ->
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
    module private GL =
        open OpenTK.Graphics.OpenGL4
        open Aardvark.Rendering.GL
        let blit (src : Framebuffer) (dst : Framebuffer) =
            let drawBuffer =
                if dst.Handle = 0 then DrawBufferMode.BackLeft
                else DrawBufferMode.ColorAttachment0
            
            GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, src.Handle)
            GL.BindFramebuffer(FramebufferTarget.DrawFramebuffer, dst.Handle)
            
            GL.DrawBuffer(drawBuffer)
            GL.ReadBuffer(ReadBufferMode.ColorAttachment0)
            GL.BlitFramebuffer(
                0, 0, src.Size.X, src.Size.Y,
                0, 0, dst.Size.X, dst.Size.Y,
                ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit ||| ClearBufferMask.StencilBufferBit,
                BlitFramebufferFilter.Nearest
            )
            GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, dst.Handle)
            
        let readPixel (src : Framebuffer) (px : V2i) =
            use __ = src.Context.ResourceLock
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, src.Handle)
            let res = [| V4i.Zero |]
            let (KeyValue(slot, _)) = src.Signature.Layout.ColorAttachments |> Seq.find (fun (KeyValue(id, sem)) -> sem.Name = pickBuffer)
            GL.ReadBuffer(unbox (int ReadBufferMode.ColorAttachment0 + slot))
            GL.BindBuffer(BufferTarget.PixelPackBuffer, 0)
            GL.BindBuffer(BufferTarget.PixelUnpackBuffer, 0)
            GL.ReadPixels(px.X, src.Size.Y - 1 - px.Y, 1, 1, PixelFormat.RgbaInteger, PixelType.Int, res)
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
            res.[0]
            
            
    module Vulkan =
        open Aardvark.Rendering.Vulkan
        let blit (src : Framebuffer) (dst : Framebuffer) =
            let device = src.Device
            
            device.perform {
                for (KeyValue(name, srcView)) in src.Attachments do
                    match dst.Attachments.TryGetValue name with
                    | (true, dstView) ->
                        let ap = if name = DefaultSemantic.DepthStencil then TextureAspect.DepthStencil else TextureAspect.Color
                        let lSrc = srcView.Image.Layout
                        let lDst = dstView.Image.Layout
                        do! Command.TransformLayout(srcView.Image, VkImageLayout.TransferSrcOptimal)
                        do! Command.TransformLayout(dstView.Image, VkImageLayout.TransferDstOptimal)
                        do! Command.Copy(srcView.Image.[ap, 0, *], dstView.Image.[ap, 0, *])
                        do! Command.TransformLayout(srcView.Image, lSrc)
                        do! Command.TransformLayout(dstView.Image, lDst)
                    | _ ->
                        ()
            }
                      
        let readPixel (src : Framebuffer) (px : V2i) =
            let att = src.Attachments.[pickBuffer].Image :> IBackendTexture
            let rt = src.Device.Runtime
            let dst = PixImage<int>(Col.Format.RGBA, V2i.II)
            rt.Download(att.[TextureAspect.Color, 0, 0], dst, px, V2i.II)
            let r = V4i dst.Volume.Data
            r
            
    type IRuntime with
        member x.BlitFramebuffer(src : IFramebuffer, dst : IFramebuffer) =
            match x with
            | :? Aardvark.Rendering.GL.Runtime as r ->
                GL.blit (unbox src) (unbox dst)
            | _ ->
                Vulkan.blit (unbox src) (unbox dst)
                
        member x.ReadPixel(src : IFramebuffer, pixel : V2i) =
            match x with
            | :? Aardvark.Rendering.GL.Runtime as r ->
                GL.readPixel (unbox src) pixel
            | _ ->
                Vulkan.readPixel (unbox src) pixel
        
        
type private SceneHandlerFramebuffers =
    {
        PickableFramebuffer         : IFramebuffer
        NonPickableFramebuffer      : IFramebuffer
        PickTexture                 : IBackendTexture
        PickTextureResolved         : IBackendTexture
        PickFramebufferResolved     : IFramebuffer
        Textures                    : list<IBackendTexture>

    }
         

type SceneHandlerEvent =
    | Resize of V2i
    | PreRender
    | PostRender


type SceneHandler(signature : IFramebufferSignature, trigger : SceneHandlerEvent -> unit, setCursor : option<Cursor> -> unit, scene : ISceneNode, view : aval<Trafo3d>, proj : aval<Trafo3d>, fboSize : cval<V2i>, time : cval<System.DateTime>) =
    static let pickBuffer = Symbol.Create "PickId"
    
    let runtime = signature.Runtime :?> IRuntime

    let mutable currentId = 0
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


    let getId(scope : TraversalState) =
        pickIds.GetOrCreate(scope, fun s -> 
            let i = currentId
            currentId <- i + 1
            scopes.[i] <- s
            i
        )

    let mutable lastMousePosition = None
     
    let read (pixel : V2i) =
        match pickTexture with
        | Some (pickTexture, pickFbo) when pixel.AllGreaterOrEqual 0 && pixel.AllSmaller pickTexture.Size.XY ->
            let value = runtime.ReadPixel(pickFbo, pixel)
            
            let id = value.X
            transact (fun () -> hoverId.Value <- id)
            if id >= 0 then
                let n = value.Y |> uint32 |> Normal32.decode
                let depth = MemoryMarshal.Cast<int, float32>(System.Span<int> [|value.Z|]).[0]
                    
                match scopes.TryGetValue id with
                | (true, scope) ->
                    Some (scope, float depth, n)
                | _ ->
                    None
            else
                None
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
                    | Surface.FShadeSimple e ->
                        let newSurface =
                            if Option.isSome (o.VertexAttributes.TryGetAttribute DefaultSemantic.Normals) then
                                Surface.FShadeSimple (FShade.Effect.compose [PickShader.vertexPickEffect; e; PickShader.pickEffect])
                            else
                                Log.warn "no normal"
                                Surface.FShadeSimple (FShade.Effect.compose [e; PickShader.pickEffectNoNormal])
                                    
                        let r = RenderObject.Clone o
                        r.Uniforms <- UniformProvider.union o.Uniforms (UniformProvider.ofList ["PickId", AVal.constant pickId :> IAdaptiveValue])
                        r.Surface <- newSurface
                        r :> IRenderObject, true
                    | _ ->
                        o :> IRenderObject, true
              
                | :? MultiRenderObject as o ->
                    let res = o.Children |> List.map (wrapObject t)
                    if res |> List.forall snd then
                        let n = MultiRenderObject(List.map fst res)
                        n :> IRenderObject, true
                    else
                        o :> IRenderObject, false
                | o ->
                    o, false
            else
                o, false
            

        let objs = 
            render
            |> ASet.map (fun o ->
                match RenderObject.traversalStates.TryGetValue o with
                | (true, t) ->
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
                    //for f in o.PickLevelFramebuffers do runtime.DeleteFramebuffer f
                    for t in o.Textures do runtime.DeleteTexture t
                | None ->
                    ()
                    
                let semantics =
                    let res = newSignature.ColorAttachments |> Map.toList |> List.map (fun (slot, a) -> a.Name, a.Format) |> Map.ofList
                    match newSignature.DepthStencilAttachment with
                    | Some ds -> Map.add DefaultSemantic.DepthStencil ds res
                    | None -> res
                    
                let textures =
                    semantics |> Map.map (fun _ a ->
                        if newSignature.LayerCount > 1 then runtime.CreateTexture2DArray(size, a, 1, newSignature.Samples, newSignature.LayerCount)
                        else runtime.CreateTexture2D(size, a, 1, newSignature.Samples)    
                    )
                let outputs =
                    textures |> Map.map (fun _ t -> t.[TextureAspect.Color, 0, *] :> IFramebufferOutput)
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
                        PickTexture                 = textures.[pickBuffer]
                        PickTextureResolved         = pickResolvedTex
                        Textures                    = pickResolvedTex :: (textures |> Map.toList |> List.map snd)
                        //RenderOutline               = sTex
                        //CreateSelection             = rSel
                        //DisposeSelection            = dSel
                    }
                
                
                //attachments <- textures |> Map.toList |> List.map snd
                fbos <- Some result
                result
                
                
                
          
        let clear = runtime.CompileClear (newSignature, clear { colors (Map.ofList [DefaultSemantic.Colors, ClearColor.op_Implicit C4f.Black; pickBuffer, ClearColor.op_Implicit V4i.NNNN]); depth 1.0; stencil 0})
        
        let realTask =
            AVal.custom (fun t ->
                let s = fboSize.GetValue t

                let rt = RenderToken.Empty
                let outputInfo = getFramebuffers s
                trigger SceneHandlerEvent.PreRender

                clear.Run(t, rt, outputInfo.PickableFramebuffer)
                renderPickable.Run(t, rt, outputInfo.PickableFramebuffer)
                renderNonPickable.Run(t, rt, outputInfo.NonPickableFramebuffer)
                
                let pickBuffer = outputInfo.PickTexture
                if pickBuffer.Samples > 1 then runtime.ResolveMultisamples(pickBuffer.[TextureAspect.Color, 0, *], outputInfo.PickTextureResolved, ImageTrafo.Identity)
                else runtime.Copy(pickBuffer.[TextureAspect.Color, 0, *], outputInfo.PickTextureResolved.[TextureAspect.Color, 0, *])
                
                pickTexture <- Some (outputInfo.PickTextureResolved, outputInfo.PickFramebufferResolved)
                viewportSize <- outputInfo.PickTextureResolved.Size.XY
                    
                
                trigger SceneHandlerEvent.PostRender
                
            )
        
        let task = 
            RenderTask.custom (fun (t, _rt, o) ->
                let outputInfo = getFramebuffers o.framebuffer.Size
                if o.framebuffer.Size <> fboSize.Value then
                    transact (fun () -> fboSize.Value <- o.framebuffer.Size)
                    trigger (SceneHandlerEvent.Resize o.framebuffer.Size)
                
                realTask.GetValue t
                runtime.BlitFramebuffer(outputInfo.NonPickableFramebuffer, o.framebuffer)
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
                for t in o.Textures do runtime.DeleteTexture t
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

    member x.Runtime = runtime
    member x.FramebufferSignature = signature

    member x.Cursor = cursor

    member x.RenderTask = renderTask
  

    member x.Read(pixel : V2i, pointerId : int) =

        let capturedScope =
            match capturedScopes.TryGetValue pointerId with
            | (true, s) -> Some s
            | _ -> None

        let result = 
            if pixel.AllGreaterOrEqual 0 && pixel.AllSmaller viewportSize then
                lastMousePosition <- Some pixel
                let pixelResult = read pixel

                let s =
                    match pickTexture with
                    | Some(t,_) -> t.Size.XY
                    | None -> V2i.II

                let v = AVal.force view
                let p = AVal.force proj
                let vp = v * p
                let ndc = V2d(2.0 * float pixel.X / float s.X - 1.0, 1.0 - 2.0 * float pixel.Y / float s.Y)
                let ray =
                    let p0 = vp.Backward.TransformPosProj(V3d(ndc, -1.0))
                    let d = vp.Backward.TransformPosProj(V3d(ndc, 0.0)) - p0 |> Vec.normalize
                    Ray3d(p0, d)
                
            
                
                let mutable t = System.Double.PositiveInfinity
                let mutable best = None
            
                // limit t if pixel-result found
                match pixelResult with
                | Some (_, depth, _) ->
                    let world = vp.Backward.TransformPosProj(V3d(ndc, depth))
                    t <- Vec.dot ray.Direction (world - ray.Origin)
                | None ->
                    ()
            
                let tryIntersect (state : TraversalState) (i : IIntersectable, trafo : Trafo3d) =
                    let mutable r = 0.0
                    let mutable n = V3d.Zero
                    if i.Intersects(ray.Transformed(trafo.Backward), 0.0, t, &r, &n) then
                        Some (r, (state, Vec.normalize (trafo.Backward.TransposedTransformDir n)))
                    else
                        None
            
                match AVal.force(bvh).GetClosestHit(ray, 0.0, t, tryIntersect) with
                | None ->
                    pixelResult
                | Some (bestT, (best, bestNormal)) ->
                    let bestDepth = vp.Forward.TransformPosProj(ray.GetPointOnRay bestT).Z
                    match pixelResult with
                    | Some (scope, depth, viewNormal) ->
                        if depth < bestDepth then Some (scope, depth, viewNormal)
                        else Some (best, bestDepth, Vec.normalize (v.Backward.TransposedTransformDir bestNormal))
                    | None ->
                        Some(best, bestDepth, Vec.normalize (v.Backward.TransposedTransformDir bestNormal))
            else
            
                None

        let capturedResult = 
            match capturedScope with
            | None -> 
                match result with
                | Some (state, depth, normal) ->
                    Some (state, state :> obj, depth, normal)
                | None ->
                    None
            | Some c ->
                match result with
                | Some (target, depth, normal) -> Some (c, target :> obj, depth, normal)
                | None -> Some (c, null, 1.0, V3d.Zero)

        lastMouseInfo <- Some (pixel, capturedResult)
        lastRealMouseInfo <- Some (pixel, result)

        capturedResult

    member x.HandlePointerEvent(kind : SceneEventKind, pixel : V2i, ctrl : bool, shift : bool, alt : bool, meta : bool, pointerId : int, ?scrollDelta : V2d, ?button : int) : bool =
        let s = viewportSize
        if s.AllGreater 0 then
            let view = AVal.force view 
            let proj = AVal.force proj
            let scrollDelta = defaultArg scrollDelta V2d.Zero

            match x.Read(pixel, pointerId) with
            | Some (best, target, depth, viewNormal) ->
                let model = TraversalState.modelTrafo best
                let button = defaultArg button -1
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, depth, viewNormal)
                let evt = ScenePointerEvent(x, best, target, kind, loc, ctrl, shift, alt, meta, scrollDelta, pointerId, button)

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
                    let button = defaultArg button -1
                    let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d pixel, s, 1.0, V3d.Zero)
                    let evt = ScenePointerEvent(x, lastFocus.Value.Value, null, kind, loc, ctrl, shift, alt, meta, scrollDelta, pointerId, button)
                    TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
                | _ -> ()
                    

                if Option.isSome lastOver.Value then
                    let button = defaultArg button -1
                    let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d pixel, s, 1.0, V3d.Zero)
                    let evt = ScenePointerEvent(x, lastOver.Value.Value, null, kind, loc, ctrl, shift, alt, meta, scrollDelta, pointerId, button)
                    TraversalState.handleMove lastOver evt None

                true
        else
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
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, viewportSize, 1.0, V3d.Zero)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, viewportSize, 1.0, V3d.Zero)

            let target =
                match newTarget with
                | Some s -> s :> obj
                | None -> null

            let evt = ScenePointerEvent(x, target, target, SceneEventKind.FocusLeave, evtLocation, false, false, false, false, V2d.Zero, -1, -1)
            TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt newTarget
                
    member x.HandleKeyEvent(kind : SceneEventKind, ctrl : bool, shift : bool, alt : bool, meta : bool, key : Keys, text : string, isRepeat : bool) : bool =
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
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, s, 1.0, V3d.Zero)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, s, 1.0, V3d.Zero)
                
            let evt = SceneKeyboardEvent(x, best, best, kind, evtLocation, ctrl, shift, alt, meta, key, text, isRepeat)
            TraversalState.handleEvent true evt best

        | None ->
            true

    interface IEventHandler with
        member x.SetFocus(dst : option<obj>) =
            match dst with
            | Some (:? TraversalState as s) -> x.SetFocus(Some s)
            | _ -> x.SetFocus None
            
        member x.HandlePointerEvent(kind : SceneEventKind, pixel : V2i, ctrl : bool, shift : bool, alt : bool, meta : bool, scrollDelta : V2d, pointerId : int, button : int) =
            x.HandlePointerEvent(kind, pixel, ctrl, shift, alt, meta, pointerId, scrollDelta, button)
              
        member x.HandleKeyEvent(kind : SceneEventKind, ctrl : bool, shift : bool, alt : bool, meta : bool, key : Keys, text : string, isRepeat : bool) =
            x.HandleKeyEvent(kind, ctrl, shift, alt, meta, key, text, isRepeat)
   
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
            | Some (px, scope) ->
                let view = AVal.force view 
                let proj = AVal.force proj
                let loc, target = 
                    match scope with
                    | Some (scope, depth, viewNormal) -> SceneEventLocation(TraversalState.modelTrafo scope, view, proj, V2d px, viewportSize, depth, viewNormal), Some scope
                    | None -> SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d px, viewportSize, 1.0, V3d.Zero), None

                let eventTarget =
                    match target with
                    | Some t -> t :> obj
                    | None -> null

                let evt = ScenePointerEvent(x, eventTarget, eventTarget, SceneEventKind.PointerMove, loc, false, false, false, false, V2d.Zero, pointerId, -1)
                TraversalState.handleMove lastOver evt target
                match target with
                | Some t -> TraversalState.handleEvent true evt t |> ignore
                | None -> ()
            | None ->
                ()

            
        member x.Cursor = x.Cursor