namespace Aardvark.Dom.Utilities


open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Rendering.Text
open FSharp.Data.Adaptive
open Aardvark.Dom
open Aardvark.FontProvider
open Microsoft.FSharp.Core.CompilerServices

[<RequireQualifiedAccess>]
type GizmoAnchor =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    
[<RequireQualifiedAccess>]
type GizmoAxis =
    | XAxis
    | YAxis
    | ZAxis
    
[<RequireQualifiedAccess>]
type GizmoConfig =
    {
        Anchor : aval<GizmoAnchor>
        Offset : aval<V2d>
        Click : option<GizmoAxis -> bool -> unit>
        XColor : aval<C4b>
        YColor : aval<C4b>
        ZColor : aval<C4b>
        XHoverColor : aval<C4b>
        YHoverColor : aval<C4b>
        ZHoverColor : aval<C4b>
        TextColor : aval<C4b>
        TextHoverColor : aval<C4b>
        Size : aval<V2i>
    }
    
    static member Default =
        
        let hexColor (v : uint32) =
            C4b(byte (v >>> 16), byte (v >>> 8), byte v)
        {
            GizmoConfig.Size = AVal.constant (V2i(196, 196))
            GizmoConfig.Anchor = AVal.constant GizmoAnchor.TopRight
            GizmoConfig.Offset = AVal.constant V2d.Zero
            GizmoConfig.Click = None
            GizmoConfig.XColor = AVal.constant (hexColor 0xE06055u)
            GizmoConfig.YColor = AVal.constant (hexColor 0x6FB352u)
            GizmoConfig.ZColor = AVal.constant (hexColor 0x5B8DBEu)
            GizmoConfig.XHoverColor = AVal.constant (hexColor 0xE87A70u)
            GizmoConfig.YHoverColor = AVal.constant (hexColor 0x85C96Eu)
            GizmoConfig.ZHoverColor = AVal.constant (hexColor 0x7BA8D4u)
            GizmoConfig.TextColor = AVal.constant C4b.White
            GizmoConfig.TextHoverColor = AVal.constant C4b.White
        }
    static member TopRight = { GizmoConfig.Default with Anchor = AVal.constant GizmoAnchor.TopRight }
    static member TopLeft = { GizmoConfig.Default with Anchor = AVal.constant GizmoAnchor.TopLeft }
    static member BottomRight = { GizmoConfig.Default with Anchor = AVal.constant GizmoAnchor.BottomRight }
    static member BottomLeft = { GizmoConfig.Default with Anchor = AVal.constant GizmoAnchor.BottomLeft }
    

module Gizmo =
    type Font = GoogleFontProvider< "Roboto Mono" , Weight = 700 > 
    
    module private Shader =
        open FShade
        
        let sam =
            sampler2d {
                texture uniform?DiffuseColorTexture
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
                filter Filter.MinMagMipLinear
            }
        
        let sample (v : Effects.Vertex) =
            fragment {
                let s = 1.0 / V2d sam.Size
                let mutable sum = V4d.Zero
                for dx in -1 .. 1 do
                    for dy in -1 .. 1 do
                        let c = sam.SampleLevel(v.tc + V2d(float dx, float dy) * s, 0.0)
                        sum <- sum + c
                if sum.W < 0.01 then discard()
                return sum / sum.W
            }
            
        let light (v : Effects.Vertex) =
            fragment {
                let n = Vec.normalize ((uniform.ViewTrafo * V4d(v.n, 0.0)).XYZ)
                let diffuse = abs n.Z
                let l = 0.4  + 0.6 * diffuse
                return V4d(v.c.XYZ * l, v.c.W)
            }

    let render (config : GizmoConfig) (viewportSize : aval<V2i>) =
        Sg.Delay (fun state ->
            let m = TraversalState.modelTrafo state
            let v = state.View
            let p = state.Proj
            
            let vp = (v,p) ||> AVal.map2 (*)
            
            let view =
                vp |> AVal.map (fun vp ->
                    let c = vp.Backward.TransformPosProj V3d.OON
                    let forward = vp.Backward.TransformPosProj V3d.OOI - c |> Vec.normalize
                    let up = vp.Backward.TransformPosProj V3d.OPN - c |> Vec.normalize
                    
                    CameraView.lookAt (-forward * 2.0) V3d.Zero up
                    |> CameraView.viewTrafo
                )
                
            let proj =
                config.Size |> AVal.map (fun s ->
                    Frustum.ortho (Box3d(V3d(-2.0, -2.0, 0.0), V3d(2.0, 2.0, 10.0)))
                    |> Frustum.projTrafo
                    //Frustum.perspective 90.0 0.1 10.0 (float s.X / float s.Y)
                )
                
            let hover = cval None
            //     
            // let xColor = hover |> AVal.bind (function true -> config.XHoverColor | false -> config.XColor)
            // let yColor = config.YColor
            // let zColor = config.ZColor
                
            let xp = hover |> AVal.bind (function Some v when v = V3i.IOO -> config.XHoverColor | _ -> config.XColor)
            let yp = hover |> AVal.bind (function Some v when v = V3i.OIO -> config.YHoverColor | _ -> config.YColor)
            let zp = hover |> AVal.bind (function Some v when v = V3i.OOI -> config.ZHoverColor | _ -> config.ZColor)
            
            let spheres =
                [
                    V3i.IOO, "+X", xp
                    V3i.NOO, "-X", (hover |> AVal.bind (function Some v when v = V3i.NOO -> config.XHoverColor | _ -> config.XColor))
                    V3i.OIO, "+Y", yp
                    V3i.ONO, "-Y", (hover |> AVal.bind (function Some v when v = V3i.ONO -> config.YHoverColor | _ -> config.YColor))
                    V3i.OOI, "+Z", zp
                    V3i.OON, "-Z", (hover |> AVal.bind (function Some v when v = V3i.OON -> config.ZHoverColor | _ -> config.ZColor))
                ]
            let sphereRadius = 0.35
            
                
            let scene = 
                sg {
                    Sg.View view 
                    Sg.Proj proj 
                    Sg.Shader {
                        DefaultSurfaces.trafo
                        Shader.light
                    }
                    
                    
                    
                    
                    Primitives.Cylinder(V3d.Zero, V3d.IOO, 0.1, xp)
                    Primitives.Cylinder(V3d.Zero, V3d.OIO, 0.1, yp)
                    Primitives.Cylinder(V3d.Zero, V3d.OOI, 0.1, zp)
                    
                    
                    
                    let text (color : aval<C4b>) (content : string) (sphereCenter : V3d) (offset : float) =
                        Sg.Delay (fun state ->
                            let textConfig =
                                {
                                    font = Font.Font
                                    color = C4b.White
                                    align = TextAlignment.Left
                                    flipViewDependent = false
                                    renderStyle = RenderStyle.NoBoundary
                                }
                            let shape = textConfig.Layout content
                                
                            let center =
                                shape.bounds.Center //+ shape.renderTrafo.Forward.C3.XY
                            
                            let scale = 1.3 * sphereRadius / shape.bounds.Size.NormMax
                            
                            let billboard =
                                state.View |> AVal.map (fun v ->
                                    Trafo3d.FromBasis(v.Backward.C0.XYZ, v.Backward.C1.XYZ, v.Backward.C2.XYZ, v.Backward.C2.XYZ * offset)    
                                )
                            
                            let shape =
                                color |> AVal.map (fun c ->
                                    { shape with
                                        concreteShapes =
                                            shape.concreteShapes |> List.map (fun cs ->
                                                { cs with color = c }
                                            )
                                    }
                                )
                            
                            sg {
                                Sg.Translate(-center.X, -center.Y, 0.0)
                                Sg.Scale(scale)
                                Sg.Trafo billboard
                                Sg.Translate(sphereCenter)
                                
                                Aardvark.Dom.Sg.Shape shape
                            }
                        )
                    
                    let cc = config.TextColor
                    let hoverColor = config.TextHoverColor
                    
                    spheres |> ASet.ofList |> ASet.map (fun (off, name, color) ->
                        sg {
                            Primitives.Sphere(V3d off, sphereRadius, color)
                            let color = hover  |> AVal.bind (function Some h when h = off -> hoverColor | _ -> cc)
                            text color name (V3d off) (sphereRadius + 0.1)
                        }
                    )
                    
                }
                
            let signature =
                state.Runtime.CreateFramebufferSignature [
                    DefaultSemantic.Colors, TextureFormat.Rgba8
                    DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
                ]
            let task =
                state.Runtime.CompileRender(signature, scene.GetRenderObjects (TraversalState.empty state.Runtime))
                
            let size = config.Size
                
            let renderSize = size |> AVal.map (fun s -> s * 3)
                
            let color =
                task
                |> RenderTask.renderToColorWithClear renderSize (clear { color (C4f(0.0, 0.0, 0.0, 0.0)); depth 1.0; stencil 0 })
                
            // color.Acquire()
            // AVal.force color |> ignore
            //
            
           
                
            
            
            
            let newTrafo =
                AVal.custom (fun token ->
                    let size = size.GetValue token
                    let vps = viewportSize.GetValue token
                    let anchor = config.Anchor.GetValue token
                    let offset = config.Offset.GetValue token
                    
                    let r = V2d size / V2d vps
                    
                    let off =
                        let rp = 2.0 * offset / V2d vps
                        match anchor with
                        | GizmoAnchor.TopLeft ->
                            V2d(-1.0 + r.X + rp.X, 1.0 - r.Y - rp.Y)
                        | GizmoAnchor.TopRight ->
                            V2d(1.0 - r.X - rp.X, 1.0 - r.Y - rp.Y)
                        | GizmoAnchor.BottomLeft ->
                            V2d(-1.0 + r.X + rp.X, -1.0 + r.Y + rp.Y)
                        | GizmoAnchor.BottomRight ->
                            V2d(1.0 - r.X - rp.X, -1.0 + r.Y + rp.Y)
                    
                    
                    Trafo3d.Scale(r.X, r.Y, 1.0) *
                    Trafo3d.Translation(off.X, off.Y, 0.0)
                    
                )
                
            let tryGetSphere (e : ScenePointerEvent) =
                let ndc = 
                    let trafo = AVal.force newTrafo
                    let tc = V2d e.Original.ClientPosition / e.Original.ClientRect.Size
                    let ndc = V2d(2.0 * tc.X - 1.0, 1.0 - 2.0 * tc.Y)
                    trafo.Backward.TransformPos(V3d(ndc, -1.0)).XY
                    
                let view = AVal.force view 
                let proj = AVal.force proj
                let viewProj = view * proj
                
                let o = viewProj.Backward.TransformPosProj(V3d(ndc, -1.0))
                let dir = viewProj.Backward.TransformPosProj(V3d(ndc, 1.0)) - o |> Vec.normalize
                
                let mutable bestT = System.Double.PositiveInfinity
                let mutable bestSphere = V3i.Zero
                for (off, _, _) in spheres do
                    let s = Sphere3d(V3d off, sphereRadius)
                    // |o-c + t*d|^2 = r^2
                    // a:= o-c
                    // <a + t * d|a+ t * d> = r^2
                    
                    //<a|a> + t*2*<a|d> + t^2*<d|d>
                    let a = o - s.Center
                    let struct (t0, t1) = Polynomial.RealRootsOf(dir.LengthSquared, 2.0 * Vec.dot a dir, a.LengthSquared - sqr sphereRadius)
                    let t = 
                        if t0 >= 0.0 then t0
                        elif t1 >= 0.0 then t1
                        else System.Double.NaN
                     
                    if t < bestT then
                        bestT <- t
                        bestSphere <- off
                   
                if bestSphere <> V3i.Zero then Some bestSphere
                else None
            
            sg {
                Sg.View Trafo3d.Identity
                Sg.Proj Trafo3d.Identity
                
                
                Sg.OnPointerLeave(fun e ->
                    transact (fun () -> hover.Value <- None)
                )
                Sg.OnPointerMove(fun e ->
                    let h =  tryGetSphere e
                    transact (fun () -> hover.Value <- h)
                )
                
                Sg.OnTap (fun e ->
                    match tryGetSphere e with
                    | Some s ->
                        //let size = AVal.force sceneSize
                        
                        
                        let axis, positive =
                            if s = V3i.IOO then GizmoAxis.XAxis, true
                            elif s = V3i.NOO then GizmoAxis.XAxis, false
                            elif s = V3i.OIO then GizmoAxis.YAxis, true
                            elif s = V3i.ONO then GizmoAxis.YAxis, false
                            elif s = V3i.OOI then GizmoAxis.ZAxis, true
                            else GizmoAxis.ZAxis, false
                        
                        match config.Click with
                        | Some c -> c axis positive
                        | None -> ()
                        // env.Emit [ CameraMessage (OrbitMessage.SetTargetPhi(false, phi)) ]
                        // env.Emit [ CameraMessage (OrbitMessage.SetTargetTheta(false,theta)) ]
                        //
                        // match AVal.force cameraMode with
                        // | CameraMode.Perspective ->
                        //     env.Emit [ SetAutoOrtho true ]
                        //     env.Emit [ SetCameraMode CameraMode.Ortho ]
                        // | _ -> 
                        //     ()
                    | None ->
                        ()
                )
                
                
                //let pass = RenderPass.after "asfasd" RenderPassOrder.Arbitrary RenderPass.main
                // Sg.BlendMode BlendMode.Blend
                // SceneAttribute.Pass pass
                // Sg.DepthTest DepthTest.Always
                
                Sg.Trafo newTrafo
                Sg.Uniform("DiffuseColorTexture", color)
                Sg.Shader {
                    DefaultSurfaces.trafo
                    Shader.sample
                }
                Primitives.ScreenQuad(-1.0)
            }
                
                
        )
        

[<AutoOpen>]
module GizmoExtensions =
    
    [<RequireQualifiedAccess>]
    type GizmoAttribute =
        internal
        | Top of aval<float>
        | Right of aval<float>
        | Bottom of aval<float>
        | Left of aval<float>
        | Click of (GizmoAxis -> bool -> unit)
        | XColor of aval<C4b>
        | YColor of aval<C4b>
        | ZColor of aval<C4b>
        | XHoverColor of aval<C4b>
        | YHoverColor of aval<C4b>
        | ZHoverColor of aval<C4b>
        | TextColor of aval<C4b>
        | TextHoverColor of aval<C4b>
        | Size of aval<V2i>
    
    type private Acc = ListCollector<GizmoAttribute> -> ListCollector<GizmoAttribute>
    type GizmoBuilder() =
        
        member x.Zero() : Acc =
            id
        
        member x.Yield(att : GizmoAttribute) : Acc =
            fun l ->
                let mutable l = l
                l.Add att
                l
                
        member x.Delay(a : unit -> Acc) =
            a ()
                
        member x.Combine(l : Acc, r : Acc) =
            fun b ->
                let b = l b
                r b
                
        member x.Run(a : Acc) =
            let l = ListCollector()
            let l = a l
            let atts = l.Close()
            
            let mutable state = GizmoConfig.Default
            
            let mutable y = 1
            let mutable x = 1
            
            let mutable mx = AVal.constant 0.0
            let mutable my = AVal.constant 0.0
            
            for a in atts do
                match a with
                | GizmoAttribute.Size s ->
                    state <- { state with Size = s }
                | GizmoAttribute.Click c ->
                    let callback = 
                        match state.Click with
                        | Some o -> fun a b -> o a b; c a b
                        | None -> c
                    state <- { state with Click = Some callback }
                | GizmoAttribute.Bottom off ->
                    y <- -1
                    my <- off
                | GizmoAttribute.Top off ->
                    y <- 1
                    my <- off
                | GizmoAttribute.Left off ->
                    x <- -1
                    mx <- off
                | GizmoAttribute.Right off ->
                    x <- 1
                    mx <- off
                | GizmoAttribute.XColor c ->
                    state <- { state with XColor = c }
                | GizmoAttribute.YColor c ->
                    state <- { state with YColor = c }
                | GizmoAttribute.ZColor c ->
                    state <- { state with ZColor = c }
                | GizmoAttribute.XHoverColor c ->
                    state <- { state with XHoverColor = c }
                | GizmoAttribute.YHoverColor c ->
                    state <- { state with YHoverColor = c }
                | GizmoAttribute.ZHoverColor c ->
                    state <- { state with ZHoverColor = c }
                | GizmoAttribute.TextColor c ->
                    state <- { state with TextColor = c }
                | GizmoAttribute.TextHoverColor c ->
                    state <- { state with TextHoverColor = c }
            
            
            
            let anchor =
                match x, y with
                | -1, 1 -> GizmoAnchor.TopLeft
                | -1, -1 -> GizmoAnchor.BottomLeft
                | 1, -1 -> GizmoAnchor.BottomRight
                | _ -> GizmoAnchor.TopRight
                
            let margin = (mx, my) ||> AVal.map2 (fun x y -> V2d(x,y))
                
            state <- { state with Anchor = AVal.constant anchor; Offset = margin }
                
                
            Sg.Delay (fun ts ->
                let vs = ts.Uniforms.["ViewportSize"] :?> aval<V2i>
                Gizmo.render state vs
            )
            
    
        
    let gizmo = GizmoBuilder()
    
    
        // | Size of aval<V2i>
    
    [<AbstractClass; Sealed>]
    type Gizmo private() =
        static member Top() = GizmoAttribute.Top (AVal.constant 0.0)
        static member Top(margin : float) = GizmoAttribute.Top (AVal.constant margin)
        static member Top(margin : aval<float>) = GizmoAttribute.Top margin
        
        static member Bottom() = GizmoAttribute.Bottom (AVal.constant 0.0)
        static member Bottom(margin : float) = GizmoAttribute.Bottom (AVal.constant margin)
        static member Bottom(margin : aval<float>) = GizmoAttribute.Bottom margin
        
        static member Left() = GizmoAttribute.Left (AVal.constant 0.0)
        static member Left(margin : float) = GizmoAttribute.Left (AVal.constant margin)
        static member Left(margin : aval<float>) = GizmoAttribute.Left margin
        
        static member Right() = GizmoAttribute.Right (AVal.constant 0.0)
        static member Right(margin : float) = GizmoAttribute.Right (AVal.constant margin)
        static member Right(margin : aval<float>) = GizmoAttribute.Right margin
        
        static member Click(action : GizmoAxis -> bool -> unit) = GizmoAttribute.Click action
        
        static member XColor(color : C4b) = GizmoAttribute.XColor(AVal.constant color)
        static member YColor(color : C4b) = GizmoAttribute.YColor(AVal.constant color)
        static member ZColor(color : C4b) = GizmoAttribute.ZColor(AVal.constant color)
        static member XColor(color : aval<C4b>) = GizmoAttribute.XColor color
        static member YColor(color : aval<C4b>) = GizmoAttribute.YColor color
        static member ZColor(color : aval<C4b>) = GizmoAttribute.ZColor color
        
        static member XHoverColor(color : C4b) = GizmoAttribute.XHoverColor(AVal.constant color)
        static member YHoverColor(color : C4b) = GizmoAttribute.YHoverColor(AVal.constant color)
        static member ZHoverColor(color : C4b) = GizmoAttribute.ZHoverColor(AVal.constant color)
        static member XHoverColor(color : aval<C4b>) = GizmoAttribute.XHoverColor color
        static member YHoverColor(color : aval<C4b>) = GizmoAttribute.YHoverColor color
        static member ZHoverColor(color : aval<C4b>) = GizmoAttribute.ZHoverColor color
        
    
        static member TextColor(color : C4b) = GizmoAttribute.TextColor(AVal.constant color)
        static member TextHoverColor(color : C4b) = GizmoAttribute.TextHoverColor(AVal.constant color)
        static member TextColor(color : aval<C4b>) = GizmoAttribute.TextColor color
        static member TextHoverColor(color : aval<C4b>) = GizmoAttribute.TextHoverColor color
    
        static member Size(size : V2i) = GizmoAttribute.Size (AVal.constant size)
        static member Size(size : aval<V2i>) = GizmoAttribute.Size size
        
        
        static member Size(size : int) = GizmoAttribute.Size (AVal.constant (V2i(size, size)))
        static member Size(size : aval<int>) = GizmoAttribute.Size (size |> AVal.map (fun s -> V2i(s,s)))
    
    
    let private test() =
        gizmo {
            Gizmo.Top 50
            Gizmo.Right()
            
            Gizmo.Size 256
            
        }



