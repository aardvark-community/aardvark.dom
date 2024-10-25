namespace Aardvark.Dom.Utilities.OrbitController

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.Application
open Aardvark.Dom.Utilities.OrbitController
open Aardvark.Dom

[<AutoOpen>]
module TimeUtilities =

    let private anytime = System.DateTime.Now
    let private sw = System.Diagnostics.Stopwatch.StartNew()

    let now() = sw.MicroTime

type MouseEvent =
    {
        pixel       : V2d
        viewport    : V2d
        button      : MouseButtons
        pointerId   : int
        alt         : bool
        ctrl        : bool
        shift       : bool
    }
  
type KeyEvent =
    {
        key         : Keys
        repeat      : bool
        alt         : bool
        ctrl        : bool
        shift       : bool
    }

module Integrator = 


    let inline private dbl one = one + one

    let inline rungeKutta (f : ^t -> ^a -> ^da) (y0 : ^a) (h : ^t) : ^a =
        let twa : ^t = dbl LanguagePrimitives.GenericOne
        let half : ^t = LanguagePrimitives.GenericOne / twa
        let hHalf = h * half

        let k1 = h * f LanguagePrimitives.GenericZero y0
        let k2 = h * f hHalf (y0 + k1 * half)
        let k3 = h * f hHalf (y0 + k2 * half)
        let k4 = h * f h (y0 + k3)
        let sixth = LanguagePrimitives.GenericOne / (dbl twa + twa)
        y0 + (k1 + twa*k2 + twa*k3 + k4) * sixth

    let inline euler (f : ^t -> ^a -> ^da) (y0 : ^a) (h : ^t) : ^a=
        y0 + h * f LanguagePrimitives.GenericZero y0

    let rec integrate (maxDt : float) (f : 'm -> float -> 'm) (m0 : 'm) (dt : float) =
        if dt <= maxDt then
            f m0 dt
        else
            integrate maxDt f (f m0 maxDt) (dt - maxDt) 



type OrbitMessage =
    | PointerDown of id : int * button : Button * isTouch : bool * pos : V2i
    | PointerUp of  id : int * isTouch : bool * V2i
    | PointerMove of id : int * button : Button * isTouch : bool * V2i
    | Wheel of shift : bool * delta : V2d


    | Rendered
    | SetTargetCenter of user : bool * AnimationKind * V3d
    | SetTargetPhi of user : bool * float
    | SetTargetTheta of user : bool * float
    | SetTargetRadius of user : bool * float
    | SetTarget of user : bool * center : V3d * radius : float * phi : float * theta : float

    | SetPhi of float
    | SetTheta of float
    | SetRadius of float
    | SetCenter of V3d

    | Set of center : V3d * radius : float * phi : float * theta : float

    | UpdateCenter of V3d

    | Nothing 


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrbitState =
    let clamp (min : float) (max : float) (value : float) =
        if value > max then max
        elif value < min then min
        else value

    let withView (s : OrbitState) =
        let ct = cos s.theta
        let dir =
            V3d(cos s.phi * ct, sin s.phi * ct, sin s.theta)
        let l =
            if s.radius <= 1.02 * s.radiusRange.X then
                s.center
            else
                dir * s.radius + s.center
        //let l = V2d(s.phi, s.theta).CartesianFromSpherical() * s.radius + s.center
        
        
        
        
        let r = Vec.cross s.sky dir |> Vec.normalize
        let up = Vec.cross dir r |> Vec.normalize
        let view = CameraView(s.sky, l, -dir, up, r) //CameraView.lookAt l s.center s.sky 
        let winPan = view.WithLocation(view.Location + float s.shift.X * -0.001 * view.Right + float s.shift.Y  * 0.001 * view.Up)

        { s with view = winPan }

    let create (center : V3d) (phi : float) (theta : float) (r : float) (rotateButton : Button) (panButton : Button) =
        let thetaRange = V2d(-Constant.PiHalf + 0.0001, Constant.PiHalf - 0.0001)
        let radiusRange = V2d(0.1, 40000000.0)

        let r = clamp radiusRange.X radiusRange.Y r
        let theta = clamp thetaRange.X thetaRange.Y theta
        let phi = phi % Constant.PiTimesTwo
        withView {
            userModifiedAngles = false
            userModifiedRadius = false
            userModifiedCenter = false
            shift = V2d.Zero
            sky     = V3d.OOI
            center  = center
            phi     = phi
            theta   = theta
            radius  = r
            
            locationAnimation = None
            centerAnimation = None
            panAnimation = None
            targetPhi = phi
            targetTheta = theta
            targetRadius = r
            dragging = false

            dragStarts = MapExt.empty
            rotateButton = rotateButton
            panButton = panButton
            
            lastRender = None
            view = Unchecked.defaultof<_>

            radiusRange = radiusRange
            thetaRange = thetaRange
            moveSensitivity = 0.5
            zoomSensitivity = 1.0
            speed = 0.3
            lockedToScene = true
            isOrtho = false 
            pick = fun _ -> Log.warn "no pick installed"; None
        }

module OrbitController = 
    let private sw = System.Diagnostics.Stopwatch.StartNew()
    let time() = sw.MicroTime

    let private clamp (min : float) (max : float) (value : float) =
        if value > max then max
        elif value < min then min
        else value

    type WeakMapVal<'a, 'b when 'b : not struct>(mapping : 'a -> 'b, input : aval<'a>) =
        inherit AdaptiveObject()
        
        let mutable cache : WeakReference<'b> = null
        
        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                let inline recompute() =
                    let res = mapping (input.GetValue token)
                    cache <- new WeakReference<'b>(res)
                    res
                    
                if x.OutOfDate then
                    recompute()
                else
                    if isNull cache then
                        recompute()
                    else
                        match cache.TryGetTarget() with
                        | true, res -> res
                        | false, _ -> recompute()
            )
        interface IAdaptiveValue with
            member x.IsConstant = false
            member x.GetValueUntyped token = x.GetValue token :> obj
            member x.Accept v = v.Visit x
            member x.ContentType = typeof<'b>
        interface aval<'b> with
            member x.GetValue token = x.GetValue token

    module AVal =
        let mapWeak mapping input = WeakMapVal<'a, 'b>(mapping, input) :> aval<'b>
    
    let test() =
        let input = cval 'a'

        // some huge object depending on the input
        let hugeMemory = input |> AVal.mapWeak (fun c -> System.String(c, 1000000))

        // two small things depending on the huge object
        let a = hugeMemory |> AVal.map (fun str -> str.Length)
        let b = hugeMemory |> AVal.map (fun str -> hash str)

        // forces the string to exist but no-one will reference it => GC
        printfn "%A" (AVal.force a)

        // forces the string to exist again (likely already GCed)
        printfn "%A" (AVal.force b)
        ()
        
        
        
    
    let rec update (model : OrbitState) (msg : OrbitMessage) =
        match msg with

        | Set(center, r, phi, theta) ->
            let phi = phi % Constant.PiTimesTwo
            OrbitState.withView {
                model with
                    center = center
                    centerAnimation = None
                    panAnimation = None
                    shift = V2d.Zero
                    radius = r
                    targetRadius = r
                    phi = phi
                    targetPhi = phi
                    theta = theta
                    targetTheta = theta
                    lastRender = None
            }
            
        | SetTarget(user, center, r, phi, theta) ->
            let phi = phi % Constant.PiTimesTwo
            let now = time()
            
            let dstLocation =
                let ct = cos theta
                V3d(cos phi * ct, sin phi * ct, sin theta) * r + center
            
            
            let centerAnim =
                {
                    kind = AnimationKind.Tanh
                    startValue = model.center
                    stopValue = center
                    startTime = now
                    stopTime = now + MicroTime.Second
                }
                
            let locationAnim =
                { 
                    kind = AnimationKind.Tanh
                    startValue = model.view.Location
                    stopValue = dstLocation
                    startTime = now
                    stopTime = now + MicroTime.Second
                }
                
            OrbitState.withView {
                model with
                    userModifiedAngles = user
                    userModifiedCenter = user
                    userModifiedRadius = user
                    centerAnimation = Some centerAnim
                    locationAnimation = Some locationAnim
                    // targetRadius = r
                    // targetPhi = phi
                    // targetTheta = theta
                    lastRender = None
                    shift = V2d.Zero
            }

        | SetPhi phi -> 
            let phi = phi % Constant.PiTimesTwo
            OrbitState.withView  { model with phi = phi; targetPhi = phi }
        | SetTheta theta -> OrbitState.withView  { model with theta = theta; targetTheta = theta }
        | SetRadius r -> OrbitState.withView  { model with radius = r; targetRadius = r }
        | SetCenter c -> OrbitState.withView  { model with center = c; centerAnimation = None }

        | OrbitMessage.Nothing -> model

        | UpdateCenter pt ->
            let _fw = model.view.Forward
            let pos = model.view.Location

            let newCenter = pt
            let newFw = newCenter - pos
            let newR = Vec.length newFw
            let _newFw = newFw / newR

            OrbitState.withView { 
                model with 
                    centerAnimation = None
                    center = newCenter
                    targetRadius = newR
                    radius = newR 
                    shift = V2d.Zero
            } 
        
        | SetTargetPhi(user, tphi) ->
            let tphi = tphi % Constant.PiTimesTwo
            OrbitState.withView { model with userModifiedAngles = user; targetPhi = tphi; lastRender = None; locationAnimation = None }
        
        | SetTargetTheta(user, ttheta) -> 
            OrbitState.withView { model with userModifiedAngles = user; targetTheta = clamp model.thetaRange.X model.thetaRange.Y ttheta; lastRender = None; locationAnimation = None }
        
        | SetTargetRadius(user, tr) ->
            OrbitState.withView { model with userModifiedRadius = user; targetRadius = clamp model.radiusRange.X model.radiusRange.Y tr; lastRender = None; locationAnimation = None }

        | SetTargetCenter(user, kind, tc) ->
            let now = time()
            if model.shift.IsTiny then
                let _oldLocation = model.view.Location
                let newRadius, newCenter =
                    // match model.pick V2d.Half with
                    // | Some h ->
                    //     let dOld = model.radius
                    //     let dNew = Vec.distance oldLocation h
                    //     if dNew < dOld then
                    //         dNew, h
                    //     else
                    //         dOld, model.center
                    // | None -> 
                        model.radius, model.center
                   
                let anim =
                    { 
                        kind = kind
                        startValue = newCenter
                        stopValue = tc
                        startTime = now
                        stopTime = now + MicroTime.Second
                    }
                OrbitState.withView { 
                    model with
                        userModifiedCenter = user
                        center = newCenter
                        radius = newRadius
                        targetRadius = newRadius
                        centerAnimation = Some anim; 
                        lastRender = None; lockedToScene = true 
                 }
            else
                let anim =
                    { 
                        kind = kind
                        startValue = model.center
                        stopValue = tc
                        startTime = now + MicroTime.Second
                        stopTime = now + MicroTime.Second + MicroTime.Second 
                    }
                let panAnimation =
                    { 
                        kind = AnimationKind.Linear
                        startValue = model.shift
                        stopValue = V2d.Zero
                        startTime = now
                        stopTime = now + MicroTime.Second
                    }
                OrbitState.withView { 
                    model with 
                        centerAnimation = Some anim; 
                        panAnimation = Some panAnimation
                        lastRender = None; lockedToScene = true 
                 }
        

        | PointerDown(id, button, _isTouch, p) ->
            let s = MapExt.add id (p, button) model.dragStarts
            { model with 
                dragging = true
                dragStarts = s
                lastRender = None 
            }

        | PointerUp(id, _isTouch, _p) ->
            match MapExt.tryRemove id model.dragStarts with
            | Some ((_, button), s) ->
                
                let model =
                    { model with 
                        dragging = not (MapExt.isEmpty s)
                        dragStarts = s; 
                        lastRender = None 
                    }
                
                if button = model.panButton then
                    let oldLocation = model.view.Location
                    match model.pick V2d.Half with
                    | Some hit ->
                        
                        let forward = model.view.Forward
                        let newCenter =
                            let d = hit - oldLocation
                            let ds = Vec.dot d forward * forward
                            oldLocation + ds
                        
                        let r =
                            if model.isOrtho then model.radius
                            else Vec.distance newCenter oldLocation
                        OrbitState.withView
                            { model with
                                center = newCenter
                                radius = r
                                targetRadius = r
                                centerAnimation = None
                                locationAnimation = None
                                lockedToScene = true
                                
                            }
                    | None ->
                        { model with lockedToScene = false } 
                else
                    model
            | None ->
                model

        | Wheel(shift,delta) ->
            if shift || model.lockedToScene || model.isOrtho then
                OrbitState.withView { 
                    model with
                        userModifiedRadius = true
                        targetRadius = 
                            clamp 
                                model.radiusRange.X 
                                model.radiusRange.Y 
                                (model.targetRadius * 1.1 ** (delta.Y * model.zoomSensitivity))
                }
            else
                match model.centerAnimation with
                | Some a ->
                    let now = time()
                    let targetTime = a.stopTime + MicroTime.FromMilliseconds 120.0
                    let newCenter = 
                        let dir = model.view.Forward
                        a.stopValue + dir * -delta.Y * 0.5 * model.zoomSensitivity

                    let anim =
                        { 
                            kind = AnimationKind.Exp
                            startValue = Animation.interpolate now a
                            stopValue = newCenter
                            startTime = now
                            stopTime = targetTime
                        }
                    OrbitState.withView { model with userModifiedCenter = true; centerAnimation = Some anim; lastRender = None }
                | None ->
                    let now = time()
                    let targetTime = now + MicroTime.FromMilliseconds 120.0
                    let newCenter = 
                        let dir = model.view.Forward
                        model.center + dir * -delta.Y * 0.5 * model.zoomSensitivity

                    let anim =
                        { 
                            kind = AnimationKind.Exp
                            startValue = model.center
                            stopValue = newCenter
                            startTime = now
                            stopTime = targetTime
                        }
                    OrbitState.withView { model with userModifiedCenter = true; centerAnimation = Some anim; lastRender = None }
        

        | PointerMove(id, _button, isTouch, p) ->
            let down = model.dragStarts.Count
            let devicePixelRatio = 1.0
            match down with
            | 1 -> 
                match MapExt.tryFind id model.dragStarts with
                | Some(start,button) ->
                    let left = button = model.rotateButton
                    let middle = button = model.panButton
                    //let right = button = Button.Right


                    if isTouch || left then
                        let delta = p - start
                        let dphi = float delta.X * -0.01 * model.moveSensitivity * devicePixelRatio
                        let dtheta = float delta.Y * 0.01 * model.moveSensitivity * devicePixelRatio
            
                        if not (Fun.IsTiny dphi) || not (Fun.IsTiny dtheta) then
                            OrbitState.withView 
                                { model with
                                    dragStarts = MapExt.add id (p, button) model.dragStarts
                                    userModifiedAngles = true
                                    targetPhi = (model.targetPhi + dphi) % Constant.PiTimesTwo
                                    targetTheta = clamp model.thetaRange.X model.thetaRange.Y (model.targetTheta + dtheta)
                                }
                        else
                            model
                    elif middle then

                        let freeMovePan = true
                        if freeMovePan then
                            let delta = p - start
                            
                            let newRadius, newCenter, locked =
                                model.radius, model.center, false
                                
                            let newCenter = 
                                let r = max newRadius 0.3
                                newCenter + 
                                model.view.Right * float delta.X * -0.001 * r + 
                                model.view.Up * float delta.Y * 0.001 * r
                                
                            OrbitState.withView 
                                { model with
                                    userModifiedCenter = false
                                    dragStarts = MapExt.add id (p, button) model.dragStarts
                                    centerAnimation = None
                                    center = newCenter
                                    radius = newRadius
                                    targetRadius = if newRadius <> model.radius then newRadius else model.targetRadius
                                    lockedToScene = locked
                                }
                        else
                            let delta = p - start

                            OrbitState.withView 
                                { model with
                                    userModifiedCenter = true
                                    dragStarts = MapExt.add id (p, button) model.dragStarts
                                    panAnimation = None
                                    shift = (clamp 0.01 5.0 (model.radius / 2.0)) * V2d delta + model.shift
                                }

                    //         
                    // elif right  then
                    //     let _delta = p - start
                    //
                    //     let _now = time()
                    //     OrbitState.withView 
                    //         { model with
                    //             dragStarts = MapExt.add id (p, button) model.dragStarts
                    //             //centerAnimation = None
                    //             //lockedToScene = false
                    //             //shift = (clamp 0.01 5.0 (model.radius / 2.0)) * V2d delta + model.shift
                    //         }

                    else
                        model
                | None ->
                    model
            | 2  ->
                match MapExt.tryFind id model.dragStarts with
                | Some (op, button) ->
                    let np = p
                    let _otherId, (otherPos, _) = model.dragStarts |> MapExt.toSeq |> Seq.find (fun (k,_) -> k <> id)

                    let scale = Vec.length (np - otherPos) / Vec.length (op - otherPos)
                    
                    let r = clamp model.radiusRange.X model.radiusRange.Y (model.targetRadius / scale)
                    
                    let delta = 0.5 * V2d(np  - op)
                    let dphi = delta.X * -0.01 * model.moveSensitivity  * devicePixelRatio
                    let dtheta = delta.Y * 0.01 * model.moveSensitivity * devicePixelRatio
            
                    OrbitState.withView 
                        { model with
                            userModifiedAngles = true
                            userModifiedRadius = true
                            dragStarts = MapExt.add id (p, button) model.dragStarts
                            targetPhi = (model.targetPhi + dphi) % Constant.PiTimesTwo
                            targetTheta = clamp model.thetaRange.X model.thetaRange.Y (model.targetTheta + dtheta)
                            targetRadius = r
                        }

                | None ->
                    model
            | _ ->
                model
        | Rendered ->
            let dphi = 
                let a = (model.targetPhi - model.phi) % Constant.PiTimesTwo
                if a < -Constant.Pi then Constant.PiTimesTwo + a
                elif a > Constant.Pi then a - Constant.PiTimesTwo
                else a

            let dtheta = model.targetTheta - model.theta
            let dradius = model.targetRadius - model.radius

            let now = time()
            let dt =
                match model.lastRender with
                | Some last -> (now - last)
                | None -> MicroTime.Zero
            
            let delta = model.speed * dt.TotalSeconds / 0.05
            let part = if dt.TotalSeconds > 0.0 then clamp 0.0 1.0 delta else 0.0
            let model = { model with lastRender = Some now }

            let model = 
                if abs dphi > 0.0 then
                    if Fun.IsTiny(dphi, 1E-4) then
                        OrbitState.withView { model with phi = model.targetPhi }
                    else
                        OrbitState.withView { model with phi = (model.phi + part * dphi) % Constant.PiTimesTwo }
                else
                    model

            let model = 
                if abs dtheta > 0.0 then
                    if Fun.IsTiny(dtheta, 1E-4) then
                        OrbitState.withView { model with theta = model.targetTheta }
                    else
                        OrbitState.withView { model with theta  = model.theta + part * dtheta }
                else
                    model

            let model = 
                if abs dradius > 0.0 then
                    if Fun.IsTiny(dradius, 1E-4) then
                        OrbitState.withView { model with radius = model.targetRadius }
                    else
                        OrbitState.withView { model with radius  = model.radius + part * dradius }
                else
                    model

            let model = 
                match model.centerAnimation, model.panAnimation with // first always to pan animation
                | Some anim, None ->
                    match model.locationAnimation with
                    | Some locAnim ->
                        let dLoc = locAnim.stopValue - model.view.Location
                        let _dLocLen = Vec.length dLoc
                        let dCenter = anim.stopValue - model.center
                        let _dCenterLen = Vec.length dCenter
                        
                        let inline setLocation (location : V3d) (center : V3d) (model : OrbitState) =
                            let diff = location - center
                            let r = Vec.Length diff
                            let phi = atan2 diff.Y diff.X
                            let theta = asin (diff.Z / r)
                            OrbitState.withView {
                                model with
                                    center = center
                                    radius = r
                                    targetRadius = r
                                    phi = phi
                                    targetPhi = phi
                                    theta = theta
                                    targetTheta = theta
                            }
                        
                        match anim.kind with
                        | AnimationKind.Exp ->
                            if now >= anim.stopTime then //Fun.IsTiny(dCenterLen, 1E-2) && Fun.IsTiny(dLocLen, 1E-2) then
                                setLocation locAnim.stopValue anim.stopValue {
                                    model with
                                        centerAnimation = None
                                        locationAnimation = None
                                }
                            else
                                setLocation (model.view.Location + part * dLoc) (model.center + part * dCenter) model
                            
                        | _ ->
                            if now < anim.stopTime then
                                let pos = Animation.interpolate now anim
                                let loc = Animation.interpolate now locAnim
                                setLocation loc pos model
                            else
                                setLocation locAnim.stopValue anim.stopValue { model with centerAnimation = None; locationAnimation = None }
                    | None ->
                        let dcenter = anim.stopValue - model.center
                        let dCurrent = Vec.length dcenter
                        match anim.kind with
                        | AnimationKind.Exp ->
                            if Fun.IsTiny(dCurrent, 1E-4) then
                                OrbitState.withView { model with center = anim.stopValue; centerAnimation = None }
                            else
                                OrbitState.withView { model with center = model.center + part * dcenter }
                            
                        | _ ->
                            if dCurrent > 0.0 then
                                let pos = Animation.interpolate now anim
                                OrbitState.withView { model with center  = pos }
                            else
                                { model with centerAnimation = None }
                | Some _anim, Some _ -> 
                    Log.line "pan anim blocked"
                    // blocked, first pan to center
                    model
                | _ ->
                    model

            let model = 
                match model.panAnimation with
                | Some anim ->
                    let dcenter = anim.stopValue - model.shift
                    let dCurrent = Vec.length dcenter
                    match anim.kind with
                    | AnimationKind.Exp ->
                        if Fun.IsTiny(dCurrent, 1E-4) then
                            OrbitState.withView { model with shift = anim.stopValue; panAnimation = None }
                        else
                            OrbitState.withView { model with shift = model.shift + part * dcenter }
                        
                    | _ ->
                        if dCurrent > 0.0 then
                            let pos = Animation.interpolate now anim
                            OrbitState.withView { model with shift  = pos }
                        else
                            Log.line "finished pan animation"
                            { model with panAnimation = None }
                | None ->
                    model

            model


    let animationRunning (model : OrbitState) = model.animationRunning

    let getAttributes (env : Env<OrbitMessage>) =
        att {
            Dom.OnPointerDown ((fun e ->
                if e.PointerType = PointerType.Mouse then
                    env.Emit [PointerDown(e.PointerId, e.Button, false, e.OffsetPosition)]
            ), pointerCapture = true) 
            Dom.OnPointerUp ((fun e ->
                if e.PointerType = PointerType.Mouse then
                    env.Emit [PointerUp(e.PointerId, false, e.OffsetPosition)]
            ), pointerCapture = true)
            Dom.OnPointerMove(fun e ->
                if e.PointerType = PointerType.Mouse then
                    env.Emit [PointerMove(e.PointerId, e.Button, false, e.OffsetPosition)]
            )
            Dom.OnContextMenu(ignore, preventDefault = true)
            Dom.OnMouseWheel(fun e -> env.Emit [Wheel(false, V2d(e.DeltaX, e.DeltaY) / 120.0)])

            Dom.OnTouchStart((fun e ->
                e.ChangedTouches |> HashMap.toList |> List.map (fun (id, t) ->
                    PointerDown(id, Button.None, true, t.OffsetPosition)
                )
                |> env.Emit
            ), preventDefault = true)
            Dom.OnTouchMove((fun e ->
                e.ChangedTouches |> HashMap.toList |> List.map (fun (id, t) ->
                    PointerMove(id, Button.None, true, t.OffsetPosition)
                )
                |> env.Emit
            ), preventDefault = true)
            
            Dom.OnTouchCancel((fun e ->
                e.ChangedTouches |> HashMap.toList |> List.map (fun (id, t) ->
                    PointerUp(id, true, t.OffsetPosition)
                )
                |> env.Emit
            ), preventDefault = true)
            
            Dom.OnTouchEnd((fun e ->
                e.ChangedTouches |> HashMap.toList |> List.map (fun (id, t) ->
                    PointerUp(id, true, t.OffsetPosition)
                )
                |> env.Emit
            ), preventDefault = true)
            //RenderControl.OnRendered(fun e -> env.Emit [Rendered])
        }
       
