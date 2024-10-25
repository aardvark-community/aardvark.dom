namespace Aardvark.Dom.Utilities.OrbitController

open Aardvark.Base
open Adaptify
open Aardvark.Rendering
open Aardvark.Dom

[<RequireQualifiedAccess>]
type AnimationKind =
    | Linear
    | QuadInOut
    | Parametric
    | Cubic
    | Exp
    | Tanh
    | Spring of frequency : float
    | Pow of exp : float

type Animation<'a> =
    {
        kind        : AnimationKind
        startTime   : MicroTime
        stopTime    : MicroTime
        startValue  : 'a
        stopValue   : 'a
    }

module Animation =

    type TimesFloat private() =
        static member Lerp(a : float, b : float, t : float) = a + (b-a)*t
        static member Lerp(a : V2d, b : V2d, t : float) = a + (b-a)*t
        static member Lerp(a : V3d, b : V3d, t : float) = a + (b-a)*t
        static member Lerp(a : V4d, b : V4d, t : float) = a + (b-a)*t


    let getParameter (k : AnimationKind) (t : float) =
        match k with
        | AnimationKind.Linear -> 
            t
        | AnimationKind.Parametric ->   
            let sq = t*t
            sq / (2.0 * (sq - t) + 1.0)
        | AnimationKind.QuadInOut ->
            if t < 0.5 then 2.0 * t * t
            else -1.0 - 2.0*(t-2.0)*t
        | AnimationKind.Cubic ->
            let t2 = t*t
            -2.0 * t*t2 + 3.0 * t2
        | AnimationKind.Exp ->
            1.0 - exp(-8.0 * t)
        | AnimationKind.Tanh ->
            tanh(t * 7.0 - 3.5) * 0.5 + 0.5
        | AnimationKind.Spring frequency ->
            1.0 - cos(t * Constant.PiTimesTwo * frequency) * exp(-6.0*t)
        | AnimationKind.Pow e ->
            t ** e

    let inline interpolateAux< ^a, ^b when (^a or ^b) : (static member Lerp : ^b * ^b * float -> ^b)> (_dummy : ^a) (now : MicroTime) (x : Animation< ^b >) : ^b =
        let t = (now - x.startTime) / (x.stopTime - x.startTime)
        if t < 0.0 then x.startValue
        elif t > 1.0 then x.stopValue
        else 
            let v = getParameter x.kind t
            ((^a or ^b) : (static member Lerp : ^b * ^b * float -> ^b) (x.startValue, x.stopValue, v))

    let inline interpolate t a = interpolateAux Unchecked.defaultof<TimesFloat> t a


[<ModelType>]
type OrbitState =
    {
        sky     : V3d
        center  : V3d
        phi     : float
        theta   : float
        radius  : float

        shift : V2d
        userModifiedAngles : bool
        userModifiedCenter : bool
        userModifiedRadius : bool
        
        centerAnimation : Option<Animation<V3d>>
        locationAnimation : Option<Animation<V3d>>
        panAnimation    : Option<Animation<V2d>>

        targetPhi : float
        targetTheta : float
        targetRadius : float
        dragging : bool
        [<NonAdaptive>]
        lockedToScene : bool
        
        [<NonAdaptive>]
        isOrtho : bool
        
        [<NonAdaptive>]
        rotateButton : Button
        [<NonAdaptive>]
        panButton : Button

        dragStarts : MapExt<int, V2i * Button>

        [<NonAdaptive>]
        lastRender : Option<MicroTime>

        view : CameraView
        
        radiusRange : V2d
        thetaRange : V2d
        moveSensitivity : float
        zoomSensitivity : float
        speed : float

        pick : V2d -> option<V3d>
    
    } with 

        member x.super = 0
        member model.animationRunning = 
            let dphi = model.targetPhi - model.phi
            let dtheta = model.targetTheta - model.theta
            let dradius = model.targetRadius - model.radius

            model.locationAnimation.IsSome || model.centerAnimation.IsSome || abs dradius > 0.01 || abs dtheta > 0.1|| abs dphi > 0.1 || not (MapExt.isEmpty model.dragStarts)

