namespace Aardvark.Dom.Utilities.FreeFlyController

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Adaptify
open Aardvark.Rendering
open Aardvark.Dom

[<ModelType>]
type FreeFlyState =
    {
        LastRender : TimeSpan
        Position : V3d
        Sky : V3d
        Forward : V3d
        
        MoveSpeed : float
        Damping : float
        
        [<NonAdaptive>]
        MoveVectors : HashMap<string, V3d>
        
        [<NonAdaptive>]
        TurnVectors : HashMap<string, V2d>
        
        SprintFactor : float
        
        Momentum : V3d
        TargetTurn : V2d
        Camera : CameraView
    }
    
    member x.MoveVec =
        (V3d.Zero, x.MoveVectors) ||> HashMap.fold (fun a _ b -> a + b)
    
    member x.TurnVec =
        (V2d.Zero, x.TurnVectors) ||> HashMap.fold (fun a _ b -> a + b)
    
    member x.IsAnimating =
        not (Fun.IsTiny(x.Momentum, 1E-8)) ||
        not (Fun.IsTiny(x.MoveVec, 1E-8)) ||
        not (Fun.IsTiny(x.TargetTurn, 1E-8)) ||
        not (Fun.IsTiny(x.TurnVec, 1E-8))
    
    
module FreeFlyState =
    let withCamera (model : FreeFlyState) =
        let cam = CameraView.lookAt model.Position (model.Position + model.Forward) model.Sky
        { model with Camera = cam }