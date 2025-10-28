namespace Aardvark.Dom.Utilities.FreeFlyController

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Adaptify
open Aardvark.Rendering
open Aardvark.Dom

type FreeFlyConfig =
    {
        MoveSpeed : float
        Damping : float
        TurnSpeed : float
        TurnFactor : float
        MoveFactor : float
        
        WheelSensitivity : float
        TurnSensitivity : float
        PanSensitivity : float
        
        StickExponent : float
    }

    static member Default =
        {
            MoveSpeed = 1.5
            Damping = 20.0
            TurnSpeed = 1.5
            TurnFactor = 30.0
            MoveFactor = 10.0
            
            WheelSensitivity = 1.0
            TurnSensitivity = 1.0
            PanSensitivity = 1.0
            StickExponent = 2.0
        }

[<ModelType>]
type FreeFlyState =
    {
        LastRender : TimeSpan
        Position : V3d
        Sky : V3d
        Forward : V3d
        
        Config : FreeFlyConfig
        
        [<NonAdaptive>]
        MoveVectors : HashMap<string, V3d>
        
        [<NonAdaptive>]
        TurnVectors : HashMap<string, V2d>
        
        SprintFactor : float
        
        Momentum : V3d
        
        TargetMoveLocal : V3d
        TargetMoveGlobal : V3d
        TargetTurn : V2d
        Camera : CameraView
        
        [<NonAdaptive>]
        Handler : option<IEventHandler>
    }
    
    member x.MoveVec =
        (V3d.Zero, x.MoveVectors) ||> HashMap.fold (fun a _ b -> a + b)
    
    member x.TurnVec =
        (V2d.Zero, x.TurnVectors) ||> HashMap.fold (fun a _ b -> a + b)
    
    member x.IsAnimating =
        not (Fun.IsTiny(x.Momentum, 1E-8)) ||
        not (Fun.IsTiny(x.MoveVec, 1E-8)) ||
        not (Fun.IsTiny(x.TargetTurn, 1E-8)) ||
        not (Fun.IsTiny(x.TurnVec, 1E-8)) ||
        not (Fun.IsTiny(x.TargetMoveLocal, 1E-8)) ||
        not (Fun.IsTiny(x.TargetMoveGlobal, 1E-8))
    
    
module FreeFlyState =
    let withCamera (model : FreeFlyState) =
        let cam = CameraView.lookAt model.Position (model.Position + model.Forward) model.Sky
        { model with Camera = cam }