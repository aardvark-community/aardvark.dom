namespace Aardvark.Dom.Utilities.FreeFlyController

open System
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
        
        MoveVec : V3i
        Momentum : V3d
        TargetTurn : V2d
        Camera : CameraView
    }
    
    member x.IsAnimating =
        not (Fun.IsTiny(x.Momentum, 1E-8)) ||
        not (Fun.IsTiny(x.TargetTurn, 1E-8)) ||
        x.MoveVec <> V3i.Zero
    
    
module FreeFlyState =
    let withCamera (model : FreeFlyState) =
        let cam = CameraView.lookAt model.Position (model.Position + model.Forward) model.Sky
        { model with Camera = cam }