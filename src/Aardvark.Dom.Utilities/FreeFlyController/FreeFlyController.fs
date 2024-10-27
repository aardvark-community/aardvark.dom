namespace Aardvark.Dom.Utilities.FreeFlyController

open Aardvark.Base
open Aardvark.Dom

type FreeFlyMessage =
    | AddMoveVec of direction : V3i
    | AddMomentum of direction : V3d
    | AddTargetTurn of turn : V2d
    | AdjustMoveSpeed of factor : float
    | Rendered
    
module FreeFlyController =
    
    let private sw = System.Diagnostics.Stopwatch.StartNew()
    let inline private now() = sw.Elapsed
    
    let private withStartTime (oldModel : FreeFlyState) (newModel : FreeFlyState) =
        if newModel.IsAnimating && not oldModel.IsAnimating then
            FreeFlyState.withCamera { newModel with LastRender = now() }
        else
            newModel
    
    let update (model : FreeFlyState) (msg : FreeFlyMessage) =
        match msg with
        | AddMoveVec v ->
            { model with MoveVec = model.MoveVec + v }
            |> withStartTime model
        | AddMomentum v ->
            { model with Momentum = model.Momentum + v }
            |> withStartTime model
        | AddTargetTurn t ->
            { model with TargetTurn = model.TargetTurn + t }
            |> withStartTime model
        | AdjustMoveSpeed f ->
            { model with MoveSpeed = model.MoveSpeed * f }
        | Rendered ->
            if model.IsAnimating then
                let now = now()
                let dt = now - model.LastRender
                
                let right = Vec.cross model.Forward model.Sky |> Vec.normalize
                let up = Vec.cross right model.Forward |> Vec.normalize
                
                
                let targetMove =
                    model.Momentum * model.MoveSpeed * dt.TotalSeconds
                let moveLocal =
                    V3d model.MoveVec * model.MoveSpeed * dt.TotalSeconds
                
                let moveVec = targetMove + moveLocal
                
                let move =
                    float moveVec.Z * model.Forward +
                    float moveVec.Y * up +
                    float moveVec.X * right
                
                let rotSkyAngle = model.TargetTurn.X * (30.0 * dt.TotalSeconds |> min 1.0)
                let rotRightAngle = model.TargetTurn.Y * (30.0 * dt.TotalSeconds |> min 1.0)
                       
                let rotation =
                    Rot3d.Rotation(model.Sky, rotSkyAngle) *
                    Rot3d.Rotation(right, rotRightAngle)
                 
                
                { model with
                    TargetTurn = model.TargetTurn - V2d(rotSkyAngle, rotRightAngle) 
                    LastRender = now
                    Position = model.Position + move
                    Forward = rotation.Transform model.Forward 
                    Momentum = model.Momentum * 0.5 ** (model.Damping * dt.TotalSeconds)
                } |> FreeFlyState.withCamera
            else
                model

    let getAttributes (env : Env<FreeFlyMessage>) =
        att {
            
            Dom.OnMouseWheel(fun e ->
                env.Emit [FreeFlyMessage.AddMomentum (V3d(0.0, 0.0, -0.1 * e.DeltaY))]
            )
            
            let mutable rotDown = false
            Dom.OnPointerDown((fun e ->
                if e.Button = Button.Left then
                    rotDown <- true
                
            ), pointerCapture = true)
            
            Dom.OnPointerUp((fun e ->
                if e.Button = Button.Left then
                    rotDown <- false
            ), pointerCapture = true)
            
            Dom.OnPointerMove(fun e ->
                if rotDown then
                    let tx = -0.005 * e.MovementX
                    let ty = -0.005 * e.MovementY
                    
                    env.Emit [ AddTargetTurn(V2d(tx, ty)) ]
            )
            
            Dom.OnKeyDown (fun e ->
                if not e.Repeat then
                    match e.Key with
                    | "W" | "w" ->
                        env.Emit [FreeFlyMessage.AddMoveVec V3i.OOI ]
                    | "S" | "s" ->
                        env.Emit [FreeFlyMessage.AddMoveVec V3i.OON ]
                    | "A" | "a" ->
                        env.Emit [FreeFlyMessage.AddMoveVec V3i.NOO ]
                    | "D" | "d" ->
                        env.Emit [FreeFlyMessage.AddMoveVec V3i.IOO ]
                    | "ArrowUp" when e.Shift ->
                        env.Emit [FreeFlyMessage.AdjustMoveSpeed 1.5] 
                    | "ArrowDown" when e.Shift ->
                        env.Emit [FreeFlyMessage.AdjustMoveSpeed (1.0 / 1.5)]    
                    | "PageUp" ->
                        env.Emit [FreeFlyMessage.AdjustMoveSpeed 1.5] 
                    | "PageDown" ->
                        env.Emit [FreeFlyMessage.AdjustMoveSpeed (1.0 / 1.5)]    
                    | _ ->
                        ()
            )
            
            Dom.OnKeyUp (fun e ->
                match e.Key with
                | "W" | "w" ->
                    env.Emit [FreeFlyMessage.AddMoveVec V3i.OON ]
                | "S" | "s" ->
                    env.Emit [FreeFlyMessage.AddMoveVec V3i.OOI ]
                | "A" | "a" ->
                    env.Emit [FreeFlyMessage.AddMoveVec V3i.IOO ]
                | "D" | "d" ->
                    env.Emit [FreeFlyMessage.AddMoveVec V3i.NOO ]
                | _ ->
                    ()
            )
            
        }
