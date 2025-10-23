namespace Aardvark.Dom.Utilities.FreeFlyController

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Dom

type FreeFlyMessage =
    | AddMoveVec of source : string * direction : V3d
    | SetMoveVec of source : string * direction : V3d
    | AddTurnVec of source : string * direction : V2d
    | SetTurnVec of source : string * direction : V2d
    | SetSprintFactor of factor : float
    | AddMomentum of direction : V3d
    | AddTargetTurn of turn : V2d
    | UpdateConfig of cfg : FreeFlyConfig
    | AdjustMoveSpeed of factor : float
    | Rendered
    
module FreeFlyController =

    let private virtualTouchSticksJs =
        let ass = typeof<FreeFlyMessage>.Assembly
        let resourceName = $"{ass.GetName().Name}.FreeFlyController.virtual-touch-sticks.js"
        use s = ass.GetManifestResourceStream resourceName
        use r = new System.IO.StreamReader(s)
        r.ReadToEnd()

    let inline private isTinyAux<'a, 'b when ('a or 'b) : (static member IsTiny : 'a * float -> bool)> (foo : 'b) (v : 'a) (eps : float) =
        ((^a or ^b) : (static member IsTiny : 'a * float -> bool) (v, eps))
    let inline isTiny v eps = isTinyAux Unchecked.defaultof<Fun> v eps
    
    
    let private sw = System.Diagnostics.Stopwatch.StartNew()
    let inline private now() = sw.Elapsed
    
    let private withStartTime (oldModel : FreeFlyState) (newModel : FreeFlyState) =
        if newModel.IsAnimating && not oldModel.IsAnimating then
            FreeFlyState.withCamera { newModel with LastRender = now() }
        else
            newModel
    
    let inline private addVec (source : string) (v : 'a) (map : HashMap<string, 'a>) =
        map |> HashMap.alter source (fun old ->
            let newValue = 
                match old with
                | Some old -> old + v
                | None -> v
            if isTiny newValue 1E-8 then None
            else Some newValue
        )
    
    let update (model : FreeFlyState) (msg : FreeFlyMessage) =
        match msg with
        | SetSprintFactor f ->
            { model with SprintFactor = f }
        | AddMoveVec(source, v) ->
            { model with MoveVectors = addVec source v model.MoveVectors }
            |> withStartTime model
        | SetMoveVec(source, v) ->
            let newMoveVecs = if Fun.IsTiny(v, 1E-8) then HashMap.remove source model.MoveVectors else HashMap.add source v model.MoveVectors
            { model with MoveVectors = newMoveVecs}
            |> withStartTime model
        | AddTurnVec(source, v) ->
            { model with TurnVectors = addVec source v model.TurnVectors }
            |> withStartTime model
        | SetTurnVec(source, v) ->
            let newTurnVecs = if Fun.IsTiny(v, 1E-8) then HashMap.remove source model.TurnVectors else HashMap.add source v model.TurnVectors
            { model with TurnVectors = newTurnVecs }
            |> withStartTime model
        | AddMomentum v ->
            { model with Momentum = model.Momentum + v }
            |> withStartTime model
        | AddTargetTurn t ->
            { model with TargetTurn = model.TargetTurn + t }
            |> withStartTime model
        | UpdateConfig cfg ->
            { model with Config = cfg }
        | AdjustMoveSpeed f ->
            { model with Config = { model.Config with MoveSpeed = model.Config.MoveSpeed * f } }
        | Rendered ->
            if model.IsAnimating then
                let now = now()
                let dt = now - model.LastRender
                
                let right = Vec.cross model.Forward model.Sky |> Vec.normalize
                let up = Vec.cross right model.Forward |> Vec.normalize
                
                
                let targetMove =
                    model.Momentum * model.Config.MoveSpeed * dt.TotalSeconds
                let moveLocal =
                    V3d model.MoveVec * model.Config.MoveSpeed * model.SprintFactor * dt.TotalSeconds
                
                let moveVec = targetMove + moveLocal
                
                let move =
                    float moveVec.Z * model.Forward +
                    float moveVec.Y * up +
                    float moveVec.X * right
                
                let rotSkyAngle = model.TargetTurn.X * (30.0 * dt.TotalSeconds |> min 1.0)
                let rotRightAngle = model.TargetTurn.Y * (30.0 * dt.TotalSeconds |> min 1.0)
                       
                let turn = model.TurnVec * model.Config.TurnSpeed * dt.TotalSeconds
                       
                let rotation =
                    Rot3d.Rotation(model.Sky, rotSkyAngle + turn.X) *
                    Rot3d.Rotation(right, rotRightAngle + turn.Y)
                 
                
                { model with
                    TargetTurn = model.TargetTurn - V2d(rotSkyAngle, rotRightAngle) 
                    LastRender = now
                    Position = model.Position + move
                    Forward = rotation.Transform model.Forward 
                    Momentum = model.Momentum * 0.5 ** (model.Config.Damping * dt.TotalSeconds)
                } |> FreeFlyState.withCamera
            else
                model

    let getAttributes (env : Env<FreeFlyMessage>) =
        att {

            // Install virtual touch sticks
            Dom.OnBoot(virtualTouchSticksJs)

            Dom.OnMouseWheel(fun e ->
                env.Emit [FreeFlyMessage.AddMomentum (V3d(0.0, 0.0, -0.1 * e.DeltaY))]
            )
            
            let mutable rotDown = false
            Dom.OnPointerDown((fun e ->
                if e.PointerType = PointerType.Mouse && e.Button = Button.Left then
                    rotDown <- true
                
            ), pointerCapture = true)
            
            Dom.OnPointerUp((fun e ->
                if e.PointerType = PointerType.Mouse && e.Button = Button.Left then
                    rotDown <- false
            ), pointerCapture = true)
            
            Dom.OnPointerMove(fun e ->
                if e.PointerType = PointerType.Mouse && rotDown then
                    let tx = -0.005 * e.MovementX
                    let ty = -0.005 * e.MovementY
                    
                    env.Emit [ AddTargetTurn(V2d(tx, ty)) ]
            )
            
            Dom.OnGamepadAxisChange(fun e ->
                let v = float (sign e.Value) * (abs e.Value ** 2.0)
                match e.AxisName with
                | "LeftStickX" ->
                    env.Emit [FreeFlyMessage.SetMoveVec($"{e.ControllerId}Right", V3d.IOO * v * 1.5) ]
                | "LeftStickY" ->
                    env.Emit [FreeFlyMessage.SetMoveVec($"{e.ControllerId}Forward", V3d.OON * v * 1.5) ]
                | "RightStickX" ->
                    env.Emit [FreeFlyMessage.SetTurnVec($"{e.ControllerId}TurnHorizontal", V2d(-v, 0.0)) ]
                | "RightStickY" ->
                    env.Emit [FreeFlyMessage.SetTurnVec($"{e.ControllerId}TurnVertical", V2d(0.0, -v)) ]
                | _ ->
                    ()
            )
            
            Dom.OnGamepadButtonDown(fun e ->
                match e.ButtonName with
                | "LB" -> env.Emit [FreeFlyMessage.AdjustMoveSpeed (1.0 / 1.5) ]
                | "RB" -> env.Emit [FreeFlyMessage.AdjustMoveSpeed 1.5 ]
                | "LT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.ONO) ]
                | "RT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.OIO) ]
                | _ -> ()
            )
            Dom.OnGamepadButtonUp(fun e ->
                match e.ButtonName with
                | "LT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.OIO) ]
                | "RT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.ONO) ]
                | _ -> ()
            )
            
            Dom.OnKeyDown (fun e ->
                if not e.Repeat then
                    match e.Key with
                    | "W" | "w" ->
                        env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.OOI) ]
                    | "S" | "s" ->
                        env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.OON) ]
                    | "A" | "a" ->
                        env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.NOO) ]
                    | "D" | "d" ->
                        env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.IOO) ]
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
                    env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.OON) ]
                | "S" | "s" ->
                    env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.OOI) ]
                | "A" | "a" ->
                    env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.IOO) ]
                | "D" | "d" ->
                    env.Emit [FreeFlyMessage.AddMoveVec("keyboard", V3d.NOO) ]
                | _ ->
                    ()
            )
            
        }
