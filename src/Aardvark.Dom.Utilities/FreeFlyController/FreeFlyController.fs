namespace Aardvark.Dom.Utilities.FreeFlyController

open Aardvark.Dom.Utilities
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Dom

type FreeFlyMessage =
    | AddMoveVec of source : string * direction : V3d
    | SetMoveVec of source : string * direction : V3d
    | AddTurnVec of source : string * direction : V2d
    | SetTurnVec of source : string * direction : V2d
    | AddTargetMove of direction : V3d
    | AddTargetMoveGlobal of direction : V3d
    | SetSprintFactor of factor : float
    | AddMomentum of direction : V3d
    | AddTargetTurn of turn : V2d
    | UpdateConfig of cfg : FreeFlyConfig
    | AdjustMoveSpeed of factor : float
    | Rendered
    | SetEventHandler of handler : IEventHandler
    | FlyTo of location : V3d * forward : V3d
    
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
        | FlyTo(location, newForward) ->
            
            let cam = model.Camera
            let sky = cam.Sky
            let currentRight = cam.Right
            let currentForward = cam.Forward
            let currentUp = cam.Up
                
            let dx =
                let r = currentRight - Vec.dot currentRight sky * sky |> Vec.normalize
                let o = currentForward - Vec.dot currentForward sky * sky |> Vec.normalize
                let n = newForward - Vec.dot newForward sky * sky |> Vec.normalize
                let x = Vec.dot r n
                let y = Vec.dot o n
                atan2 y x - Constant.PiHalf
                
            let rot = M33d.Rotation(sky, dx)
            let currentForward = rot * currentForward
            let currentRight = rot * currentRight
            let currentUp = rot * currentUp
            
            let dy =
                let o = currentForward - Vec.dot currentForward currentRight * currentRight |> Vec.normalize
                let n = newForward - Vec.dot newForward currentRight * currentRight |> Vec.normalize
                let u = currentUp - Vec.dot currentUp currentRight * currentRight |> Vec.normalize
                
                let y = Vec.dot u n
                let z = Vec.dot o n
                atan2 z -y - Constant.PiHalf
            
            { model with
                TargetTurn = model.TargetTurn + V2d(dx, dy)
                TargetMoveGlobal = location - model.Position
            }
            |> withStartTime model
            
            
            
        | SetEventHandler h ->
            { model with Handler = Some h }
        | AddTargetMoveGlobal m ->
            { model with TargetMoveGlobal = model.TargetMoveGlobal + m }
            |> withStartTime model
        | AddTargetMove m ->
            { model with TargetMoveLocal = model.TargetMoveLocal + m }
            |> withStartTime model
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
                
                let dstMove = model.TargetMoveLocal * (model.Config.MoveFactor * dt.TotalSeconds |> min 1.0)
                let dstMoveGlobal = model.TargetMoveGlobal * (model.Config.MoveFactor * dt.TotalSeconds |> min 1.0)
                
                let targetMove =
                    model.Momentum * model.Config.MoveSpeed * dt.TotalSeconds
                let moveLocal =
                    V3d model.MoveVec * model.Config.MoveSpeed * model.SprintFactor * dt.TotalSeconds
                
                let moveVec = targetMove + moveLocal + dstMove
                
                let move =
                    float moveVec.Z * model.Forward +
                    float moveVec.Y * up +
                    float moveVec.X * right +
                    dstMoveGlobal
                
                let rotSkyAngle = model.TargetTurn.X * (model.Config.TurnFactor * dt.TotalSeconds |> min 1.0)
                let rotRightAngle = model.TargetTurn.Y * (model.Config.TurnFactor * dt.TotalSeconds |> min 1.0)
                       
                let turn = model.TurnVec * model.Config.TurnSpeed * dt.TotalSeconds
                       
                let rotation =
                    Rot3d.Rotation(model.Sky, rotSkyAngle + turn.X) *
                    Rot3d.Rotation(right, rotRightAngle + turn.Y)
                 
                
                { model with
                    TargetMoveLocal = model.TargetMoveLocal - dstMove
                    TargetMoveGlobal = model.TargetMoveGlobal - dstMoveGlobal
                    TargetTurn = model.TargetTurn - V2d(rotSkyAngle, rotRightAngle) 
                    LastRender = now
                    Position = model.Position + move
                    Forward = rotation.Transform model.Forward 
                    Momentum = model.Momentum * 0.5 ** (model.Config.Damping * dt.TotalSeconds)
                } |> FreeFlyState.withCamera
            else
                model

    let getAttributes (model : AdaptiveFreeFlyState) (env : Env<FreeFlyMessage>) =
        renderControlExt {

            RenderControl.OnRendered (fun _ -> env.Emit [FreeFlyMessage.Rendered])
            RenderControl.OnReady(fun h -> env.Emit [FreeFlyMessage.SetEventHandler h])
            
            // Install virtual touch sticks
            Dom.OnBoot(virtualTouchSticksJs)

            Dom.OnMouseWheel(fun e ->
                env.Emit [FreeFlyMessage.AddMomentum (V3d(0.0, 0.0, -0.1 * (AVal.force model.Config).WheelSensitivity * e.DeltaY))]
            )
            
            let mutable rotDown = false
            let mutable panDown = false
            let mutable zoomDown = false
            let mutable panStartDepth : option<float> = None
            
            Dom.OnMouseDown(fun e ->
                if e.Button = Button.Left then
                    rotDown <- true
                if e.Button = Button.Middle then
                    panStartDepth <-
                        match model.Handler with
                        | Some handler ->
                            let pixel = e.ClientPosition - V2i e.ClientRect.Min
                            match handler.Read(pixel, SceneEventKind.PointerDown) with
                            | Some loc -> Some -loc.ViewPosition.Z
                            | None -> None
                        | None ->
                            None
                    panDown <- true
                if e.Button = Button.Right then
                    zoomDown <- true
            )
            
            Dom.OnMouseUp(fun e ->
                if e.Button = Button.Left then
                    rotDown <- false
                if e.Button = Button.Middle then
                    panDown <- false
                    panStartDepth <- None
                if e.Button = Button.Right then
                    zoomDown <- false
            )
            
            Dom.OnPointerDown(ignore, pointerCapture = true)
            Dom.OnPointerUp(ignore, pointerCapture = true)
            
            Dom.OnPointerMove(fun e ->
                if e.PointerType = PointerType.Mouse  then
                    if rotDown then
                        let tx = -0.007 * (AVal.force model.Config).TurnSensitivity * e.MovementX
                        let ty = -0.007 * (AVal.force model.Config).TurnSensitivity * e.MovementY
                        
                        env.Emit [ AddTargetTurn(V2d(tx, ty)) ]
                    if panDown then
                        // printfn "%A" panStartPos
                        // match panStartPos with
                        // | Some (proj, worldPosition, depth) ->
                        //     let view = AVal.force model.Camera |> Aardvark.Rendering.CameraView.viewTrafo
                        //     let vp = view * proj
                        //     
                        //     let tc = (V2d e.ClientPosition - e.ClientRect.Min) / e.ClientRect.Size
                        //     let ndc = V3d(2.0 * tc.X - 1.0, 1.0 - 2.0 * tc.Y, depth)
                        //     let newWorld = vp.Backward.TransformPosProj(ndc)
                        //     
                        //     let delta = newWorld - worldPosition
                        //     env.Emit [AddTargetMoveGlobal(delta) ]
                        //     printfn "DELTA: %A" delta
                        //     panStartPos <- Some (proj, newWorld, depth)
                        //     ()
                        // | _ ->
                        //
                        //
                        
                        let speed = 
                            match panStartDepth with
                            | Some d -> 0.005 * d * (AVal.force model.Config).PanSensitivity
                            | None -> 0.025 * (AVal.force model.Config).PanSensitivity
                            
                        let dx = speed*e.MovementX
                        let dy = -speed*e.MovementY
                        env.Emit [AddTargetMove(V3d(dx, dy, 0.0)) ]
                    if zoomDown then
                        let dy = -0.05*e.MovementY
                        env.Emit [AddTargetMove(V3d(0.0, 0.0, dy)) ]
                        
            )
            
            Dom.OnGamepadAxisChange(fun e ->
                let exponent = (AVal.force model.Config).StickExponent
                let v = float (sign e.Value) * (abs e.Value ** exponent)
                match e.AxisName with
                | "LeftStickX" ->
                    env.Emit [FreeFlyMessage.SetMoveVec($"{e.ControllerId}Right", V3d.IOO * v * 1.5) ]
                | "LeftStickY" ->
                    env.Emit [FreeFlyMessage.SetMoveVec($"{e.ControllerId}Forward", V3d.OON * v * 1.5) ]
                | "RightStickX" ->
                    env.Emit [FreeFlyMessage.SetTurnVec($"{e.ControllerId}TurnHorizontal", V2d(-v * 1.5, 0.0)) ]
                | "RightStickY" ->
                    env.Emit [FreeFlyMessage.SetTurnVec($"{e.ControllerId}TurnVertical", V2d(0.0, -v * 1.5)) ]
                | _ ->
                    printfn "other axis: %A" e.AxisIndex
            )
            
            Dom.OnGamepadButtonDown(fun e ->
                match e.ButtonName with
                | "DPadUp" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.OIO) ]
                | "DPadDown" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.ONO) ]
                | "DPadRight" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.IOO) ]
                | "DPadLeft" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.NOO) ]
                | "LB" -> env.Emit [FreeFlyMessage.AdjustMoveSpeed (1.0 / 1.5) ]
                | "RB" -> env.Emit [FreeFlyMessage.AdjustMoveSpeed 1.5 ]
                | "LT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.ONO) ]
                | "RT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.OIO) ]
                | _ -> ()
            )
            Dom.OnGamepadButtonUp(fun e ->
                match e.ButtonName with
                | "DPadUp" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.ONO) ]
                | "DPadDown" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.OIO) ]
                | "DPadRight" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.NOO) ]
                | "DPadLeft" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Pad", V3d.IOO) ]
                | "LT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.OIO) ]
                | "RT" -> env.Emit [FreeFlyMessage.AddMoveVec($"{e.ControllerId}Up", V3d.ONO) ]
                | _ -> ()
            )
            
            let mutable currentKeyboardMove = V3d.Zero
            let mutable shiftDown = false
            let flushKeyboardMove() =
                let m = if shiftDown then currentKeyboardMove.XZY else currentKeyboardMove
                env.Emit [FreeFlyMessage.SetMoveVec("keyboard", m) ]
            Dom.OnKeyDown (fun e ->
                if not e.Repeat then
                    match e.Key with
                    | "Shift" ->
                        shiftDown <- true
                        flushKeyboardMove()
                    | "W" | "w" ->
                        shiftDown <- e.Shift
                        currentKeyboardMove <- currentKeyboardMove + V3d.OOI
                        flushKeyboardMove()
                    | "S" | "s" ->
                        shiftDown <- e.Shift
                        currentKeyboardMove <- currentKeyboardMove - V3d.OOI
                        flushKeyboardMove()
                    | "A" | "a" ->
                        shiftDown <- e.Shift
                        currentKeyboardMove <- currentKeyboardMove + V3d.NOO
                        flushKeyboardMove()
                    | "D" | "d" ->
                        shiftDown <- e.Shift
                        currentKeyboardMove <- currentKeyboardMove + V3d.IOO
                        flushKeyboardMove()
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
                | "Shift" ->
                    shiftDown <- false
                    flushKeyboardMove()
                | "W" | "w" ->
                    currentKeyboardMove <- currentKeyboardMove - V3d.OOI
                    flushKeyboardMove()
                | "S" | "s" ->
                    currentKeyboardMove <- currentKeyboardMove + V3d.OOI
                    flushKeyboardMove()
                | "A" | "a" ->
                    currentKeyboardMove <- currentKeyboardMove + V3d.IOO
                    flushKeyboardMove()
                | "D" | "d" ->
                    currentKeyboardMove <- currentKeyboardMove - V3d.IOO
                    flushKeyboardMove()
                | _ ->
                    ()
            )
            
        }
