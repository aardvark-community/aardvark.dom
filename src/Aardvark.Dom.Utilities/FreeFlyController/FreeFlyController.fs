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
            Dom.OnBoot("""
                (function() {
                    const container = document.body;
                    const gamepadLib = window.aardvark.gamepad;

                    if (!gamepadLib) {
                        console.error('GamepadEventLibrary not found');
                        return;
                    }

                    // Register virtual controller
                    const controllerId = gamepadLib.registerController('virtual-touch-controller');

                    // Configuration
                    const MOVE_THRESHOLD = 10; // pixels to move before showing sticks
                    const STICK_RADIUS = 60; // radius of each stick area
                    const MAX_DISTANCE = 50; // max distance from center

                    // Touch state
                    const touches = new Map();

                    // Create stick visual elements
                    function createStick(side) {
                        const stick = document.createElement('div');
                        stick.style.cssText = `
                            position: fixed;
                            width: ${STICK_RADIUS * 2}px;
                            height: ${STICK_RADIUS * 2}px;
                            border-radius: 50%;
                            background: radial-gradient(circle at 30% 30%, rgba(80, 80, 90, 0.5), rgba(40, 40, 50, 0.7));
                            border: 2px solid rgba(150, 150, 170, 0.6);
                            pointer-events: none;
                            z-index: 10000;
                            display: none;
                        `;

                        const knob = document.createElement('div');
                        knob.style.cssText = `
                            position: absolute;
                            width: 40px;
                            height: 40px;
                            border-radius: 50%;
                            background: radial-gradient(circle at 35% 35%, rgba(120, 120, 140, 0.8), rgba(60, 60, 80, 0.9));
                            border: 2px solid rgba(180, 180, 200, 0.7);
                            top: 50%;
                            left: 50%;
                            transform: translate(-50%, -50%);
                            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
                        `;

                        stick.appendChild(knob);
                        document.body.appendChild(stick);

                        return { stick, knob };
                    }

                    const leftStick = createStick('left');
                    const rightStick = createStick('right');

                    function updateStickPosition(stickElements, centerX, centerY, currentX, currentY) {
                        const dx = currentX - centerX;
                        const dy = currentY - centerY;
                        const distance = Math.sqrt(dx * dx + dy * dy);
                        const clampedDistance = Math.min(distance, MAX_DISTANCE);
                        const angle = Math.atan2(dy, dx);

                        const knobX = Math.cos(angle) * clampedDistance;
                        const knobY = Math.sin(angle) * clampedDistance;

                        stickElements.knob.style.transform = `translate(calc(-50% + ${knobX}px), calc(-50% + ${knobY}px))`;

                        // Calculate normalized values (-1 to 1)
                        const normalizedX = knobX / MAX_DISTANCE;
                        const normalizedY = knobY / MAX_DISTANCE;

                        return { x: normalizedX, y: normalizedY };
                    }

                    function showStick(stickElements, x, y) {
                        stickElements.stick.style.left = (x - STICK_RADIUS) + 'px';
                        stickElements.stick.style.top = (y - STICK_RADIUS) + 'px';
                        stickElements.stick.style.display = 'block';
                    }

                    function hideStick(stickElements) {
                        stickElements.stick.style.display = 'none';
                        stickElements.knob.style.transform = 'translate(-50%, -50%)';
                    }

                    container.addEventListener('touchstart', function(e) {
                        for (let i = 0; i < e.changedTouches.length; i++) {
                            const touch = e.changedTouches[i];
                            const screenWidth = window.innerWidth;
                            const side = touch.clientX < screenWidth / 2 ? 'left' : 'right';

                            // Check if this side already has a touch
                            let sideHasTouch = false;
                            touches.forEach(t => {
                                if (t.side === side) sideHasTouch = true;
                            });

                            if (!sideHasTouch) {
                                touches.set(touch.identifier, {
                                    side: side,
                                    startX: touch.clientX,
                                    startY: touch.clientY,
                                    currentX: touch.clientX,
                                    currentY: touch.clientY,
                                    visible: false,
                                    movedDistance: 0
                                });
                            }
                        }
                    }, { passive: true });

                    container.addEventListener('touchmove', function(e) {
                        for (let i = 0; i < e.changedTouches.length; i++) {
                            const touch = e.changedTouches[i];
                            const touchState = touches.get(touch.identifier);

                            if (!touchState) continue;

                            touchState.currentX = touch.clientX;
                            touchState.currentY = touch.clientY;

                            // Calculate distance moved from start
                            const dx = touchState.currentX - touchState.startX;
                            const dy = touchState.currentY - touchState.startY;
                            const distanceMoved = Math.sqrt(dx * dx + dy * dy);
                            touchState.movedDistance = distanceMoved;

                            // Show stick only after moving threshold distance
                            if (!touchState.visible && distanceMoved >= MOVE_THRESHOLD) {
                                touchState.visible = true;
                                const stickElements = touchState.side === 'left' ? leftStick : rightStick;
                                showStick(stickElements, touchState.startX, touchState.startY);
                            }

                            // Update stick position and emit gamepad events
                            if (touchState.visible) {
                                const stickElements = touchState.side === 'left' ? leftStick : rightStick;
                                const values = updateStickPosition(
                                    stickElements,
                                    touchState.startX,
                                    touchState.startY,
                                    touchState.currentX,
                                    touchState.currentY
                                );

                                // Emit gamepad axis events
                                if (touchState.side === 'left') {
                                    // Left stick controls movement (axes 0 and 1)
                                    gamepadLib.dispatchAxisEvent(controllerId, 0, values.x);
                                    gamepadLib.dispatchAxisEvent(controllerId, 1, values.y);
                                } else {
                                    // Right stick controls camera (axes 2 and 3)
                                    gamepadLib.dispatchAxisEvent(controllerId, 2, values.x);
                                    gamepadLib.dispatchAxisEvent(controllerId, 3, values.y);
                                }
                            }
                        }
                    }, { passive: true });

                    function handleTouchEnd(e) {
                        for (let i = 0; i < e.changedTouches.length; i++) {
                            const touch = e.changedTouches[i];
                            const touchState = touches.get(touch.identifier);

                            if (!touchState) continue;

                            // Hide stick if it was visible
                            if (touchState.visible) {
                                const stickElements = touchState.side === 'left' ? leftStick : rightStick;
                                hideStick(stickElements);

                                // Reset axes to zero
                                if (touchState.side === 'left') {
                                    gamepadLib.dispatchAxisEvent(controllerId, 0, 0);
                                    gamepadLib.dispatchAxisEvent(controllerId, 1, 0);
                                } else {
                                    gamepadLib.dispatchAxisEvent(controllerId, 2, 0);
                                    gamepadLib.dispatchAxisEvent(controllerId, 3, 0);
                                }
                            }

                            touches.delete(touch.identifier);
                        }
                    }

                    container.addEventListener('touchend', handleTouchEnd, { passive: true });
                    container.addEventListener('touchcancel', handleTouchEnd, { passive: true });

                    console.log('Virtual touch sticks initialized for FreeFlyController');
                })();
            """)

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
