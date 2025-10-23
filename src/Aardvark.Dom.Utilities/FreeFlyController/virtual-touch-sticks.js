(function() {
    const container = __THIS__;
    const gamepadLib = window.gamepadEvents;

    
    if (!gamepadLib || !container) {
        console.error('GamepadEventLibrary not found at window.gamepadEvents');
        return;
    }
    gamepadLib.setGlobalTarget(container);

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
                    gamepadLib.dispatchAxisEvent(controllerId, 0, values.x, container);
                    gamepadLib.dispatchAxisEvent(controllerId, 1, values.y, container);
                } else {
                    // Right stick controls camera (axes 2 and 3)
                    gamepadLib.dispatchAxisEvent(controllerId, 2, values.x, container);
                    gamepadLib.dispatchAxisEvent(controllerId, 3, values.y, container);
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
                    gamepadLib.dispatchAxisEvent(controllerId, 0, 0, container);
                    gamepadLib.dispatchAxisEvent(controllerId, 1, 0, container);
                } else {
                    gamepadLib.dispatchAxisEvent(controllerId, 2, 0, container);
                    gamepadLib.dispatchAxisEvent(controllerId, 3, 0, container);
                }
            }

            touches.delete(touch.identifier);
        }
    }

    container.addEventListener('touchend', handleTouchEnd, { passive: true });
    container.addEventListener('touchcancel', handleTouchEnd, { passive: true });

    console.log('Virtual touch sticks initialized for FreeFlyController');
})();
