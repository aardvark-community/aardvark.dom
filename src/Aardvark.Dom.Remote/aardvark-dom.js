(function() {
    
    function findAardvark(win) {
        if(!win)return null;
        if(win.aardvark) return win.aardvark;
        if(win.top && win.top.aardvark) return findAardvark(win.top.aardvark);
        return null;
    }
    
    let aardvark = findAardvark(window);
    if(!aardvark) aardvark = {};
    window.aardvark = aardvark;
    
    // =========================================================================
    // table
    // =========================================================================
    let table = [];

    aardvark.nodeId = Symbol("aardvarkId");

    aardvark.register = (id, element) => {
        if (id === 0) return;
        element[aardvark.nodeId] = id;
        table[id] = element;
    };
    
    aardvark.registerSelector = (id, selector) => {
        if (id === 0) return;
        const res = document.querySelector(selector);
        if(!res) return;
        aardvark.register(id, res);
    };
    
    aardvark.delete = (id, n) => {
        if(id === 0) return;
        delete table[id];
        if (n) {
            n.remove();
            delete n[aardvark.nodeId];
        }
    };
    
    aardvark.get = (id) => {
        if(id === 0) return document.body;
        else return table[id];
    };

    aardvark.getId = (node) => {
        if (node) {
            if (node.tag === "body") return 0
            else return node[aardvark.nodeId];
        }
        else return undefined;
    };

    aardvark.getEventTargetId = (e) => {
        const p = e.composedPath();
        let i = 0;
        while (i < p.length) {
            let id = aardvark.getId(p[i]);
            if (id !== undefined) return id;
            i++;
        }
        return -1;
    };

    aardvark.onResize =
        (function () {

        let subscriptions = new WeakMap();

        const o = new ResizeObserver((entries) => {
            for (let entry of entries) {
                const element = entry.target;
                const rect = element.getBoundingClientRect();
                const size = { width: rect.width, height: rect.height };
                const states = subscriptions.get(element);
                if (states) {
                    for (let state of states) {
                        if (state.size.width !== size.width || state.size.height !== size.height) {
                            state.action(size);
                            state.size = size;
                        }
                    }
                }
            }
        })
            
        return function (element, action) {
            const rect = element.getBoundingClientRect();
            const size = { width: rect.width, height: rect.height };
            const sub = { size: size, action: action };

            let l = subscriptions.get(element);
            if (l) { l.push(sub); }
            else {
                subscriptions.set(element, [sub]);
                o.observe(element);
            }

            return function () {
                let l = subscriptions.get(element); 
                if (l) {
                    const idx = l.indexOf(sub);
                    if (idx >= 0) {
                        l.splice(idx, 1);
                        if (l.length === 0) {
                            subscriptions.delete(element);
                            o.unobserve(element);
                        }
                    }
                }
            };

        };

        })();
        

    aardvark.createTextNode = function(id) {
        const res = document.createTextNode("");
        aardvark.register(id, res);
        return res;
    }
    aardvark.createElement = function(tag, id) {
        const res = document.createElement(tag);
        aardvark.register(id, res);
        return res;
    }

    aardvark.insertAfter = function(refId, newNode) {
        let ref = aardvark.get(refId);
        if(!ref) return;
        let parent = ref.parentNode;
        if(!parent) return;
        if(ref.nextSibling) parent.insertBefore(newNode, ref.nextSibling);
        else parent.appendChild(newNode);
    };

    aardvark.insertFirst = function(parentId, newNode) {
        let parent = aardvark.get(parentId);
        if(!parent) return;
        if(parent.firstChild) parent.insertBefore(newNode, parent.firstChild);
        else parent.appendChild(newNode);
    };

    aardvark.appendChild = function (parentId, newNode) {
        let parent = aardvark.get(parentId);
        if(!parent) return;
        parent.appendChild(newNode);
    };

    aardvark.loadResults = {};

    aardvark.parseURL = function (str) {
        try { return new URL(str); }
        catch (e) { return new URL(aardvark.relativePath("http", str)); }
    };

    const pointerCaptureSym = Symbol("changePointerCapture");
    const preventDefaultSym = Symbol("preventDefault");

    aardvark.setListenerFlags = function (node, evtName, pointerCapture, preventDefault) {
        let captureDict = node[pointerCaptureSym];
        if (!captureDict) {
            captureDict = {};
            node[pointerCaptureSym] = captureDict;
        }

        let preventDefaultDict = node[preventDefaultSym];
        if (!preventDefaultDict) {
            preventDefaultDict = {};
            node[preventDefaultSym] = preventDefaultDict;
        }

        captureDict[evtName] = pointerCapture;
        preventDefaultDict[evtName] = preventDefault;
    };

    aardvark.getListenerFlags = function (node, evtName) {
        let result = { pointerCapture: false, preventDefault: false };

        let captureDict = node[pointerCaptureSym];
        if (captureDict) result.pointerCapture = captureDict[evtName] || false;

        let preventDefaultDict = node[preventDefaultSym];
        if (preventDefaultDict) result.preventDefault = preventDefaultDict[evtName] || false;
        return result;
    };



    aardvark.require = function (urls, cont) {
        const run = function (urls, i, cont) {
            if (i >= urls.length) {
                cont();
            }
            else {
                let u = urls[i];
                
                console.log("loading " + u);
                if (u in aardvark.loadResults) {
                    const res = aardvark.loadResults[u];
                    res(() => run(urls, i+1, cont));
                }
                else {
                    let s = null;
                    let conts = [() => run(urls, i + 1, cont)];

                    let path = aardvark.parseURL(u).pathname.toLowerCase();

                    if (path.endsWith(".js") || path.startsWith("data:text/javascript")) {
                        s = document.createElement("script");
                        s.type = "application/javascript";
                        s.src = u;
                    }
                    else if (path.endsWith(".css")  || path.startsWith("data:text/css")) {
                        s = document.createElement("link");
                        s.type = "text/css";
                        s.rel = "stylesheet";
                        s.href = u;
                    }
                    else {
                        console.warn("unknown require-type " + u)
                        run(urls, i + 1, cont);
                        return;
                    }

                    s.addEventListener("load", (_e) => {
                        aardvark.loadResults[u] = function (c) { c(true); };
                        console.log("loaded " + u);
                        for (const c of conts) {
                            c(true);
                        }
                    });
                    s.addEventListener("error", (_e) => {
                        aardvark.loadResults[u] = function (c) { c(false); };
                        console.warn("could not load " + u);
                        for (const c of conts) {
                            c(false);
                        }
                    });
                    aardvark.loadResults[u] = function (c) { conts.push(c); };
                    document.head.appendChild(s);
                }
            }
        };

        run(urls, 0, cont)
    };

    const hasTouchList = window.TouchList !== undefined;
    
    aardvark.stringify = (e) => {
        if (typeof e === "object") {
            if (e instanceof Event) {
                const obj = {};
                for (let k in e) {
                    obj[k] = e[k];
                }
                e = obj;
            }


            
            return JSON.stringify(e, (k, v) => {
                if (v instanceof Node) {
                    
                    return {
                        id: v.id,
                        type: v.type,
                        value: v.value,
                        checked: v.checked
                    };
                }
                if (hasTouchList && v instanceof window.TouchList) {
                    let res = [];
                    for (let i = 0; i < v.length; i++) {
                        res.push(v.item(i));
                    }
                    return res;
                }
                if (v instanceof Window) return 'Window';
                if (v instanceof Event || (hasTouchList && v instanceof window.Touch)) {
                    const obj = {};
                    for (let k in v) {
                        obj[k] = v[k];
                    }
                    return obj;
                }
                return v;
            }, ' ');
        }
        else return JSON.stringify(e);
    };
    
    aardvark.relativePath = (protocol, path) => {
        let qi = path.indexOf('?');
        let search = new URLSearchParams();
        if(qi >= 0) {
            search = new URLSearchParams(path.substring(qi));
            path = path.substring(0, qi);
        }
        let p = document.location.pathname.split('/');
        p = p.slice(0, p.length - 1);

        let parts = path.split('/');
        if(parts.length > 0 && parts[0] === '') parts = parts.slice(1);
        for (let i = 0; i < parts.length; i++) {
            if (parts[i] === '..') {
                if(p.length > 1 && p[p.length-1] !== '..') p.pop();
                else p.push('..');
            }
            else if (parts[i] !== '.') p.push(parts[i]);
        }

        let currentQuery = new URLSearchParams(document.location.search);
        for (const element of search.entries()) {
            currentQuery.set(element[0], element[1]);
        }
        path = p.join('/');
        if(document.location.protocol === 'https:') protocol = protocol + "s";

        let q = ""
        if(!currentQuery.entries().next().done) q = "?" + currentQuery.toString()

        return protocol + "://" + document.location.host + path + q;
    };


    let buffer = [];
    aardvark.send = function (str) {
        buffer.push(str);
    };

    aardvark.connect = function (connect, arg) {
        connect((sender) => {
            aardvark.send = sender;
            for (let i = 0; i < buffer.length; i++) {
                sender(buffer[i]);
            }
            buffer = null;
        }, arg);
    };

    aardvark.trigger = function (srcNode, srcId, typ, evt) {
        evt.clientRect = srcNode.getBoundingClientRect();
        aardvark.send(aardvark.stringify({ source : srcId, type : typ, data : evt }));
    };

    aardvark.subscribe = function(node, type, action, capture) {
        const listener = { handleEvent: action };
        node.addEventListener(type, listener, capture);
        const suffix = capture ? "_capture" : "_bubble";
        return { 
            destroy: function() { 
                node.removeEventListener(type, listener, capture);
                delete node["evt_" + type + suffix]; 
            } 
        };
    }

    aardvark.callbackWithMinDelay = function (minDt, action, add) {
        let sum = null;
        let lastRun = -minDt;
        let running = true;

        function run() {
            if (!running) return;

            if (sum) {
                const now = performance.now();
                const dt = now - lastRun;
                if (dt >= minDt) {
                    action(sum);
                    lastRun = now;
                    sum = null;
                }
            }

            window.requestAnimationFrame(run);
        }

        run()

        function trigger(arg) {

            const now = performance.now();
            const dt = now - lastRun;
            if (dt >= minDt) {
                if (sum && add) {
                    arg = add(sum, arg);
                    sum = null;
                }
                action(arg);
                lastRun = now;
            }
            else {
                if (add) {
                    if (sum) { sum = add(sum, arg); }
                    else { sum = arg; }
                }
                else { sum = arg; }


            }
        }

        function destroy() {
            sum = null;
            running = false;
        }

        return { invoke: trigger, destroy: destroy };
    }

    

    aardvark.setListener = function (node, type, action, capture) {
        let realAction = action;
        let destroyCallback = () => { };
        //if (type == "pointermove" || type == "mousemove") {
        //    let o = aardvark.callbackWithMinDelay(16.6666, action, (a, b) => { a.movementX += b.movementX; a.movementY += b.movementY; return a });
        //    realAction = function (e) {
        //        const p = e.composedPath();
        //        e.stopImmediatePropagation();
        //        e.composedPath = () => p;
        //        o.invoke(e);
        //    };
        //    destroyCallback = o.destroy;
        //}


        const suffix = capture ? "_capture" : "_bubble";
        const fieldName = "evt_" + type + suffix;

        if (node[fieldName]) {
            node[fieldName].destroy();
            delete node[fieldName];
        }

        const listener = { handleEvent: realAction };
        node.addEventListener(type, listener, capture);
        node[fieldName] =
        {
            destroy: function () {
                destroyCallback();
                node.removeEventListener(type, listener, capture);
            }
        };
    };

    aardvark.getDataAttributeDict = function (node) {
        let dict = {}
        node.getAttributeNames().filter((n) => n.startsWith("data-")).forEach((n) => {
            dict[n.substring(5)] = node.getAttribute(n);
        });
        return dict;
    }


    aardvark.removeListener = function (node, type, capture) {
        const suffix = capture ? "_capture" : "_bubble";
        const fieldName = "evt_" + type + suffix;
        if (node[fieldName]) {
            node[fieldName].destroy();
            delete node[fieldName];
        }
        aardvark.setListenerFlags(node, type, false, false);
    };



    aardvark.onReady = function(action) {
        if(document.readyState === "complete") {
            action();
        }
        else {
            document.addEventListener("readystatechange", () => {
                if(document.readyState === "complete") {
                    action();
                }
            });
        }
    }


    aardvark.connectWebSocket = function (cont, path) {


        aardvark.newSocket = function (name) {
            return new WebSocket(aardvark.relativePath("ws", name));
        };

        aardvark.logCode = false;
        
        aardvark.onReady(() => {

            function receive(data) {
                if (!data) return;
                try {
                    const msg = JSON.parse(data);
                    switch (msg.command) {
                        case "execute":
                            if(aardvark.logCode) console.debug(msg.code);
                            try { new Function(msg.code)(); }
                            catch (e) { console.error("bad code", msg.code, e); }
                            break;
                        default:
                            console.log(msg);
                            break;
                    }
                }
                catch (e) {
                    console.error("bad message", data);
                }
            }

            function runSocket(path, receive) {
                let messageBuffer = [];
                let socket = new WebSocket(path);
                let connected = false;

                let result =
                {
                    socket: null,
                    send: (msg) => { messageBuffer.push(msg); }
                }

                socket.onopen = () => {
                    connected = true;
                    while (document.body.firstChild) document.body.firstChild.remove();

                    try {
                        while (messageBuffer.length > 0) {
                            const m = messageBuffer[0];
                            socket.send(m);
                            messageBuffer.splice(0, 1);
                        }
                    }
                    catch (e) { }
                };
                socket.onmessage = (event) => {
                    if (event.data) {
                        if (event.data !== "!pong") receive(event.data);
                    }
                };
                socket.onerror = () => { };
                socket.onclose = () => {
                    connected = false;
                    console.log("reconnect");
                    setTimeout(() => {
                        const res = runSocket(path, receive);
                        result.socket = res.socket;
                        result.send = res.send;
                    }, 500);
                };

                function ping() {
                    if (connected) {
                        try {
                            socket.send("!ping");
                            setTimeout(ping, 1000);
                        }
                        catch (e) {
                            connected = false;
                        }
                    }
                }
                ping();

                function send(msg) {
                    if (connected) socket.send(msg);
                    else messageBuffer.push(msg);
                }
                result.send = send;
                result.socket = socket;

                return result;
            }


            let ws = runSocket(path, receive);
            cont((msg) => { ws.send(msg); });
        });
    }

    const downHandlerSym = Symbol("down");
    const upHandlerSym = Symbol("up");
    
    function installGlobalPointerEvents(element) {
        let downHandler =
            {
                handleEvent: function (e) {
                    if(aardvark.globalPointerDown) {
                        aardvark.globalPointerDown(e, false);
                    }
                }
            }
        let upHandler =
            {
                handleEvent: function (e) {
                    if(aardvark.globalPointerUp) {
                        aardvark.globalPointerUp(e, false);
                    }
                }
            }
            
        element.addEventListener("pointerdown", downHandler, true);
        element.addEventListener("pointerup", upHandler, true);
        
        element[downHandlerSym] = downHandler;
        element[upHandlerSym] = upHandler;
    }
    
    function releaseGlobalPointerEvents(element) {
        let downHandler = element[downHandlerSym];
        let upHandler = element[upHandlerSym];
        
        if(downHandler) {
            element.removeEventListener("pointerdown", downHandler, true);
            delete element[downHandlerSym];
        }
        
        if(upHandler) {
            element.removeEventListener("pointerup", upHandler, true);
            delete element[upHandlerSym];
        }
    }
    
    aardvark.setPointerCapture = function(element, pointerId, capture) {
        if(capture) {
            element.setPointerCapture(pointerId);
            installGlobalPointerEvents(element);
        }
        else {
            element.releasePointerCapture(pointerId);
            releaseGlobalPointerEvents(element);
        }
    };
    
    aardvark.onReady(function () {
        let down = new Map();
        
        aardvark.globalPointerDown = function(e, fromWindow) {
            down.set(e.pointerId, e);
        };
        
        aardvark.globalPointerUp = function(e, fromWindow) {
            let downEvt = down.get(e.pointerId);
            if (downEvt) {
                down.delete(e.pointerId);
                if (true) {
                    let dt = e.timeStamp - downEvt.timeStamp;
                    let dx = e.clientX - downEvt.clientX;
                    let dy = e.clientY - downEvt.clientY;
                    let dp = Math.sqrt(dx * dx + dy * dy);
                    if (dt <= 400 && dp <= 20) {

                        const obj = {};
                        for (let k in downEvt) {
                            obj[k] = downEvt[k];
                        }
                        obj.bubbles = true;

                        let tapEvt = new PointerEvent("tap", obj);
                        tapEvt.movementX = dx;
                        tapEvt.movementY = dy;
                        tapEvt.deltaTime = dt;
                        tapEvt.bubbles = true;
                        e.target.dispatchEvent(tapEvt);
                    }
                }
            }
        };
        
        window.addEventListener("pointerdown", (e) => {
            aardvark.globalPointerDown(e, true);
        }, true);
        
        window.addEventListener("pointerup", (e) => {
            aardvark.globalPointerUp(e, true);
        }, true);

        let lastTap = null;
        window.addEventListener("tap", (e) => {
            if (lastTap) {
                let dt = e.timeStamp - lastTap.timeStamp;
                let dx = e.clientX - lastTap.clientX;
                let dy = e.clientY - lastTap.clientY;
                let dp = Math.sqrt(dx * dx + dy * dy);

                if (dt < 600 && dp < 30) {

                    const obj = {};
                    for (let k in lastTap) {
                        obj[k] = lastTap[k];
                    }
                    obj.bubbles = true;

                    let tapEvt = new PointerEvent("dbltap", obj);
                    tapEvt.movementX = dx;
                    tapEvt.movementY = dy;
                    tapEvt.deltaTime = dt;
                    tapEvt.bubbles = true;
                    e.target.dispatchEvent(tapEvt);
                    lastTap = null;
                }
                else {
                    lastTap = e;
                }
                
            }
            else {
                lastTap = e;
            }
        });

        let pressTimeouts = new Map();
        function triggerLongPress(e) {

            const obj = {};
            for (let k in e) {
                obj[k] = e[k];
            }
            obj.bubbles = true;

            let pressEvt = new PointerEvent("longpress", obj);
            e.target.dispatchEvent(pressEvt);
        }


        
        window.addEventListener("pointerdown", (e) => {
            let timeout = setTimeout(() => triggerLongPress(e), 500);
            pressTimeouts.set(e.pointerId, [e, timeout]);
        }, true);

        window.addEventListener("pointerup", (e) => {
            let t = pressTimeouts.get(e.pointerId);
            if (t) {
                clearTimeout(t[1]);
                pressTimeouts.delete(e.pointerId);
            }
        }, true);
        
        window.addEventListener("pointermove", (e) => {
            let t = pressTimeouts.get(e.pointerId);
            if (t) {
                let downEvt = t[0];
                let dx = e.clientX - downEvt.clientX;
                let dy = e.clientY - downEvt.clientY;
                let dp = Math.sqrt(dx * dx + dy * dy);
                if (dp > 10) {
                    clearTimeout(t[1]);
                    pressTimeouts.delete(e.pointerId);
                }
            }
        }, true);


    })

}) ();


// GAMEPAD
(function(window){'use strict';class GamepadEventLibrary{constructor(){this.controllers=new Map();this.nextControllerId=0;this.realGamepads=new Map();this.previousStates=new Map();this.pollingActive=false}registerController(stableId,isReal=false){if(!stableId){stableId=`gamepad-${this.nextControllerId++}`}if(this.controllers.has(stableId)){return stableId}this.controllers.set(stableId,{buttons:[16].fill(false),axes:[,,,].fill(0),triggers:new Map(),connected:true,isReal:isReal});return stableId}startRealGamepadMonitoring(){if(this.pollingActive){return}this.pollingActive=true;window.addEventListener('gamepadconnected',(e)=>{if(e.detail&&e.detail.controllerId){return}const gamepad=e.gamepad;if(!gamepad){return}const controllerId=`real-gamepad-${gamepad.index }`;this.registerController(controllerId,true);this.realGamepads.set(gamepad.index,controllerId);console.log(`[GamepadEvents] Real gamepad connected: ${gamepad.id } (${ controllerId })`)});window.addEventListener('gamepaddisconnected',(e)=>{if(e.detail&&e.detail.controllerId){return}const gamepad=e.gamepad;if(!gamepad){return}const controllerId=this.realGamepads.get(gamepad.index);if(controllerId){this.disconnectController(controllerId);this.realGamepads.delete(gamepad.index);this.previousStates.delete(gamepad.index);console.log(`[GamepadEvents] Real gamepad disconnected: ${ controllerId }`)}});this._pollRealGamepads()}_pollRealGamepads(){if(!this.pollingActive){return}const gamepads=navigator.getGamepads?navigator.getGamepads():[];for(let i=0;i<gamepads.length;i+=1){const gamepad=gamepads[i];if(!gamepad){continue}const controllerId=this.realGamepads.get(gamepad.index);if(!controllerId){const newId=`real-gamepad-${gamepad.index }`;this.registerController(newId,true);this.realGamepads.set(gamepad.index,newId)}const currentControllerId=this.realGamepads.get(gamepad.index);const previousState=this.previousStates.get(gamepad.index)||{buttons:[],axes:[]};for(let j=0;j<gamepad.buttons.length;j+=1){const button=gamepad.buttons[j];const pressed=button.pressed;const previousPressed=previousState.buttons[j]?previousState.buttons[j].pressed:false;if(pressed!==previousPressed){this.dispatchButtonEvent(currentControllerId,j,pressed)}}const deadzone=0.12;const changeThreshold=0.01;for(let j=0;j<gamepad.axes.length;j+=1){let rawValue=gamepad.axes[j];let value=Math.abs(rawValue)<deadzone?0:rawValue;const previousValue=previousState.axes[j];if(previousValue===undefined||Math.abs(value-previousValue)>changeThreshold){this.dispatchAxisEvent(currentControllerId,j,value)}}this.previousStates.set(gamepad.index,{buttons:gamepad.buttons.map(b=>({pressed:b.pressed,value:b.value})),axes:gamepad.axes.map((v,i)=>Math.abs(v)<deadzone?0:v)})}requestAnimationFrame(()=>this._pollRealGamepads())}stopRealGamepadMonitoring(){this.pollingActive=false}disconnectController(controllerId){if(!this.controllers.has(controllerId)){throw new Error(`Controller ${ controllerId } not registered`)}const controller=this.controllers.get(controllerId);controller.connected=false;controller.buttons.fill(false);controller.axes.fill(0);controller.triggers.clear()}reconnectController(controllerId){if(!this.controllers.has(controllerId)){throw new Error(`Controller ${ controllerId } not registered. Use registerController() first.`)}this.controllers.get(controllerId).connected=true}unregisterController(controllerId){this.controllers.delete(controllerId)}isConnected(controllerId){return this.controllers.has(controllerId)&&this.controllers.get(controllerId).connected}_getEventTarget(){return document.activeElement&&document.activeElement!==document.body?document.activeElement:document.body}dispatchButtonEvent(controllerId,buttonIndex,pressed,target=null,options={}){if(!this.controllers.has(controllerId)){throw new Error(`Controller ${ controllerId } not registered`)}if(!target){target=this._getEventTarget()}const controller=this.controllers.get(controllerId);const previousState=controller.buttons[buttonIndex];controller.buttons[buttonIndex]=pressed;const eventType=pressed?'gamepadbuttondown':'gamepadbuttonup';const event=new CustomEvent(eventType,{bubbles:options.bubbles!==false,cancelable:options.cancelable!==false,composed:options.composed!==false,detail:{controllerId,buttonIndex,pressed,previousState,timestamp:performance.now(),...options.detail}});target.dispatchEvent(event);return event}dispatchAxisEvent(controllerId,axisIndex,value,target=null,options={}){if(!this.controllers.has(controllerId)){throw new Error(`Controller ${ controllerId } not registered`)}if(!target){target=this._getEventTarget()}const controller=this.controllers.get(controllerId);const previousValue=controller.axes[axisIndex];controller.axes[axisIndex]=value;const event=new CustomEvent('gamepadaxischange',{bubbles:options.bubbles!==false,cancelable:options.cancelable!==false,composed:options.composed!==false,detail:{controllerId,axisIndex,value,previousValue,timestamp:performance.now(),...options.detail}});target.dispatchEvent(event);return event}dispatchTriggerValueChange(controllerId,triggerIndex,value,target=null,options={}){if(!this.controllers.has(controllerId)){throw new Error(`Controller ${ controllerId } not registered`)}if(!target){target=this._getEventTarget()}const controller=this.controllers.get(controllerId);const triggerKey=`trigger-${ triggerIndex }`;const previousState=controller.triggers.get(triggerKey)||{pressed:false,value:0};controller.triggers.set(triggerKey,{pressed:previousState.pressed,value});const event=new CustomEvent('gamepadtriggervaluechange',{bubbles:options.bubbles!==false,cancelable:options.cancelable!==false,composed:options.composed!==false,detail:{controllerId,triggerIndex,value,previousValue:previousState.value,pressed:previousState.pressed,timestamp:performance.now(),...options.detail}});target.dispatchEvent(event);return event}dispatchTriggerButtonEvent(controllerId,triggerIndex,pressed,value,target=null,options={}){if(!this.controllers.has(controllerId)){throw new Error(`Controller ${ controllerId } not registered`)}if(!target){target=this._getEventTarget()}const controller=this.controllers.get(controllerId);const triggerKey=`trigger-${ triggerIndex }`;const previousState=controller.triggers.get(triggerKey)||{pressed:false,value:0};controller.triggers.set(triggerKey,{pressed,value:value!==undefined?value:previousState.value});const eventType=pressed?'gamepadtriggerbuttondown':'gamepadtriggerbuttonup';const event=new CustomEvent(eventType,{bubbles:options.bubbles!==false,cancelable:options.cancelable!==false,composed:options.composed!==false,detail:{controllerId,triggerIndex,pressed,value:value!==undefined?value:previousState.value,previousPressed:previousState.pressed,timestamp:performance.now(),...options.detail}});target.dispatchEvent(event);return event}dispatchTriggerChange(controllerId,triggerIndex,value,pressThreshold=0.9,target=null,options={}){if(!target){target=this._getEventTarget()}const controller=this.controllers.get(controllerId);const triggerKey=`trigger-${ triggerIndex }`;const previousState=controller.triggers.get(triggerKey)||{pressed:false,value:0};const wasPressed=previousState.pressed;const isPressed=value>=pressThreshold;if(isPressed!==wasPressed){this.dispatchTriggerButtonEvent(controllerId,triggerIndex,isPressed,value,target,options)}this.dispatchTriggerValueChange(controllerId,triggerIndex,value,target,options)}getControllerState(controllerId){if(!this.controllers.has(controllerId)){throw new Error(`Controller ${ controllerId } not registered`)}const controller=this.controllers.get(controllerId);const triggers={};controller.triggers.forEach((state,key)=>{triggers[key]={...state}});return{controllerId,connected:controller.connected,buttons:[...controller.buttons],axes:[...controller.axes],triggers,isReal:controller.isReal}}getAllControllerIds(){return Array.from(this.controllers.keys())}createVirtualGamepad(options={}){const controllerId=options.controllerId||this.registerController();const container=document.createElement('div');container.style.cssText=`
        position: fixed; bottom: 0; left: 0; right: 0; width: 100%; height: 350px;
        z-index: 10000; touch-action: none; user-select: none; pointer-events: none;
      `;const createJoystick=(x,y,axes)=>{const stick=document.createElement('div');stick.style.cssText=`position: absolute; left: ${ x }; top: ${ y }; width: 140px; height: 140px; transform: translate(-50%, -50%); pointer-events: auto;`;const base=document.createElement('div');base.style.cssText=`width: 100%; height: 100%; background: radial-gradient(circle at 30% 30%, rgba(50,50,55,0.8), rgba(25,25,30,0.9));
          border: 4px solid rgba(60,60,65,0.8); border-radius: 50%; position: relative; box-shadow: inset 0 4px 15px rgba(0,0,0,0.4);`;const knob=document.createElement('div');knob.style.cssText=`width: 45%; height: 45%; background: radial-gradient(circle at 35% 35%, rgba(80,80,85,0.9), rgba(40,40,45,0.95));
          border: 3px solid rgba(70,70,75,0.9); border-radius: 50%; position: absolute; top: 50%; left: 50%;
          transform: translate(-50%, -50%); box-shadow: 0 3px 10px rgba(0,0,0,0.4);`;base.appendChild(knob);stick.appendChild(base);let activeTouch=null;const handleStart=(e)=>{if(e.type==='touchstart'&&activeTouch===null){const rect=stick.getBoundingClientRect();for(let touch of e.changedTouches){if(touch.clientX>=rect.left&&touch.clientX<=rect.right&&touch.clientY>=rect.top&&touch.clientY<=rect.bottom){activeTouch=touch.identifier;e.preventDefault();break}}}};const handleMove=(e)=>{if(e.type!=='touchmove'||activeTouch===null){return}let touch=null;for(let t of e.touches){if(t.identifier===activeTouch){touch=t;break}}if(!touch){return}const rect=base.getBoundingClientRect();let dx=touch.clientX-(rect.left+rect.width/2);let dy=touch.clientY-(rect.top+rect.height/2);const max=rect.width/2.5;const dist=Math.sqrt(dx*dx+dy*dy);if(dist>max){dx=(dx/dist)*max;dy=(dy/dist)*max}knob.style.transform=`translate(calc(-50% + ${ dx }px), calc(-50% + ${ dy }px))`;this.dispatchAxisEvent(controllerId,axes[0],Math.max(-1,Math.min(1,dx/max)));this.dispatchAxisEvent(controllerId,axes[1],Math.max(-1,Math.min(1,dy/max)));e.preventDefault()};const handleEnd=(e)=>{if(e.type==='touchend'||e.type==='touchcancel'){for(let t of e.changedTouches){if(t.identifier===activeTouch){activeTouch=null;knob.style.transform='translate(-50%, -50%)';this.dispatchAxisEvent(controllerId,axes[0],0);this.dispatchAxisEvent(controllerId,axes[1],0);break}}}};stick.addEventListener('touchstart',handleStart,{passive:false});document.addEventListener('touchmove',handleMove,{passive:false});document.addEventListener('touchend',handleEnd);document.addEventListener('touchcancel',handleEnd);return stick};const createButton=(x,y,size,btnIdx,label,color)=>{const btn=document.createElement('div');btn.style.cssText=`position: absolute; left: ${ x }; top: ${ y }; width: ${ size }; height: ${ size };
          background: ${color||'radial-gradient(circle at 30% 30%, rgba(60,60,65,0.8), rgba(35,35,40,0.9))'};
          border: 3px solid rgba(50,50,55,0.8); border-radius: 50%; display: flex; align-items: center; justify-content: center;
          font-size: 20px; font-weight: bold; color: white; transform: translate(-50%, -50%); cursor: pointer;
          box-shadow: 0 3px 8px rgba(0,0,0,0.3); text-shadow: 0 2px 4px rgba(0,0,0,0.5); pointer-events: auto;`;btn.textContent=label;const press=()=>{btn.style.transform='translate(-50%, -50%) scale(0.92)';this.dispatchButtonEvent(controllerId,btnIdx,true)};const release=()=>{btn.style.transform='translate(-50%, -50%) scale(1)';this.dispatchButtonEvent(controllerId,btnIdx,false)};btn.addEventListener('touchstart',(e)=>{press();e.preventDefault()});btn.addEventListener('mousedown',(e)=>{press();e.preventDefault()});btn.addEventListener('touchend',release);btn.addEventListener('mouseup',release);return btn};const createTrigger=(x,idx,label)=>{const trig=document.createElement('div');trig.style.cssText=`position: absolute; left: ${ x }; top: 15%; width: 120px; height: 50px;
          background: linear-gradient(180deg, rgba(40,40,45,0.8), rgba(30,30,35,0.9));
          border: 3px solid rgba(50,50,55,0.8); border-radius: 12px; transform: translateX(-50%);
          overflow: hidden; cursor: pointer; box-shadow: inset 0 3px 10px rgba(0,0,0,0.4); pointer-events: auto;`;const fill=document.createElement('div');fill.style.cssText=`width: 0%; height: 100%; background: linear-gradient(90deg, #4ade80, #22c55e);`;trig.appendChild(fill);const lbl=document.createElement('div');lbl.style.cssText=`position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
          color: white; font-size: 14px; font-weight: bold; pointer-events: none;`;lbl.textContent=label;trig.appendChild(lbl);const update=(v)=>{fill.style.width=`${v*100}%`;this.dispatchTriggerChange(controllerId,idx,v)};trig.addEventListener('mousedown',()=>update(1));trig.addEventListener('mouseup',()=>update(0));trig.addEventListener('touchstart',(e)=>{update(1);e.preventDefault()});trig.addEventListener('touchend',()=>update(0));return trig};const createBumper=(x,btn,label)=>{const bumper=document.createElement('div');bumper.style.cssText=`position: absolute; left: ${ x }; top: 25%; width: 140px; height: 35px;
          background: linear-gradient(180deg, rgba(55,55,60,0.85), rgba(40,40,45,0.9));
          border: 3px solid rgba(60,60,65,0.8); border-radius: 10px; transform: translateX(-50%);
          display: flex; align-items: center; justify-content: center; color: white; font-size: 14px;
          font-weight: bold; cursor: pointer; box-shadow: 0 3px 8px rgba(0,0,0,0.3); pointer-events: auto;`;bumper.textContent=label;const press=()=>{bumper.style.transform='translateX(-50%) scale(0.95)';this.dispatchButtonEvent(controllerId,btn,true)};const release=()=>{bumper.style.transform='translateX(-50%) scale(1)';this.dispatchButtonEvent(controllerId,btn,false)};bumper.addEventListener('touchstart',(e)=>{press();e.preventDefault()});bumper.addEventListener('mousedown',(e)=>{press();e.preventDefault()});bumper.addEventListener('touchend',release);bumper.addEventListener('mouseup',release);return bumper};container.appendChild(createJoystick('15%','75%',[0,1]));container.appendChild(createJoystick('85%','75%',[2,3]));document.body.appendChild(container);return{controllerId,element:container,remove:()=>container.remove()}}}const gamepadEvents=new GamepadEventLibrary();gamepadEvents.startRealGamepadMonitoring();window.gamepadEvents=gamepadEvents;window.createGamepad=()=>gamepadEvents.registerController();window.disconnectGamepad=(id)=>gamepadEvents.disconnectController(id);window.reconnectGamepad=(id)=>gamepadEvents.reconnectController(id);window.removeGamepad=(id)=>gamepadEvents.unregisterController(id);window.sendGamepadButton=(controllerId,buttonIndex,pressed)=>gamepadEvents.dispatchButtonEvent(controllerId,buttonIndex,pressed);window.sendGamepadAxis=(controllerId,axisIndex,value)=>gamepadEvents.dispatchAxisEvent(controllerId,axisIndex,value);window.sendGamepadTrigger=(controllerId,triggerIndex,value)=>gamepadEvents.dispatchTriggerChange(controllerId,triggerIndex,value);window.createVirtualGamepad=(options)=>gamepadEvents.createVirtualGamepad(options);if(typeof module!=='undefined'&&module.exports){module.exports={GamepadEventLibrary,gamepadEvents}}console.log('[GamepadEvents] Polyfill loaded with real gamepad support.')})(typeof window!=='undefined'?window:this);
