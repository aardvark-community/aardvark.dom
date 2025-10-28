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
(function(window) {
  'use strict';

  class GamepadEventLibrary {
    constructor() {
      this.controllers = new Map();
      this.nextControllerId = 0;
      this.realGamepads = new Map();
      this.previousStates = new Map();
      this.pollingActive = false;
      this.globalTargetOverride = null;
    }

    setGlobalTarget(target) {
      this.globalTargetOverride = target;
    }

    getGlobalTarget() {
      return this.globalTargetOverride;
    }

    clearGlobalTarget() {
      this.globalTargetOverride = null;
    }

    registerController(stableId, isReal) {
      isReal = isReal || false;
      if (!stableId) {
        stableId = 'gamepad-' + this.nextControllerId++;
      }
      if (this.controllers.has(stableId)) {
        return stableId;
      }
      this.controllers.set(stableId, {
        buttons: new Array(16).fill(false),
        axes: new Array(4).fill(0),
        triggers: new Map(),
        connected: true,
        isReal: isReal
      });
      return stableId;
    }

    startRealGamepadMonitoring() {
      if (this.pollingActive) return;
      this.pollingActive = true;

      window.addEventListener('gamepadconnected', (e) => {
        if (e.detail && e.detail.controllerId) return;
        var gamepad = e.gamepad;
        if (!gamepad) return;
        var controllerId = 'real-gamepad-' + gamepad.index;
        this.registerController(controllerId, true);
        this.realGamepads.set(gamepad.index, controllerId);
        console.log('[GamepadEvents] Real gamepad connected: ' + gamepad.id + ' (' + controllerId + ')');
      });

      window.addEventListener('gamepaddisconnected', (e) => {
        if (e.detail && e.detail.controllerId) return;
        var gamepad = e.gamepad;
        if (!gamepad) return;
        var controllerId = this.realGamepads.get(gamepad.index);
        if (controllerId) {
          this.disconnectController(controllerId);
          this.realGamepads.delete(gamepad.index);
          this.previousStates.delete(gamepad.index);
          console.log('[GamepadEvents] Real gamepad disconnected: ' + controllerId);
        }
      });

      this._pollRealGamepads();
    }

    _pollRealGamepads() {
      if (!this.pollingActive) return;
      var self = this;
      var gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
      
      for (var i = 0; i < gamepads.length; i++) {
        var gamepad = gamepads[i];
        if (!gamepad) continue;

        var controllerId = this.realGamepads.get(gamepad.index);
        if (!controllerId) {
          var newId = 'real-gamepad-' + gamepad.index;
          this.registerController(newId, true);
          this.realGamepads.set(gamepad.index, newId);
        }

        var currentControllerId = this.realGamepads.get(gamepad.index);
        var previousState = this.previousStates.get(gamepad.index) || {
          buttons: [],
          axes: []
        };

        for (var j = 0; j < gamepad.buttons.length; j++) {
          var button = gamepad.buttons[j];
          var pressed = button.pressed;
          var previousPressed = previousState.buttons[j] ? previousState.buttons[j].pressed : false;

          if (pressed !== previousPressed) {
            this.dispatchButtonEvent(currentControllerId, j, pressed);
          }
        }

        var deadzone = 0.12;
        var changeThreshold = 0.005;
        for (var j = 0; j < gamepad.axes.length; j++) {
          var rawValue = gamepad.axes[j];
          var value = Math.abs(rawValue) < deadzone ? 0 : rawValue;
          var previousValue = previousState.axes[j];
          
          if (previousValue === undefined || Math.abs(value - previousValue) > changeThreshold) {
            this.dispatchAxisEvent(currentControllerId, j, value);
          }
        }

        this.previousStates.set(gamepad.index, {
          buttons: gamepad.buttons.map(function(b) { return { pressed: b.pressed, value: b.value }; }),
          axes: gamepad.axes.map(function(v) { return Math.abs(v) < deadzone ? 0 : v; })
        });
      }

      requestAnimationFrame(function() { self._pollRealGamepads(); });
    }

    stopRealGamepadMonitoring() {
      this.pollingActive = false;
    }

    disconnectController(controllerId) {
      if (!this.controllers.has(controllerId)) {
        throw new Error('Controller ' + controllerId + ' not registered');
      }
      var controller = this.controllers.get(controllerId);
      controller.connected = false;
      controller.buttons.fill(false);
      controller.axes.fill(0);
      controller.triggers.clear();
    }

    reconnectController(controllerId) {
      if (!this.controllers.has(controllerId)) {
        throw new Error('Controller ' + controllerId + ' not registered. Use registerController() first.');
      }
      this.controllers.get(controllerId).connected = true;
    }

    unregisterController(controllerId) {
      this.controllers.delete(controllerId);
    }

    isConnected(controllerId) {
      return this.controllers.has(controllerId) && this.controllers.get(controllerId).connected;
    }

    _getEventTarget() {
      if (this.globalTargetOverride) {
        return this.globalTargetOverride;
      }
      return document.activeElement && document.activeElement !== document.body 
        ? document.activeElement 
        : document.body;
    }

    dispatchButtonEvent(controllerId, buttonIndex, pressed, target, options) {
      target = target || null;
      options = options || {};
      if (!this.controllers.has(controllerId)) {
        throw new Error('Controller ' + controllerId + ' not registered');
      }
      if (!target) {
        target = this._getEventTarget();
      }
      var controller = this.controllers.get(controllerId);
      var previousState = controller.buttons[buttonIndex];
      controller.buttons[buttonIndex] = pressed;
      var eventType = pressed ? 'gamepadbuttondown' : 'gamepadbuttonup';
      var event = new CustomEvent(eventType, {
        bubbles: options.bubbles !== false,
        cancelable: options.cancelable !== false,
        composed: options.composed !== false,
        detail: {
          controllerId: controllerId,
          buttonIndex: buttonIndex,
          pressed: pressed,
          previousState: previousState,
          timestamp: performance.now()
        }
      });
      target.dispatchEvent(event);
      return event;
    }

    dispatchAxisEvent(controllerId, axisIndex, value, target, options) {
      target = target || null;
      options = options || {};
      if (!this.controllers.has(controllerId)) {
        throw new Error('Controller ' + controllerId + ' not registered');
      }
      if (!target) {
        target = this._getEventTarget();
      }
      var controller = this.controllers.get(controllerId);
      var previousValue = controller.axes[axisIndex];
      controller.axes[axisIndex] = value;
      var event = new CustomEvent('gamepadaxischange', {
        bubbles: options.bubbles !== false,
        cancelable: options.cancelable !== false,
        composed: options.composed !== false,
        detail: {
          controllerId: controllerId,
          axisIndex: axisIndex,
          value: value,
          previousValue: previousValue,
          timestamp: performance.now()
        }
      });
      target.dispatchEvent(event);
      return event;
    }

    dispatchTriggerValueChange(controllerId, triggerIndex, value, target, options) {
      target = target || null;
      options = options || {};
      if (!this.controllers.has(controllerId)) {
        throw new Error('Controller ' + controllerId + ' not registered');
      }
      if (!target) {
        target = this._getEventTarget();
      }
      var controller = this.controllers.get(controllerId);
      var triggerKey = 'trigger-' + triggerIndex;
      var previousState = controller.triggers.get(triggerKey) || { pressed: false, value: 0 };
      controller.triggers.set(triggerKey, {
        pressed: previousState.pressed,
        value: value
      });
      var event = new CustomEvent('gamepadtriggervaluechange', {
        bubbles: options.bubbles !== false,
        cancelable: options.cancelable !== false,
        composed: options.composed !== false,
        detail: {
          controllerId: controllerId,
          triggerIndex: triggerIndex,
          value: value,
          previousValue: previousState.value,
          pressed: previousState.pressed,
          timestamp: performance.now()
        }
      });
      target.dispatchEvent(event);
      return event;
    }

    dispatchTriggerButtonEvent(controllerId, triggerIndex, pressed, value, target, options) {
      target = target || null;
      options = options || {};
      if (!this.controllers.has(controllerId)) {
        throw new Error('Controller ' + controllerId + ' not registered');
      }
      if (!target) {
        target = this._getEventTarget();
      }
      var controller = this.controllers.get(controllerId);
      var triggerKey = 'trigger-' + triggerIndex;
      var previousState = controller.triggers.get(triggerKey) || { pressed: false, value: 0 };
      controller.triggers.set(triggerKey, {
        pressed: pressed,
        value: value !== undefined ? value : previousState.value
      });
      var eventType = pressed ? 'gamepadtriggerbuttondown' : 'gamepadtriggerbuttonup';
      var event = new CustomEvent(eventType, {
        bubbles: options.bubbles !== false,
        cancelable: options.cancelable !== false,
        composed: options.composed !== false,
        detail: {
          controllerId: controllerId,
          triggerIndex: triggerIndex,
          pressed: pressed,
          value: value !== undefined ? value : previousState.value,
          previousPressed: previousState.pressed,
          timestamp: performance.now()
        }
      });
      target.dispatchEvent(event);
      return event;
    }

    dispatchTriggerChange(controllerId, triggerIndex, value, pressThreshold, target, options) {
      pressThreshold = pressThreshold || 0.9;
      target = target || null;
      options = options || {};
      if (!target) {
        target = this._getEventTarget();
      }
      var controller = this.controllers.get(controllerId);
      var triggerKey = 'trigger-' + triggerIndex;
      var previousState = controller.triggers.get(triggerKey) || { pressed: false, value: 0 };
      var wasPressed = previousState.pressed;
      var isPressed = value >= pressThreshold;

      if (isPressed !== wasPressed) {
        this.dispatchTriggerButtonEvent(controllerId, triggerIndex, isPressed, value, target, options);
      }
      this.dispatchTriggerValueChange(controllerId, triggerIndex, value, target, options);
    }

    getControllerState(controllerId) {
      if (!this.controllers.has(controllerId)) {
        throw new Error('Controller ' + controllerId + ' not registered');
      }
      var controller = this.controllers.get(controllerId);
      var triggers = {};
      controller.triggers.forEach(function(state, key) {
        triggers[key] = { pressed: state.pressed, value: state.value };
      });
      return {
        controllerId: controllerId,
        connected: controller.connected,
        buttons: controller.buttons.slice(),
        axes: controller.axes.slice(),
        triggers: triggers,
        isReal: controller.isReal
      };
    }

    getAllControllerIds() {
      return Array.from(this.controllers.keys());
    }

    createTouchOnlyGamepad(options) {
      options = options || {};
      if (!options.targetElement) {
        throw new Error('targetElement is required for createTouchOnlyGamepad');
      }
      
      var controllerId = options.controllerId || this.registerController();
      var targetElement = options.targetElement;
      
      var container = document.createElement('div');
      container.style.cssText = 'position: absolute; inset: 0; width: 100%; height: 100%; z-index: 999999; pointer-events: none; touch-action: none;';

      var leftStick = this._createInvisibleStick('left', [0, 1], controllerId, container);
      var rightStick = this._createInvisibleStick('right', [2, 3], controllerId, container);

      container.appendChild(leftStick.element);
      container.appendChild(rightStick.element);

      targetElement.appendChild(container);
      
      return { 
        controllerId: controllerId, 
        element: container, 
        remove: function() {
          leftStick.cleanup();
          rightStick.cleanup();
          container.remove();
        }
      };
    }

    _createInvisibleStick(side, axes, controllerId, container) {
      var self = this;
      var isLeft = side === 'left';
      var touchZone = document.createElement('div');
      touchZone.style.cssText = 'position: absolute; left: ' + (isLeft ? '0' : '50%') + '; top: 0; width: 50%; height: 100%; pointer-events: none;';

      var activeTouch = null;
      var stickVisual = null;

      var createStickVisual = function(x, y) {
        var stick = document.createElement('div');
        var zoneRect = touchZone.getBoundingClientRect();
        var relativeX = x - zoneRect.left;
        var relativeY = y - zoneRect.top;
        stick.style.cssText = 'position: absolute; left: ' + relativeX + 'px; top: ' + relativeY + 'px; width: 140px; height: 140px; transform: translate(-50%, -50%); pointer-events: none;';

        var base = document.createElement('div');
        base.style.cssText = 'width: 100%; height: 100%; background: radial-gradient(circle at 30% 30%, rgba(220,220,230,0.3), rgba(200,200,210,0.4)); border: 3px solid rgba(255,255,255,0.4); border-radius: 50%; position: relative; box-shadow: 0 2px 15px rgba(0,0,0,0.2);';

        var knob = document.createElement('div');
        knob.style.cssText = 'width: 40%; height: 40%; background: radial-gradient(circle at 35% 35%, rgba(240,240,250,0.6), rgba(220,220,230,0.7)); border: 2px solid rgba(255,255,255,0.6); border-radius: 50%; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); box-shadow: 0 2px 8px rgba(0,0,0,0.2);';

        base.appendChild(knob);
        stick.appendChild(base);
        touchZone.appendChild(stick);

        return { stick: stick, base: base, knob: knob };
      };

      var handleStart = function(e) {
        if (activeTouch !== null) return;

        for (var i = 0; i < e.changedTouches.length; i++) {
          var touch = e.changedTouches[i];
          
          var target = document.elementFromPoint(touch.clientX, touch.clientY);
          var isInteractive = target && (
            target.tagName === 'INPUT' ||
            target.tagName === 'BUTTON' ||
            target.tagName === 'A' ||
            target.tagName === 'TEXTAREA' ||
            target.tagName === 'SELECT' ||
            target.isContentEditable ||
            target.hasAttribute('onclick') ||
            (target.closest && target.closest('button, a, input, textarea, select, [onclick]'))
          );
          
          if (isInteractive) {
            continue;
          }
          
          var rect = touchZone.getBoundingClientRect();
          if (touch.clientX >= rect.left && touch.clientX <= rect.right &&
              touch.clientY >= rect.top && touch.clientY <= rect.bottom) {
            activeTouch = touch.identifier;
            stickVisual = createStickVisual(touch.clientX, touch.clientY);
            e.preventDefault();
            e.stopPropagation();
            break;
          }
        }
      };

      var handleMove = function(e) {
        if (activeTouch === null || !stickVisual) return;

        var touch = null;
        for (var i = 0; i < e.touches.length; i++) {
          if (e.touches[i].identifier === activeTouch) {
            touch = e.touches[i];
            break;
          }
        }
        if (!touch) return;

        var baseRect = stickVisual.base.getBoundingClientRect();
        var centerX = baseRect.left + baseRect.width / 2;
        var centerY = baseRect.top + baseRect.height / 2;

        var dx = touch.clientX - centerX;
        var dy = touch.clientY - centerY;

        var maxDist = baseRect.width / 2.5;
        var dist = Math.sqrt(dx * dx + dy * dy);

        if (dist > maxDist) {
          dx = (dx / dist) * maxDist;
          dy = (dy / dist) * maxDist;
        }

        stickVisual.knob.style.transform = 'translate(calc(-50% + ' + dx + 'px), calc(-50% + ' + dy + 'px))';

        var axisX = Math.max(-1, Math.min(1, dx / maxDist));
        var axisY = Math.max(-1, Math.min(1, dy / maxDist));

        self.dispatchAxisEvent(controllerId, axes[0], axisX);
        self.dispatchAxisEvent(controllerId, axes[1], axisY);

        e.preventDefault();
      };

      var handleEnd = function(e) {
        for (var i = 0; i < e.changedTouches.length; i++) {
          if (e.changedTouches[i].identifier === activeTouch) {
            activeTouch = null;

            if (stickVisual) {
              stickVisual.stick.remove();
              stickVisual = null;
            }

            self.dispatchAxisEvent(controllerId, axes[0], 0);
            self.dispatchAxisEvent(controllerId, axes[1], 0);
            break;
          }
        }
      };

      touchZone.addEventListener('touchstart', handleStart, { passive: false });
      document.addEventListener('touchmove', handleMove, { passive: false });
      document.addEventListener('touchend', handleEnd);
      document.addEventListener('touchcancel', handleEnd);

      return {
        element: touchZone,
        cleanup: function() {
          document.removeEventListener('touchstart', handleStart, { capture: true });
          document.removeEventListener('touchmove', handleMove);
          document.removeEventListener('touchend', handleEnd);
          document.removeEventListener('touchcancel', handleEnd);
        }
      };
    }

    createVirtualGamepad(options) {
      options = options || {};
      var controllerId = options.controllerId || this.registerController();
      var self = this;
      var container = document.createElement('div');
      container.style.cssText = 'position: fixed; bottom: 0; left: 0; right: 0; width: 100%; height: 350px; z-index: 10000; touch-action: none; user-select: none; pointer-events: none;';

      var createJoystick = function(x, y, axes) {
        var stick = document.createElement('div');
        stick.style.cssText = 'position: absolute; left: ' + x + '; top: ' + y + '; width: 140px; height: 140px; transform: translate(-50%, -50%); pointer-events: auto;';
        var base = document.createElement('div');
        base.style.cssText = 'width: 100%; height: 100%; background: radial-gradient(circle at 30% 30%, rgba(50,50,55,0.8), rgba(25,25,30,0.9)); border: 4px solid rgba(60,60,65,0.8); border-radius: 50%; position: relative; box-shadow: inset 0 4px 15px rgba(0,0,0,0.4);';
        var knob = document.createElement('div');
        knob.style.cssText = 'width: 45%; height: 45%; background: radial-gradient(circle at 35% 35%, rgba(80,80,85,0.9), rgba(40,40,45,0.95)); border: 3px solid rgba(70,70,75,0.9); border-radius: 50%; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); box-shadow: 0 3px 10px rgba(0,0,0,0.4);';
        base.appendChild(knob);
        stick.appendChild(base);
        
        var activeTouch = null;
        
        var handleStart = function(e) {
          if (e.type === 'touchstart' && activeTouch === null) {
            var rect = stick.getBoundingClientRect();
            for (var i = 0; i < e.changedTouches.length; i++) {
              var touch = e.changedTouches[i];
              if (touch.clientX >= rect.left && touch.clientX <= rect.right &&
                  touch.clientY >= rect.top && touch.clientY <= rect.bottom) {
                activeTouch = touch.identifier;
                e.preventDefault();
                break;
              }
            }
          }
        };
        
        var handleMove = function(e) {
          if (e.type !== 'touchmove' || activeTouch === null) return;
          var touch = null;
          for (var i = 0; i < e.touches.length; i++) {
            if (e.touches[i].identifier === activeTouch) {
              touch = e.touches[i];
              break;
            }
          }
          if (!touch) return;
          var rect = base.getBoundingClientRect();
          var dx = touch.clientX - (rect.left + rect.width/2);
          var dy = touch.clientY - (rect.top + rect.height/2);
          var max = rect.width / 2.5;
          var dist = Math.sqrt(dx*dx + dy*dy);
          if (dist > max) { dx = (dx/dist) * max; dy = (dy/dist) * max; }
          knob.style.transform = 'translate(calc(-50% + ' + dx + 'px), calc(-50% + ' + dy + 'px))';
          self.dispatchAxisEvent(controllerId, axes[0], Math.max(-1, Math.min(1, dx/max)));
          self.dispatchAxisEvent(controllerId, axes[1], Math.max(-1, Math.min(1, dy/max)));
          e.preventDefault();
        };
        
        var handleEnd = function(e) {
          if (e.type === 'touchend' || e.type === 'touchcancel') {
            for (var i = 0; i < e.changedTouches.length; i++) {
              if (e.changedTouches[i].identifier === activeTouch) {
                activeTouch = null;
                knob.style.transform = 'translate(-50%, -50%)';
                self.dispatchAxisEvent(controllerId, axes[0], 0);
                self.dispatchAxisEvent(controllerId, axes[1], 0);
                break;
              }
            }
          }
        };
        
        stick.addEventListener('touchstart', handleStart, { passive: false });
        document.addEventListener('touchmove', handleMove, { passive: false });
        document.addEventListener('touchend', handleEnd);
        document.addEventListener('touchcancel', handleEnd);
        return stick;
      };

      var createButton = function(x, y, size, btnIdx, label, color) {
        var btn = document.createElement('div');
        btn.style.cssText = 'position: absolute; left: ' + x + '; top: ' + y + '; width: ' + size + '; height: ' + size + '; background: ' + (color || 'radial-gradient(circle at 30% 30%, rgba(60,60,65,0.8), rgba(35,35,40,0.9))') + '; border: 3px solid rgba(50,50,55,0.8); border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 20px; font-weight: bold; color: white; transform: translate(-50%, -50%); cursor: pointer; box-shadow: 0 3px 8px rgba(0,0,0,0.3); text-shadow: 0 2px 4px rgba(0,0,0,0.5); pointer-events: auto;';
        btn.textContent = label;
        var press = function() { btn.style.transform = 'translate(-50%, -50%) scale(0.92)'; self.dispatchButtonEvent(controllerId, btnIdx, true); };
        var release = function() { btn.style.transform = 'translate(-50%, -50%) scale(1)'; self.dispatchButtonEvent(controllerId, btnIdx, false); };
        btn.addEventListener('touchstart', function(e) { press(); e.preventDefault(); });
        btn.addEventListener('mousedown', function(e) { press(); e.preventDefault(); });
        btn.addEventListener('touchend', release);
        btn.addEventListener('mouseup', release);
        return btn;
      };

      var createTrigger = function(x, idx, label) {
        var trig = document.createElement('div');
        trig.style.cssText = 'position: absolute; left: ' + x + '; top: 15%; width: 120px; height: 50px; background: linear-gradient(180deg, rgba(40,40,45,0.8), rgba(30,30,35,0.9)); border: 3px solid rgba(50,50,55,0.8); border-radius: 12px; transform: translateX(-50%); overflow: hidden; cursor: pointer; box-shadow: inset 0 3px 10px rgba(0,0,0,0.4); pointer-events: auto;';
        var fill = document.createElement('div');
        fill.style.cssText = 'width: 0%; height: 100%; background: linear-gradient(90deg, #4ade80, #22c55e);';
        trig.appendChild(fill);
        var lbl = document.createElement('div');
        lbl.style.cssText = 'position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); color: white; font-size: 14px; font-weight: bold; pointer-events: none;';
        lbl.textContent = label;
        trig.appendChild(lbl);
        var update = function(v) { fill.style.width = (v*100) + '%'; self.dispatchTriggerChange(controllerId, idx, v); };
        trig.addEventListener('mousedown', function() { update(1); });
        trig.addEventListener('mouseup', function() { update(0); });
        trig.addEventListener('touchstart', function(e) { update(1); e.preventDefault(); });
        trig.addEventListener('touchend', function() { update(0); });
        return trig;
      };

      var createBumper = function(x, btn, label) {
        var bumper = document.createElement('div');
        bumper.style.cssText = 'position: absolute; left: ' + x + '; top: 25%; width: 140px; height: 35px; background: linear-gradient(180deg, rgba(55,55,60,0.85), rgba(40,40,45,0.9)); border: 3px solid rgba(60,60,65,0.8); border-radius: 10px; transform: translateX(-50%); display: flex; align-items: center; justify-content: center; color: white; font-size: 14px; font-weight: bold; cursor: pointer; box-shadow: 0 3px 8px rgba(0,0,0,0.3); pointer-events: auto;';
        bumper.textContent = label;
        var press = function() { bumper.style.transform = 'translateX(-50%) scale(0.95)'; self.dispatchButtonEvent(controllerId, btn, true); };
        var release = function() { bumper.style.transform = 'translateX(-50%) scale(1)'; self.dispatchButtonEvent(controllerId, btn, false); };
        bumper.addEventListener('touchstart', function(e) { press(); e.preventDefault(); });
        bumper.addEventListener('mousedown', function(e) { press(); e.preventDefault(); });
        bumper.addEventListener('touchend', release);
        bumper.addEventListener('mouseup', release);
        return bumper;
      };

      container.appendChild(createJoystick('25%', '55%', [0, 1]));
      container.appendChild(createJoystick('60%', '75%', [2, 3]));
      
      var dpad = document.createElement('div');
      dpad.style.cssText = 'position: absolute; left: 25%; top: 75%; width: 110px; height: 110px; transform: translate(-50%, -50%);';
      var dpadButtons = [{x:'50%',y:'5%',l:'▲',i:12},{x:'50%',y:'95%',l:'▼',i:13},{x:'5%',y:'50%',l:'◄',i:14},{x:'95%',y:'50%',l:'►',i:15}];
      for (var i = 0; i < dpadButtons.length; i++) {
        dpad.appendChild(createButton(dpadButtons[i].x, dpadButtons[i].y, '35px', dpadButtons[i].i, dpadButtons[i].l));
      }
      container.appendChild(dpad);
      
      var faceButtons = document.createElement('div');
      faceButtons.style.cssText = 'position: absolute; left: 75%; top: 55%; width: 160px; height: 160px; transform: translate(-50%, -50%);';
      var colors = {A: '#5cb85c', B: '#d9534f', X: '#5bc0de', Y: '#f0ad4e'};
      var buttons = [{x:'50%',y:'85%',l:'A',i:0},{x:'85%',y:'50%',l:'B',i:1},{x:'15%',y:'50%',l:'X',i:2},{x:'50%',y:'15%',l:'Y',i:3}];
      for (var i = 0; i < buttons.length; i++) {
        faceButtons.appendChild(createButton(buttons[i].x, buttons[i].y, '52px', buttons[i].i, buttons[i].l, 'radial-gradient(circle at 30% 30%, ' + colors[buttons[i].l] + ', ' + colors[buttons[i].l] + 'aa)'));
      }
      container.appendChild(faceButtons);
      
      container.appendChild(createTrigger('25%', 0, 'LT'));
      container.appendChild(createTrigger('75%', 1, 'RT'));
      container.appendChild(createBumper('25%', 4, 'LB'));
      container.appendChild(createBumper('75%', 5, 'RB'));
      
      document.body.appendChild(container);
      return { controllerId: controllerId, element: container, remove: function() { container.remove(); } };
    }
  }

  var gamepadEvents = new GamepadEventLibrary();
  gamepadEvents.startRealGamepadMonitoring();

  window.gamepadEvents = gamepadEvents;

  window.createGamepad = function() { return gamepadEvents.registerController(); };
  window.disconnectGamepad = function(id) { return gamepadEvents.disconnectController(id); };
  window.reconnectGamepad = function(id) { return gamepadEvents.reconnectController(id); };
  window.removeGamepad = function(id) { return gamepadEvents.unregisterController(id); };
  
  window.sendGamepadButton = function(controllerId, buttonIndex, pressed) { 
    return gamepadEvents.dispatchButtonEvent(controllerId, buttonIndex, pressed);
  };
  window.sendGamepadAxis = function(controllerId, axisIndex, value) { 
    return gamepadEvents.dispatchAxisEvent(controllerId, axisIndex, value);
  };
  window.sendGamepadTrigger = function(controllerId, triggerIndex, value) { 
    return gamepadEvents.dispatchTriggerChange(controllerId, triggerIndex, value);
  };
  
  window.createVirtualGamepad = function(options) { return gamepadEvents.createVirtualGamepad(options); };
  window.createTouchOnlyGamepad = function(options) { return gamepadEvents.createTouchOnlyGamepad(options); };

  window.setGamepadTarget = function(target) { return gamepadEvents.setGlobalTarget(target); };
  window.clearGamepadTarget = function() { return gamepadEvents.clearGlobalTarget(); };

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = { GamepadEventLibrary: GamepadEventLibrary, gamepadEvents: gamepadEvents };
  }

  console.log('[GamepadEvents] Polyfill loaded with real gamepad support.');

})(typeof window !== 'undefined' ? window : this);
