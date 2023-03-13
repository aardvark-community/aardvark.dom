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

                    if (path.endsWith(".js")) {
                        s = document.createElement("script");
                        s.type = "application/javascript";
                        s.src = u;
                    }
                    else if (path.endsWith(".css")) {
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

