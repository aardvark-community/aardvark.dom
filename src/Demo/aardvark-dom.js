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

    aardvark.register = (id, element) => {
        if(id === 0) return;
        table[id] = element;
    };
    
    aardvark.delete = (id, n) => {
        if(id === 0) return;
        delete table[id];
        if(n) n.remove();
    };
    
    aardvark.get = (id) => {
        if(id === 0) return document.body;
        else return table[id];
    };

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
    aardvark.appendChild = function(parentId, newNode) {
        let parent = aardvark.get(parentId);
        if(!parent) return;
        parent.appendChild(newNode);
    };
    
    aardvark.stringify = (e) => {
        const obj = {};
        for (let k in e) {
            obj[k] = e[k];
        }
        return JSON.stringify(obj, (k, v) => {
            if (v instanceof Node) return v.id;
            if (v instanceof Window) return 'Window';
            if (v instanceof Event) {
                const obj = {};
                for (let k in v) {
                    obj[k] = v[k];
                }
                return obj;
            }
            return v;
        }, ' ');
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
        let proto = protocol;
        if(document.location.protocol === 'https:') protocol = protocol + "s";

        let q = ""
        if(!currentQuery.entries().next().done) q = "?" + currentQuery.toString()

        return protocol + "://" + document.location.host + path + q;
    };

    aardvark.trigger = function(srcId, typ, evt) {
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

    function onReady(action) {
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

    onReady(() => {
        const path = aardvark.relativePath("ws", "/socket");
        
        function receive(data) {
            if(!data)return;
            try {
                const msg = JSON.parse(data);
                switch(msg.command) {
                    case "execute":
                        console.debug(msg.code);
                        try { new Function(msg.code)(); }
                        catch(e) { console.error("bad code", msg.code); }
                        break;
                    default:
                        console.log(msg);
                        break;
                }
            } 
            catch(e) {
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
                while(document.body.firstChild) document.body.firstChild.remove();
                
                try { 
                    while(messageBuffer.length > 0) {
                        const m = messageBuffer[0];
                        socket.send(m); 
                        messageBuffer.splice(0, 1);
                    }
                 }
                catch(e) {}
            };
            socket.onmessage = (event) => {
                if(event.data) {
                    if(event.data !== "!pong") receive(event.data);
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
                if(connected) {
                    try {
                        socket.send("!ping");
                        setTimeout(ping, 1000);
                    }
                    catch(e) {
                        connected = false;
                    }   
                }
            }
            ping();
            
            function send(msg) {
                if(connected) socket.send(msg);
                else messageBuffer.push(msg);
            }
            result.send = send;
            result.socket = socket;
            
            return result;
        }
        
        
        let ws = runSocket(path, receive);
        aardvark.send = (msg) => { ws.send(msg); };
    });
})();

