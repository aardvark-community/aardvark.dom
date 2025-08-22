namespace Aardvark.Dom.Remote

open Aardvark.Base
open Aardvark.Dom
open FSharp.Data.Adaptive
open System.Reflection
open System
open Aardvark.Rendering
open System.Threading.Tasks
open System.Web

module private EventParser =
    open Aardvark.Base.IL
    open Microsoft.FSharp.Reflection
    let private cache = System.Collections.Concurrent.ConcurrentDictionary<Type, System.Text.Json.JsonElement -> option<Event>>()

    let private cilSupported =  
        try
            let test : int -> int =
                cil { 
                    do! IL.ldarg 0
                    do! IL.ldconst 10
                    do! IL.add
                    do! IL.ret
                }
            test 1 = 11
        with _ ->
            false

    let getParser (typ : Type) =
        cache.GetOrAdd(typ, System.Func<_,_>(fun (typ : Type) ->
            let mTryParse = typ.GetMethod("TryParse", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public, [| typeof<System.Text.Json.JsonElement> |])
            if isNull mTryParse || not mTryParse.ReturnType.IsGenericType || mTryParse.ReturnType.GetGenericTypeDefinition() <> typedefof<option<_>> then
                Log.warn "%s lacks a proper `TryParse : JsonElement -> option<SelfType>` method and cannot be parsed" typ.FullName
                fun _ -> None
            else
                if cilSupported then
                    let cases = FSharpType.GetUnionCases(mTryParse.ReturnType, true)
                    let value = 
                        let cSome = cases |> Array.find (fun c -> c.Name = "Some")
                        cSome.GetFields().[0]

                    let mCreateRes = 
                        let t = typeof<option<Event>>
                        let c = FSharpType.GetUnionCases(t) |> Array.find (fun c -> c.Name = "Some")
                        FSharpValue.PreComputeUnionConstructorInfo c
                    let res = Local(mTryParse.ReturnType)
                    let lNull = Label()
                    cil {
                        do! IL.ldarg 0
                        do! IL.call mTryParse
                        do! IL.stloc res

                        do! IL.ldloc res
                        do! IL.jmp JumpCondition.False lNull

                        do! IL.ldloc res
                        do! IL.call value.GetMethod
                        do! IL.call mCreateRes
                        do! IL.ret

                        do! IL.mark lNull
                        do! IL.ldnull
                        do! IL.ret
                    }
                else
                    let cSome = FSharpType.GetUnionCases(mTryParse.ReturnType, true) |> Array.find (fun c -> c.Name = "Some")
                    let readSome = FSharpValue.PreComputeUnionReader cSome
                    fun (data : System.Text.Json.JsonElement) ->
                        let res = mTryParse.Invoke(null, [|data|])
                        if isNull res then 
                            None
                        else
                            let values = readSome res
                            Some (values.[0] :?> Event)
        ))
        
            //let m = evtType.GetMethod("TryParse", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            //if isNull m then failwithf "non-parsable event type: %A" evtType
            //let t = typedefof<option<_>>.MakeGenericType [| evtType |]
            //let pValue = t.GetProperty("Value")


type CodeBuilder(variables : System.Collections.Generic.Dictionary<int64, string>) =
    let builder = System.Text.StringBuilder()

    new() = CodeBuilder(System.Collections.Generic.Dictionary())

    member x.Variables = variables

    member x.AppendLine (code : string) =
        builder.AppendLine code |> ignore

    override x.ToString() =
        builder.ToString()

    member x.Clear() =
        builder.Clear() |> ignore
        variables.Clear()
    
    member x.GetOrCreateVar(id : int64, def : bool) =
        match variables.TryGetValue id with
        | (true, v) -> 
            v
        | _ ->
            if def then builder.AppendLine $"let n{id} = aardvark.get({id});" |> ignore
            let n = $"n{id}"
            variables.[id] <- n
            n

    
    member x.GetOrCreateVar(id : int64) =
        x.GetOrCreateVar(id, true)

type IServer =
    abstract RegisterWebSocket : (IChannel -> Task<unit>) -> string * System.IDisposable
    abstract RegisterResource : id : Guid * mime : string * content : byte[] -> string
    

type RemoteEventHandler = { Callback : option<System.Text.Json.JsonElement -> bool>; PointerCapture : bool; PreventDefault : bool } with
    member x.IsEmpty = Option.isNone x.Callback
    static member Empty = { Callback = None; PointerCapture = false; PreventDefault = false }

type RemoteEventCallbacks() =
    let mutable capture = RemoteEventHandler.Empty
    let mutable bubble = RemoteEventHandler.Empty

    member x.PointerCapture = capture.PointerCapture || bubble.PointerCapture
    member x.PreventDefault = capture.PreventDefault || bubble.PreventDefault

    member x.Capture
        with get() = capture
        and set v = capture <- v
          
    member x.Bubble
        with get() = bubble
        and set v = bubble <- v

    member x.Item
        with get(cap : bool) =
            if cap then capture
            else bubble
        and set (cap : bool) value =
            if cap then capture <- value
            else bubble <- value

    member x.IsEmpty =
        capture.IsEmpty && bubble.IsEmpty
        
    member x.IsNonEmpty =
        not capture.IsEmpty || not bubble.IsEmpty

                    
[<AbstractClass>]
type AbstractRemoteHtmlBackend(server : IServer, eventListeners : Dict<int64, Dict<string, RemoteEventCallbacks>>, currentId : ref<int>, parentIds : Dict<int64, int64>, childIds : Dict<int64, DictSet<int64>>, code : CodeBuilder) =
    static let aardvarkDom =
        let ass = typeof<AbstractRemoteHtmlBackend>.Assembly
        let resourceName = $"{ass.GetName().Name}.aardvark-dom.js"
        use s = ass.GetManifestResourceStream resourceName
        use r = new System.IO.StreamReader(s)
        r.ReadToEnd()
        
    static let specialAttributes =
        System.Collections.Generic.HashSet [
            "colspan"
            "rowspan"
        ]
        
    let mutable server = server
    let mutable eventListeners = eventListeners
    let mutable currentId = currentId
    let mutable parentIds = parentIds
    let mutable childIds = childIds
    let mutable code = code
    
    let newId() =
        System.Threading.Interlocked.Increment(&currentId.contents)

    let addChild(parent : int64) (child : int64) =
        parentIds.[child] <- parent
        let s = childIds.GetOrCreate(parent, fun _ -> DictSet())
        s.Add child |> ignore

    let rec destroyRefs (node : int64) =
        match childIds.TryRemove node with
        | (true, cs) -> cs |> Seq.iter destroyRefs
        | _ -> ()
        parentIds.Remove node |> ignore
        
    member x.Server
        with get() = server
        and private set s = server <- s 
        
    member x.EventListeners
        with get() = eventListeners
        and private set s = eventListeners <- s 

    member x.CurrentId
        with get() = currentId
        and private set s = currentId <- s 
    
    member x.ParentIds
        with get() = parentIds
        and private set s = parentIds <- s 
        
    member x.ChildIds
        with get() = childIds
        and private set s = childIds <- s 
        
    member x.Code
        with get() = code
        and private set s = code <- s 
        
    new(server : IServer) = AbstractRemoteHtmlBackend(server, Dict(), ref 0, Dict(), Dict(), CodeBuilder())

    abstract SetupRenderer : element : int64 * scene : SceneHandler -> unit
    abstract DestroyRenderer : element : int64 * scene : SceneHandler -> unit
    abstract Clone : unit -> AbstractRemoteHtmlBackend
    
    member x.Delay(action : IHtmlBackend<int64> -> unit) =
        let d = x.Clone()
        d.Server <- server
        d.EventListeners <- eventListeners
        d.CurrentId <- currentId
        d.ParentIds <- parentIds
        d.ChildIds <- childIds
        d.Code <- CodeBuilder(System.Collections.Generic.Dictionary code.Variables)
        action d
        d.GetCode() :> obj
    
    member x.Run(thing : obj) =
        code.AppendLine (thing :?> string)
    
    static member AardvarkDomJavascript = aardvarkDom
    
    interface IHtmlBackend<int64> with

        member x.Root = 0L

        member x.NewId() = newId()
        member x.Register(selector, id) =
            code.AppendLine $"aardvark.registerSelector({id}, \"{selector}\");"
        
        member x.Delay(action : IHtmlBackend<int64> -> unit) = x.Delay(action)
        member x.Run(thing : obj) = x.Run thing
            
        member x.SetupRenderer(element, scene) = x.SetupRenderer(element, scene)
        member x.DestroyRenderer(element, scene) = x.DestroyRenderer(element, scene)
        
        member x.Require(urls : Set<string>, action : IHtmlBackend<int64> -> unit) =
            if Set.isEmpty urls then
                action x
            else
                let str = urls |> Seq.map (sprintf "\"%s\"") |> String.concat ", "
                code.AppendLine $"aardvark.require([{str}], () => {{"
                action x
                code.AppendLine "});"

        member x.Execute(this : option<int64>, channels : array<IChannel -> Task<unit>>, js : array<string> -> list<string>) =
            //let bootCode arr = 
            //    match this with
            //    | Some this ->
            //        let var = code.GetOrCreateVar this
            //        js arr |> List.map (fun str -> str.Replace("__THIS__", var) |> sprintf "{ %s }") |> String.concat "\n" 
            //    | None ->
            //        js arr |> List.map (fun str -> str |> sprintf "{ %s }") |> String.concat "\n" 
                   
            let urls = 
                channels |> Array.map (fun action -> 
                    let e = Event<unit>()
                    let url, disp = 
                        server.RegisterWebSocket(fun channel ->
                        
                            //let channel =
                            //    { new IChannel with
                            //        member x.OnClose = e.Publish
                            //        member x.Send(msg) =
                            //            match msg with
                            //            | ChannelMessage.Binary a -> task { do! socket.Send a }
                            //            | ChannelMessage.Text a -> task { do! socket.Send a }
                            //            | ChannelMessage.Close -> 
                            //                e.Trigger()
                            //                task { do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "close", Unchecked.defaultof<_>) }
                            //        member x.Receive() =
                            //            task {
                            //                let! (typ, content) = socket.ReceiveMessage()
                            //                match typ with
                            //                | WebSocketMessageType.Binary -> return ChannelMessage.Binary content
                            //                | WebSocketMessageType.Text -> return ChannelMessage.Text (Encoding.UTF8.GetString content)
                            //                | WebSocketMessageType.Close -> 
                            //                    e.Trigger()
                            //                    return ChannelMessage.Close
                            //                | _ -> return ChannelMessage.Text "bad"
                            //            }
                            //    }
                            action channel
                        )

                    url, { new IDisposable with member x.Dispose() = disp.Dispose(); e.Trigger() }
                )
                
            let newName() = 
                "__channel" + string(System.Guid.NewGuid()).Replace("-", "")

            let names = urls |> Array.map (fun _ -> newName())

            let prefix = 
                (names, urls) ||> Array.map2 (fun n (u,_) -> 
                    $"const {n} = aardvark.newSocket(\"{u}\");"
                )   
                |> Array.toList

            let realCode = 
                let js = prefix @ js names
                match this with
                | Some this ->
                    let var = code.GetOrCreateVar this
                    js |> List.map (fun str -> str.Replace("__THIS__", var)) |> String.concat "\n" |> sprintf "{ %s }"
                | None ->
                    js |> String.concat "\n" |> sprintf "{ %s }"
                   
            code.AppendLine(realCode)
            { new System.IDisposable with
                member x.Dispose() =
                    printfn "dispose channel"
                    urls |> Array.iter (fun (_, d) -> d.Dispose())
            }

        member x.CreateElement(tag : string) =
            let id = newId()
            let var = code.GetOrCreateVar(id, false)
            code.AppendLine $"let {var} = aardvark.createElement(\"{tag}\", {id});"
            id

        member x.CreateTextElement() =
            let id = newId()
            let var = code.GetOrCreateVar(id, false)
            code.AppendLine $"let {var} = aardvark.createTextNode({id});"
            id

        member x.RemoveAttribute(node : int64, name : string) =
            let var = code.GetOrCreateVar node
            let attName =
                match name with
                | "classList" -> "class"
                | "className" -> "class"
                | n -> n.ToLower()
            code.AppendLine $"{var}.{name} = null; {var}.removeAttribute(\"{attName}\");"
            
        member x.SetAttribute(node : int64, name : string, value : string) =
            if name.Contains "-" || specialAttributes.Contains (name.ToLower()) then
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.setAttribute(\"{name}\", \"{HttpUtility.JavaScriptStringEncode(value)}\");"
            else
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.{name} = \"{HttpUtility.JavaScriptStringEncode(value)}\";"
             
        member x.SetAttribute(node : int64, name : string, value : bool) =
            let value = if value then "true" else "false"
            if name.Contains "-" then
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.setAttribute(\"{name}\", \"{value}\");"
            else
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.{name} = {value};"
            
        member x.SetAttribute(node : int64, name : string, value : int) =
            if name.Contains "-" then
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.setAttribute(\"{name}\", \"{value}\");"
            else
                let var = code.GetOrCreateVar node
                code.AppendLine $"{var}.{name} = {value};"
            
        member x.SetAttribute(node : int64, name : string, value : Set<string>) =
            let value = String.concat " " value
            let var = code.GetOrCreateVar node

            let attName =
                match name with
                | "classList" -> "className"
                | n -> n

            code.AppendLine $"{var}.{attName} = \"{HttpUtility.JavaScriptStringEncode(value)}\";"
            
        member x.InsertFirst(parent : int64, node : int64) =
            addChild parent node
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.insertFirst({parent}, {c});"
            
        member x.AppendChild(parent : int64, node : int64) =
            addChild parent node
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.appendChild({parent}, {c});"
            
        member x.InsertAfter(ref : int64, node : int64) =
            match parentIds.TryGetValue ref with
            | (true, p) -> addChild p node
            | _ -> ()
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.insertAfter({ref}, {c});"
            
        member x.InsertBefore(ref : int64, node : int64) =
            match parentIds.TryGetValue ref with
            | (true, p) -> addChild p node
            | _ -> ()
            let c = code.GetOrCreateVar node
            code.AppendLine $"aardvark.insertBefore({ref}, {c});"

        member x.Remove(node : int64) =
            destroyRefs node
            let var = code.GetOrCreateVar node
            code.AppendLine $"aardvark.delete({node}, {var});"
            eventListeners.Remove node |> ignore

        member x.SetListener(element, name, capture, preventDefault, pointerCapture, evtType, callback) =
            let listeners = eventListeners.GetOrCreate(element, fun _ -> Dict()).GetOrCreate(name, fun _ -> RemoteEventCallbacks())


            if listeners.IsEmpty then
                let var = code.GetOrCreateVar element
                let body =
                    String.concat "" [
                        "if(e.bubbles) {"
                        "  if(e.seenByAardvark) return;"
                        "  e.seenByAardvark = true;"
                        "}"
                        $"const flags = aardvark.getListenerFlags({var}, \"{name}\");"
                        //"if(e.bubbles) e.stopPropagation();"
                        "if(flags.preventDefault) { e.preventDefault(); }"
                            
                        match name.ToLower() with
                        | "pointerdown" -> $"if(flags.pointerCapture) {{ aardvark.setPointerCapture({var}, e.pointerId, true); }}"
                        | "pointerup" -> $"if(flags.pointerCapture) {{ aardvark.setPointerCapture({var}, e.pointerId, false); }}"
                        | _ -> ()
                        $"aardvark.trigger({var}, {element}, \"{name}\", e);"
                    ]

                code.AppendLine $"aardvark.setListener({var}, \"{name}\", ((e) => {{ {body} }}), false);"

            let oldPointerCapture = listeners.PointerCapture
            let oldPreventDefault = listeners.PreventDefault

            let tryParse = EventParser.getParser evtType
            let callback json =
                match tryParse json with
                | Some e -> callback e
                | None -> true

            listeners.[capture] <- { Callback = Some callback; PreventDefault = preventDefault; PointerCapture = pointerCapture }

            if listeners.PreventDefault <> oldPreventDefault || listeners.PointerCapture <> oldPointerCapture then
                let var = code.GetOrCreateVar element
                let inline bstr a = if a then "true" else "false"
                code.AppendLine $"aardvark.setListenerFlags({var}, \"{name}\", {bstr listeners.PointerCapture}, {bstr listeners.PreventDefault});"


        member x.RemoveListener(element, name, capture) =   
            match eventListeners.TryGetValue element with
            | (true, elementListeners) ->
                match elementListeners.TryGetValue name with
                | (true, listeners) ->
                    
                    let oldPointerCapture = listeners.PointerCapture
                    let oldPreventDefault = listeners.PreventDefault

                    listeners.[capture] <- RemoteEventHandler.Empty
                    if listeners.IsEmpty then
                        let var = code.GetOrCreateVar element
                        code.AppendLine $"aardvark.removeListener({var}, \"{name}\", false);"
                        if elementListeners.Remove name && elementListeners.Count = 0 then
                            eventListeners.Remove element |> ignore
                    else
                        if listeners.PreventDefault <> oldPreventDefault || listeners.PointerCapture <> oldPointerCapture then
                            let var = code.GetOrCreateVar element
                            let inline bstr a = if a then "true" else "false"
                            code.AppendLine $"aardvark.setListenerFlags({var}, \"{name}\", {bstr listeners.PointerCapture}, {bstr listeners.PreventDefault});"
                        
                | _ ->
                    ()
            | _ ->
                ()

    
    member x.RunCallback(srcId : int64, name : string, data : System.Text.Json.JsonElement) =
        let bubbles = 
            match data.TryGetProperty "bubbles" with
            | (true, prop) ->
                try prop.GetBoolean()
                with _ -> false
            | _ ->
                false

        let rec getPath (acc : list<RemoteEventCallbacks>) (nodeId : int64) =
            let newAcc =
                match eventListeners.TryGetValue nodeId with
                | (true, l) ->
                    match l.TryGetValue name with
                    | (true, evt) -> evt :: acc
                    | _ -> acc
                | _ ->
                    acc

            match parentIds.TryGetValue nodeId with
            | (true, pid) -> getPath newAcc pid
            | _ -> newAcc
            
        let rec run (bubble : list<System.Text.Json.JsonElement -> bool>) (data : System.Text.Json.JsonElement) (p : list<RemoteEventCallbacks>) =
            match p with
            | [] -> 
                bubble |> List.forall (fun cb -> cb data)
            | h :: rest ->
                let cont = 
                    match h.Capture.Callback with
                    | Some cb -> cb data
                    | None -> true

                if cont then
                    let newBubble =
                        if bubbles then
                            match h.Bubble.Callback with
                            | Some b -> b :: bubble
                            | None -> bubble
                        else
                            match h.Bubble.Callback with
                            | Some b -> [b]
                            | None -> []

                    run newBubble data rest
                else
                    false

        getPath [] srcId
        |> run [] data 
        |> ignore

    member x.GetCode() =
        try code.ToString()
        finally code.Clear()


