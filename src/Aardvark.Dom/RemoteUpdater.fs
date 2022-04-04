namespace Aardvark.Dom

open System.Web
open System.Text
open System.Text.Json
open System.Collections.Generic
open FSharp.Data.Adaptive

type UpdateState =
    {
        token : AdaptiveToken
        eventHandlers : Dictionary<int64, Dictionary<string, JsonElement -> unit>>
    }

type CodeBuilder() =
    let builder = System.Text.StringBuilder()
    let variables = System.Collections.Generic.Dictionary<int64, string>()

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


[<AbstractClass>]
type Updater(id : int64) =
    inherit AdaptiveObject()

    static let mutable currentId = 0L
    static let newId() = System.Threading.Interlocked.Increment(&currentId)

    member x.Id = id
    abstract PerformUpdate : state : UpdateState * code : CodeBuilder -> unit
    abstract PerformDelete : state : UpdateState * code : CodeBuilder -> unit

    abstract Boot : list<string>
    abstract Shutdown : list<string> 

    member x.Update(state : UpdateState, code : CodeBuilder) =
        x.EvaluateIfNeeded state.token () (fun token ->
            let state = { state with token = token }
            x.PerformUpdate(state, code)
        )

    member x.Delete(state : UpdateState, code : CodeBuilder) =
        x.PerformDelete(state, code)
        let var = code.GetOrCreateVar(id)
        code.AppendLine $"aardvark.delete({id}, {var});"

    static member Body(node : DomNode) =
        let id = 0
        match node with
        | DomNode.Text value -> 
            TextUpdater(id, value) :> Updater
        | DomNode.VoidElement(tag, attributes) ->
            VoidUpdater(id, tag, attributes) :> Updater
        | DomNode.Element(tag, attributes, children) ->
            ElementUpdater(id, tag, attributes, children) :> Updater


    static member internal New(node : DomNode, code : CodeBuilder) =
        let id = newId()
        let var = code.GetOrCreateVar(id, false)
        match node with
        | DomNode.Text value -> 
            code.AppendLine $"let {var} = aardvark.createTextNode({id});"
            var, TextUpdater(id, value) :> Updater
        | DomNode.VoidElement(tag, attributes) ->
            code.AppendLine $"let {var} = aardvark.createElement(\"{tag}\", {id});"
            var, VoidUpdater(id, tag, attributes) :> Updater
        | DomNode.Element(tag, attributes, children) ->
            code.AppendLine $"let {var} = aardvark.createElement(\"{tag}\", {id});"
            var, ElementUpdater(id, tag, attributes, children) :> Updater

and internal AttributeUpdater(targetId : int64, attributes : AttributeMap) =
    let reader = attributes.Content.GetReader()
    let events = System.Collections.Generic.HashSet<string>()
    let sets = System.Collections.Generic.HashSet<string>()

    let bootShutdown =
        lazy (
            let atts = AVal.force attributes.Content.Content
            let bootCode =
                match HashMap.tryFind "boot" atts with
                | Some (AttributeValue.Event(capture, bubble)) ->
                    capture.Code @ bubble.Code
                | _ ->
                    []
            let shutdownCode =
                match HashMap.tryFind "shutdown" atts with
                | Some (AttributeValue.Event(capture, bubble)) ->
                    capture.Code @ bubble.Code
                | _ ->
                    []
            bootCode, shutdownCode
        )

    member x.Boot =
        let (boot, _) = bootShutdown.Value
        boot

    member x.Shutdown =
        let (_, shutdown) = bootShutdown.Value
        shutdown

    member x.Update(state : UpdateState, code : CodeBuilder) =
        let ops = reader.GetChanges state.token
        let inline line (str : string) = code.AppendLine str

        let ops = 
            ops 
            |> HashMapDelta.toHashMap
            |> HashMap.remove "boot"
            |> HashMap.remove "shutdown"
            |> HashMapDelta.ofHashMap

        if not (HashMapDelta.isEmpty ops) then
            let var = code.GetOrCreateVar(targetId)
            for key, op in ops do
                match op with
                | Set value ->
                    match value with
                    | AttributeValue.String value ->
                        sets.Remove key |> ignore
                        events.Remove key |> ignore
                        line $"{var}.{key} = \"{HttpUtility.JavaScriptStringEncode(value)}\";"
                        
                    | AttributeValue.Set value ->
                        sets.Add key |> ignore
                        events.Remove key |> ignore
                        let array = value |> Seq.map (HttpUtility.JavaScriptStringEncode >> sprintf "\"%s\"") |> String.concat ", "
                        line $"{var}.{key} = [{array}];"

                    | AttributeValue.Event(capture, bubble) ->
                        sets.Remove key |> ignore
                        events.Add key |> ignore
                        let nodeEvents =
                            match state.eventHandlers.TryGetValue targetId with
                            | (true, d) -> d
                            | _ ->
                                let d = Dictionary()
                                state.eventHandlers.[targetId] <- d
                                d

                        let captureCode =
                            match capture.Callback with
                            | Some cb ->
                                let name = $"{key}_capture"
                                nodeEvents.[name] <- cb
                                $"aardvark.trigger({targetId}, \"{name}\", e);" ::
                                capture.Code |> List.map (fun l -> l.Replace("{0}", "e"))
                            | None ->
                                capture.Code |> List.map (fun l -> l.Replace("{0}", "e"))

                        let bubbleCode =
                            match bubble.Callback with
                            | Some cb ->
                                let name = $"{key}_bubble"
                                nodeEvents.[name] <- cb
                                $"aardvark.trigger({targetId}, \"{name}\", e);" ::
                                bubble.Code |> List.map (fun l -> l.Replace("{0}", "e"))
                            | None ->
                                bubble.Code |> List.map (fun l -> l.Replace("{0}", "e"))

                        let bubbleField = $"{var}.evt_{key}_bubble"
                        match bubbleCode with
                        | [] ->
                            line $"if({bubbleField}) {{ {bubbleField}.destroy(); }}"
                        | code ->
                            let action = String.concat ";" code
                            line $"if({bubbleField}) {{ {bubbleField}.destroy(); }}"
                            line $"{bubbleField} = aardvark.subscribe({var}, \"{key}\", (function (e) {{ {action} }}), false);"

                        let captureField = $"{var}.evt_{key}_capture"
                        match captureCode with
                        | [] ->
                            line $"if({captureField}) {{ {captureField}.destroy(); }}"
                        | code ->
                            let action = String.concat ";" code
                            line $"if({captureField}) {{ {captureField}.destroy(); }}"
                            line $"{captureField} = aardvark.subscribe({var}, \"{key}\", (function (e) {{ {action} }}), true);"


                | Remove ->
                    if events.Remove key then
                        let target = $"{var}.evt_{key}_bubble"
                        line $"if({target}) {{ {target}.destroy(); }}"
                        let target = $"{var}.evt_{key}_capture"
                        line $"if({target}) {{ {target}.destroy(); }}"
                        match state.eventHandlers.TryGetValue targetId with
                        | (true, d) ->
                            d.Remove key |> ignore
                            if d.Count = 0 then state.eventHandlers.Remove targetId |> ignore
                        | _ ->
                            ()
                    elif sets.Remove key then
                        line $"{var}.{key} = [];"
                    else
                        line $"{var}.{key} = null;"

    member x.Delete(state : UpdateState, code : CodeBuilder) =
        reader.Outputs.Consume(ref (Array.zeroCreate 4)) |> ignore
        state.eventHandlers.Remove targetId |> ignore
        
and internal VoidUpdater(id : int64, tag : string, attributes : AttributeMap) =
    inherit Updater(id)
    let att = AttributeUpdater(id, attributes)

    override x.Boot = att.Boot
    override x.Shutdown = att.Shutdown

    override x.PerformUpdate(state : UpdateState, code : CodeBuilder) =
        att.Update(state, code)

    override x.PerformDelete(state : UpdateState, code : CodeBuilder) =
        att.Delete(state, code)

and internal TextUpdater(id : int64, value : aval<string>) =
    inherit Updater(id)
    let mutable oldValue = None

    override x.Boot = []
    override x.Shutdown = []

    override x.PerformUpdate(state, code) =
        let v = value.GetValue state.token
        match oldValue with
        | Some o when o = v ->
            ()
        | _ ->
            oldValue <- Some v
            code.AppendLine $"aardvark.get({id}).textContent = \"{HttpUtility.JavaScriptStringEncode(v)}\";" |> ignore

    override x.PerformDelete(state : UpdateState, code : CodeBuilder) =
        value.Outputs.Remove x |> ignore

and internal ElementUpdater(id : int64, tag : string, attributes : AttributeMap, children : alist<DomNode>) =
    inherit Updater(id)

    let att = AttributeUpdater(id, attributes)
    let reader = children.GetReader()


    let mutable updaters = IndexList.empty<Updater>
    let dirty = ref (System.Collections.Generic.HashSet<Updater>())

    override x.Boot = att.Boot
    override x.Shutdown = att.Shutdown

    override x.InputChangedObject(_, o) =
        match o with
        | :? Updater as o -> lock dirty (fun () -> dirty.Value.Add o |> ignore)
        | _ -> ()

    override x.PerformUpdate(state, code) =
        let dirty =
            lock dirty (fun () ->
                let d = dirty.Value
                dirty.Value <- System.Collections.Generic.HashSet()
                d
            )
        att.Update(state, code)

        let boots = List<string * list<string>>()

        for index, op in reader.GetChanges state.token do
            match op with
            | Set node ->   
                let (l, s, r) = IndexList.neighbours index updaters
                match s with
                | Some o -> 
                    dirty.Remove o |> ignore
                    o.Delete(state, code)
                | None -> ()
                let (var, updater) = Updater.New(node, code)
                updaters <- IndexList.set index updater updaters
                dirty.Add updater |> ignore

                match updater.Boot with
                | [] -> ()
                | boot -> boots.Add(var, boot)

                match l with
                | Some (_, ln) -> code.AppendLine $"aardvark.insertAfter({ln.Id}, {var});" |> ignore
                | None -> 
                    match r with
                    | None -> code.AppendLine $"aardvark.appendChild({id}, {var});" |> ignore
                    | Some _ -> code.AppendLine $"aardvark.insertFirst({id}, {var});" |> ignore

            | Remove ->
                match IndexList.tryRemove index updaters with
                | Some (old, rest) ->
                    dirty.Remove old |> ignore
                    updaters <- rest
                    match old.Shutdown with
                    | [] -> ()
                    | shutdown ->
                        let var = code.GetOrCreateVar(old.Id)
                        let shutdownCode = shutdown |> List.map (fun str -> str.Replace("__THIS__", var) |> sprintf "{ %s }") |> String.concat "\n"
                        code.AppendLine shutdownCode |> ignore
                    old.Delete(state, code)
                | None ->
                    ()

        for d in dirty do d.Update(state, code)


        for (var, boot) in boots do
            let bootCode = boot |> List.map (fun str -> str.Replace("__THIS__", var) |> sprintf "{ %s }") |> String.concat "\n"
            code.AppendLine bootCode |> ignore

    override x.PerformDelete(state : UpdateState, code : CodeBuilder) =
        att.Delete(state, code)
        for u in updaters do u.Delete(state, code)