namespace Aardvark.Dom

open System.Web
open System.Text
open System.Text.Json
open System.Collections.Generic
open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive

#nowarn "1337"


type RenderControlInfo =
    {
        Runtime                 : IRuntime
        FramebufferSignature    : aval<IFramebufferSignature>
        ViewportSize            : aval<V2i>
        Time                    : aval<System.DateTime>
    }
    
type DomScene =
    {
        View    : aval<Trafo3d>
        Proj    : aval<Trafo3d>
        Scene   : ISceneNode
        OnReady : IEventHandler -> unit
    }

type DomNode =
    | VoidElement of tag : string * attributes : AttributeMap
    | Text of content : aval<string>
    | Element of tag : string * attributes : AttributeMap * children : alist<DomNode>
    | RenderControl of (RenderControlInfo -> AttributeMap * amap<RenderControlEventKind, RenderControlEventInfo -> unit> * DomScene)

module NodeBuilderHelpers = 
    open FSharp.Data.Traceable

    type NodeListElement =
        | Constant of list<DomNode>
        | AList of alist<DomNode>
        | AValCustom of mapping : obj * value : IAdaptiveValue

    type IndexMapping(minExclusive : Index, maxExclusive : Index) =
        let mutable store = IndexList.empty<Index>

        member x.Invoke(k : Index) =
            let (left, self, right) = IndexList.neighbours k store
            match self with
            | Some i -> 
                i
            | None -> 
                let newIndex = 
                    match left, right with
                    | None, None -> Index.between minExclusive maxExclusive
                    | Some(_,l), None -> Index.between l maxExclusive
                    | None, Some(_,r) -> Index.between minExclusive r
                    | Some (_,l), Some(_,r) -> Index.between l r
                store <- IndexList.set k newIndex store
                newIndex

        member x.Revoke(k : Index) =
            match IndexList.tryRemove k store with
            | Some(i, rest) ->
                store <- rest
                Some i
            | None -> 
                None

        member x.Clear() =
            store <- IndexList.empty
           
    type NodeList private(content : IndexList<NodeListElement>) =
        static let empty = new NodeList(IndexList.empty<NodeListElement>)

        static member Empty = empty


        member x.ToAList() =
            let content = content

            let constant =
                content 
                |> IndexList.choose (function Constant c -> Some c | _ -> None)
                |> IndexList.mapi (fun idx values ->
                    let (_, _, r) = IndexList.neighbours idx content
                    let mutable res = IndexList.empty
                    let mutable i = idx
                    for v in values do
                        res <- IndexList.set i v res
                        match r with
                        | Some(r,_) -> i <- Index.between i r
                        | None -> i <- Index.after i

                    res
                )

            let avalCustom =
                content
                |> IndexList.choose (function AValCustom(m, v) -> Some(m,v) | _ -> None)
                |> IndexList.mapi (fun idx (mapping, value) ->
                    let (_, _, r) = IndexList.neighbours idx content
                    let maxIndex = 
                        match r with
                        | Some (ri, _) -> Some ri
                        | None -> None
                    mapping, value, maxIndex
                )
                |> IndexList.map(fun (m, v, i) -> ref None, m, v, i)
                
            let alist =
                content
                |> IndexList.choose (function AList v -> Some v | _ -> None)
                |> IndexList.mapi(fun i value ->
                    let (_, _, r) = IndexList.neighbours i content
                    let max =
                        match r with
                        | Some (r,_) -> r
                        | _ -> Index.after i
                    let mapping = IndexMapping(i, max)
                    mapping, value
                )

            if IndexList.isEmpty avalCustom && IndexList.isEmpty alist then
                let mutable res = IndexList.empty
                constant |> IndexList.iter (fun part ->
                    part |> IndexList.iteri (fun idx element ->
                        res <- IndexList.set idx element res
                    )
                )
                AList.ofIndexList res
            else
                AList.ofReader (fun () ->
                    let mutable initial = true
                    let readers = alist |> IndexList.map (fun (m, l) -> m, l.GetReader())


                    let indices =
                        content
                        |> IndexList.choosei (fun i o ->
                            match o with 
                            | AList o -> Some (snd readers.[i] :> IAdaptiveObject, i)
                            | AValCustom(_, o) -> Some (o :> IAdaptiveObject, i)
                            | Constant _ -> None
                        )
                        |> Seq.groupBy fst
                        |> Seq.map (fun (o, l) -> o, l |> Seq.map snd |> Seq.toList)
                        |> HashMap.ofSeq

                    let dirtyReader = ref <| IndexList.empty
                    let dirtyAValCustom = ref <| IndexList.empty
                    { new FSharp.Data.Traceable.AbstractReader<IndexListDelta<DomNode>>(IndexListDelta.empty) with

                        override x.InputChangedObject(_, o) =
                            match HashMap.tryFind o indices with
                            | Some idx ->
                                match o with
                                | :? FSharp.Data.Traceable.IOpReader<IndexList<DomNode>, IndexListDelta<DomNode>> as o ->
                                    lock dirtyReader (fun () -> 
                                        for i in idx do
                                            dirtyReader.Value <- IndexList.set i readers.[i] dirtyReader.Value
                                    )
                                | :? IAdaptiveValue as o ->
                                    lock dirtyAValCustom (fun () -> 
                                        for i in idx do
                                            dirtyAValCustom.Value <- IndexList.set i avalCustom.[i] dirtyAValCustom.Value
                                    )

                                | _ ->
                                    ()
                            | None ->
                                ()
            
                        member x.Compute (token : AdaptiveToken) =
                            let mutable delta = IndexList.empty

                            let avalCustom, readers =
                                if initial then 
                                    avalCustom, readers
                                else 
                                    let readers = lock dirtyReader (fun () -> let d = dirtyReader.Value in dirtyReader.Value <- IndexList.empty; d)
                                    let avalCustom = lock dirtyAValCustom (fun () -> let d = dirtyAValCustom.Value in dirtyAValCustom.Value <- IndexList.empty; d)
                                    avalCustom, readers

                            if initial then
                                initial <- false
                                constant |> IndexList.iter (fun part ->
                                    part |> IndexList.iteri (fun idx element ->
                                        delta <- IndexList.set idx (Set element) delta
                                    )
                                )


                            avalCustom |> IndexList.iteri (fun idx (cache, mapping, value, maxIndex) ->
                                let nodes = 
                                    value.Accept { 
                                        new IAdaptiveValueVisitor<list<DomNode>> with
                                            member x.Visit (value : aval<'a>) =
                                                let mapping = unbox<'a -> list<DomNode>> mapping
                                                value.GetValue token |> mapping
                                    }

                                match cache.Value with
                                | None ->
                                    let mutable i = idx
                                    let mutable state = IndexList.empty
                                    for n in nodes do
                                        let id = 
                                            match maxIndex with
                                            | Some maxIndex -> Index.between i maxIndex
                                            | None -> Index.after i
                                        state <- IndexList.set id n state
                                        delta <- IndexList.set id (Set n) delta
                                        i <- id

                                    cache.Value <- Some state
                                | Some o ->
                                    let mutable o = o
                                    let mutable i = idx
                                    let mutable state = IndexList.empty
                                    for n in nodes do
                                        match IndexList.tryFirstIndex o with
                                        | Some fi ->
                                            state <- IndexList.set fi n state
                                            delta <- IndexList.set fi (Set n) delta
                                            i <- fi
                                            o <- IndexList.remove fi o
                                        | None ->
                                            let fi =
                                                match maxIndex with
                                                | Some maxIndex -> Index.between i maxIndex
                                                | None -> Index.after i
                                            state <- IndexList.set fi n state
                                            delta <- IndexList.set fi (Set n) delta
                                            i <- fi

                                    o |> IndexList.iteri (fun i _ ->
                                        delta <- IndexList.set i Remove delta
                                    )

                                    cache.Value <- Some state




                                ()
                            )

                            for (mapping, reader) in readers do
                                let ops = reader.GetChanges token
                                for index, op in ops do
                                    match op with
                                    | Set v -> 
                                        delta <- IndexList.set (mapping.Invoke index) (Set v) delta
                                    | Remove -> 
                                        match mapping.Revoke index with
                                        | Some oi -> delta <- IndexList.set oi Remove delta
                                        | None -> ()
                    
                            IndexListDelta.ofIndexList delta
                    }
                )

        member private x.Content = content

        static member Combine(l : NodeList, r : NodeList) =
            NodeList(IndexList.append l.Content r.Content)

        static member Custom(value : aval<'a>, mapping : 'a -> list<DomNode>) =
            NodeList(IndexList.single (AValCustom(mapping :> obj, value :> IAdaptiveValue)))

        new(value : DomNode) =
            NodeList(IndexList.single (Constant [value]))

        new(value : seq<DomNode>) =
            NodeList(IndexList.single (Constant (Seq.toList value)))

        new(value : alist<DomNode>) =
            NodeList(IndexList.single (AList value))

        new(value : aval<DomNode>) =
            let mapping (n : DomNode) = [n]
            NodeList(IndexList.single (AValCustom(mapping, value)))

        new(value : aval<option<DomNode>>) =
            let mapping (n : option<DomNode>) = Option.toList n
            NodeList(IndexList.single (AValCustom(mapping, value)))


    module HashMap =
        let ofAttributes (attributes : #seq<Attribute>) =
            let mutable res = HashMap.empty<string, AttributeValue>
            for att in attributes do
                res <- 
                    res |> HashMap.alter att.Name (function 
                        | Some o -> Attribute.Merge(Attribute(att.Name, o), att).Value |> Some
                        | None -> Some att.Value
                    )
            res

    type AttributeTableElement =
        | Constant of HashMap<string, AttributeValue>
        | AMap of amap<string, AttributeValue>
        // | AValCustom of mapping : obj * value : IAdaptiveValue

    
    type AttributeTable private(store : IndexList<AttributeTableElement>) =
        static let empty = AttributeTable IndexList.empty<AttributeTableElement>

        static member Empty = empty

        static member Combine(l : AttributeTable, r : AttributeTable) =
            AttributeTable(IndexList.append l.Store r.Store)

        member x.Store : IndexList<AttributeTableElement> = store

        member x.ToAMap() =
            let adaptive = store |> IndexList.choose (function AMap m -> Some m | _ -> None)
            if IndexList.isEmpty adaptive then
                let mutable res = HashMap.empty
                for s in store do
                    match s with
                    | Constant m -> res <- (res, m) ||> HashMap.unionWith (fun k a b -> Attribute.Merge(Attribute(k, a), Attribute(k, b)).Value)
                    | AMap _ -> ()
                    
                AMap.ofHashMap res
            else
                AMap.ofReader (fun () ->
                    let readers =
                        adaptive |> IndexList.map (fun m -> m.GetReader())

                    let mutable state = HashMap.empty<string, IndexList<AttributeValue>>

                    store |> IndexList.iteri (fun idx value ->
                        match value with
                        | Constant table ->
                            for (k, v) in table do
                                state <- 
                                    state |> HashMap.alter k (fun o ->
                                        let o = 
                                            match o with
                                            | Some o -> o
                                            | None -> IndexList.empty
                                        Some (IndexList.set idx v o)
                                    )
                        | _ ->
                            ()
                    )
                    let mutable initial = true
                    { new AbstractReader<HashMapDelta<string, AttributeValue>>(HashMapDelta.empty) with
                        override x.Compute(token : AdaptiveToken) = 
                            let oldState = state
                            let mutable touched = HashSet.empty
                            readers |> IndexList.iteri (fun idx reader ->
                                let ops = reader.GetChanges token
                                for k, op in ops do
                                    touched <- HashSet.add k touched
                                    match op with
                                    | Set v -> 
                                        state <- state |> HashMap.alter k (fun o ->
                                            let o = 
                                                match o with
                                                | Some o -> o
                                                | None -> IndexList.empty
                                            Some (IndexList.set idx v o)
                                        )
                                    | Remove -> 
                                        state <- state |> HashMap.alter k (fun o ->
                                            match o with
                                            | Some o -> 
                                                let n = IndexList.remove idx o
                                                if n.IsEmpty then None
                                                else Some n
                                            | None -> 
                                                None
                                        )
                            )
                        
                            let tryReduce (name : string) (values : IndexList<AttributeValue>) =
                                if values.Count < 1 then
                                    None
                                elif values.Count = 1 then
                                    let v = IndexList.first values
                                    Some (Set v)
                                else
                                    let mutable res : option<AttributeValue> = None
                                    for v in values do
                                        match res with
                                        | Some old -> 
                                            let r = Attribute.Merge(Attribute(name, old), Attribute(name, v))
                                            res <- Some r.Value
                                        | None ->
                                            res <- Some v
                                    Some (Set res.Value)

                            if initial then
                                initial <- false

                                state |> HashMap.choose tryReduce |> HashMapDelta.ofHashMap
                            else
                                touched.MapToMap (fun k -> 
                                    match HashMap.tryFind k state with
                                    | Some n -> Set n
                                    | None -> Remove
                                )
                                |> HashMap.choose (fun name op ->
                                    match op with
                                    | Set values -> 
                                        match tryReduce name values with
                                        | Some res -> Some res
                                        | None -> Some Remove
                                    | Remove ->
                                        Some Remove
                                )
                                |> HashMapDelta.ofHashMap
                    }
                )

        new (att : seq<Attribute>) =
            AttributeTable(
                att 
                |> HashMap.ofAttributes 
                |> Constant
                |> IndexList.single
            )

        new (att : AttributeMap) =
            AttributeTable(
                att.Content
                |> AMap
                |> IndexList.single
            )

    type NodeBuilderState =
        {
            children : NodeList
            attributes : AttributeTable
        }


    type RenderControlBuilderState(info : RenderControlInfo) =
        let mutable attributes = AttributeTable.Empty
        let mutable actions = AMap.empty
        let mutable view = None
        let mutable proj = None
        let scene = SceneNodeBuilderState()
        do scene.Append [Sg.Uniform("ViewportSize", info.ViewportSize)]
        
        let mutable onReady : list<IEventHandler -> unit> = []

        member x.Info = info

        member x.Append(atts : AttributeMap) =
            attributes <- AttributeTable.Combine(attributes, AttributeTable atts)
            
        member x.Append(atts : list<SceneAttribute>) =
            match view with
            | None -> view <- atts |> List.tryPick (function SceneAttribute.View v -> Some v | _ -> None)
            | _ -> ()
            match proj with
            | None -> proj <- atts |> List.tryPick (function SceneAttribute.Proj v -> Some v | _ -> None)
            | _ -> ()

            scene.Append atts

        member x.Append(atts : aset<ISceneNode>) =
            scene.Append atts
            
        member x.Append(kind : RenderControlEventKind, action : RenderControlEventInfo -> unit) =
            actions <- AMap.unionWith (fun _ l r -> fun i -> l i; r i) actions (AMap.single kind action)

        member x.Append(ready : IEventHandler -> unit) =
            onReady <- ready :: onReady
        
        member x.Build() =
            let scene = 
                let view =
                    match view with
                    | Some v -> v
                    | None -> Log.warn "please apply a View-Transformation directly in your RenderControl for picking"; AVal.constant Trafo3d.Identity

                let proj =
                    match proj with
                    | Some v -> v
                    | None -> Log.warn "please apply a View-Transformation directly in your RenderControl for picking"; AVal.constant Trafo3d.Identity
                    
                let ready =
                    match onReady with
                    | [] -> ignore
                    | _ -> fun h -> onReady |> List.iter (fun a -> a h)
                    
                { Scene = scene.Build(); View = view; Proj = proj; OnReady = ready }

            AttributeMap (attributes.ToAMap()), actions, scene


    type RenderControlBuilder<'a> = RenderControlBuilderState -> 'a



open NodeBuilderHelpers

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RenderControl = 
    type ViewportSize = ViewportSize
    type Time = Time
    type Info = Info

    type OnReady = OnReady of (IEventHandler -> unit)


type RenderControlBuilderExt() =
    static member wrap (sg : Aardvark.SceneGraph.ISg) =
        SgAdapter.Node(sg) :> ISceneNode


    member inline x.For(elements : seq<'a>, [<InlineIfLambda>] action : 'a -> RenderControlBuilder<unit>) =
        fun (state : RenderControlBuilderState) -> 
            for e in elements do action e state

    member inline x.While([<InlineIfLambda>] guard : unit -> bool, [<InlineIfLambda>] action : unit -> RenderControlBuilder<unit>) =
        fun (state : RenderControlBuilderState) -> 
            while guard() do action () state

    member x.TryFinally(action : unit -> RenderControlBuilder<'a>, comp : unit -> unit) =
        fun (state : RenderControlBuilderState) -> 
            try action () state 
            finally comp()
                
    member inline x.Zero() =
        fun (state : RenderControlBuilderState) ->  ()

    member inline x.Combine(l : RenderControlBuilder<unit>, r : RenderControlBuilder<'a>) : RenderControlBuilder<'a> =   
        fun (state : RenderControlBuilderState) ->
            l state
            r state

    member inline x.Delay([<InlineIfLambda>] action : unit -> RenderControlBuilder<'a>) = 
        fun (state : RenderControlBuilderState) ->
            action () state

    
    member inline x.Yield(RenderControl.OnReady onReady) =
        fun (state : RenderControlBuilderState) ->
            state.Append onReady

    member inline x.Bind(size : RenderControl.ViewportSize, [<InlineIfLambda>] action : aval<V2i> -> RenderControlBuilder<'a>) =
        fun (state : RenderControlBuilderState) ->
            action state.Info.ViewportSize state

            
    member inline x.Bind(size : RenderControl.Time, [<InlineIfLambda>] action : aval<System.DateTime> -> RenderControlBuilder<'a>) =
        fun (state : RenderControlBuilderState) ->
            action state.Info.Time state
            
    member inline x.Bind(size : RenderControl.Info, [<InlineIfLambda>] action : RenderControlInfo -> RenderControlBuilder<'a>) =
        fun (state : RenderControlBuilderState) ->
            action state.Info state

    member inline x.Bind(value : RenderControlBuilder<'a>, [<InlineIfLambda>] action : 'a -> RenderControlBuilder<'b>) =
        fun (state : RenderControlBuilderState) ->
            let a = value state
            action a state

    member inline x.Yield(att : SceneAttribute) : RenderControlBuilder<unit> =
        fun (s : RenderControlBuilderState) -> s.Append [att]
        
    member inline x.Yield(att : list<SceneAttribute>) : RenderControlBuilder<unit> =
        fun (s : RenderControlBuilderState) -> s.Append att

    member inline x.Yield(node : aset<ISceneNode>) : RenderControlBuilder<unit> =
        fun (s : RenderControlBuilderState) -> s.Append node
        
    member inline x.Yield(node : ISceneNode) : RenderControlBuilder<unit> =
        x.Yield(ASet.single node)
        
    member inline x.Bind((info : 'a, node : ISceneNode), action : 'a -> RenderControlBuilder<'b>) : RenderControlBuilder<'b> =
        x.Combine(x.Yield(node), action info)
        
    member inline x.Yield((_info : #ISceneNodeMetaInfo, node : ISceneNode)) : RenderControlBuilder<unit> =
        x.Yield(ASet.single node)

    member inline x.Yield(node : Aardvark.SceneGraph.ISg) : RenderControlBuilder<unit> =
        x.Yield(ASet.single (RenderControlBuilderExt.wrap node))

    member inline x.Yield(node : aval<seq<ISceneNode>>) : RenderControlBuilder<unit> =
        x.Yield(ASet.ofAVal node)
        
    member inline x.Yield(node : aval<seq<Aardvark.SceneGraph.ISg>>) : RenderControlBuilder<unit> =
        x.Yield(ASet.ofAVal node |> ASet.map RenderControlBuilderExt.wrap)

    member inline x.Yield(node : aval<ISceneNode>) : RenderControlBuilder<unit> =
        x.Yield(node |> ASet.bind ASet.single)
        
    member inline x.Yield(node : aval<Aardvark.SceneGraph.ISg>) : RenderControlBuilder<unit>=
        x.Yield(node |> ASet.bind (RenderControlBuilderExt.wrap >> ASet.single))
        
    member inline x.Yield(node : aval<option<ISceneNode>>) : RenderControlBuilder<unit> =
        x.Yield(node |> ASet.bind (function Some v -> ASet.single v | None -> ASet.empty))
        
    member inline x.Yield(node : aval<option<Aardvark.SceneGraph.ISg>>) : RenderControlBuilder<unit> =
        x.Yield(node |> ASet.bind (function Some v -> ASet.single (RenderControlBuilderExt.wrap v) | None -> ASet.empty))
        
    member inline x.Yield(att : Attribute) =
        fun (state : RenderControlBuilderState) -> state.Append (AttributeMap.single att)
        
    member inline x.Yield(att : list<Attribute>) =
        fun (state : RenderControlBuilderState) -> state.Append (AttributeMap.ofList att)
        
    member inline x.Yield(att : aval<Attribute>) =
        fun (state : RenderControlBuilderState) -> state.Append (AttributeMap.ofAVal att)

    member inline x.Yield(att : aval<#seq<Attribute>>) =
        fun (state : RenderControlBuilderState) -> state.Append (AttributeMap.ofSeqA att)

    member inline x.Yield(att : aval<option<Attribute>>) =
        fun (state : RenderControlBuilderState) -> state.Append (AttributeMap.ofOptionA att) 

    member inline x.Yield(att : AttributeMap) =
        fun (state : RenderControlBuilderState) -> state.Append att

    member inline x.Yield((kind : RenderControlEventKind, callback : RenderControlEventInfo -> unit)) : RenderControlBuilder<unit> =
        fun (s : RenderControlBuilderState) -> s.Append(kind, callback)

    member inline x.Yield(att : RenderControlBuilder<unit>) =
        att


type RenderControlBuilder() =
    inherit RenderControlBuilderExt()
    
    member inline x.Run(run : RenderControlBuilder<unit>) =
        let getContent (info : RenderControlInfo) =
            let state = RenderControlBuilderState(info)
            run state
            state.Build()
        DomNode.RenderControl(getContent)

type NodeBuilder =
    val mutable public Tag : string

    member inline x.Yield (element : DomNode) =
        { children = NodeList element; attributes = AttributeTable.Empty }

    member inline x.Yield(content : aval<string>) =
        x.Yield (DomNode.Text content)

    member inline x.Yield(content : string) =
        x.Yield (DomNode.Text (AVal.constant content))

    member inline x.Yield(elements : #seq<DomNode>) =
        { children = NodeList (elements :> seq<_>); attributes = AttributeTable.Empty }

    member inline x.Yield(elements : alist<DomNode>) =
        { children = NodeList elements; attributes = AttributeTable.Empty }

    member inline x.Yield(element : aval<DomNode>) =
        { children = NodeList element; attributes = AttributeTable.Empty }

    member inline x.Yield(element : aval<option<DomNode>>) =
        { children = NodeList element; attributes = AttributeTable.Empty }
 
    member inline x.Yield(element : aval<seq<DomNode>>) =
        { children = NodeList.Custom(element, Seq.toList); attributes = AttributeTable.Empty }

    member inline x.Yield(element : aval<list<DomNode>>) =
        { children = NodeList.Custom(element, id); attributes = AttributeTable.Empty }

    member inline x.Yield(element : aval<DomNode[]>) =
        { children = NodeList.Custom(element, Array.toList); attributes = AttributeTable.Empty }


    member inline x.Yield(att : Attribute) =
        { children = NodeList.Empty; attributes = AttributeTable [att] }
        
    member inline x.Yield(att : list<Attribute>) =
        { children = NodeList.Empty; attributes = AttributeTable att }

    member inline x.Yield(att : AttributeMap) =
        { children = NodeList.Empty; attributes = AttributeTable att }

    member inline x.Yield(att : amap<string, AttributeValue>) =
        { children = NodeList.Empty; attributes = AttributeTable (AttributeMap att) }

    member inline x.Yield(att : HashMap<string, AttributeValue>) =
        { children = NodeList.Empty; attributes = AttributeTable (AttributeMap (AMap.ofHashMap att)) }

    member inline x.Yield(att : aval<#seq<Attribute>>) =
        { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofSeqA att) }
        
    member inline x.Yield(att : aval<Attribute>) =
        { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofAVal att) }

    member inline x.Yield(att : aval<option<Attribute>>) =
        { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofOptionA att)  }

    member inline x.Yield(state : NodeBuilderState) =
        state
        
    member inline x.For(elements : seq<'a>, [<InlineIfLambda>] action : 'a -> NodeBuilderState) =
        let mutable res = { children = NodeList.Empty; attributes = AttributeTable.Empty }
        for e in elements do
            let r = action e
            res <- 
                {
                    children = NodeList.Combine(res.children, r.children)
                    attributes = AttributeTable.Combine(res.attributes, r.attributes)
                }

        res

    member inline x.While([<InlineIfLambda>] guard : unit -> bool, [<InlineIfLambda>] action : unit -> NodeBuilderState) =
        let mutable res = { children = NodeList.Empty; attributes = AttributeTable.Empty }
        while guard() do
            let r = action()
            res <- 
                {
                    children = NodeList.Combine(res.children, r.children)
                    attributes = AttributeTable.Combine(res.attributes, r.attributes)
                }
        res

    member x.TryFinally(action : unit -> NodeBuilderState, comp : unit -> unit) =
        try action() 
        finally comp()

    member inline x.Zero() =
        { children = NodeList.Empty; attributes = AttributeTable.Empty }

    member inline x.Combine(l : NodeBuilderState, r : unit -> NodeBuilderState) =   
        let r = r()
        {
            children = NodeList.Combine(l.children, r.children)
            attributes = AttributeTable.Combine(l.attributes, r.attributes)
        }

    member inline x.Delay([<InlineIfLambda>] action : unit -> NodeBuilderState) = 
        action

    member inline x.Run([<InlineIfLambda>] run : unit -> NodeBuilderState) =
        let state = run()
        DomNode.Element(x.Tag, AttributeMap (state.attributes.ToAMap()), state.children.ToAList())

    new(tag : string) = { Tag = tag }

type VoidNodeBuilder =
    val mutable public Tag : string

    member inline x.Yield(att : Attribute) =
        AttributeMap.single att

    member inline x.Yield(att : list<Attribute>) =
        AttributeMap.ofList att

    member inline x.Yield(att : AttributeMap) =
        att

    member inline x.Zero() =
        AttributeMap.empty

    member inline x.Combine(l : AttributeMap, r : AttributeMap) =
        AttributeMap.union l r

    member inline x.Delay([<InlineIfLambda>] action : unit -> AttributeMap) = 
        action()

    member inline x.Run(state : AttributeMap) =
        DomNode.Element(x.Tag, state, AList.empty)

    new(tag : string) = { Tag = tag }

type AttributeMapBuilder() =

    member inline x.Yield(att : Attribute) =
        AttributeTable [att]
        
    member inline x.Yield(att : list<Attribute>) =
        AttributeTable att

    member inline x.Yield(att : AttributeMap) =
        AttributeTable att

    member inline x.Yield(att : aval<#seq<Attribute>>) =
        AttributeTable (AttributeMap.ofSeqA att)

    member inline x.Yield(att : aval<option<Attribute>>) =
        AttributeTable (AttributeMap.ofOptionA att) 

    member inline x.For(elements : seq<'a>, [<InlineIfLambda>] action : 'a -> AttributeTable) =
        let mutable res = AttributeTable.Empty
        for e in elements do
            let r = action e
            res <- AttributeTable.Combine(res, r)

        res

    member inline x.While([<InlineIfLambda>] guard : unit -> bool, [<InlineIfLambda>] action : unit -> AttributeTable) =
        let mutable res = AttributeTable.Empty
        while guard() do
            let r = action()
            res <- AttributeTable.Combine(res, r)
        res


    member x.TryFinally(action : unit -> AttributeTable, comp : unit -> unit) =
        try action() 
        finally comp()

    member inline x.Zero() = AttributeTable.Empty

    member inline x.Combine(l : AttributeTable, r : unit -> AttributeTable) =   
        let r = r()
        AttributeTable.Combine(l, r)

    member inline x.Delay([<InlineIfLambda>] action : unit -> AttributeTable) = 
        action

    member inline x.Run(run : unit -> AttributeTable) =
        let state = run()
        AttributeMap (state.ToAMap())

    
    
[<AutoOpen>]
module AttributeMapBuilder =
    
    let att = AttributeMapBuilder()
