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
        ViewportSize : aval<V2i>
    }
    
type DomScene =
    {
        View : aval<Trafo3d>
        Proj : aval<Trafo3d>
        Scene : ISceneNode
    }

type DomNode =
    | VoidElement of tag : string * attributes : AttributeMap
    | Text of content : aval<string>
    | Element of tag : string * attributes : AttributeMap * children : alist<DomNode>
    | RenderControl of attributes : AttributeMap * getScene : (RenderControlInfo -> DomScene)

[<CompilerMessage("internal", 1337, IsHidden = true)>]
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
            let mutable res = HashMap.empty
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
            AMap.ofReader (fun () ->
                let readers =
                    store |> IndexList.choose (function AMap m -> Some (m.GetReader()) | _ -> None)

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

open NodeBuilderHelpers

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

    member inline x.Yield(att : AttributeMap) =
        { children = NodeList.Empty; attributes = AttributeTable att }

    member inline x.Yield(att : aval<#seq<Attribute>>) =
        { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofSeqA att) }

    member inline x.Yield(att : aval<option<Attribute>>) =
        { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofOptionA att)  }

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

    member inline x.Run(run : unit -> NodeBuilderState) =
        let state = run()
        DomNode.Element(x.Tag, AttributeMap (state.attributes.ToAMap()), state.children.ToAList())

    new(tag : string) = { Tag = tag }

type VoidNodeBuilder =
    val mutable public Tag : string

    member inline x.Yield(att : Attribute) =
        AttributeMap.single att

    member inline x.Zero() =
        AttributeMap.empty

    member inline x.Combine(l : AttributeMap, r : AttributeMap) =
        AttributeMap.union l r

    member inline x.Delay([<InlineIfLambda>] action : unit -> AttributeMap) = 
        action()

    member inline x.Run(state : AttributeMap) =
        DomNode.Element(x.Tag, state, AList.empty)

    new(tag : string) = { Tag = tag }


[<AutoOpen>]
module NodeBuilders =
    let h1 = NodeBuilder "h1"
    let code = NodeBuilder "code"
    let pre = NodeBuilder "pre"
    let span = NodeBuilder "span"
    let div = NodeBuilder "div"
    let table = NodeBuilder "table"
    let tr = NodeBuilder "tr"
    let td = NodeBuilder "td"
    let ul = NodeBuilder "ul"
    let li = NodeBuilder "li"
    let a = NodeBuilder "a"
    let p = NodeBuilder "p"
    let button = NodeBuilder "button"
    let input = NodeBuilder "input"
    let textarea = NodeBuilder "textarea"
    let img = NodeBuilder "img"
    let br = VoidNodeBuilder "br"
    let hr = NodeBuilder "hr"
    let h2 = NodeBuilder "h2"
    let h3 = NodeBuilder "h3"
    let h4 = NodeBuilder "h4"
    let h5 = NodeBuilder "h5"
    let h6 = NodeBuilder "h6"
    let b = NodeBuilder "b"
    let i = NodeBuilder "i"
    let strong = NodeBuilder "strong"
    let em = NodeBuilder "em"
    let small = NodeBuilder "small"
    let sub = NodeBuilder "sub"
    let sup = NodeBuilder "sup"
    let s = NodeBuilder "s"
    let u = NodeBuilder "u"
    let ol = NodeBuilder "ol"
    let dl = NodeBuilder "dl"
    let dt = NodeBuilder "dt"
    let dd = NodeBuilder "dd"
    let cite = NodeBuilder "cite"
    let blockquote = NodeBuilder "blockquote"
    let q = NodeBuilder "q"
    let abbr = NodeBuilder "abbr"
    let acronym = NodeBuilder "acronym"
    let address = NodeBuilder "address"
    let bdo = NodeBuilder "bdo"
    let big = NodeBuilder "big"
    let blink = NodeBuilder "blink"
    let center = NodeBuilder "center"
    let cnter = NodeBuilder "cnter"
    let strike = NodeBuilder "strike"
    let dir = NodeBuilder "dir"
    let font = NodeBuilder "font"
    let nobr = NodeBuilder "nobr"
    let noembed = NodeBuilder "noembed"
    let noframes = NodeBuilder "noframes"
    let plaintext = NodeBuilder "plaintext"
    let samp = NodeBuilder "samp"
    let select = NodeBuilder "select"
    let tt = NodeBuilder "tt"
    let xmp = NodeBuilder "xmp"
    let marquee = NodeBuilder "marquee"
    let area = NodeBuilder "area"
    let basefont = NodeBuilder "basefont"
    let col = NodeBuilder "col"
    let colgroup = NodeBuilder "colgroup"
    let frame = NodeBuilder "frame"
    let isindex = NodeBuilder "isindex"
    let link = NodeBuilder "link"
    let meta = NodeBuilder "meta"
    let param = NodeBuilder "param"
    let embed = NodeBuilder "embed"
    let base_ = NodeBuilder "base"
    let body = NodeBuilder "body"
    let head = NodeBuilder "head"
    let html = NodeBuilder "html"
    let form = NodeBuilder "form"
    let fieldset = NodeBuilder "fieldset"
    let legend = NodeBuilder "legend"
    let optgroup = NodeBuilder "optgroup"
    let option = NodeBuilder "option"
    let script = NodeBuilder "script"
    let style = NodeBuilder "style"
    let title = NodeBuilder "title"
    let caption = NodeBuilder "caption"
    let colgroup_ = NodeBuilder "colgroup"
    let table_ = NodeBuilder "table"
    let tbody = NodeBuilder "tbody"
    let thead = NodeBuilder "thead"
    let tfoot = NodeBuilder "tfoot"
    let iframe = NodeBuilder "iframe"
    let frame_ = NodeBuilder "frame"
    let frameset = NodeBuilder "frameset"
    let frame_set = NodeBuilder "frame_set"
    let label = NodeBuilder "label"
    let option_ = NodeBuilder "option"
    let optgroup_ = NodeBuilder "optgroup"
    let keygen = NodeBuilder "keygen"
    let output = NodeBuilder "output"
    let progress = NodeBuilder "progress"
    let meter = NodeBuilder "meter"
    let details = NodeBuilder "details"
    let summary = NodeBuilder "summary"
    let datalist = NodeBuilder "datalist"
    let menu = NodeBuilder "menu"
    let menuitem = NodeBuilder "menuitem"
    let multicol = NodeBuilder "multicol"