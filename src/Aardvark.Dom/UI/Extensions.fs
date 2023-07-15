namespace Aardvark.Dom.Extensions

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Dom

[<AutoOpen>]
module ElementIds =
    let inline RandomElementId() =
        let g = System.Guid.NewGuid()
        let b = g.ToByteArray() |> System.Convert.ToBase64String
        let b = b.Replace("/", "_").Replace("=", "").Replace("+", "Aa")
        $"N{b}"

module NodeBuilderHelpers =
    open NodeBuilderHelpers
    
    type MapNodeBuilder<'x>(mapping : AttributeMap -> alist<DomNode> -> 'x) =
        inherit NodeBuilder("")

        member x.Run(action : unit -> NodeBuilderState) =
            let s = action()
            let att = s.attributes.ToAMap() |> AttributeMap
            let cs = s.children.ToAList()
            mapping att cs
    
    type MapStateNodeBuilder<'x>(mapping : NodeBuilderState -> 'x) =
        inherit NodeBuilder("")

        member x.Run(action : unit -> NodeBuilderState) =
            mapping (action())
    
    [<AbstractClass>]
    type StateNodeBuilder<'s, 'x>() =
        member inline x.Yield (element : DomNode) =
            fun (s : 's) -> s, { children = NodeList element; attributes = AttributeTable.Empty }

        member inline x.Yield(content : aval<string>) =
            x.Yield (DomNode.Text content)

        member inline x.Yield(content : string) =
            x.Yield (DomNode.Text (AVal.constant content))
            
        member inline x.Yield(elements : #seq<DomNode>) =
            fun (s : 's) -> s, { children = NodeList (elements :> seq<_>); attributes = AttributeTable.Empty }

        member inline x.Yield(elements : alist<DomNode>) =
            fun (s : 's) -> s, { children = NodeList elements; attributes = AttributeTable.Empty }

        member inline x.Yield(element : aval<DomNode>) =
            fun (s : 's) -> s, { children = NodeList element; attributes = AttributeTable.Empty }

        member inline x.Yield(element : aval<option<DomNode>>) =
            fun (s : 's) -> s, { children = NodeList element; attributes = AttributeTable.Empty }
 
        member inline x.Yield(element : aval<seq<DomNode>>) =
            fun (s : 's) -> s, { children = NodeList.Custom(element, Seq.toList); attributes = AttributeTable.Empty }

        member inline x.Yield(element : aval<list<DomNode>>) =
            fun (s : 's) -> s, { children = NodeList.Custom(element, id); attributes = AttributeTable.Empty }

        member inline x.Yield(element : aval<DomNode[]>) =
            fun (s : 's) -> s, { children = NodeList.Custom(element, Array.toList); attributes = AttributeTable.Empty }


        member inline x.Yield(att : Attribute) =
            fun (s : 's) -> s, { children = NodeList.Empty; attributes = AttributeTable [att] }
        
        member inline x.Yield(att : list<Attribute>) =
            fun (s : 's) -> s, { children = NodeList.Empty; attributes = AttributeTable att }

        member inline x.Yield(att : AttributeMap) =
            fun (s : 's) -> s, { children = NodeList.Empty; attributes = AttributeTable att }

        member inline x.Yield(att : aval<#seq<Attribute>>) =
            fun (s : 's) -> s, { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofSeqA att) }
        
        member inline x.Yield(att : aval<Attribute>) =
            fun (s : 's) -> s, { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofAVal att) }

        member inline x.Yield(att : aval<option<Attribute>>) =
            fun (s : 's) -> s, { children = NodeList.Empty; attributes = AttributeTable (AttributeMap.ofOptionA att)  }

        member inline x.For(elements : seq<'a>, [<InlineIfLambda>] action : 'a -> 's -> 's * NodeBuilderState) =
            fun (s : 's) -> 
                let mutable state = s
                let mutable res = { children = NodeList.Empty; attributes = AttributeTable.Empty }
                for e in elements do
                    let s1, r = action e state
                    state <- s1
                    res <- 
                        {
                            children = NodeList.Combine(res.children, r.children)
                            attributes = AttributeTable.Combine(res.attributes, r.attributes)
                        }

                state, res

        member inline x.While([<InlineIfLambda>] guard : unit -> bool, [<InlineIfLambda>] action : 's -> 's * NodeBuilderState) =
            fun (s : 's) -> 
                let mutable state = s
                let mutable res = { children = NodeList.Empty; attributes = AttributeTable.Empty }
                while guard() do
                    let s1, r = action state
                    state <- s1
                    res <- 
                        {
                            children = NodeList.Combine(res.children, r.children)
                            attributes = AttributeTable.Combine(res.attributes, r.attributes)
                        }
                state, res


        member x.TryFinally(action : 's -> 's * NodeBuilderState, comp : unit -> unit) =
            fun s ->
                try action s
                finally comp()

        member inline x.Zero() =
            fun (s : 's) -> s, { children = NodeList.Empty; attributes = AttributeTable.Empty }

        member inline x.Combine(l : 's -> 's * NodeBuilderState, r : 's -> 's * NodeBuilderState) =   
            fun (s : 's) -> 
                let s, l = l s
                let s, r = r s
                s, {
                    children = NodeList.Combine(l.children, r.children)
                    attributes = AttributeTable.Combine(l.attributes, r.attributes)
                }

        member inline x.Delay([<InlineIfLambda>] action : unit -> 's -> 's * NodeBuilderState) = 
            action ()

        member inline x.Run(run : 's -> 's * NodeBuilderState) =
        
            let materialize (s : 's) =
                let s, bs = run s
                s, AttributeMap (bs.attributes.ToAMap()), bs.children.ToAList()

            x.Run materialize

        member x.Yield(action : 's -> 's) =
            fun (s : 's) ->
                action s, { children = NodeList.Empty; attributes = AttributeTable.Empty }
            
        member x.Bind(action : 's -> 's * 'a, cont : 'a -> 's -> 's * NodeBuilderState) =
            fun (s : 's) ->
                let s, a = action s
                cont a s
            
        member x.Yield(action : 's -> 's * DomNode) =
            fun (s : 's) ->
                let s, n = action s
                s, { attributes = AttributeTable.Empty; children = NodeList n }
            
        member x.Yield(action : 's -> DomNode) =
            fun (s : 's) ->
                let n = action s
                s, { attributes = AttributeTable.Empty; children = NodeList n }
            
        member x.Yield(action : alist<'s -> DomNode>) =
            fun (s : 's) ->
                let n = action |> AList.map (fun a -> a s)
                s, { attributes = AttributeTable.Empty; children = NodeList n }
            
        abstract Run : ('s -> 's * AttributeMap * alist<DomNode>) -> 'x
        
    module NodeBuilderState =
        let empty =
            {
                attributes = AttributeTable.Empty
                children = NodeList.Empty
            }

    type AListCollector<'a>(store : IndexList<Choice<alist<'a>, IndexList<'a>>>) =
    
        member private x.Store = store

        static member Empty = AListCollector<'a>(IndexList.empty)

        static member Combine(l : AListCollector<'a>, r : AListCollector<'a>) =
            if l.Store.IsEmpty then r
            elif r.Store.IsEmpty then l
            else
                let _, ls, lr = IndexList.splitAti 0 l.Store
                let rl, rs, _ = IndexList.splitAti (r.Store.Count - 1) r.Store
                match ls with
                | Some (_, Choice2Of2 ll) ->
                    match rs with
                    | Some (_, Choice2Of2 rr) ->
                        let l = IndexList.append ll rr
                        AListCollector (IndexList.append (IndexList.add (Choice2Of2 l) lr) rl)
                    | _ ->
                        AListCollector(IndexList.append l.Store r.Store)
                | _ ->
                    AListCollector(IndexList.append l.Store r.Store)

        member x.Append(value : 'a) =
            if IndexList.isEmpty store then
                AListCollector(IndexList.add (Choice2Of2 (IndexList.single value)) store)
            else
                let before, s, _ = IndexList.splitAti (store.Count - 1) store
                match s with
                | Some (idx, Choice2Of2 l) ->
                    AListCollector(
                        before |> IndexList.set idx (Choice2Of2 (IndexList.add value l))
                    )
                | _ ->
                    AListCollector(IndexList.add (Choice2Of2 (IndexList.single value)) store)
                
        member x.Append(values : #seq<'a>) =
            if IndexList.isEmpty store then
                AListCollector(IndexList.add (Choice2Of2 (IndexList.ofSeq values)) store)
            else
                let before, s, _ = IndexList.splitAti (store.Count - 1) store
                match s with
                | Some (idx, Choice2Of2 l) ->
                    let newList = (l, values) ||> Seq.fold (fun l v -> IndexList.add v l)
                    AListCollector(
                        before |> IndexList.set idx (Choice2Of2 newList)
                    )
                | _ ->
                    AListCollector(IndexList.add (Choice2Of2 (IndexList.ofSeq values)) store)
                   
        member x.Append(values : IndexList<'a>) =
            if IndexList.isEmpty store then
                AListCollector(IndexList.add (Choice2Of2 values) store)
            else
                let before, s, _ = IndexList.splitAti (store.Count - 1) store
                match s with
                | Some (idx, Choice2Of2 l) ->
                    let newList = IndexList.append l values
                    AListCollector(
                        before |> IndexList.set idx (Choice2Of2 newList)
                    )
                | _ ->
                    AListCollector(IndexList.add (Choice2Of2 values) store)
             
        member x.Append(values : list<'a>) =
            if IndexList.isEmpty store then
                AListCollector(IndexList.add (Choice2Of2 (IndexList.ofList values)) store)
            else
                let before, s, _ = IndexList.splitAti (store.Count - 1) store
                match s with
                | Some (idx, Choice2Of2 l) ->
                    let newList = (l, values) ||> List.fold (fun l v -> IndexList.add v l)
                    AListCollector(
                        before |> IndexList.set idx (Choice2Of2 newList)
                    )
                | _ ->
                    AListCollector(IndexList.add (Choice2Of2 (IndexList.ofList values)) store)

        member x.Append(values : alist<'a>) =
            if values.IsConstant then
                x.Append (AList.force values)
            else
                AListCollector(IndexList.add (Choice1Of2 values) store)
        
        member x.ToAList() =
            store 
            |> IndexList.toArray
            |> Array.map (fun l ->
                match l with
                | Choice1Of2 l -> l
                | Choice2Of2 l -> AList.ofIndexList l
            )
            |> AList.concat
        
    type NodeLikeBuilderState<'a> =
        {
            attributes : AttributeTable
            children : AListCollector<'a>
        }

        static member Combine(l : NodeLikeBuilderState<'a>, r : NodeLikeBuilderState<'a>) =
            {
                attributes = AttributeTable.Combine(l.attributes, r.attributes)
                children = AListCollector.Combine(l.children, r.children)
            }

        static member Empty : NodeLikeBuilderState<'a> =
            {
                attributes = AttributeTable.Empty
                children = AListCollector.Empty
            }

    [<AbstractClass>]
    type NodeLikeBuilder<'a, 'res>() =
        
        abstract Run : NodeLikeBuilderState<'a> -> 'res

        member inline x.Run(action : unit -> NodeLikeBuilderState<'a>) =
            x.Run (action ())

        member inline x.Yield(value : NodeLikeBuilderState<'a>) =
            value
            
        member inline x.Yield(value : 'a) =
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : seq<'a>) =
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : list<'a>) =
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : array<'a>) =
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : IndexList<'a>) =
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
    
        member inline x.Yield(value : aval<option<'a>>) =
            let list = 
                value 
                |> AVal.map (function Some v -> IndexList.single v | _ -> IndexList.empty)
                |> AList.ofAVal
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : aval<seq<'a>>) =
            let list = 
                value 
                |> AVal.map IndexList.ofSeq
                |> AList.ofAVal
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : aval<list<'a>>) =
            let list = 
                value 
                |> AVal.map IndexList.ofList
                |> AList.ofAVal
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : aval<array<'a>>) =
            let list = 
                value 
                |> AVal.map IndexList.ofArray
                |> AList.ofAVal
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
  
        member inline x.Yield(value : aval<'a>) =
            let list = 
                value 
                |> AVal.map IndexList.single
                |> AList.ofAVal
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : alist<'a>) =
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            

        member inline x.Zero() =
            NodeLikeBuilderState<'a>.Empty

        member inline x.Delay([<InlineIfLambda>] action : unit -> NodeLikeBuilderState<'a>) =
            action
            
        member inline x.Combine(l : NodeLikeBuilderState<'a>, [<InlineIfLambda>] r : unit -> NodeLikeBuilderState<'a>) =
            NodeLikeBuilderState.Combine(l, r())
            
        member inline x.For(elements : seq<'x>, [<InlineIfLambda>] mapping : 'x -> NodeLikeBuilderState<'a>) =
            let mutable s = NodeLikeBuilderState.Empty
            for e in elements do
                s <- NodeLikeBuilderState.Combine(s, mapping e)
            s
            
        member inline x.While([<InlineIfLambda>] guard : unit -> bool, [<InlineIfLambda>] body : unit -> NodeLikeBuilderState<'a>) =
            let mutable s = NodeLikeBuilderState.Empty
            while guard() do
                s <- NodeLikeBuilderState.Combine(s, body())

        member inline x.Yield(att : Attribute) =
            { children = AListCollector.Empty; attributes = AttributeTable [att] }
        
        member inline x.Yield(att : list<Attribute>) =
            { children = AListCollector.Empty; attributes = AttributeTable att }

        member inline x.Yield(att : AttributeMap) =
            { children = AListCollector.Empty; attributes = AttributeTable att }

        member inline x.Yield(att : aval<#seq<Attribute>>) =
            { children = AListCollector.Empty; attributes = AttributeTable (AttributeMap.ofSeqA att) }
        
        member inline x.Yield(att : aval<Attribute>) =
            { children = AListCollector.Empty; attributes = AttributeTable (AttributeMap.ofAVal att) }

        member inline x.Yield(att : aval<option<Attribute>>) =
            { children = AListCollector.Empty; attributes = AttributeTable (AttributeMap.ofOptionA att)  }
            
    [<AbstractClass>]
    type StatefulNodeLikeBuilder<'s, 'a, 'res>() =
        
        abstract Run : ('s -> 's * NodeLikeBuilderState<'a>) -> 'res
        
        member inline x.Yield(action : 's -> 's) =
            fun (s : 's) -> action s, NodeLikeBuilderState.Empty
            
        member inline x.Yield(action : 's -> 's * NodeLikeBuilderState<'a>) =
            action
            
        member inline x.Yield(value : NodeLikeBuilderState<'a>) =
            fun (s : 's) -> s, value
            
        member inline x.Yield(value : 'a) =
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : seq<'a>) =
            { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : list<'a>) =
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : array<'a>) =
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            
        member inline x.Yield(value : IndexList<'a>) =
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
    
        member inline x.Yield(value : aval<option<'a>>) =
            let list = 
                value 
                |> AVal.map (function Some v -> IndexList.single v | _ -> IndexList.empty)
                |> AList.ofAVal
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : aval<seq<'a>>) =
            let list = 
                value 
                |> AVal.map IndexList.ofSeq
                |> AList.ofAVal
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : aval<list<'a>>) =
            let list = 
                value 
                |> AVal.map IndexList.ofList
                |> AList.ofAVal
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : aval<array<'a>>) =
            let list = 
                value 
                |> AVal.map IndexList.ofArray
                |> AList.ofAVal
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
  
        member inline x.Yield(value : aval<'a>) =
            let list = 
                value 
                |> AVal.map IndexList.single
                |> AList.ofAVal
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append list }
            
        member inline x.Yield(value : alist<'a>) =
            fun (s : 's) -> s, { attributes = AttributeTable.Empty; children = AListCollector<'a>.Empty.Append value }
            

        member inline x.Zero() =
            fun (s : 's) -> s, NodeLikeBuilderState<'a>.Empty

        member inline x.Delay([<InlineIfLambda>] action : unit -> 's -> 's * NodeLikeBuilderState<'a>) =
            fun (s : 's) -> action () s
            
        member inline x.Combine(l : 's -> 's * NodeLikeBuilderState<'a>, [<InlineIfLambda>] r : 's -> 's * NodeLikeBuilderState<'a>) =
            fun (s : 's) -> 
                let s, lv = l s
                let s, rv = r s
                s, NodeLikeBuilderState.Combine(lv, rv)
            
        member inline x.For(elements : seq<'x>, [<InlineIfLambda>] mapping : 'x -> 's -> 's * NodeLikeBuilderState<'a>) =
            fun (s : 's) -> 
                let mutable res = NodeLikeBuilderState.Empty
                let mutable state = s
                for e in elements do
                    let s, v = mapping e state
                    res <- NodeLikeBuilderState.Combine(res, v)
                    state <- s
                res
            
        member inline x.While([<InlineIfLambda>] guard : unit -> bool, [<InlineIfLambda>] body : 's -> 's * NodeLikeBuilderState<'a>) =
            fun (s : 's) -> 
                let mutable state = s
                let mutable res = NodeLikeBuilderState.Empty
                while guard() do
                    let s, v = body state
                    state <- s
                    res <- NodeLikeBuilderState.Combine(res, v)
                state, res

        member inline x.Yield(att : Attribute) =
            fun (s : 's) -> s, { children = AListCollector.Empty; attributes = AttributeTable [att] }
        
        member inline x.Yield(att : list<Attribute>) =
            fun (s : 's) -> s, { children = AListCollector.Empty; attributes = AttributeTable att }

        member inline x.Yield(att : AttributeMap) =
            fun (s : 's) -> s, { children = AListCollector.Empty; attributes = AttributeTable att }

        member inline x.Yield(att : aval<#seq<Attribute>>) =
            fun (s : 's) -> s, { children = AListCollector.Empty; attributes = AttributeTable (AttributeMap.ofSeqA att) }
        
        member inline x.Yield(att : aval<Attribute>) =
            fun (s : 's) -> s, { children = AListCollector.Empty; attributes = AttributeTable (AttributeMap.ofAVal att) }

        member inline x.Yield(att : aval<option<Attribute>>) =
            fun (s : 's) -> s, { children = AListCollector.Empty; attributes = AttributeTable (AttributeMap.ofOptionA att)  }

