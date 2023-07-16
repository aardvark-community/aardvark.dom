namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers
    
type SelectOption<'k> =
    {
        Key : 'k
        Attributes : AttributeMap
        Children : alist<DomNode>
    }

type SelectState<'k> =
    {
        OnChange : list<'k -> unit>
        Selected : option<aval<'k>>
    }

type SelectOptionBuilder<'k>(key : 'k) =
    inherit MapNodeBuilder<SelectOption<'k>>(fun att n ->
        { Key = key; Attributes = att; Children = n }
    )

type SelectBuilder<'k when 'k : equality>() =
    inherit StatefulNodeLikeBuilder<SelectState<'k>, SelectOption<'k>, DomNode>()

    override x.Run(run : SelectState<'k> -> SelectState<'k> * NodeLikeBuilderState<SelectOption<'k>>) =
        let state, res = run { OnChange = []; Selected = None }
        
        let options = res.children.ToAList()
        let atts = res.attributes.ToAMap() |> AttributeMap

        let options = 
            options |> AList.map (fun o ->
                let value = RandomElementId()
                value, o
            )

        let toKey =
            options
            |> AList.toASet
            |> ASet.map (fun (v, e) -> v, e.Key)
            |> AMap.ofASet
            
        let fromKey =
            options
            |> AList.toASet
            |> ASet.map (fun (v, e) -> e.Key, v)
            |> AMap.ofASet


        select {
            Class "form-select"
            atts
            match state.OnChange with
            | [] -> ()
            | actions ->
                OnChange (fun e ->
                    let map = toKey |> AMap.force
                    match HashMap.tryFind e.Value map with
                    | Some ks when not (HashSet.isEmpty ks) ->
                        let k = Seq.head ks
                        actions |> List.iter (fun a -> a k)
                    | _ ->
                        ()
                )

            match state.Selected with
            | Some v ->
                let value =
                    v 
                    |> AVal.bind (fun v -> AMap.tryFind v fromKey)
                    |> AVal.map (function Some m -> m | None -> HashSet.empty)
                    |> AVal.map (fun s -> if s.IsEmpty then "" else Seq.head s)

                value |> AVal.map Value
            | None ->
                ()

            options |> AList.map (fun (value, element) ->
                option {
                    Value value
                    element.Attributes
                    element.Children
                }
            )
        }


module Select =
    let Option (key : 'k) = SelectOptionBuilder<'k>(key)

    let Value (value : aval<'k>) = 
        fun (s : SelectState<'k>) -> { s with Selected = Some value }, NodeLikeBuilderState.Empty
        
    let OnChange (action : 'k -> unit) = 
        fun (s : SelectState<'k>) -> { s with OnChange = action :: s.OnChange }, NodeLikeBuilderState.Empty
        
[<AutoOpen>]
module SelectBuilder =
    [<GeneralizableValue>]
    let select<'k when 'k : equality> = SelectBuilder<'k>()
    
    [<GeneralizableValue>]
    let sel<'k when 'k : equality> = SelectBuilder<'k>()

    let private test() =
        select {
            Class "form-select"
            Select.Value (AVal.constant 10)
            Select.OnChange (fun a -> ())
            
            Select.Option 10 {
                "Hello"
            }
            Select.Option 25 {
                "Hello"
            }
        }
    