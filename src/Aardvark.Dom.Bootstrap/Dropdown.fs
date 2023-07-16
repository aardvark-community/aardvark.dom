namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers
    


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type DropdownItem<'k> =
    | Separator
    | Item of 'k * AttributeMap * alist<DomNode>


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type DropdownState<'k> =
    {
        Selected : option<aval<option<'k>>>
        OnSelect : list<option<'k> -> unit>
        Placeholder : option<DomNode>
        CustomClass : option<string>
    }

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type DropdownBuilder<'k when 'k : equality>() =
    inherit StatefulNodeLikeBuilder<DropdownState<'k>, DropdownItem<'k>, DomNode>()
    
    override x.Run(state : DropdownState<'k> -> DropdownState<'k> * NodeLikeBuilderState<DropdownItem<'k>>) =
        let state, res = 
            state { Selected = None; OnSelect = []; Placeholder = None; CustomClass = None }
            
        let children = res.children.ToAList()
        let att = res.attributes.ToAMap() |> AttributeMap
        
        let current = 
            match state.Selected with
            | Some s -> s
            | None -> AVal.constant None
        let update sel =
            state.OnSelect |> List.iter (fun a -> a sel)
            
        let placeholder = state.Placeholder
        
        let elements =
            children |> AList.map (function 
                | DropdownItem.Item(k, att, cs) ->
                    
                    Some k, Some (att, cs)
                | DropdownItem.Separator ->
                    //let node =
                    //    div { 
                    //        Class "dropdown-divider"
                    //    }
                    None, None
            )
        
        let map = elements |> AList.toASet |> ASet.choose (function (Some k, v) -> Some (k, v) | _ -> None) |> AMap.ofASet
            

        let selectedNodes =
            current |> AVal.bind (function  
                | Some s -> 
                    AMap.tryFind s map |> AVal.map (function Some s -> s | None -> HashSet.empty)
                    |> AVal.map (HashSet.choose id)
                | None -> 
                    AVal.constant HashSet.empty
            )
    
        let selected =
            selectedNodes |> AList.bind (fun sel ->
                if HashSet.isEmpty sel then
                    match placeholder with
                    | Some p ->
                        AList.single p
                    | None -> 
                        AList.empty
                else
                    sel 
                    |> Seq.map (fun (att, cs) ->
                        a { att; cs }
                    ) 
                    |> AList.ofSeq
            )
        
        let id = RandomElementId()
        div {
            Dom.Class "btn-group"
            att
            button {
                let btnClass =
                    match state.CustomClass with
                    | Some c -> c
                    | None -> "btn btn-sm btn-secondary dropdown-toggle"
                Dom.Class btnClass; Attribute("data-bs-toggle", AttributeValue.String "dropdown")
                Style [ Display "flex"; StyleProperty("align-items", "center"); StyleProperty("justify-content", "space-between") ]
                Id id
                OnBoot "__THIS__.addEventListener('focus', function() { __THIS__.blur(); });"
                selected
            }
            div {
                Dom.Class "dropdown-menu"; Attribute("aria-labelledby", AttributeValue.String id)
                Dom.Style [ Css.Cursor "pointer" ]
                elements |> AList.map (fun (key, content) ->
                    match content with
                    | Some (att, content) ->
                        a { 
                            att
                            match key with
                            | Some key ->
                                Dom.OnClick(fun e ->
                                    
                                    update (Some key)
                                )
                                current |> AVal.map (function Some s when s = key -> Some (Class "active") | _ -> None)
                            | _ ->
                                ()
                            
                            Dom.Class "dropdown-item"
                            content 
                        }
                    | None ->
                        div { 
                            Dom.Class "dropdown-divider"
                        }
                
                )
            }
        }
    
[<CompilerMessage("internal", 3180, IsHidden = true)>]
type DropdownItemBuilder<'k>(key : 'k) =
    inherit MapStateNodeBuilder<DropdownItem<'k>>(fun s -> 
        let att = s.attributes.ToAMap() |> AttributeMap
        let cs = s.children.ToAList()
        DropdownItem.Item(key, att, cs)
        
    )

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type DropdownPlaceholderBuilder<'k>() =
    inherit MapStateNodeBuilder<DropdownState<'k> -> DropdownState<'k>>(fun s -> 
        let att = s.attributes.ToAMap() |> AttributeMap
        let cs = s.children.ToAList()
        let placeholder =
            div {
                att
                cs
            }
        fun (s : DropdownState<'k>) -> { s with Placeholder = Some placeholder }
        
    )
    

type Dropdown =   
    static member Item (key : 'k) = DropdownItemBuilder<'k>(key)
    
    static member Selected (key : aval<option<'k>>) =
        fun (s : DropdownState<'k>) -> { s with Selected = Some key }
        
    static member OnSelect (action : option<'k> -> unit) =
        fun (s : DropdownState<'k>) -> { s with OnSelect = action :: s.OnSelect }
        
    static member CustomClass (name : string) =
        fun (s : DropdownState<'k>) -> { s with CustomClass = Some name }
        
        
module Dropdown =
    [<GeneralizableValue>]
    let Separator<'k> : DropdownItem<'k> = DropdownItem<'k>.Separator
    
    [<GeneralizableValue>]
    let Placeholder<'k> : DropdownPlaceholderBuilder<'k> =
        DropdownPlaceholderBuilder<'k>()


[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module DropdownBuilder =

    let dropdown<'k when 'k : equality> = DropdownBuilder<'k>()

    let private test() =
       dropdown {
           AttributeMap.empty
           Dropdown.Selected (AVal.constant (Some 4))
           Dropdown.OnSelect (fun v -> printfn "%A" v)
           Dropdown.Placeholder { "Place" }
           Dropdown.Item 10 {
               "hans"
           }
           Dropdown.Separator
           Dropdown.Item 5 {
               "hans"
           }
       }