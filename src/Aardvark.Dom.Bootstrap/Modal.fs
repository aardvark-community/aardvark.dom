namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open Aardvark.Dom.NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type ModalState =
    {
        Header : NodeBuilderState
        Footer : NodeBuilderState
        Closable : bool
    }

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type ModalBuilder() =
    inherit StateNodeBuilder<ModalState, DomNode>()

    override x.Run(action : ModalState -> ModalState * AttributeMap * alist<DomNode>) =
        let state, att, children =
            action { Header = NodeBuilderState.empty;  Footer = NodeBuilderState.empty; Closable = true }
            
        let lid = RandomElementId()
            
        let root = att.Content |> AMap.filter (fun k _ -> k = "id") |> AttributeMap
        let dialog = att.Content |> AMap.filter (fun k _ -> k <> "id") |> AttributeMap
            
        div {
            Class "modal"
            TabIndex -1
            Attribute("role", AttributeValue.String "dialog")
            Attribute("aria-labelledby", AttributeValue.String $"#{lid}")
            if not state.Closable then Attribute("data-bs-backdrop", AttributeValue.String "static")
            root
            
            div {
                Class "modal-dialog"
                Attribute("role", AttributeValue.String "document")
                dialog
                
                div {
                    Class "modal-content"
                    div {
                        Class "modal-header"
                        h5 {
                            Class "modal-title"
                            Id lid
                            state.Header.attributes.ToAMap() |> AttributeMap
                            state.Header.children.ToAList()
                        }
                        if state.Closable then 
                            button {
                                Type "button"
                                Class "btn-close"
                                Attribute("data-bs-dismiss", AttributeValue.String "modal")
                                Attribute("aria-label", AttributeValue.String "close")
                            }
                    }
                    div {
                        Class "modal-body"
                        children
                    }
                    
                    let footerChildren = state.Footer.children.ToAList()
                    let footerEmpty =
                        footerChildren.IsConstant && AList.force(footerChildren).Count = 0
                    if not footerEmpty then
                        div {
                            Class "modal-footer"
                            state.Footer.attributes.ToAMap() |> AttributeMap
                            footerChildren
                        }
                }
            }
            
                
        }
           
[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module ModalBuilder =
    let modal = ModalBuilder()
    
module Modal =
    let Header =
        MapStateNodeBuilder (fun state ->
            fun (s : ModalState) ->
                { s with 
                    Header =
                        {
                            attributes = AttributeTable.Combine(s.Header.attributes, state.attributes)
                            children = NodeList.Combine(s.Header.children, state.children)
                        }
                }
        )
    let Footer =
        MapStateNodeBuilder (fun state ->
            fun (s : ModalState) ->
                { s with 
                    Footer =
                        {
                            attributes = AttributeTable.Combine(s.Footer.attributes, state.attributes)
                            children = NodeList.Combine(s.Footer.children, state.children)
                        }
                }
        )
    let Closable (value : bool) =
        fun (state : ModalState) ->
            { state with Closable = value }
    
    // let Id (value : string) =
    //     fun (state : ModalState) ->
    //         { state with Id = value }
    
            
            