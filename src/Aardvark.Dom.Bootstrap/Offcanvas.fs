namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open Aardvark.Dom.NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type OffcanvasState =
    {
        Header : NodeBuilderState
        Body : NodeBuilderState
        AllowBodyScroll : bool
        Backdrop : bool
        Keyboard : bool
    }
    
[<CompilerMessage("internal", 3180, IsHidden = true)>]
type OffcanasBuilder() =
    inherit StateNodeBuilder<OffcanvasState, DomNode>()

    override x.Run(action : OffcanvasState -> OffcanvasState * AttributeMap * alist<DomNode>) =
        let state, att, children =
            action { Header = NodeBuilderState.empty; Body = NodeBuilderState.empty; AllowBodyScroll = false; Backdrop = true; Keyboard = false }
            
        let lid = RandomElementId()
            
        div {
            Class "offcanvas offcanvas-start"
            att
                
                
            Attribute("data-bs-scroll", AttributeValue.String (if state.AllowBodyScroll then "true" else "false"))
            Attribute("data-bs-backdrop", AttributeValue.String (if state.Backdrop then "true" else "false"))
            Attribute("data-bs-keyboard", AttributeValue.String (if state.Keyboard then "true" else "false"))
            Attribute("aria-labelledby", AttributeValue.String lid)
                    
            div {
                Id lid
                Class "offcanvas-header"
                state.Header.attributes.ToAMap() |> AttributeMap
                h5 {
                    Class "offcanvas-title"
                    state.Header.children.ToAList()
                }
                button {
                    Class "btn-close text-reset"
                    Attribute("data-bs-dismiss", AttributeValue.String "offcanvas")
                    Attribute("aria-label", AttributeValue.String "Close")
                }
            }

            div {
                Class "offcanvas-body"
                state.Body.attributes.ToAMap() |> AttributeMap
                state.Body.children.ToAList()
                children
            }

                
        }
           
[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module OffcanvasBuilder =
    let offcanvas = OffcanasBuilder()

module Offcanvas =
    let Header =
        MapStateNodeBuilder (fun state ->
            fun (s : OffcanvasState) ->
                { s with 
                    Header =
                        {
                            attributes = AttributeTable.Combine(s.Header.attributes, state.attributes)
                            children = NodeList.Combine(s.Header.children, state.children)
                        }
                }
        )
            
    let Body =
        MapStateNodeBuilder (fun state ->
            fun (s : OffcanvasState) ->
                { s with 
                    Body =
                        {
                            attributes = AttributeTable.Combine(s.Body.attributes, state.attributes)
                            children = NodeList.Combine(s.Body.children, state.children)
                        }
                }
        )
    
    let AllowBodyScroll (scrolling : bool) =
        fun (s : OffcanvasState) -> { s with AllowBodyScroll = scrolling }
            
    let Backdrop (backdrop : bool) =
        fun (s : OffcanvasState) -> { s with Backdrop = backdrop }
 
    let Keyboard (keyboard : bool) =
        fun (s : OffcanvasState) -> { s with Keyboard = keyboard }

    let Toggle (id : string) =
        [
            Attribute("data-bs-toggle", AttributeValue.String "offcanvas")
            Attribute("data-bs-target", AttributeValue.String ("#" + id))
            Attribute("aria-controls", AttributeValue.String id)
        ]
      