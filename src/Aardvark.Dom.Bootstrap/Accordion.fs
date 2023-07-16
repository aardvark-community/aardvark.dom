namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open Aardvark.Dom.NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type AccordionItemState =
    {
        Active : bool
        Header : NodeBuilderState
        Body : NodeBuilderState
    }
    
[<CompilerMessage("internal", 3180, IsHidden = true)>]
type AccordionState =
    {
        Id : string
        AllowMultiOpen : bool
    }


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type AccordionItemBuilder() =
    inherit StateNodeBuilder<AccordionItemState, AccordionState -> DomNode>()

    override x.Run(action : AccordionItemState -> AccordionItemState * AttributeMap * alist<DomNode>) =
        fun (parentState : AccordionState) ->
            let state, att, children =
                action { Active = false;  Header = NodeBuilderState.empty; Body = NodeBuilderState.empty }
            let sid = RandomElementId()
            let hid = RandomElementId()
            let id = parentState.Id

            let item = 
                div {
                    Class "accordion-item"
                    att
                    h2 {
                        Id hid
                        Class "accordion-header"
                        button {
                            if state.Active then Class "accordion-button accordion-button-sm"
                            else Class "accordion-button collapsed accordion-button-sm"
                            
                            Attribute("data-bs-toggle", AttributeValue.String "collapse")
                            Attribute("data-bs-target", AttributeValue.String $"#{sid}")
                            Attribute("aria-controls", AttributeValue.String $"#{sid}")
                            Attribute("aria-expanded", AttributeValue.String (if state.Active then "true" else "false"))
                            state.Header.attributes.ToAMap() |> AttributeMap
                            state.Header.children.ToAList()
                        }
                    }

                    div {
                        Id sid
                        Class (
                            if state.Active then "accordion-collapse collapse show"
                            else "accordion-collapse collapse"
                        )
                        Attribute("aria-labelledby", AttributeValue.String hid)
                        if not parentState.AllowMultiOpen then
                            Attribute("data-bs-parent", AttributeValue.String $"#{id}")
                        div {
                            Class "accordion-body"
                            state.Body.attributes.ToAMap() |> AttributeMap
                            state.Body.children.ToAList()
                            children
                        }
                                
                    }
                }
            
            item

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type AccordionBuilder() =
    inherit StateNodeBuilder<AccordionState, DomNode>()


    override x.Run(action : AccordionState -> AccordionState * AttributeMap * alist<DomNode>) =
        let id = RandomElementId()
        let _state, att, children =
            action { AllowMultiOpen = false; Id = id }
        div {
            att
            Class "accordion"
            Id id
            children           
        }
    

module AccordionItem =
    let Header =
        MapStateNodeBuilder(fun s ->
            fun (state : AccordionItemState) -> { state with Header = s }
        )

    let Title (str : string) =
        Header { str }
        
    let Body =
        MapStateNodeBuilder(fun s ->
            fun (state : AccordionItemState) -> { state with Body = s }
        )

    let Active (a : bool) =     
        fun (state : AccordionItemState) -> { state with Active = a }

module Accordion =
    let Item = AccordionItemBuilder()
    
    let AllowMultiOpen (allow : bool) =
        fun (state : AccordionState) -> { state with AllowMultiOpen = allow }
        


                  
[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module AccordionBuilder =
    let accordion = AccordionBuilder()
    
    let test() =
        accordion {
            Accordion.AllowMultiOpen false
            
            Accordion.Item {
                AccordionItem.Active true
                AccordionItem.Header { "hans" }
                "BlaBla"
            }   
            
            Accordion.Item {
                AccordionItem.Active true
                AccordionItem.Header { "sepp" }
                "Foobar"
            }   

            AList.single 1 |> AList.map (fun v ->
                Accordion.Item {
                    AccordionItem.Active true
                    AccordionItem.Header { string v }
                    "Foobar"
                }   
            )

        }
