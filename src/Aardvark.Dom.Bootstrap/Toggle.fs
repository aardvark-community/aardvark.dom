namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers
    


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type ToggleState =
    {
        Checked : option<aval<bool>>
        OnChange : list<bool -> unit>
    }


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type ToggleBuilder() =
    inherit StateNodeBuilder<ToggleState, DomNode>()
    
    override x.Run(state : ToggleState -> ToggleState * AttributeMap * alist<DomNode>) =
        let state, att, cs = 
            state { Checked = None; OnChange = [] }
            
        let id = RandomElementId()

        div {
            Class "form-check form-switch"
            att
            
            input {
                Class "form-check-input" 
                Type "checkbox"
                match state.OnChange with
                | [] -> ()
                | change ->
                    OnChange (fun e -> change |> List.iter (fun a -> a e.Checked))
                match state.Checked with
                | Some state ->
                    AVal.map Checked state
                | None ->
                    ()
                Id id
            }
            let isEmpty = cs.IsConstant && IndexList.isEmpty (AList.force cs)
            if not isEmpty then
                label {
                    Class "form-check-label"
                    For id
                    cs
                }
        }
        
type Toggle =
    static member Checked (value : aval<bool>) =
        fun (s : ToggleState) -> { s with Checked = Some value }
    static member OnChange (action : bool -> unit) =
        fun (s : ToggleState) -> { s with OnChange = action :: s.OnChange }


[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module ToggleBuilder =

    let toggle = ToggleBuilder()
