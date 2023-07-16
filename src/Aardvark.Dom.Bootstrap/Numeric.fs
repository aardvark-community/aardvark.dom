namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers
    


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type NumericState =
    {
        Min : aval<float>
        Max : aval<float>
        Step : aval<float>
        Value : option<aval<float>>
        ChangeWhileTyping : bool
        OnChange : list<float -> unit>
    }
    static member Empty =
        {
            Min = AVal.constant 0.0
            Max = AVal.constant 1.0
            Step = AVal.constant 0.01
            Value = None
            ChangeWhileTyping = true
            OnChange = []
        }
        
[<CompilerMessage("internal", 3180, IsHidden = true)>]
type NumericBuilder() =
    inherit StateNodeBuilder<NumericState, DomNode>()

    override x.Run(run : NumericState -> NumericState * AttributeMap * alist<DomNode>) =
        let state, att, cs = run NumericState.Empty

        let id = RandomElementId()
        let inline string (v : float) =
            v.ToString(System.Globalization.CultureInfo.InvariantCulture)
            
        let wrap (input : DomNode) =
            if cs.IsConstant && IndexList.isEmpty (AList.force cs) then
                input
            else
                div {
                    label { For id; cs }
                    input
                }
                
        wrap (
            input {
                att
                Type "number"
                Class "form-control"
                state.Min |> AVal.map (fun min -> Attribute("min", AttributeValue.String (string min)))
                state.Max |> AVal.map (fun max -> Attribute("max", AttributeValue.String (string max)))
                state.Step |> AVal.map (fun step -> Attribute("step", AttributeValue.String (string step)))
                match state.Value with
                | Some value -> value |> AVal.map (string >> Value)
                | None -> ()
            
                let change (value : string) =
                    match System.Double.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> state.OnChange |> List.iter (fun a -> a value)
                    | _ -> ()

                if state.ChangeWhileTyping then
                    Dom.OnInput(fun e ->
                        change e.Value
                    )
                else
                    Dom.OnChange(fun e ->
                        change e.Value
                    )
                Id id
            }
        )


type Numeric =
    static member Min(value : aval<float>) =
        fun (s : NumericState) -> { s with Min = value }
        
    static member Min(value : float) =
        fun (s : NumericState) -> { s with Min = AVal.constant value }
        
    static member Max(value : aval<float>) =
        fun (s : NumericState) -> { s with Max = value }
        
    static member Max(value : float) =
        fun (s : NumericState) -> { s with Max = AVal.constant value }
        
    static member Step(value : aval<float>) =
        fun (s : NumericState) -> { s with Step = value }
        
    static member Step(value : float) =
        fun (s : NumericState) -> { s with Step = AVal.constant value }
        
    static member Value(value : aval<float>) =
        fun (s : NumericState) -> { s with Value = Some value }
        
    static member OnChange(action : float -> unit) =
        fun (s : NumericState) -> { s with OnChange = action :: s.OnChange }

    static member ChangeWhileTyping(value : bool) =
        fun (s : NumericState) -> { s with ChangeWhileTyping = value }
        

[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module NumericBuilder =
    let numeric = NumericBuilder()

    let private test() =
        numeric {
            Numeric.Min 10.0
            Numeric.Max 1000.0
            Numeric.Value (AVal.constant 500.0)
            Numeric.OnChange(fun v -> printfn "Value: %f" v)
            Numeric.Step 1.0
            Numeric.ChangeWhileTyping false
            
            Style [ Width "100%" ]
            "Label"
        }