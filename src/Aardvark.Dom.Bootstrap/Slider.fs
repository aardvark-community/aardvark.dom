namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open NodeBuilderHelpers
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers


[<CompilerMessage("internal", 3180, IsHidden = true)>]
type SliderState =
    {
        Min : aval<float>
        Max : aval<float>
        Step : aval<float>
        Value : option<aval<float>>
        ChangeWhileDragging : bool
        OnChange : list<float -> unit>
    }
    static member Empty =
        {
            Min = AVal.constant 0.0
            Max = AVal.constant 1.0
            Step = AVal.constant 0.01
            Value = None
            ChangeWhileDragging = true
            OnChange = []
        }
        
[<CompilerMessage("internal", 3180, IsHidden = true)>]
type SliderBuilder() =
    inherit StateNodeBuilder<SliderState, DomNode>()

    static member Build(state : SliderState, att : AttributeMap, cs : alist<DomNode>) =
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
                Type "range"
                Class "form-range"
                
                let list =
                    (state.Min, state.Max, state.Step) |||> AVal.bind3 (fun min max step ->
                        match state.Value with
                        | Some v ->
                            v |> AVal.map (fun v -> [min; max; step; v])
                        | None ->
                            AVal.constant [min; max; step]
                    )
                    
                AVal.channel list (fun name ->
                    [
                        $"__THIS__.min = {name}[0];"
                        $"__THIS__.max = {name}[1];"
                        $"__THIS__.step = {name}[2];"
                        $"if({name}.length > 3) {{ __THIS__.value = {name}[3]; }}"
                    ]
                )
                    
                
                // OnBoot [
                //     "setTimeout(function() {"
                //     $"__THIS__.min = {AVal.force state.Min}"
                //     $"__THIS__.max = {AVal.force state.Max}"
                //     $"__THIS__.step = {AVal.force state.Step}"
                //     match state.Value with
                //     | Some v ->
                //         $"console.warn('asdasdasd', {AVal.force state.Min}, {AVal.force state.Max}, {AVal.force state.Step}, {AVal.force v});"
                //         $"__THIS__.value = {AVal.force v}"
                //     | None ->
                //         ()
                //     "}, 0);"
                // ]
                
                // state.Min |> AVal.map (fun min -> Attribute("min", AttributeValue.String (string min)))
                // state.Max |> AVal.map (fun max -> Attribute("max", AttributeValue.String (string max)))
                // state.Step |> AVal.map (fun step -> Attribute("step", AttributeValue.String (string step)))
                // match state.Value with
                // | Some value ->
                //     // AVal.channel (AVal.map string value) (fun str ->
                //     //     [
                //     //         "__THIS__.value = " + str + ";"
                //     //     ]    
                //     // )
                //     // value |> AVal.map (string >> Value)
                //     value |> AVal.map (fun v -> Attribute("value", AttributeValue.String (string v)))
                // | None -> ()
                
                
            
                let change (value : string) =
                    match System.Double.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> state.OnChange |> List.iter (fun a -> a value)
                    | _ -> ()

                if state.ChangeWhileDragging then
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

    override x.Run(run : SliderState -> SliderState * AttributeMap * alist<DomNode>) =
        let state, att, cs = run SliderState.Empty
        SliderBuilder.Build(state, att, cs)


type Slider =
    static member Min(value : aval<float>) =
        fun (s : SliderState) -> { s with Min = value }
        
    static member Min(value : float) =
        fun (s : SliderState) -> { s with Min = AVal.constant value }
        
    static member Max(value : aval<float>) =
        fun (s : SliderState) -> { s with Max = value }
        
    static member Max(value : float) =
        fun (s : SliderState) -> { s with Max = AVal.constant value }
        
    static member Step(value : aval<float>) =
        fun (s : SliderState) -> { s with Step = value }
        
    static member Step(value : float) =
        fun (s : SliderState) -> { s with Step = AVal.constant value }
        
    static member Value(value : aval<float>) =
        fun (s : SliderState) -> { s with Value = Some value }
        
    static member OnChange(action : float -> unit) =
        fun (s : SliderState) -> { s with OnChange = action :: s.OnChange }

    static member ChangeWhileDragging(value : bool) =
        fun (s : SliderState) -> { s with ChangeWhileDragging = value }
        

[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module SliderBuilder =
    let slider = SliderBuilder()

    let private test() =
        slider {
            Slider.Min 10.0
            Slider.Max 1000.0
            Slider.Value (AVal.constant 500.0)
            Slider.OnChange(fun v -> printfn "Value: %f" v)
            Slider.Step 1.0
            Slider.ChangeWhileDragging false
            
            Style [ Width "100%" ]
            "Label"
        }