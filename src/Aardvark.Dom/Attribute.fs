namespace Aardvark.Dom

open System

type EventCode =
    {
        Callback : option<System.Text.Json.JsonElement -> unit>
        Code : list<string>
    }

    static member Empty = { Callback = None; Code = [] }

[<RequireQualifiedAccess>]
type AttributeValue =
    | Set of values : Set<string>
    | String of value : string
    | Event of capture : EventCode * bubble : EventCode
    
    static member Merge(l : AttributeValue, r : AttributeValue) =
        match r with
        | AttributeValue.Set rv -> 
            match l with
            | AttributeValue.Set lv -> AttributeValue.Set (Set.union lv rv)
            | AttributeValue.String lv -> AttributeValue.Set (Set.add lv rv)
            | _ -> r
        | AttributeValue.String rv ->
            match l with
            | AttributeValue.Set lv -> AttributeValue.Set (Set.add rv lv)
            | AttributeValue.String _ -> r
            | _ -> r
        | AttributeValue.Event(rb, rc) ->
            match l with
            | AttributeValue.Event(lb, lc) ->

                let bubble =
                    match rb.Callback with
                    | Some rb ->
                        match lb.Callback with
                        | Some lb -> Some (fun e -> lb e; rb e)
                        | None -> Some rb
                    | None ->
                        lb.Callback

                let capture =
                    match rc.Callback with
                    | Some rc ->
                        match lc.Callback with
                        | Some lc -> Some (fun e -> lc e; rc e)
                        | None -> Some rc
                    | None ->
                        lc.Callback

                AttributeValue.Event(
                    { Callback = bubble; Code = lb.Code @ rb.Code },
                    { Callback = capture; Code = lc.Code @ rc.Code }
                )
            | _ ->
                r

[<Struct>]
type Attribute(name : string, value : AttributeValue) =
    member x.Name = name
    member x.Value = value

    static member Merge(l : Attribute, r : Attribute) =
        match l.Name with
        | "style" ->
            let newValue = 
                match l.Value with
                | AttributeValue.String ls ->
                    match r.Value with
                    | AttributeValue.String rs -> AttributeValue.String $"{ls}; {rs}"
                    | r -> AttributeValue.Merge(l.Value, r)
                | lv -> 
                    AttributeValue.Merge(lv, r.Value)
            Attribute(l.Name, newValue)
        | _ ->
            Attribute(l.Name, AttributeValue.Merge(l.Value, r.Value))


[<AbstractClass; Sealed; AutoOpen>]
type Att private() =

    static let code (preventDefault : option<bool>) (stopPropagation : option<bool>) =
        let mutable res = System.Text.StringBuilder()
        match preventDefault with
        | Some true -> res <- res.AppendLine "{0}.preventDefault();"
        | _ -> ()
        match stopPropagation with
        | Some true -> res <- res.AppendLine "{0}.stopPropagation();"
        | _ -> ()
        res.ToString()

    static member Id(id : string) = Attribute("id", AttributeValue.String id)
    static member Class(cls : string) = Attribute("classList", AttributeValue.Set (Set.singleton cls))
    static member Class(cls : #seq<string>) = Attribute("classList", AttributeValue.Set (Set.ofSeq cls))
    static member Style(style : string) = Attribute("style", AttributeValue.String style)

    static member Style(props : #seq<StyleProperty>) =
        let style = props |> Seq.map (fun p -> $"{p.Name}: {p.Value}") |> String.concat "; "
        Attribute("style", AttributeValue.String style)

    static member inline On<'a when 'a :> Event and 'a : (static member TryParse : System.Text.Json.JsonElement -> option<'a>)>(name : string, js : string, callback : 'a -> unit, ?useCapture : bool) =
        let run (e : System.Text.Json.JsonElement) =
            match (^a : (static member TryParse : System.Text.Json.JsonElement -> option<'a>) (e)) with
            | Some evt -> callback evt
            | None -> ()

        let code =
            if System.String.IsNullOrWhiteSpace js then []
            else [js]

        match useCapture with
        | Some true -> 
            Attribute(name, AttributeValue.Event({ Callback = Some run; Code = code}, { Callback = None; Code = [] }))
        | _ -> 
            Attribute(name, AttributeValue.Event({ Callback = None; Code = [] }, { Callback = Some run; Code = code}))

    static member inline On<'a when 'a :> Event and 'a : (static member TryParse : System.Text.Json.JsonElement -> option<'a>)>(name : string, callback : 'a -> unit, ?useCapture : bool) =
        Att.On(name, "", callback, ?useCapture = useCapture)

    static member OnContextMenu(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "contextmenu", 
            code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnClick(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "click", 
            code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnDoubleClick(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "dblclick", 
            code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )
    static member OnMouseDown(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "mousedown", 
            code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnMouseUp(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "mousedown", 
            code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnMouseMove(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "mousemove", 
            code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnPointerDown(callback : PointerEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "pointerdown", 
            "{0}.target.setPointerCapture({0}.pointerId);" + code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnPointerUp(callback : PointerEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "pointerup", 
            "{0}.target.releasePointerCapture({0}.pointerId);" + code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnPointerMove(callback : PointerEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?stopPropagation : bool) = 
        Att.On(
            "pointermove", 
            code preventDefault stopPropagation, 
            callback, 
            ?useCapture = useCapture
        )

    static member OnBoot(code : string) =
        Attribute("boot", AttributeValue.Event({ Code = [code]; Callback = None }, EventCode.Empty))

    static member OnShutdown(code : string) =
        Attribute("shutdown", AttributeValue.Event({ Code = [code]; Callback = None }, EventCode.Empty))

    static member OnBoot(code : #seq<string>) =
        Attribute("boot", AttributeValue.Event({ Code = [String.concat "\n" code]; Callback = None }, EventCode.Empty))

    static member OnShutdown(code : #seq<string>) =
        Attribute("shutdown", AttributeValue.Event({ Code = [String.concat "\n" code]; Callback = None }, EventCode.Empty))

    
open FSharp.Data.Adaptive

type StyleBuilder() =   
    member x.Yield(prop : StyleProperty) =
        [prop]


    member x.Yield(prop : aval<StyleProperty>) =
        [AVal.map Some prop]


    member x.Yield(prop : aval<option<StyleProperty>>) =
        [prop]

    member x.Combine(l : list<StyleProperty>, r : unit -> list<StyleProperty>) =
        l @ r()

    member x.Combine(l : list<aval<option<StyleProperty>>>, r : unit -> list<StyleProperty>) =
        l @ (r() |> List.map (Some >> AVal.constant))

    member x.Combine(l : list<aval<option<StyleProperty>>>, r : unit -> list<aval<option<StyleProperty>>>) =
        l @ r()

    member x.Combine(l : list<StyleProperty>, r : unit -> list<aval<option<StyleProperty>>>) =
        (l |> List.map (Some >> AVal.constant)) @ r()

    member x.Zero() : list<StyleProperty> =
        []

    member x.Delay(action : unit -> 'a) = action

    member x.Run(action : unit -> list<StyleProperty>) =
        action() |> Style

    member x.Run(action : unit -> list<aval<option<StyleProperty>>>) =
        let l = action()
        if l |> List.forall (fun c -> c.IsConstant) then
            l |> List.choose AVal.force |> Style |> AVal.constant
        else
            AVal.custom (fun token ->
                l |> List.choose (fun c -> c.GetValue token) |> Style 
            )


module private StyleBuilderTest =
    open Aardvark.Base

    let style = StyleBuilder()

    let a =
        style {
            Width "100%"
            AVal.constant (Height "100%")
            Background C4b.Red
        }

