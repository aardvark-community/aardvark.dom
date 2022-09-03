namespace Aardvark.Dom

open System
open System.Threading.Tasks

type ChannelMessage =
    | Text of string
    | Binary of byte[]
    | Close

type IChannel =
    abstract Send : ChannelMessage -> Task<unit>
    abstract Receive : unit -> Task<ChannelMessage>


type EventCode =
    {
        EventType : Type
        Callback : option<Event -> bool>
        PointerCapture : bool
        PreventDefault : bool
    }

    static member Empty = { EventType = typeof<Event>; Callback = None; PointerCapture = false; PreventDefault = false }

[<RequireQualifiedAccess>]
type AttributeValue =
    | Set of values : Set<string>
    | String of value : string
    | Event of capture : EventCode * bubble : EventCode
    | Execute of channels : array<IChannel -> Task<unit>> * code : (array<string> -> list<string>)
    
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
                        | Some lb -> Some (fun e -> lb e && rb e)
                        | None -> Some rb
                    | None ->
                        lb.Callback

                let capture =
                    match rc.Callback with
                    | Some rc ->
                        match lc.Callback with
                        | Some lc -> Some (fun e -> lc e && rc e)
                        | None -> Some rc
                    | None ->
                        lc.Callback

                let bubbleEvtType =
                    if lb.EventType.IsAssignableFrom rb.EventType then rb.EventType
                    elif rb.EventType.IsAssignableFrom lb.EventType then lb.EventType
                    else failwithf "inconsistent event-types: %A vs %A" lb.EventType rb.EventType
                    
                let captureEvtType =
                    if lc.EventType.IsAssignableFrom rc.EventType then rc.EventType
                    elif rc.EventType.IsAssignableFrom lc.EventType then lc.EventType
                    else failwithf "inconsistent event-types: %A vs %A" lc.EventType rc.EventType

                AttributeValue.Event(
                    { EventType = bubbleEvtType; Callback = bubble; PointerCapture = lb.PointerCapture || rb.PointerCapture; PreventDefault = lb.PreventDefault || rb.PreventDefault },
                    { EventType = captureEvtType; Callback = capture; PointerCapture = lc.PointerCapture || rc.PointerCapture; PreventDefault = lc.PreventDefault || rc.PreventDefault }
                )
            | _ ->
                r

        | AttributeValue.Execute(lc, ljs) ->
            match r with
            | AttributeValue.Execute(rc, rjs) -> 
                AttributeValue.Execute (Array.append lc rc, fun names -> ljs (Array.take lc.Length names) @ rjs (Array.skip lc.Length names))
            | r -> r

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

    static member inline On<'a when 'a :> Event>(name : string, callback : 'a -> bool, ?useCapture : bool, ?pointerCapture : bool, ?preventDefault : bool) =
        let pointerCapture = defaultArg pointerCapture false
        let preventDefault = defaultArg preventDefault false
        match useCapture with
        | Some true -> 
            Attribute(name, AttributeValue.Event({ EventType = typeof<'a>; Callback = Some (unbox<'a> >> callback); PointerCapture = pointerCapture; PreventDefault = preventDefault}, EventCode.Empty))
        | _ -> 
            Attribute(name, AttributeValue.Event(EventCode.Empty, { EventType = typeof<'a>; Callback = Some (unbox<'a> >> callback); PointerCapture = pointerCapture; PreventDefault = preventDefault }))

            
    static member inline On<'a when 'a :> Event>(name : string, callback : 'a -> unit, ?useCapture : bool, ?pointerCapture : bool, ?preventDefault : bool) =
        let pointerCapture = defaultArg pointerCapture false
        let preventDefault = defaultArg preventDefault false
        match useCapture with
        | Some true -> 
            Attribute(name, AttributeValue.Event({ EventType = typeof<'a>; Callback = Some (fun e -> callback (unbox e); true); PointerCapture = pointerCapture; PreventDefault = preventDefault}, EventCode.Empty))
        | _ -> 
            Attribute(name, AttributeValue.Event(EventCode.Empty, { EventType = typeof<'a>; Callback = Some (fun e -> callback (unbox e); true); PointerCapture = pointerCapture; PreventDefault = preventDefault }))

    static member OnContextMenu(callback : MouseEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "contextmenu", 
            callback, 
            ?useCapture = useCapture
        )
        
    static member OnContextMenu(callback : MouseEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "contextmenu", 
            callback, 
            ?useCapture = useCapture
        )

    static member OnMouseEnter(callback : MouseEvent -> unit) = 
        Att.On(
            "mouseenter", 
            callback
        )
        
    static member OnMouseLeave(callback : MouseEvent -> unit) = 
        Att.On(
            "mouseleave", 
            callback
        )

    static member OnClick(callback : MouseEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "click", 
            callback, 
            ?useCapture = useCapture
        )
        
    static member OnClick(callback : MouseEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "click", 
            callback, 
            ?useCapture = useCapture
        )

    static member OnDoubleClick(callback : MouseEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "dblclick", 
            callback, 
            ?useCapture = useCapture
        )
    
    static member OnDoubleClick(callback : MouseEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "dblclick", 
            callback, 
            ?useCapture = useCapture
        )
    
    static member OnMouseDown(callback : MouseEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "mousedown", 
            callback, 
            ?useCapture = useCapture
        )
        
    static member OnMouseDown(callback : MouseEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "mousedown", 
            callback, 
            ?useCapture = useCapture
        )

    static member OnMouseUp(callback : MouseEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "mousedown", 
            callback, 
            ?useCapture = useCapture
        )
        
    static member OnMouseUp(callback : MouseEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "mousedown", 
            callback, 
            ?useCapture = useCapture
        )

    static member OnMouseMove(callback : MouseEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "mousemove", 
            callback, 
            ?useCapture = useCapture
        )
    
    static member OnMouseMove(callback : MouseEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "mousemove", 
            callback, 
            ?useCapture = useCapture
        )
    
    static member OnMouseWheel(callback : WheelEvent -> unit, ?useCapture : bool) =
        Att.On(
            "wheel", 
            callback, 
            ?useCapture = useCapture
        )

    static member OnMouseWheel(callback : WheelEvent -> bool, ?useCapture : bool) =
        Att.On(
            "wheel", 
            callback, 
            ?useCapture = useCapture
        )
        

    static member OnPointerDown(callback : PointerEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "pointerdown", 
            callback, 
            pointerCapture = true,
            ?useCapture = useCapture
        )
        
    static member OnPointerDown(callback : PointerEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "pointerdown", 
            callback, 
            pointerCapture = true,
            ?useCapture = useCapture
        )

    static member OnPointerUp(callback : PointerEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "pointerup", 
            callback,
            pointerCapture = true,
            ?useCapture = useCapture
        )
        
    static member OnPointerUp(callback : PointerEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "pointerup", 
            callback,
            pointerCapture = true,
            ?useCapture = useCapture
        )

    static member OnPointerMove(callback : PointerEvent -> unit, ?useCapture : bool) = 
        Att.On(
            "pointermove", 
            callback, 
            ?useCapture = useCapture
        )
        
    static member OnPointerMove(callback : PointerEvent -> bool, ?useCapture : bool) = 
        Att.On(
            "pointermove", 
            callback, 
            ?useCapture = useCapture
        )

    static member Require(url : string) = 
        Attribute("require", AttributeValue.Set (Set.singleton url))

    static member Require(urls : #seq<string>) =
        Attribute("require", AttributeValue.Set (Set.ofSeq urls))

    static member OnBoot(code : string) =
        Attribute("boot", AttributeValue.Execute([||], fun _ -> [code]))

    static member OnShutdown(code : string) =
        Attribute("shutdown", AttributeValue.Execute([||], fun _ -> [code]))

    static member OnBoot(code : #seq<string>) =
        Attribute("boot", AttributeValue.Execute([||], fun _ -> [String.concat "\n" code]))

    static member OnShutdown(code : #seq<string>) =
        Attribute("shutdown", AttributeValue.Execute([||], fun _ -> [String.concat "\n" code]))

    
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

