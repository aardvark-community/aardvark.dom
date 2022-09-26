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
    abstract OnClose : IObservable<unit>

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
    | Int of value : int
    | Bool of value : bool
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

        | AttributeValue.Bool _ | AttributeValue.Int _ ->
            r

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
type StyleProperty(name : string, value : string) =
    member x.Name = name
    member x.Value = value


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
        let props = action() |> List.map (fun s -> $"{s.Name}: {s.Value}") |> String.concat "; "
        Attribute("style", AttributeValue.String props)

    member x.Run(action : unit -> list<aval<option<StyleProperty>>>) =
        let l = action()
        if l |> List.forall (fun c -> c.IsConstant) then
            let props = l |> List.choose AVal.force |> List.map (fun s -> $"{s.Name}: {s.Value}") |> String.concat "; "
            Attribute("style", AttributeValue.String props) |> AVal.constant
        else
            AVal.custom (fun token ->
                let props = l |> List.choose (fun c -> c.GetValue token)  |> List.map (fun s -> $"{s.Name}: {s.Value}") |> String.concat "; "
                Attribute("style", AttributeValue.String props)
            )

