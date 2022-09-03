namespace Aardvark.Dom

open System
open Aardvark.Base
open Aardvark.Rendering

[<AutoOpen>]
module private EventParserUtilities =
    open System.Text.Json
    open FSharp.Reflection



    type OptionBuilder() =
        member inline x.Zero () = Some ()

        member inline x.Bind(m : option<'a>, [<InlineIfLambda>] f : 'a -> option<'b>) =
            match m with
            | Some m -> f m
            | None -> None
        member inline x.Return(value : 'a) =
            Some value

    let opt = OptionBuilder()

    let private someCreators = System.Collections.Concurrent.ConcurrentDictionary<System.Type, obj -> obj>()

    let private createSome (t : System.Type) (value : obj) =
        let creator =
            someCreators.GetOrAdd(t, System.Func<_,_>(fun t ->
                let tOpt = typedefof<option<_>>.MakeGenericType [|t|]
                let ci = FSharpType.GetUnionCases(tOpt) |> Array.find (fun ci -> ci.Name = "Some")
                let ctor = FSharpValue.PreComputeUnionConstructor ci
                fun o -> ctor [|o|]
            ))
        creator value

    module JSON =
        let parse (str : string) =
            let doc = JsonDocument.Parse(str)
            doc.RootElement

    let (?) (o : JsonElement) (name : string) : 'a =
        if typeof<'a> = typeof<JsonElement> then
            match o.TryGetProperty name with
            | (true, prop) -> prop :> obj :?> 'a
            | _ -> JsonElement() :> obj :?> 'a
        elif typedefof<'a> = typedefof<option<_>> then
            let t = typeof<'a>.GetGenericArguments().[0]
            match o.TryGetProperty name with
            | (true, prop) ->
                match prop.ValueKind with
                | JsonValueKind.Null | JsonValueKind.Undefined ->
                    if t.IsValueType then  null :> obj :?> 'a
                    elif t = typeof<double> then Some System.Double.NaN :> obj :?> 'a
                    elif t = typeof<float32> then Some System.Single.NaN :> obj :?> 'a
                    else createSome t null :?> 'a  

                | JsonValueKind.Number ->
                    if t = typeof<double> then Some (prop.GetDouble()) :> obj :?> 'a
                    elif t = typeof<float32> then Some (prop.GetSingle()) :> obj :?> 'a
                    elif t = typeof<decimal> then Some (prop.GetDecimal()) :> obj :?> 'a
                    elif t = typeof<uint8> then Some (prop.GetByte()) :> obj :?> 'a
                    elif t = typeof<uint16> then Some (prop.GetUInt16()) :> obj :?> 'a
                    elif t = typeof<uint32> then Some (prop.GetUInt32()) :> obj :?> 'a
                    elif t = typeof<uint64> then Some (prop.GetUInt64()) :> obj :?> 'a
                    elif t = typeof<int8> then Some (prop.GetSByte()) :> obj :?> 'a
                    elif t = typeof<int16> then Some (prop.GetInt16()) :> obj :?> 'a
                    elif t = typeof<int32> then Some (prop.GetInt32()) :> obj :?> 'a
                    elif t = typeof<int64> then Some (prop.GetInt64()) :> obj :?> 'a
                    else Unchecked.defaultof<'a>

                | JsonValueKind.String ->
                    if t = typeof<string> then prop.GetString() |> Some :> obj :?> 'a
                    else Unchecked.defaultof<'a>

                | JsonValueKind.True ->
                    if t = typeof<bool> then Some true :> obj :?> 'a
                    else Unchecked.defaultof<'a>

                | JsonValueKind.False ->
                    if t = typeof<bool> then Some false :> obj :?> 'a
                    else Unchecked.defaultof<'a>

                | JsonValueKind.Object ->
                    if t = typeof<JsonElement> then Some prop :> obj :?> 'a
                    else Unchecked.defaultof<'a>

                | JsonValueKind.Array ->
                    Unchecked.defaultof<'a>

                | _ ->  
                    Unchecked.defaultof<'a>

            | _ ->
                Unchecked.defaultof<'a>
        else
            Unchecked.defaultof<'a>

[<AbstractClass>]
type Event(target : string, timeStamp : float, isTrusted : bool, typ : string) =
    member x.Target = target
    member x.TimeStamp = timeStamp
    member x.IsTrusted = isTrusted
    member x.Type = typ

type Button =
    | Left = 0
    | Middle = 1
    | Right = 2
    | Button4 = 3
    | Button5 = 4

[<Flags>]
type Buttons =
    | None = 0
    | Left = 1
    | Middle = 2
    | Right = 4
    | Button4 = 8
    | Button5 = 16

type MouseEvent(    
        target : string, timeStamp : float, 
        isTrusted : bool, typ : string, 
        clientX : float, clientY : float,
        screenX : float, screenY : float,
        pageX : float, pageY : float,
        offsetX : float, offsetY : float,
        movementX : float, movementY : float,
        ctrlKey : bool, shiftKey : bool, altKey : bool, metaKey : bool,
        button : Button, buttons : Buttons
    ) =
    inherit Event(target, timeStamp, isTrusted, typ)

    static member TryParse(str : System.Text.Json.JsonElement) =
        opt {
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = str?target

            let! (screenX : float) = str?screenX
            let! (screenY : float) = str?screenY
            let! (clientX : float) = str?clientX
            let! (clientY : float) = str?clientY
            let! (pageX : float) = str?pageX
            let! (pageY : float) = str?pageY
            let! (offsetX : float) = str?offsetX
            let! (offsetY : float) = str?offsetY
            // let! (x : float) = str?x
            // let! (y : float) = str?y
            let! (movementX : float) = str?movementX
            let! (movementY : float) = str?movementY
            let! (ctrlKey : bool) = str?ctrlKey
            let! (shiftKey : bool) = str?shiftKey
            let! (altKey : bool) = str?altKey
            let! (metaKey : bool) = str?metaKey
            let! (button : int) = str?button
            let! (buttons : int) = str?buttons

            return
                MouseEvent(
                    target, timeStamp, isTrusted, typ,
                    clientX, clientY, screenX, screenY,
                    pageX, pageY, offsetX, offsetY,
                    movementX, movementY, ctrlKey, shiftKey, altKey, metaKey,
                    unbox button, unbox buttons
                )
        }

    member x.ClientPosition = V2i(clientX, clientY)
    member x.PagePosition = V2i(pageX, pageY)
    member x.OffsetPosition = V2i(offsetX, offsetY)
    member x.Movement = V2i(movementX, movementY)

    member x.ClientX = clientX
    member x.ClientY = clientY
    member x.ScreenX = screenX
    member x.ScreenY = screenY
    member x.PageX = pageX
    member x.PageY = pageY
    member x.OffsetX = offsetX
    member x.OffsetY = offsetY
    member x.MovementX = movementX
    member x.MovementY = movementY
    member x.Ctrl = ctrlKey
    member x.Shift = shiftKey
    member x.Alt = altKey
    member x.Meta = metaKey
    member x.Button = button
    member x.Buttons = buttons

type WheelDeltaMode =
    | Pixel = 0
    | Line = 1
    | Page = 2

type WheelEvent(
        target : string, timeStamp : float, 
        isTrusted : bool, typ : string, 
        clientX : float, clientY : float,
        screenX : float, screenY : float,
        pageX : float, pageY : float,
        offsetX : float, offsetY : float,
        movementX : float, movementY : float,
        ctrlKey : bool, shiftKey : bool, altKey : bool, metaKey : bool,
        button : Button, buttons : Buttons,
        deltaX : float, deltaY : float, deltaZ : float, deltaMode : WheelDeltaMode
    ) =
    inherit Event(target, timeStamp, isTrusted, typ)

    static member TryParse(str : System.Text.Json.JsonElement) =
        opt {
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = str?target

            let! (screenX : float) = str?screenX
            let! (screenY : float) = str?screenY
            let! (clientX : float) = str?clientX
            let! (clientY : float) = str?clientY
            let! (pageX : float) = str?pageX
            let! (pageY : float) = str?pageY
            let! (offsetX : float) = str?offsetX
            let! (offsetY : float) = str?offsetY
            // let! (x : float) = str?x
            // let! (y : float) = str?y
            let! (movementX : float) = str?movementX
            let! (movementY : float) = str?movementY
            let! (ctrlKey : bool) = str?ctrlKey
            let! (shiftKey : bool) = str?shiftKey
            let! (altKey : bool) = str?altKey
            let! (metaKey : bool) = str?metaKey
            let! (button : int) = str?button
            let! (buttons : int) = str?buttons

            let! (dx : float) = str?deltaX
            let! (dy : float) = str?deltaY
            let! (dz : float) = str?deltaZ
            let! (dm : int) = str?deltaMode

            return
                WheelEvent(
                    target, timeStamp, isTrusted, typ,
                    clientX, clientY, screenX, screenY,
                    pageX, pageY, offsetX, offsetY,
                    movementX, movementY, ctrlKey, shiftKey, altKey, metaKey,
                    unbox button, unbox buttons,
                    dx, dy, dz, unbox dm
                )
        }

    member x.ClientPosition = V2i(clientX, clientY)
    member x.PagePosition = V2i(pageX, pageY)
    member x.OffsetPosition = V2i(offsetX, offsetY)
    member x.Movement = V2i(movementX, movementY)
    member x.Delta = V3i(deltaX, deltaY, deltaZ)

    member x.ClientX = clientX
    member x.ClientY = clientY
    member x.ScreenX = screenX
    member x.ScreenY = screenY
    member x.PageX = pageX
    member x.PageY = pageY
    member x.OffsetX = offsetX
    member x.OffsetY = offsetY
    member x.MovementX = movementX
    member x.MovementY = movementY
    member x.Ctrl = ctrlKey
    member x.Shift = shiftKey
    member x.Alt = altKey
    member x.Meta = metaKey
    member x.Button = button
    member x.Buttons = buttons
    member x.DeltaX = deltaX
    member x.DeltaY = deltaY
    member x.DeltaZ = deltaZ
    member x.DeltaMode = deltaMode

type PointerEvent(  
        target : string, timeStamp : float, 
        isTrusted : bool, typ : string, 
        clientX : float, clientY : float,
        screenX : float, screenY : float,
        pageX : float, pageY : float,
        offsetX : float, offsetY : float,
        movementX : float, movementY : float,
        ctrlKey : bool, shiftKey : bool, altKey : bool, metaKey : bool,
        button : Button, buttons : Buttons,

        pointerId : int, width : float, height : float, pressure : float,
        tiltX : float, tiltY : float, pointerType : string
    ) =
    inherit Event(
        target, timeStamp, isTrusted, typ
    )
    
    member x.ClientPosition = V2i(clientX, clientY)
    member x.PagePosition = V2i(pageX, pageY)
    member x.OffsetPosition = V2i(offsetX, offsetY)
    member x.Movement = V2i(movementX, movementY)

    member x.ClientX = clientX
    member x.ClientY = clientY
    member x.ScreenX = screenX
    member x.ScreenY = screenY
    member x.PageX = pageX
    member x.PageY = pageY
    member x.OffsetX = offsetX
    member x.OffsetY = offsetY
    member x.MovementX = movementX
    member x.MovementY = movementY
    member x.Ctrl = ctrlKey
    member x.Shift = shiftKey
    member x.Alt = altKey
    member x.Meta = metaKey
    member x.Button = button
    member x.Buttons = buttons
    member x.PointerId = pointerId
    member x.Width = width
    member x.Height = height
    member x.Pressure = pressure
    member x.TiltX = tiltX
    member x.TiltY = tiltY
    member x.PointerType = pointerType
    

    static member TryParse(str : System.Text.Json.JsonElement) =

        opt {
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = str?target

            let! (screenX : float) = str?screenX
            let! (screenY : float) = str?screenY
            let! (clientX : float) = str?clientX
            let! (clientY : float) = str?clientY
            let! (pageX : float) = str?pageX
            let! (pageY : float) = str?pageY
            let! (offsetX : float) = str?offsetX
            let! (offsetY : float) = str?offsetY
            // let! (x : float) = str?x
            // let! (y : float) = str?y
            let! (movementX : float) = str?movementX
            let! (movementY : float) = str?movementY
            let! (ctrlKey : bool) = str?ctrlKey
            let! (shiftKey : bool) = str?shiftKey
            let! (altKey : bool) = str?altKey
            let! (metaKey : bool) = str?metaKey
            let! (button : int) = str?button
            let! (buttons : int) = str?buttons


            let! (pointerId : int) = str?pointerId
            let (width : option<float>) = str?width
            let (height : option<float>) = str?height
            let (pressure : option<float>) = str?pressure
            let (tiltX : option<float>) = str?tiltX
            let (tiltY : option<float>) = str?tiltY
            let (pointerType : option<string>) = str?pointerType

            return
                PointerEvent(
                    target, timeStamp, isTrusted, typ,
                    clientX, clientY, screenX, screenY,
                    pageX, pageY, offsetX, offsetY,
                    movementX, movementY, ctrlKey, shiftKey, altKey, metaKey,
                    unbox button, unbox buttons,
                    pointerId, defaultArg width 1.0, defaultArg height 1.0, defaultArg pressure 0.0,
                    defaultArg tiltX 0.0, defaultArg tiltY 0.0, defaultArg pointerType ""
                )
        }
