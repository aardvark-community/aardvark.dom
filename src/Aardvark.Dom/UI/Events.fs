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

type Event(target : string, timeStamp : float, isTrusted : bool, typ : string, clientRect : Box2d) =
    member x.Target = target
    member x.TimeStamp = timeStamp
    member x.IsTrusted = isTrusted
    member x.Type = typ
    member x.ClientRect = clientRect
    
    static member TryParse(str : System.Text.Json.JsonElement) =
        opt {
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = str?target?id

            let! (rect : System.Text.Json.JsonElement) = str?clientRect
            let! (x : float) = rect?x
            let! (y : float) = rect?y
            let! (w : float) = rect?width
            let! (h : float) = rect?height
            let clientRect = Box2d.FromMinAndSize(V2d(x,y), V2d(w,h))

            return
                Event(
                    target, timeStamp, isTrusted, typ, clientRect
                )
        }

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
        isTrusted : bool, typ : string, clientRect : Box2d,
        clientX : float, clientY : float,
        screenX : float, screenY : float,
        pageX : float, pageY : float,
        offsetX : float, offsetY : float,
        movementX : float, movementY : float,
        ctrlKey : bool, shiftKey : bool, altKey : bool, metaKey : bool,
        button : Button, buttons : Buttons
    ) =
    inherit Event(target, timeStamp, isTrusted, typ, clientRect)

    static member TryParse(str : System.Text.Json.JsonElement) =
        opt {
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = str?target?id

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

            let! (rect : System.Text.Json.JsonElement) = str?clientRect
            let! (x : float) = rect?x
            let! (y : float) = rect?y
            let! (w : float) = rect?width
            let! (h : float) = rect?height
            let clientRect = Box2d.FromMinAndSize(V2d(x,y), V2d(w,h))

            return
                MouseEvent(
                    target, timeStamp, isTrusted, typ, clientRect,
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
        isTrusted : bool, typ : string, clientRect : Box2d,
        clientX : float, clientY : float,
        screenX : float, screenY : float,
        pageX : float, pageY : float,
        offsetX : float, offsetY : float,
        movementX : float, movementY : float,
        ctrlKey : bool, shiftKey : bool, altKey : bool, metaKey : bool,
        button : Button, buttons : Buttons,
        deltaX : float, deltaY : float, deltaZ : float, deltaMode : WheelDeltaMode
    ) =
    inherit Event(target, timeStamp, isTrusted, typ, clientRect)

    static member TryParse(str : System.Text.Json.JsonElement) =
        opt {
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = str?target?id

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
            
            let! (rect : System.Text.Json.JsonElement) = str?clientRect
            let! (x : float) = rect?x
            let! (y : float) = rect?y
            let! (w : float) = rect?width
            let! (h : float) = rect?height
            let clientRect = Box2d.FromMinAndSize(V2d(x,y), V2d(w,h))

            return
                WheelEvent(
                    target, timeStamp, isTrusted, typ, clientRect,
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
        isTrusted : bool, typ : string, clientRect : Box2d,
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
        target, timeStamp, isTrusted, typ, clientRect
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
            let! (target : string) = str?target?id

            let! (screenX : float) = str?screenX
            let! (screenY : float) = str?screenY
            let! (clientX : float) = str?clientX
            let! (clientY : float) = str?clientY
            let! (pageX : float) = str?pageX
            let! (pageY : float) = str?pageY
            let! (offsetX : float) = str?offsetX
            let! (offsetY : float) = str?offsetY
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
            
            let! (rect : System.Text.Json.JsonElement) = str?clientRect
            let! (x : float) = rect?x
            let! (y : float) = rect?y
            let! (w : float) = rect?width
            let! (h : float) = rect?height
            let clientRect = Box2d.FromMinAndSize(V2d(x,y), V2d(w,h))

            return
                PointerEvent(
                    target, timeStamp, isTrusted, typ, clientRect,
                    clientX, clientY, screenX, screenY,
                    pageX, pageY, offsetX, offsetY,
                    movementX, movementY, ctrlKey, shiftKey, altKey, metaKey,
                    unbox button, unbox buttons,
                    pointerId, defaultArg width 1.0, defaultArg height 1.0, defaultArg pressure 0.0,
                    defaultArg tiltX 0.0, defaultArg tiltY 0.0, defaultArg pointerType ""
                )
        }
        
type ChangeEvent(  
        target : string, timeStamp : float, 
        isTrusted : bool, typ : string, clientRect : Box2d, nodeType : string, value : string, isChecked : bool) =
    inherit Event(target, timeStamp, isTrusted, typ, clientRect)

    member x.Value = value
    member x.Checked = isChecked
    member x.NodeType = nodeType
    static member TryParse(str : System.Text.Json.JsonElement) =

        opt {
            let targetObj = str?target
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = targetObj?id

            let! (nodeType : string) = targetObj?``type``
            
            let isChecked =
                match targetObj.TryGetProperty "checked" with
                | (true, v) -> v.GetBoolean()
                | _ -> false
            let value =
                match targetObj.TryGetProperty "value" with
                | (true, v) -> v.GetString()
                | _ -> ""

            let! (rect : System.Text.Json.JsonElement) = str?clientRect
            let! (x : float) = rect?x
            let! (y : float) = rect?y
            let! (w : float) = rect?width
            let! (h : float) = rect?height
            let clientRect = Box2d.FromMinAndSize(V2d(x,y), V2d(w,h))
            
            return
                ChangeEvent(
                    target, timeStamp, isTrusted, typ, clientRect,
                    nodeType, value, isChecked
                )
        }

type InputEvent(  
        target : string, timeStamp : float, 
        isTrusted : bool, typ : string, clientRect : Box2d, nodeType : string, value : string, isChecked : bool, data : string, inputType : string) =
    inherit ChangeEvent(target, timeStamp, isTrusted, typ, clientRect, nodeType, value, isChecked)
    
    member x.Data = data
    member x.InputType = inputType

    static member TryParse(str : System.Text.Json.JsonElement) =

        opt {
            let targetObj = str?target
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = targetObj?id

            let! (nodeType : string) = targetObj?``type``
            
            let isChecked =
                match targetObj.TryGetProperty "checked" with
                | (true, v) -> v.GetBoolean()
                | _ -> false
            let value =
                match targetObj.TryGetProperty "value" with
                | (true, v) -> v.GetString()
                | _ -> ""

            let! (rect : System.Text.Json.JsonElement) = str?clientRect
            let! (x : float) = rect?x
            let! (y : float) = rect?y
            let! (w : float) = rect?width
            let! (h : float) = rect?height
            let clientRect = Box2d.FromMinAndSize(V2d(x,y), V2d(w,h))
            
            let! data = str?data
            let! inputType = str?inputType

            return
                InputEvent(
                    target, timeStamp, isTrusted, typ, clientRect,
                    nodeType, value, isChecked, data, inputType
                )
        }


type KeyboardEvent(
        target : string, timeStamp : float, isTrusted : bool, typ : string, clientRect : Box2d,
        code : string, isComposing : bool, key : string, location : KeyLocation, repeat : bool, 
        ctrlKey : bool, shiftKey : bool, altKey : bool, metaKey : bool
    ) =
    inherit  Event(target, timeStamp, isTrusted, typ, clientRect)
    
    member x.Code = code
    member x.IsComposing = isComposing
    member x.Key = key
    member x.KeyLocation = location
    member x.Repeat = repeat
    member x.Ctrl = ctrlKey
    member x.Shift = shiftKey
    member x.Alt = altKey
    member x.Meta = metaKey
    

    static member TryParse(str : System.Text.Json.JsonElement) =

        opt {
            let! (isTrusted : bool) = str?isTrusted
            let! (typ : string) = str?``type``
            let! (timeStamp : float) = str?timeStamp
            let! (target : string) = str?target?id
            
            let! (ctrlKey : bool) = str?ctrlKey
            let! (shiftKey : bool) = str?shiftKey
            let! (altKey : bool) = str?altKey
            let! (metaKey : bool) = str?metaKey
            
            let! (code : string) = str?code
            let! (isComposing : bool) = str?isComposing
            let! (key : string) = str?key
            let! (location : int) = str?location
            let! (repeat : bool) = str?repeat
            

            let! (rect : System.Text.Json.JsonElement) = str?clientRect
            let! (x : float) = rect?x
            let! (y : float) = rect?y
            let! (w : float) = rect?width
            let! (h : float) = rect?height
            let clientRect = Box2d.FromMinAndSize(V2d(x,y), V2d(w,h))

            return
                KeyboardEvent(
                    target, timeStamp, isTrusted, typ, clientRect,
                    code, isComposing, key, unbox<KeyLocation> location, repeat,
                    ctrlKey, shiftKey, altKey, metaKey
                )
        }
        