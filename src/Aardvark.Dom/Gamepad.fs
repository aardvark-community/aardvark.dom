namespace Aardvark.Dom


open System
open Aardvark.Base
open FSharp.Data.Adaptive


type GamepadAxisChangeEvent(    
        json : option<System.Text.Json.JsonElement>,
        controllerId : string,
        axisIndex : int,
        value : float,
        previousValue : float,
        target : string, timeStamp : float, 
        isTrusted : bool, typ : string, clientRect : Box2d
    ) =
    inherit Event(json, target, timeStamp, isTrusted, typ, clientRect)

    static let xBoxAxisNames = [| "LeftStickX"; "LeftStickY"; "RightStickX"; "RightStickY" |]
    
    member x.ControllerId = controllerId
    member x.AxisIndex = axisIndex
    member x.AxisName = 
        if axisIndex >= 0 && axisIndex < xBoxAxisNames.Length then
            xBoxAxisNames.[axisIndex]
        else
            "Unknown"
    member x.Value = value
    member x.PreviousValue = previousValue

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
            
            
            let! (detail : System.Text.Json.JsonElement) = str?detail
            
            let! (axisIndex : int) = detail?axisIndex
            let! (value : float) = detail?value
            let! (previousValue : float) = detail?previousValue
            let! (controllerId : string) = detail?controllerId
            
            
            return GamepadAxisChangeEvent(Some str, controllerId, axisIndex, value, previousValue, target, timeStamp, isTrusted, typ, clientRect)
        }

type GamepadButtonEvent(    
        json : option<System.Text.Json.JsonElement>,
        controllerId : string,
        buttonIndex : int,
        target : string, timeStamp : float, 
        isTrusted : bool, typ : string, clientRect : Box2d
    ) =
    inherit Event(json, target, timeStamp, isTrusted, typ, clientRect)

    static let xBoxNames = [| "A"; "B"; "X"; "Y"; "LB"; "RB"; "LT"; "RT"; "Select"; "Start"; "LS"; "RS"; "DPadUp"; "DPadDown"; "DPadLeft"; "DPadRight" |]
    
    member x.ControllerId = controllerId
    member x.ButtonIndex = buttonIndex

    member x.ButtonName = 
        if buttonIndex >= 0 && buttonIndex < xBoxNames.Length then
            xBoxNames.[buttonIndex]
        else
            "Unknown"
    
    
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
            
            
            let! (detail : System.Text.Json.JsonElement) = str?detail
            
            let! (buttonIndex : int) = detail?buttonIndex
            let! (controllerId : string) = detail?controllerId
            
            
            return GamepadButtonEvent(Some str, controllerId, buttonIndex, target, timeStamp, isTrusted, typ, clientRect)
        }


