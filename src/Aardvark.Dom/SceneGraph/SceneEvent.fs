namespace Aardvark.Dom

open Aardvark.Base
open Aardvark.Application
open FSharp.Data.Adaptive

[<RequireQualifiedAccess>]
type SceneEventKind =
    | PointerDown
    | PointerUp
    | PointerMove

    | Scroll
    | Click
    | DoubleClick

    | PointerEnter
    | PointerLeave

    | FocusEnter
    | FocusLeave

    | KeyDown
    | KeyUp
    | KeyInput
    
type SceneEventLocation(modelTrafo : Trafo3d, viewTrafo : Trafo3d, projTrafo : Trafo3d, pixel : V2d, viewportSize : V2i, depth : float, viewNormal : V3d) =
    let ndc = V3d(2.0 * pixel.X / float viewportSize.X - 1.0, 1.0 - 2.0 * pixel.Y / float viewportSize.Y, depth)
    let viewProj = viewTrafo * projTrafo
    let worldPosition = viewProj.Backward.TransformPosProj ndc
    let worldNormal = viewTrafo.Forward.Transposed.TransformDir viewNormal |> Vec.normalize
    
    let modelPosition = modelTrafo.Backward.TransformPosProj worldPosition
    let modelNormal = modelTrafo.Forward.Transposed.TransformDir worldNormal |> Vec.normalize

    member x.ModelTrafo = modelTrafo
    member x.ViewTrafo = viewTrafo
    member x.ProjTrafo = projTrafo
    member x.ViewProjTrafo = viewProj
    member x.Pixel = pixel
    member x.ViewportSize = viewportSize
    member x.Depth = depth
    member x.ViewNormal = viewNormal
    member x.WorldPosition = worldPosition
    member x.WorldNormal = worldNormal
    member x.ModelPosition = modelPosition
    member x.ModelNormal = modelNormal
    
    
type IEventHandler =
    abstract HasPointerCapture : state : obj * pointerId : int -> bool
    abstract SetPointerCapture : state : obj * pointerId : int -> unit
    abstract ReleasePointerCapture : state : obj * pointerId : int -> unit
    abstract SetFocus : option<obj> -> unit
    abstract HandlePointerEvent : SceneEventKind * pixel : V2i * ctrl : bool * shift : bool * alt : bool * meta : bool * scrollDelta : V2d * pointerId : int * button : int -> bool 
    abstract HandleKeyEvent : SceneEventKind * ctrl : bool * shift : bool * alt : bool * meta : bool * key : Keys * text : string * isRepeat : bool -> bool 
    abstract Cursor : aval<option<Cursor>>
    
[<AbstractClass>]
type SceneEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation) =
    member x.Context = context
    member x.This = self
    member x.Target = target
    member x.Kind = kind
    member x.Location = location
    
    
    member x.ModelTrafo = location.ModelTrafo
    member x.ViewTrafo = location.ViewTrafo
    member x.ProjTrafo = location.ProjTrafo
    member x.ViewProjTrafo = location.ViewProjTrafo
    member x.Pixel = location.Pixel
    member x.ViewportSize = location.ViewportSize
    member x.Depth = location.Depth
    member x.ViewNormal = location.ViewNormal
    member x.WorldPosition = location.WorldPosition
    member x.WorldNormal = location.WorldNormal
    member x.ModelPosition = location.ModelPosition
    member x.ModelNormal = location.ModelNormal
    
    abstract WithKind : SceneEventKind -> SceneEvent
    
type ScenePointerEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, ctrl : bool, shift : bool, alt : bool, meta : bool, scrollDelta : V2d, pointerId : int, button : int) =
    inherit SceneEvent(context, self, target, kind, location)
    member x.PointerId = pointerId
    member x.Button = button
    
    member x.Ctrl = ctrl
    member x.Shift = shift
    member x.Alt = alt
    member x.Meta = meta
    member x.ScrollDelta = scrollDelta
    
    override x.WithKind(kind : SceneEventKind) =
        ScenePointerEvent(context, self, target, kind, location, ctrl, shift, alt, meta, scrollDelta, pointerId, button) :> SceneEvent

type SceneKeyboardEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, ctrl : bool, shift : bool, alt : bool, meta : bool, key : Keys, text : string, isRepeat : bool) =
    inherit SceneEvent(context, self, target, kind, location)
    
    
    member x.Ctrl = ctrl
    member x.Shift = shift
    member x.Alt = alt
    member x.Meta = meta
    member x.Key = key
    member x.Text = text
    member x.IsRepeat = isRepeat

    override x.WithKind(kind : SceneEventKind) =
        SceneKeyboardEvent(context, self, target, kind, location, ctrl, shift, alt, meta, key, text, isRepeat) :> SceneEvent
        


type SceneEventHandler =
    {
        Capture : list<SceneEvent -> bool>
        Bubble : list<SceneEvent -> bool>
    }

module SceneEventHandler =
    let bubble action = { Capture = []; Bubble = [action] }
    let capture action = { Capture = [action]; Bubble = [] }

    let merge (l : SceneEventHandler) (r : SceneEventHandler) =
        {
            Capture = l.Capture @ r.Capture
            Bubble = l.Bubble @ r.Bubble
        }


