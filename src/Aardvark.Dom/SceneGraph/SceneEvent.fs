namespace Aardvark.Dom

open Aardvark.Base
open Aardvark.Application
open FSharp.Data.Adaptive
open Aardvark.Rendering

type KeyLocation =
    | Standard = 0
    | Left = 1
    | Right = 2
    | Numpad = 3
  
type Button =
    | None = -1
    | Left = 0
    | Middle = 1
    | Right = 2
    | Button4 = 3
    | Button5 = 4
  
[<RequireQualifiedAccess>]
type SceneEventKind =
    | PointerDown
    | PointerUp
    | PointerMove

    | Scroll
    | Click
    | DoubleClick

    | Tap
    | DoubleTap
    | LongPress

    | PointerEnter
    | PointerLeave

    | FocusEnter
    | FocusLeave

    | KeyDown
    | KeyUp
    | KeyInput
    
type SceneEventLocation(modelTrafo : aval<Trafo3d>, local2World : Trafo3d, viewTrafo : Trafo3d, projTrafo : Trafo3d, pixel : V2d, viewportSize : V2i, depth : float, viewNormal : V3d) =
    let ndc = V3d(2.0 * pixel.X / float viewportSize.X - 1.0, 1.0 - 2.0 * pixel.Y / float viewportSize.Y, depth)
    let viewProj = viewTrafo * projTrafo
    let worldPosition = viewProj.Backward.TransformPosProj ndc
    let worldNormal = viewTrafo.Forward.Transposed.TransformDir viewNormal |> Vec.normalize
    
    let localPosition = local2World.Backward.TransformPosProj worldPosition
    let localNormal = local2World.Forward.Transposed.TransformDir worldNormal |> Vec.normalize

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
    member x.ModelPosition = (AVal.force modelTrafo).Backward.TransformPosProj worldPosition
    member x.ModelNormal = (AVal.force modelTrafo).Forward.Transposed.TransformDir worldNormal |> Vec.normalize
    member x.Position = localPosition
    member x.Normal = localNormal
    
    member x.Transformed(trafo : Trafo3d) =
        SceneEventLocation(
            modelTrafo, 
            trafo * local2World, 
            viewTrafo, 
            projTrafo, 
            pixel, 
            viewportSize, 
            depth, 
            viewNormal
        )

    new(modelTrafo : aval<Trafo3d>, viewTrafo : Trafo3d, projTrafo : Trafo3d, pixel : V2d, viewportSize : V2i, depth : float, viewNormal : V3d) =
        SceneEventLocation(modelTrafo, Trafo3d.Identity, viewTrafo, projTrafo, pixel, viewportSize, depth, viewNormal)

type IEventHandler =
    abstract HasPointerCapture : state : obj * pointerId : int -> bool
    abstract SetPointerCapture : state : obj * pointerId : int -> unit
    abstract ReleasePointerCapture : state : obj * pointerId : int -> unit
    abstract SetFocus : option<obj> -> unit
    abstract HandlePointerEvent : SceneEventKind * pixel : V2i * ctrl : bool * shift : bool * alt : bool * meta : bool * scrollDelta : V2d * pointerId : int * button : Button -> bool 
    abstract HandleKeyEvent : SceneEventKind * ctrl : bool * shift : bool * alt : bool * meta : bool * code : string * key : string * keyLocation : KeyLocation * text : string * isRepeat : bool -> bool 
    abstract Cursor : aval<option<string>>
    
[<AbstractClass>]
type SceneEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, time : float) =
    member x.Context = context
    member x.This = self
    member x.Target = target
    member x.Kind = kind
    member x.Location = location
    
    member x.TimeStamp = time
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
    member x.Position = location.Position
    member x.Normal = location.Normal
    
    member x.Transformed(trafo : Trafo3d) =
        x.WithLocation(location.Transformed(trafo))

    abstract WithKind : SceneEventKind -> SceneEvent
    abstract WithLocation : SceneEventLocation -> SceneEvent
 

type ScenePointerEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, time : float, ctrl : bool, shift : bool, alt : bool, meta : bool, scrollDelta : V2d, pointerId : int, button : Button) =
    inherit SceneEvent(context, self, target, kind, location, time)
    member x.PointerId = pointerId
    member x.Button = button
    
    member x.Ctrl = ctrl
    member x.Shift = shift
    member x.Alt = alt
    member x.Meta = meta
    member x.ScrollDelta = scrollDelta
    
    override x.WithKind(kind : SceneEventKind) =
        ScenePointerEvent(context, self, target, kind, location, time, ctrl, shift, alt, meta, scrollDelta, pointerId, button) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        ScenePointerEvent(context, self, target, kind, location, time, ctrl, shift, alt, meta, scrollDelta, pointerId, button) :> SceneEvent

type SceneKeyboardEvent(
        context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, time : float,
        ctrl : bool, shift : bool, alt : bool, meta : bool, 
        code : string, key : string, keyLocation : KeyLocation, text : string, isRepeat : bool
    ) =
    inherit SceneEvent(context, self, target, kind, location, time)
    
    
    member x.Ctrl = ctrl
    member x.Shift = shift
    member x.Alt = alt
    member x.Meta = meta
    member x.Code = code
    member x.Key = key
    member x.KeyLocation = keyLocation
    member x.Text = text
    member x.IsRepeat = isRepeat

    override x.WithKind(kind : SceneEventKind) =
        SceneKeyboardEvent(context, self, target, kind, location, time, ctrl, shift, alt, meta, code, key, keyLocation, text, isRepeat) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        SceneKeyboardEvent(context, self, target, kind, location, time, ctrl, shift, alt, meta, code, key, keyLocation, text, isRepeat) :> SceneEvent
        



type SceneEventHandler =
    {
        Capture : list<SceneEvent -> bool>
        Bubble : list<SceneEvent -> bool>
    }

module SceneEventHandler =
    let bubble action = { Capture = []; Bubble = [action] }
    let capture action = { Capture = [action]; Bubble = [] }


    let transform (t : aval<Trafo3d>) (h : SceneEventHandler) =
        if t.IsConstant && AVal.force(t).Forward.IsIdentity() then 
            h
        else
            {
                Capture = h.Capture |> List.map (fun cb e -> cb (e.Transformed(AVal.force t)))
                Bubble = h.Bubble |> List.map (fun cb e -> cb (e.Transformed (AVal.force t)))
            }

    let merge (l : SceneEventHandler) (r : SceneEventHandler) =
        {
            Capture = l.Capture @ r.Capture
            Bubble = l.Bubble @ r.Bubble
        }


        
[<RequireQualifiedAccess>]
type RenderControlEventKind =
    | Resize
    | PreRender
    | PostRender

type RenderControlEventInfo =
    {
        Signature   : IFramebufferSignature
        Size        : V2i
        FrameIndex  : int
        Time        : MicroTime
        FrameTime   : MicroTime
    }

type RenderControlEvent(kind : RenderControlEventKind, info : RenderControlEventInfo) =
    member x.Kind = kind
    member x.Info = info

    static member Resize(info : RenderControlEventInfo) =
        RenderControlEvent(RenderControlEventKind.Resize, info)
    static member PreRender(info : RenderControlEventInfo) =
        RenderControlEvent(RenderControlEventKind.PreRender, info)
    static member PostRender(info : RenderControlEventInfo) =
        RenderControlEvent(RenderControlEventKind.PostRender, info)

[<AutoOpen>]
module RenderControlEvent =

    let (|Resize|PreRender|PostRender|) (e : RenderControlEvent) =
        match e.Kind with
        | RenderControlEventKind.Resize -> Resize(e.Info)
        | RenderControlEventKind.PreRender -> PreRender(e.Info)
        | RenderControlEventKind.PostRender -> PostRender(e.Info)