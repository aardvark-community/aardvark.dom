namespace rec Aardvark.Dom

open Aardvark.Base
open Aardvark.Application
open FSharp.Data.Adaptive
open Aardvark.Rendering

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
    
type SceneEventLocation(modelTrafo : aval<Trafo3d>, local2World : Trafo3d, viewTrafo : Trafo3d, projTrafo : Trafo3d, pixel : V2d, viewportSize : V2i, viewPos : V3d, viewNormal : V3d) =
    
    let ndc = projTrafo.TransformPosProj viewPos
    //let ndc = V3d(2.0 * pixel.X / float viewportSize.X - 1.0, 1.0 - 2.0 * pixel.Y / float viewportSize.Y, depth)
    let viewProj = viewTrafo * projTrafo
    let worldPosition = viewTrafo.Backward.TransformPosProj viewPos
    let worldNormal = viewTrafo.Forward.Transposed.TransformDir viewNormal |> Vec.normalize
    
    let localPosition = local2World.Backward.TransformPosProj worldPosition
    let localNormal = local2World.Forward.Transposed.TransformDir worldNormal |> Vec.normalize

    member x.ModelTrafo = modelTrafo
    member x.ViewTrafo = viewTrafo
    member x.ProjTrafo = projTrafo
    member x.ViewProjTrafo = viewProj
    member x.Pixel = pixel
    member x.ViewportSize = viewportSize
    member x.Depth = ndc.Z
    member x.ViewNormal = viewNormal
    member x.WorldPosition = worldPosition
    member x.ViewPosition = viewPos
    member x.WorldNormal = worldNormal
    member x.ModelPosition = (AVal.force modelTrafo).Backward.TransformPosProj worldPosition
    member x.ModelNormal = (AVal.force modelTrafo).Forward.Transposed.TransformDir worldNormal |> Vec.normalize
    member x.Position = localPosition
    member x.Normal = localNormal
    
    member x.ViewPickRay =
        let dir = projTrafo.Backward.TransformPosProj(V3d(ndc.XY, -1.0)) |> Vec.normalize
        Ray3d(V3d.Zero, dir)
        
    member x.WorldPickRay =
        let near = viewProj.Backward.TransformPosProj(V3d(ndc.XY, -1.0))
        let far = viewProj.Backward.TransformPosProj(V3d(ndc.XY, 0.0))
        Ray3d(near, Vec.normalize (far - near))
        
    member x.PickRay =
        let mvp = AVal.force modelTrafo * viewProj
        let near = mvp.Backward.TransformPosProj(V3d(ndc.XY, -1.0))
        let far = mvp.Backward.TransformPosProj(V3d(ndc.XY, 0.0))
        Ray3d(near, Vec.normalize (far - near))
    
    member x.Transformed(trafo : Trafo3d) =
        SceneEventLocation(
            modelTrafo, 
            trafo * local2World, 
            viewTrafo, 
            projTrafo, 
            pixel, 
            viewportSize, 
            viewPos, 
            viewNormal
        )

    new(modelTrafo : aval<Trafo3d>, viewTrafo : Trafo3d, projTrafo : Trafo3d, pixel : V2d, viewportSize : V2i, viewPos : V3d, viewNormal : V3d) =
        SceneEventLocation(modelTrafo, Trafo3d.Identity, viewTrafo, projTrafo, pixel, viewportSize, viewPos, viewNormal)

type IEventHandler =
    abstract HasPointerCapture : state : obj * pointerId : int -> bool
    abstract SetPointerCapture : state : obj * pointerId : int -> unit
    abstract ReleasePointerCapture : state : obj * pointerId : int -> unit
    abstract SetFocus : option<obj> -> unit
    abstract HandlePointerEvent : SceneEventKind * PointerEvent -> bool 
    abstract HandleWheelEvent : SceneEventKind * WheelEvent -> bool 
    abstract HandleKeyEvent : SceneEventKind * KeyboardEvent -> bool 
    abstract HandleInputEvent : SceneEventKind * InputEvent -> bool 
    abstract HandleTapEvent : SceneEventKind * TapEvent -> bool
    
    abstract DispatchPointerEvent : target : obj * event : ScenePointerEvent -> bool
    
    abstract Cursor : aval<option<string>>
    
    abstract Size : V2i 
    abstract Read : pixel : V2i * kind : SceneEventKind -> option<SceneEventLocation>
    
[<AbstractClass>]
type SceneEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : Event) =
    member x.Context = context
    member x.This = self
    member x.Target = target
    member x.Kind = kind
    member x.Location = location
    
    member x.Original = original
    member x.TimeStamp = original.TimeStamp
    member x.IsTrusted = original.IsTrusted
    member x.Type = original.Type
    member x.ClientRect = original.ClientRect
    
    member x.ModelTrafo = location.ModelTrafo
    member x.ViewTrafo = location.ViewTrafo
    member x.ProjTrafo = location.ProjTrafo
    member x.ViewProjTrafo = location.ViewProjTrafo
    member x.Pixel = location.Pixel
    member x.ViewportSize = location.ViewportSize
    member x.Depth = location.Depth
    member x.ViewNormal = location.ViewNormal
    member x.WorldPosition = location.WorldPosition
    member x.ViewPosition = location.ViewPosition
    member x.WorldNormal = location.WorldNormal
    member x.ModelPosition = location.ModelPosition
    member x.ModelNormal = location.ModelNormal
    member x.Position = location.Position
    member x.Normal = location.Normal
    
    member x.Transformed(trafo : Trafo3d) =
        x.WithLocation(location.Transformed(trafo))

    abstract WithKind : SceneEventKind -> SceneEvent
    abstract WithLocation : SceneEventLocation -> SceneEvent

type SceneMouseEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : MouseEvent) =
    inherit SceneEvent(context, self, target, kind, location, original)
    
    member x.Ctrl = original.Ctrl
    member x.Shift = original.Shift
    member x.Alt = original.Alt
    member x.Meta = original.Meta
    member x.Button = original.Button
    
    override x.WithKind(kind : SceneEventKind) =
        SceneMouseEvent(context, self, target, kind, location, original) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        SceneMouseEvent(context, self, target, kind, location, original) :> SceneEvent
    
type SceneWheelEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : WheelEvent) =
    inherit SceneMouseEvent(context, self, target, kind, location, original)
    
    member x.Original = original
    member x.DeltaX = original.DeltaX
    member x.DeltaY = original.DeltaY
    member x.DeltaZ = original.DeltaZ
    member x.DeltaMode = original.DeltaMode
    member x.Delta = original.Delta
    
    override x.WithKind(kind : SceneEventKind) =
        SceneWheelEvent(context, self, target, kind, location, original) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        SceneWheelEvent(context, self, target, kind, location, original) :> SceneEvent

type ScenePointerEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : PointerEvent) =
    inherit SceneMouseEvent(context, self, target, kind, location, original)
    
    member x.Original = original
    member x.PointerId = original.PointerId
    member x.Width = original.Width
    member x.Height = original.Height
    member x.Pressure = original.Pressure
    member x.TiltX = original.TiltX
    member x.TiltY = original.TiltY
    member x.PointerType = original.PointerType
    
    override x.WithKind(kind : SceneEventKind) =
        ScenePointerEvent(context, self, target, kind, location, original) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        ScenePointerEvent(context, self, target, kind, location, original) :> SceneEvent
        
type SceneTapEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : TapEvent) =
    inherit ScenePointerEvent(context, self, target, kind, location, original)
    
    member x.Original = original
    member x.DeltaTime = original.DeltaTime
    member x.Movement = original.Movement
    
    override x.WithKind(kind : SceneEventKind) =
        SceneTapEvent(context, self, target, kind, location, original) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        SceneTapEvent(context, self, target, kind, location, original) :> SceneEvent

type internal PlainSceneEvent(context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : Event) =
    inherit SceneEvent(context, self, target, kind, location, original)
    
    override x.WithKind(kind : SceneEventKind) =
        PlainSceneEvent(context, self, target, kind, location, original) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        PlainSceneEvent(context, self, target, kind, location, original) :> SceneEvent

type SceneInputEvent(
        context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : InputEvent
    ) =
    inherit SceneEvent(context, self, target, kind, location, original)
    
    member x.Original = original
    member x.Checked = original.Checked
    member x.Value = original.Value
    member x.NodeType = original.NodeType
    member x.Data = original.Data
    member x.InputType = original.InputType

    override x.WithKind(kind : SceneEventKind) =
        SceneInputEvent(context, self, target, kind, location, original) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        SceneInputEvent(context, self, target, kind, location, original) :> SceneEvent
        
type SceneKeyboardEvent(
        context : IEventHandler, self : obj, target : obj, kind : SceneEventKind, location : SceneEventLocation, original : KeyboardEvent
    ) =
    inherit SceneEvent(context, self, target, kind, location, original)
    
    member x.Original = original
    member x.Code = original.Code
    member x.IsComposing = original.IsComposing
    member x.Key = original.Key
    member x.KeyLocation = original.KeyLocation
    member x.Repeat = original.Repeat
    member x.Ctrl = original.Ctrl
    member x.Shift = original.Shift
    member x.Alt = original.Alt
    member x.Meta = original.Meta

    override x.WithKind(kind : SceneEventKind) =
        SceneKeyboardEvent(context, self, target, kind, location, original) :> SceneEvent
        
    override x.WithLocation(location : SceneEventLocation) =
        SceneKeyboardEvent(context, self, target, kind, location, original) :> SceneEvent
        



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