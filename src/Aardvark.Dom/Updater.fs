namespace Aardvark.Dom

open System.Web
open System.Text
open System.Text.Json
open System.Collections.Generic
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application
open System.Reflection
open System
open System.Threading.Tasks



type UpdateState<'a> =
    {
        token : AdaptiveToken
        runtime : IRuntime
    }

type IHtmlBackend<'a> =
    abstract NewId : unit -> 'a
    abstract Register : selector : string * id : 'a -> unit
    abstract Execute : self : option<'a> * channels : array<IChannel -> Task<unit>> * js : (array<string> -> list<string>) -> IDisposable
    abstract Root : 'a
    abstract CreateTextElement : unit -> 'a
    abstract CreateElement : tag : string -> 'a
    abstract RemoveAttribute : element : 'a * attName : string -> unit
    abstract SetAttribute : element : 'a * attName : string * attValue : string -> unit
    abstract SetAttribute : element : 'a * attName : string * attValue : Set<string> -> unit
    abstract SetAttribute : element : 'a * attName : string * attValue : bool -> unit
    abstract SetAttribute : element : 'a * attName : string * attValue : int -> unit
    
    abstract InsertFirst : parent : 'a * node : 'a -> unit
    abstract AppendChild : parent : 'a * node : 'a -> unit
    abstract InsertAfter : ref : 'a * node : 'a -> unit
    abstract InsertBefore : ref : 'a * node : 'a -> unit
    abstract Remove : 'a -> unit

    abstract SetListener : element : 'a * evtName : string * useCapture : bool * preventDefault : bool * pointerCapture : bool * evtType : System.Type * callback : (Event -> bool) -> unit
    abstract RemoveListener : element : 'a * evtName : string * useCapture : bool -> unit

    abstract Require : urls : Set<string> * (IHtmlBackend<'a> -> unit) -> unit

    abstract SetupRenderer : element : 'a * render : SceneHandler -> unit
    abstract DestroyRenderer : element : 'a * render : SceneHandler -> unit

    abstract Delay : (IHtmlBackend<'a> -> unit) -> obj
    abstract Run : obj -> unit
  

type DomContext =
    abstract Runtime : IRuntime
    abstract Execute : string -> option<System.Text.Json.JsonElement -> unit> -> unit
    abstract StartWorker<'t, 'a, 'b when 't :> AbstractWorker<'a, 'b> and 't : (new : unit -> 't)> : unit -> Task<WorkerInstance<'b, 'a>>



[<AbstractClass>]
type Updater<'a>(runtime : IRuntime, id : 'a) =
    inherit AdaptiveObject()

    member x.Id = id
    abstract PerformUpdate : state : UpdateState<'a> * code : IHtmlBackend<'a> -> unit
    abstract PerformDelete : state : UpdateState<'a> * code : IHtmlBackend<'a> -> unit
    abstract PerformTryReplace : newNode : DomNode * state : UpdateState<'a> * code : IHtmlBackend<'a> -> bool
    abstract Boot : option<array<IChannel -> Task<unit>> * (array<string> -> list<string>)>
    abstract Shutdown : option<array<IChannel -> Task<unit>> * (array<string> -> list<string>)>
    abstract Requires : Set<string>

    member x.Runtime = runtime

    member x.Update(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        x.EvaluateIfNeeded state.token () (fun token ->
            let state = { state with token = token }
            x.PerformUpdate(state, code)
        )
        
    member x.TryReplace(newNode : DomNode, state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        x.EvaluateAlways state.token (fun token ->
            let state = { state with token = token }
            x.PerformTryReplace(newNode, state, code)
        )

    member x.Delete(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        x.PerformDelete(state, code)
        if not (Unchecked.equals id code.Root) then code.Remove id

    static member Body(runtime : IRuntime, node : DomNode, code : IHtmlBackend<'a>) =
        ElementUpdater<'a>(runtime, code.Root, "", AttributeMap.empty, AList.single node) :> Updater<'a>
        //DomNode.Element("", AttributeMap.empty, AList.single node)
        //let id = code.Root
        //match node with
        //| DomNode.Text value -> 
        //    TextUpdater<'a>(id, value) :> Updater<'a>
        //| DomNode.VoidElement(tag, attributes) ->
        //    VoidUpdater<'a>(id, tag, attributes) :> Updater<'a>
        //| DomNode.Element(tag, attributes, children) ->
        //    ElementUpdater<'a>(id, tag, attributes, children) :> Updater<'a>
        //| DomNode.RenderControl(attributes, getScene) ->
        //    RenderControlUpdater<'a>(id, attributes, getScene) :> Updater<'a>

    
    static member Create(selector : string, runtime : IRuntime, node : DomNode, code : IHtmlBackend<'a>) =
        let id = code.NewId()
        code.Register(selector, id)
        
        match node with
        | DomNode.Text value -> 
            TextUpdater(runtime, id, value) :> Updater<'a>
        | DomNode.VoidElement(tag, attributes) ->
            VoidUpdater(runtime, id, tag, attributes) :> Updater<'a>
        | DomNode.Element(tag, attributes, children) ->
            ElementUpdater(runtime, id, tag, attributes, children) :> Updater<'a>
        | DomNode.RenderControl(getContent) ->
            RenderControlUpdater<'a>(runtime, id, getContent) :> Updater<'a>
    
    static member internal New(runtime : IRuntime, node : DomNode, code : IHtmlBackend<'a>) : Updater<'a> =
        match node with
        | DomNode.Text value -> 
            let id = code.CreateTextElement()
            TextUpdater(runtime, id, value) :> Updater<'a>
        | DomNode.VoidElement(tag, attributes) ->
            let id = code.CreateElement(tag)
            VoidUpdater(runtime, id, tag, attributes) :> Updater<'a>
        | DomNode.Element(tag, attributes, children) ->
            let id = 
                if tag = "body" then code.Root
                else code.CreateElement(tag)
            ElementUpdater(runtime, id, tag, attributes, children) :> Updater<'a>
        | DomNode.RenderControl(getContent) ->
            let id = code.CreateElement("div")
            RenderControlUpdater<'a>(runtime, id, getContent) :> Updater<'a>

and internal AttributeUpdater<'a>(targetId : 'a, attributes : AttributeMap) =
    static let getRequire(attributes : AttributeMap) =
        let atts = AVal.force attributes.Content.Content
        match HashMap.tryFind "require" atts with
        | Some (AttributeValue.Set urls) ->
            urls
        | Some (AttributeValue.String url) ->
            Set.singleton url
        | _ -> 
            Set.empty
        
    static let getBootAndShutdown (attributes : AttributeMap) =
        let atts = AVal.force attributes.Content.Content
        let bootCode =
            match HashMap.tryFind "boot" atts with
            | Some (AttributeValue.Execute(a,b)) ->
                Some (a,b)
            | _ ->
                None
        let shutdownCode =
            match HashMap.tryFind "shutdown" atts with
            | Some (AttributeValue.Execute(a,b)) ->
                Some (a, b)
            | _ ->
                None
        bootCode, shutdownCode

    let mutable attributes = attributes
    let mutable reader = attributes.Content.GetReader()
    let mutable require = lazy (getRequire attributes)
    let mutable bootShutdown = lazy (getBootAndShutdown attributes)

    let applyDelta (code : IHtmlBackend<'a>) (oldState : HashMap<_,_>) (ops : HashMapDelta<_,_>) =
        let ops = 
            ops 
            |> HashMapDelta.toHashMap
            |> HashMap.remove "boot"
            |> HashMap.remove "shutdown"
            |> HashMapDelta.ofHashMap

        if not (HashMapDelta.isEmpty ops) then
            for key, op in ops do
                match op with
                | Set value ->
                    match value with
                    | AttributeValue.Execute _ ->
                        ()
                        
                    | AttributeValue.Bool value ->
                        code.SetAttribute(targetId, key, value)
                        
                    | AttributeValue.Int value ->
                        code.SetAttribute(targetId, key, value)
                        
                    | AttributeValue.String value ->
                        code.SetAttribute(targetId, key, value)
                        
                    | AttributeValue.Set value ->
                        code.SetAttribute(targetId, key, value)
                     

                    | AttributeValue.Event(capture, bubble) ->
                        match capture.Callback with
                        | Some c ->
                            code.SetListener(targetId, key, true, capture.PreventDefault, capture.PointerCapture, capture.EventType, c)
                        | None ->
                            code.RemoveListener(targetId, key, true)

                            
                        match bubble.Callback with
                        | Some c ->
                            code.SetListener(targetId, key, false, bubble.PreventDefault, bubble.PointerCapture, bubble.EventType, c)
                        | None ->
                            code.RemoveListener(targetId, key, false)

                | Remove ->
                    match HashMap.tryFind key oldState with
                    | Some (AttributeValue.Event _) ->
                        code.RemoveListener(targetId, key, false)
                        code.RemoveListener(targetId, key, true)
                    | Some _ ->
                        code.RemoveAttribute(targetId, key)
                    | None ->
                        ()
   
    member x.Require =
        require.Value

    member x.Boot =
        let (boot, _) = bootShutdown.Value
        boot

    member x.Shutdown =
        let (_, shutdown) = bootShutdown.Value
        shutdown

    //member x.Replace(newAttributes : AttributeMap, state : UpdateState<'a>, code : IHtmlBackend<'a>) =
    //    let oldState = reader.State
    //    reader.Outputs.Consume(ref (Array.zeroCreate 4)) |> ignore

    //    let oldShutdown = 
    //        if bootShutdown.IsValueCreated then bootShutdown.Value |> snd
    //        else []

    //    attributes <- newAttributes
    //    reader <- newAttributes.Content.GetReader()

    //    reader.GetChanges(state.token) |> ignore
    //    let newState = reader.State
    //    let newRequire = getRequire attributes
    //    let newBoot, newShutdown = getBootAndShutdown attributes
    //    require <- lazy newRequire
    //    bootShutdown <- lazy (newBoot, newShutdown)

    //    code.Execute(Some targetId, oldShutdown)
    //    code.Require(newRequire, fun code ->
    //        applyDelta code oldState (HashMap.computeDelta oldState newState)
    //        code.Execute(Some targetId, newBoot)
    //    )

    member x.Update(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        let old = reader.State
        let ops = reader.GetChanges state.token
        applyDelta code old ops
    
    member x.Delete(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        reader.Outputs.Consume(ref (Array.zeroCreate 4)) |> ignore
        
and internal VoidUpdater<'a>(runtime : IRuntime, id : 'a, tag : string, attributes : AttributeMap) =
    inherit Updater<'a>(runtime, id)
    let att = AttributeUpdater(id, attributes)

    override x.Boot = att.Boot
    override x.Shutdown = att.Shutdown
    override x.Requires = att.Require

    override x.PerformTryReplace(newNode : DomNode, state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        false
        //match newNode with
        //| DomNode.VoidElement(newTag, newAtt) when newTag = tag ->
        //    att.Replace(newAtt, state, code)
        //    true
        //| _ ->
        //    false

    override x.PerformUpdate(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        att.Update(state, code)

    override x.PerformDelete(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        att.Delete(state, code)

and internal TextUpdater<'a>(runtime : IRuntime, id : 'a, value : aval<string>) =
    inherit Updater<'a>(runtime, id)
    let mutable oldValue = None
    let mutable value = value

    override x.Boot = None
    override x.Shutdown = None
    override x.Requires = Set.empty

    override x.PerformTryReplace(newNode : DomNode, state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        false
        //match newNode with
        //| DomNode.Text(newContent) ->
        //    match state.token.Caller with
        //    | Some c -> value.Outputs.Remove c |> ignore
        //    | None -> ()

        //    value <- newContent
        //    let v = value.GetValue state.token
        //    match oldValue with
        //    | Some o when o = v ->
        //        ()
        //    | _ ->
        //        oldValue <- Some v
        //        code.SetAttribute(id, "textContent", v)

        //    true
        //| _ ->
        //    false

    override x.PerformUpdate(state, code) =
        let v = value.GetValue state.token
        match oldValue with
        | Some o when o = v ->
            ()
        | _ ->
            oldValue <- Some v
            code.SetAttribute(id, "textContent", v)

    override x.PerformDelete(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        value.Outputs.Remove x |> ignore

and internal ElementUpdater<'a>(runtime : IRuntime, id : 'a, tag : string, attributes : AttributeMap, children : alist<DomNode>) =
    inherit Updater<'a>(runtime, id)

    let att = AttributeUpdater(id, attributes)
    let mutable children = children
    let mutable reader = children.GetReader()
    let mutable disposables = IndexList.empty<IDisposable>

    
    let mutable updaters = IndexList.empty<Updater<'a>>
    let dirty = ref (System.Collections.Generic.HashSet<Updater<'a>>())

    override x.Boot = att.Boot
    override x.Shutdown = att.Shutdown
    override x.Requires = att.Require

    
    override x.PerformTryReplace(newNode : DomNode, state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        false
        //match newNode with
        //| DomNode.Element(newTag, newAttributes, newChildren) when newTag = tag ->
        //    att.Replace(newAttributes, state, code)

        //    let oldState = reader.State
        //    reader.Outputs.Consume(ref <| Array.zeroCreate 4) |> ignore
        //    children <- newChildren
        //    reader <- children.GetReader()
        //    reader.GetChanges state.token |> ignore

        //    let ops = IndexList.computeDelta oldState reader.State


        //    failwith ""

        //| _ ->
        //    false

    override x.InputChangedObject(_, o) =
        match o with
        | :? Updater<'a> as o -> lock dirty (fun () -> dirty.Value.Add o |> ignore)
        | _ -> ()

    override x.PerformUpdate(state, code) =
        let dirty =
            lock dirty (fun () ->
                let d = dirty.Value
                dirty.Value <- System.Collections.Generic.HashSet()
                d
            )
        
        let mutable requires = Set.empty

        let thing = 
            code.Delay (fun code ->
                att.Update(state, code)

                for index, op in reader.GetChanges state.token do
                    match op with
                    | Set node ->   
                        let (l, s, r) = IndexList.neighbours index updaters
                        match s with
                        | Some o -> 
                            dirty.Remove o |> ignore
                            match IndexList.tryRemove index disposables with
                            | Some (d, rest) ->
                                disposables <- rest
                                d.Dispose()
                            | None ->
                                ()
                            o.Delete(state, code)
                        | None -> ()
                        let updater = Updater.New(state.runtime, node, code)
                        updaters <- IndexList.set index updater updaters
                        dirty.Add updater |> ignore

                        requires <- Set.union requires updater.Requires
                        //match updater.Boot with
                        //| [] -> ()
                        //| boot -> boots.Add(updater.Id, boot)

                        if not (Unchecked.equals updater.Id code.Root) then
                            match l with
                            | Some (_, ln) -> 
                                code.InsertAfter(ln.Id, updater.Id)
                                //code.AppendLine $"aardvark.insertAfter({ln.Id}, {var});" |> ignore
                            | None -> 
                                match r with
                                | None -> 
                                    code.AppendChild(id, updater.Id)
                                    //code.AppendLine $"aardvark.appendChild({id}, {var});" |> ignore
                                | Some _ -> 
                                    code.InsertFirst(id, updater.Id)
                                    //code.AppendLine $"aardvark.insertFirst({id}, {var});" |> ignore

                        match updater.Boot with
                        | None -> ()
                        | Some (channels, js) -> 
                            let d = code.Execute(Some updater.Id, channels, js)
                            disposables <- IndexList.set index d disposables

                    | Remove ->
                        match IndexList.tryRemove index disposables with
                        | Some (d, rest) ->
                            disposables <- rest
                            d.Dispose()
                        | None ->
                            ()

                        match IndexList.tryRemove index updaters with
                        | Some (old, rest) ->
                            dirty.Remove old |> ignore
                            updaters <- rest
                            match old.Shutdown with
                            | None -> ()
                            | Some (channels, js) -> code.Execute(Some old.Id, channels, js).Dispose()
                            old.Delete(state, code)
                        | None ->
                            ()

                for d in dirty do d.Update(state, code)
            )

        code.Require(requires, fun code ->
            code.Run thing
        )

    override x.PerformDelete(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        att.Delete(state, code)
        for u in updaters do u.Delete(state, code)

and internal RenderControlUpdater<'a>(runtime : IRuntime, id : 'a, getContent : RenderControlInfo -> AttributeMap * amap<RenderControlEventKind, RenderControlEventInfo -> unit> * DomScene) =
    inherit Updater<'a>(runtime, id)

    static let signatureCache = System.Collections.Concurrent.ConcurrentDictionary<IRuntime * int, IFramebufferSignature>()

    static let getFramebufferSignature (runtime : IRuntime) (samples : int) =
        signatureCache.GetOrAdd((runtime, samples), fun (runtime, samples) ->
            runtime.CreateFramebufferSignature([
                DefaultSemantic.Colors, TextureFormat.Rgba8
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
            ], samples)
        )

    let mutable handler : option<SceneHandler> = None

    let currentSignature = cval Unchecked.defaultof<IFramebufferSignature>
    let size = cval V2i.II
    let time = cval System.DateTime.Now

    let attributes, sceneHandlers, scene = 
        getContent {
            Runtime = runtime
            FramebufferSignature = currentSignature
            ViewportSize = size
            Time = time
        }

    let cursor : cval<option<string>> = cval None
    
    let samples = 
        let att = AMap.force attributes.Content 
        match HashMap.tryFind "data-samples" att with
        | Some (AttributeValue.Int s) -> s
        | Some (AttributeValue.String v) ->
            match System.Int32.TryParse v with
            | (true, s) -> s
            | _ -> 1
        | _ ->
            1

    let handlePointerEvent (kind : SceneEventKind) (e : PointerEvent) =
        match handler with
        | Some h -> h.HandlePointerEvent(kind, e)
        | None -> true
        
    let handleMouseEvent (kind : SceneEventKind) (e : MouseEvent) =
        match handler with
        | Some h -> h.HandlePointerEvent(kind, PointerEvent e)
        | None -> true
        
    let handleTapEvent (kind : SceneEventKind) (e : TapEvent) =
        match handler with
        | Some h -> h.HandleTapEvent(kind, e)
        | None -> true
        
    let handleWheelEvent (kind : SceneEventKind) (e : WheelEvent) =
        match handler with
        | Some h -> h.HandleWheelEvent(kind, e)
        | None -> true
        
    let handleKeyEvent (kind : SceneEventKind) (e : KeyboardEvent) =
        match handler with
        | Some h -> h.HandleKeyEvent(kind, e)
        | None -> true
        
    let handleInputEvent (kind : SceneEventKind) (e : InputEvent) =
        match handler with
        | Some h -> h.HandleInputEvent(kind, e)
        | None -> true
        
    let additionalAttributesBefore =
        att {
            OnMouseLeave(handleMouseEvent SceneEventKind.PointerMove >> ignore)
        }

    let additionalAttributes =
        att {
            cursor |> AVal.map (function
                | Some c -> Some (Style [Css.Cursor c])
                | _ -> None
            )
            
            TabIndex 0
            Draggable false
            Style [ Css.UserSelect "none" ]
            OnPointerDown(handlePointerEvent SceneEventKind.PointerDown, true)
            OnPointerUp(handlePointerEvent SceneEventKind.PointerUp, true)
            OnPointerMove(handlePointerEvent SceneEventKind.PointerMove, true)
            OnClick(handleMouseEvent SceneEventKind.Click, true)
            OnDoubleClick(handleMouseEvent SceneEventKind.DoubleClick, true)
            OnMouseWheel(handleWheelEvent SceneEventKind.Scroll, true)
            OnMouseEnter(handleMouseEvent SceneEventKind.PointerMove >> ignore)
            OnKeyDown(handleKeyEvent SceneEventKind.KeyDown, true)
            OnKeyUp(handleKeyEvent SceneEventKind.KeyUp, true)
            OnInput(handleInputEvent SceneEventKind.KeyInput, true)
            OnTap(handleTapEvent SceneEventKind.Tap, true)
            OnDoubleTap(handleTapEvent SceneEventKind.DoubleTap, true)
            OnLongPress(handlePointerEvent SceneEventKind.LongPress, true)
        }

    let att = 
        new AttributeUpdater<'a>(id, AttributeMap.union additionalAttributesBefore (AttributeMap.union attributes additionalAttributes))
    
    let setCursor (c : option<string>) =
        transact (fun () -> cursor.Value <- c)

    let handleSceneEvent (e : RenderControlEvent) =
        match HashMap.tryFind e.Kind (AMap.force sceneHandlers) with
        | Some h -> h e.Info
        | None -> ()

    override x.Boot = att.Boot
    override x.Shutdown = att.Shutdown
    override x.Requires = att.Require
    
    override x.PerformTryReplace(newNode : DomNode, state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        false

    override x.PerformUpdate(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        att.Update(state, code)

        match handler with
        | None -> 
            let signature = getFramebufferSignature state.runtime samples
            transact (fun () -> currentSignature.Value <- signature)
            let h = new SceneHandler(signature, handleSceneEvent, setCursor, scene.Scene, scene.View, scene.Proj, size, time) 
            handler <- Some h
            code.SetupRenderer(id, h)
            scene.OnReady h
        | Some _ ->
            ()

    override x.PerformDelete(state : UpdateState<'a>, code : IHtmlBackend<'a>) =
        att.Delete(state, code)
        match handler with
        | Some h -> 
            code.DestroyRenderer(id, h)
            h.Dispose()
            handler <- None
        | None ->
            ()