namespace Aardvark.Dom

open Aardvark.Base
open Aardvark.Application
open Aardvark.Rendering
open FSharp.Data.Adaptive

[<ReferenceEquality>]
type TraversalState =
    {
        Runtime             : IRuntime
        Level               : int
        Parent              : option<TraversalState>
        Shader              : FShade.Effect
        Uniforms            : HashMap<string, IAdaptiveValue>
        View                : aval<Trafo3d>
        Proj                : aval<Trafo3d>
        Model               : list<aval<Trafo3d>>
        Index               : option<BufferView>
        VertexAttributes    : HashMap<string, BufferView>
        InstanceAttributes  : HashMap<string, BufferView>
        Mode                : IndexedGeometryMode
        Pass                : RenderPass
        Rasterizer          : RasterizerState
        Blend               : BlendState
        Depth               : DepthState
        Stencil             : StencilState
        Active              : aval<bool>
        CanFocus            : bool
        
        Cursor              : aval<option<string>>
        EventHandlers       : amap<SceneEventKind, SceneEventHandler>
        ForcePixelPick      : bool
        PixelPick           : bool
        PickThrough         : bool
        /// Pixel-snap radius for this scope. When the cursor is within this
        /// many pixels of a hit, the pick can "snap" to it. Default 0
        /// (no snapping). Values are clamped to the global cap at use time.
        PixelSnapRadius     : aval<int>
        /// Pick context (the owning SceneHandler) for this render, threaded from
        /// the root; `Some` only inside a dom pick render. Lets a heap reach the
        /// handler on expand and register its per-slot pickable scopes against the
        /// handler's id space. `None` ⇒ unpickable (non-dom render).
        PickContext         : option<IPickContext>
        /// Set on a "portal" node (a quad textured with an offscreen render's
        /// result). When present, the pick patcher writes a marker + the node's
        /// `PickContextCoord` (source-uv) into the pickbuffer instead of the
        /// normal id/normal/depth, and the resolver recurses into this
        /// sub-context at that uv to reach the innermost real hit.
        PickSubContext      : option<IPickSubContext>
    }

/// Minimal handle the heap uses to register/deregister per-slot pickable scopes
/// with the owning SceneHandler — carried on `TraversalState.PickContext`, reached
/// by a heap node on expand. Implemented by SceneHandler. `Register` returns a
/// dom-sourced (ref-counted) pick id; `Deregister` releases one by id.
and IPickContext =
    abstract member Register : TraversalState -> int
    abstract member Deregister : int -> unit

/// A rendered sub-scene's picking surface, reduced to exactly what the pick
/// patcher/dispatcher (in SceneHandler.fs, which compiles BEFORE RenderTo.fs)
/// need to recurse a portal. `RenderTo.fs`'s `IRenderPickContext` inherits this.
/// `PickAt` resolves an inner pixel to the innermost real hit, forcing the
/// innermost frame at pick time: (World, ModelTrafo, ViewTrafo, ProjTrafo, State).
and IPickSubContext =
    abstract member Size : aval<V2i>
    abstract member PickAt : V2i -> voption<struct (V3d * Trafo3d * Trafo3d * Trafo3d * TraversalState)>

module TraversalState =
    let private trafoCache = BinaryCache<aval<Trafo3d>, aval<Trafo3d>, aval<Trafo3d>>(AVal.map2 (*))
    
    let internal trafoOfStack (model : list<aval<Trafo3d>>) =
        match model with
        | [] ->
            AVal.constant Trafo3d.Identity
        | m :: ms ->
            let mutable res = m
            for m in ms do
                res <- trafoCache.Invoke(res, m)
            res

    let modelTrafo (state : TraversalState) =
        trafoOfStack state.Model

    let push (state : TraversalState) =
        {
            state with
                Parent = Some state
                EventHandlers = AMap.empty
                Level = state.Level + 1
        }

    let empty (rt : IRuntime) = 
        {
            Runtime = rt
            Level = 0
            Parent = None
            Shader = FShade.Effect.empty
            View = AVal.constant Trafo3d.Identity
            Proj = AVal.constant Trafo3d.Identity
            Model = []
            Index = None
            Uniforms = HashMap.empty
            VertexAttributes = HashMap.empty
            InstanceAttributes = HashMap.empty
            Mode = IndexedGeometryMode.TriangleList
            Pass = RenderPass.main
            Rasterizer = RasterizerState.Default
            Blend = BlendState.Default
            Depth = DepthState.Default
            Stencil = StencilState.Default
            Active = AVal.constant true
            EventHandlers = AMap.empty
            ForcePixelPick = false
            PixelPick = true
            CanFocus = true
            Cursor = AVal.constant None
            PickThrough = false
            // Default radius 1 absorbs the 1-pixel silhouette feather caused by
            // MS resolve averaging two distinct pickIds at object edges into a
            // non-integer that won't map to any registered scope.
            PixelSnapRadius = AVal.constant 1
            PickContext = None
            PickSubContext = None
        }

    let commonAncestor (a : TraversalState) (b : TraversalState) =
        if a.Level < b.Level then
            let mutable b = b
            while b.Level > a.Level && Option.isSome b.Parent do    
                b <- b.Parent.Value
            
            if b.Level = a.Level then
                let mutable a = a
                while a <> b && Option.isSome a.Parent && Option.isSome b.Parent do
                    a <- a.Parent.Value
                    b <- b.Parent.Value
                if a = b then Some a
                else None
            else
                None
        elif b.Level < a.Level then
            let mutable a = a
            while a.Level > b.Level && Option.isSome a.Parent do    
                a <- a.Parent.Value
            
            if b.Level = a.Level then
                let mutable b = b
                while a <> b && Option.isSome a.Parent && Option.isSome b.Parent do
                    a <- a.Parent.Value
                    b <- b.Parent.Value
                if a = b then Some a
                else None
            else
                None
        else
            let mutable a = a
            let mutable b = b
            while a <> b && Option.isSome a.Parent && Option.isSome b.Parent do
                a <- a.Parent.Value
                b <- b.Parent.Value
            if a = b then Some a
            else None
    

    let handleEvent (bubble : bool) (e : SceneEvent) (state : TraversalState) =
        if bubble then
            let rec runCapture (e : SceneEvent) (state : TraversalState) =
                match state.Parent with
                | Some parent ->
                    if runCapture e parent then
                        match HashMap.tryFind e.Kind (AMap.force state.EventHandlers) with
                        | Some h ->
                            let model = modelTrafo parent |> AVal.force
                            let e = e.Transformed model
                            h.Capture |> List.forall (fun h -> h e)
                        | None ->
                            true
                    else
                        false

                | None ->
                    match HashMap.tryFind e.Kind (AMap.force state.EventHandlers) with
                    | Some h ->
                        h.Capture |> List.forall (fun h -> h e)
                    | None ->
                        true

            let rec runBubble (e : SceneEvent) (state : TraversalState) =
                match state.Parent with
                | Some parent ->
                    let cont =
                        match HashMap.tryFind e.Kind (AMap.force state.EventHandlers) with
                        | Some h ->
                            let model = modelTrafo parent |> AVal.force
                            let e = e.Transformed model
                            h.Bubble |> List.forall (fun h -> h e)
                        | None ->
                            true
                    if cont then runBubble e parent
                    else false
                | None ->
                    match HashMap.tryFind e.Kind (AMap.force state.EventHandlers) with
                    | Some h ->
                        h.Bubble |> List.forall (fun h -> h e)
                    | None ->
                        true

            if runCapture e state then
                runBubble e state
            else
                false
        else
            match HashMap.tryFind e.Kind (AMap.force state.EventHandlers) with
            | Some h ->
                h.Bubble |> List.forall (fun h -> h e)
            | None ->
                true
            

    let handleDifferential (last : cval<option<TraversalState>>) (enter : SceneEventKind) (leave : SceneEventKind) (e : SceneEvent) (newTarget : option<TraversalState>) =
        let rec runUp (e : SceneEvent) (excl : option<TraversalState>) (state : TraversalState) =
            match excl with
            | Some e when e = state -> ()
            | _ ->
                match HashMap.tryFind e.Kind (AMap.force state.EventHandlers) with
                | Some h ->
                    let model = 
                        match state.Parent with
                        | Some p -> modelTrafo p |> AVal.force
                        | None -> Trafo3d.Identity
                    let e = e.Transformed model
                    h.Capture |> List.iter (fun h -> h e |> ignore)
                    h.Bubble |> List.iter (fun h -> h e |> ignore)
                | None ->
                    ()

                match state.Parent with
                | Some p -> runUp e excl p
                | None -> ()

        let rec runDown (e : SceneEvent) (excl : option<TraversalState>) (state : TraversalState) =
            match excl with
            | Some e when e = state -> ()
            | _ ->
                let model = 
                    match state.Parent with
                    | Some p -> 
                        runDown e excl p
                        modelTrafo p |> AVal.force
                    | None -> 
                        Trafo3d.Identity
                        
                match HashMap.tryFind e.Kind (AMap.force state.EventHandlers) with
                | Some h ->
                    let e = e.Transformed model
                    h.Capture |> List.iter (fun h -> h e |> ignore)
                    h.Bubble |> List.iter (fun h -> h e |> ignore)
                | None ->
                    ()


        match newTarget with
        | Some s ->
            match last.Value with
            | Some l ->
                if l <> s then
                    let p = commonAncestor l s
                    runUp (e.WithKind leave) p l
                    runDown (e.WithKind enter) p s
                    transact (fun () -> last.Value <- Some s)
            | None ->
                runDown (e.WithKind enter) None s
                transact (fun () -> last.Value <- Some s)
        | None ->
            match last.Value with
            | Some l ->
                runUp (e.WithKind leave) None l
                transact (fun () -> last.Value <- None)
            | None ->
                ()
        
    let handleMove (lastOver : cval<option<TraversalState>>) (e : SceneEvent) (state : option<TraversalState>) =
        handleDifferential lastOver SceneEventKind.PointerEnter SceneEventKind.PointerLeave e state


[<RequireQualifiedAccess>]
type SceneAttribute =
    | Shader of FShade.Effect
    | Uniforms of HashMap<string, IAdaptiveValue>
    | View of aval<Trafo3d>
    | Proj of aval<Trafo3d>
    | Model of list<aval<Trafo3d>>
    | Index of option<BufferView>
    | VertexAttributes of HashMap<string, BufferView>
    | InstanceAttributes of HashMap<string, BufferView>
    | Mode of IndexedGeometryMode
    | Pass of RenderPass
    | RasterizerState of RasterizerState
    | BlendState of BlendState
    | DepthState of DepthState
    | StencilState of StencilState
    | Active of aval<bool>
    | Intersectable of aval<IIntersectable>
    | ForcePixelPicking of bool
    | NoEvents
    | Cursor of aval<option<string>>
    | CanFocus of bool

    | CullMode of aval<CullMode>
    | FrontFace of aval<WindingOrder>
    | FillMode of aval<FillMode>
    | Multisample of aval<bool>
    | ConservativeRaster of aval<bool>

    | BlendMode of aval<BlendMode>
    | ColorWriteMask of aval<ColorMask>
    | BlendConstant of aval<C4f>
    | AttachmentBlendMode of aval<Map<Symbol, BlendMode>>
    | AttachmentColorWriteMask of aval<Map<Symbol, ColorMask>>

    | DepthTest of aval<DepthTest>
    | DepthBias of aval<DepthBias>
    | DepthWriteMask of aval<bool>
    | DepthClamp of aval<bool>

    | On of amap<SceneEventKind, SceneEventHandler>
    | PickThrough of bool
    | PixelSnapRadius of aval<int>
    | PickContextSub of IPickSubContext

module SceneAttribute =
    let apply (att : SceneAttribute) (state : TraversalState) =
        match att with
        | SceneAttribute.BlendMode v -> 
            { state with Blend = { state.Blend with Mode = v; AttachmentMode = AVal.constant Map.empty } }
        
        | SceneAttribute.ColorWriteMask v -> 
            { state with Blend = { state.Blend with ColorWriteMask = v; AttachmentWriteMask = AVal.constant Map.empty  } }
        
        | SceneAttribute.BlendConstant v -> 
            { state with Blend = { state.Blend with ConstantColor = v } }
        
        | SceneAttribute.AttachmentBlendMode value -> 
            let old = state.Blend.AttachmentMode
            let newAttachmentModes = 
                if old.IsConstant && Map.isEmpty (AVal.force old) then 
                    value
                else
                    (old, value) ||> AVal.map2 Map.union
            { state with Blend = { state.Blend with AttachmentMode = newAttachmentModes } }
        
        | SceneAttribute.AttachmentColorWriteMask value -> 
            let old = state.Blend.AttachmentWriteMask
            let newAttachmentMasks = 
                if old.IsConstant && Map.isEmpty (AVal.force old) then 
                    value
                else
                    (old, value) ||> AVal.map2 Map.union
            { state with Blend = { state.Blend with AttachmentWriteMask = newAttachmentMasks } }
        
        | SceneAttribute.DepthTest v -> { state with Depth = { state.Depth with Test = v } }
        | SceneAttribute.DepthBias v -> { state with Depth = { state.Depth with Bias = v } }
        | SceneAttribute.DepthWriteMask v -> { state with Depth = { state.Depth with WriteMask = v } }
        | SceneAttribute.DepthClamp v -> { state with Depth = { state.Depth with Clamp = v } }
        
        
        | SceneAttribute.CullMode v -> { state with Rasterizer = { state.Rasterizer with CullMode = v } }
        | SceneAttribute.FrontFace v -> { state with Rasterizer = { state.Rasterizer with FrontFacing = v } }
        | SceneAttribute.FillMode v -> { state with Rasterizer = { state.Rasterizer with FillMode = v } }
        | SceneAttribute.Multisample v -> { state with Rasterizer = { state.Rasterizer with Multisample = v } }
        | SceneAttribute.ConservativeRaster v -> { state with Rasterizer = { state.Rasterizer with ConservativeRaster = v } }

        | SceneAttribute.Shader e -> { state with Shader = e }
        | SceneAttribute.Uniforms u -> { state with Uniforms = HashMap.union state.Uniforms u }
        | SceneAttribute.View v -> { state with View = v }
        | SceneAttribute.Proj p -> { state with Proj = p }
        | SceneAttribute.Model t -> { state with Model = t @ state.Model }
        | SceneAttribute.Index i -> { state with Index = i }
        | SceneAttribute.VertexAttributes a -> { state with VertexAttributes = HashMap.union state.VertexAttributes a }
        | SceneAttribute.InstanceAttributes a -> { state with InstanceAttributes = HashMap.union state.InstanceAttributes a }
        | SceneAttribute.Mode m -> { state with Mode = m }
        | SceneAttribute.Pass p -> { state with Pass = p }
        | SceneAttribute.RasterizerState rs -> { state with Rasterizer = rs }
        | SceneAttribute.BlendState bs -> { state with Blend = bs }
        | SceneAttribute.DepthState ds -> { state with Depth = ds }
        | SceneAttribute.StencilState ss -> { state with Stencil = ss }
        | SceneAttribute.Active a -> { state with Active = AVal.map2 (&&) state.Active a }
        | SceneAttribute.On table -> { state with EventHandlers = (state.EventHandlers, table) ||> AMap.unionWith (fun _ a b -> SceneEventHandler.merge a b) }
        | SceneAttribute.Intersectable _ ->
            if not state.ForcePixelPick then { state with PixelPick = false }
            else state
        | SceneAttribute.ForcePixelPicking v ->
            if v then { state with ForcePixelPick = true; PixelPick = true }
            else { state with ForcePixelPick = false }
        | SceneAttribute.NoEvents -> { state with PixelPick = false }
        | SceneAttribute.CanFocus f -> { state with CanFocus = f }
        | SceneAttribute.Cursor c -> { state with Cursor = c }
        | SceneAttribute.PickThrough v -> { state with PickThrough = v }
        | SceneAttribute.PixelSnapRadius r -> { state with PixelSnapRadius = r }
        | SceneAttribute.PickContextSub ctx -> { state with PickSubContext = Some ctx }
