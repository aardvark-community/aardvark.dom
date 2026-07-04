namespace Aardvark.Dom

open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive

/// A scene node that COLLAPSES all of its children's render objects into the GPU
/// heap — one value-deduplicated arena + a single indirect multidraw
/// (`Aardvark.SceneGraph.Heap.ofRenderObjects`). Rendering becomes O(1) in object
/// count and per-object edits re-upload only the changed slice.
///
/// Picking: the heap lives below dom, so the registration of pickable parts is
/// brokered HERE (this node is dom-side, so it can read `TraversalState.PickContext`
/// and reach the owning SceneHandler). When a pick context is present the node
/// registers each child render object's TraversalState for a per-slot pick id and
/// drops the per-object pick objects (the heap's per-slot write replaces them);
/// otherwise it falls back to passing the original picks through.
type HeapNode(child : ISceneNode) =
    // RENDER path: the heap DCE-links its effects against the framebuffer signature, but
    // the Dom's TraversalState doesn't carry one, and — more importantly — the real render
    // target may add attachments the node can't know here (e.g. a Normals G-buffer added by
    // a post-processing pass). So the heap is signature-deferred: it returns
    // SignatureDependentRenderObjects that build at compile time against the ACTUAL
    // signature (opaque + transparent variants, memoized per attachment-semantics).

    // PICKING: like the render path, the pick heap defers its DCE-link to compile time. The
    // pick render signature is whatever the pickable target requested (`user semantics +
    // PickId`, built by PickProducer) — never a hardcoded {Colors, PickId, Depth} signature,
    // so extra attachments the real target carries survive; PickProducer routes the pickable
    // SDR into the PickId pass.

    // ONE HeapStorage per node, shared by the RENDER heap and the PICK heap: their
    // allocations (geometry, uniforms, constituents) dedup in the same pages, so the
    // pickable scene lives in GPU memory once. Created lazily with the traversal runtime;
    // lives and dies with the node.
    let storageLock = obj()
    let mutable storage : Aardvark.SceneGraph.Heap.HeapStorage voption = ValueNone
    let getStorage (runtime : IRuntime) =
        lock storageLock (fun () ->
            match storage with
            | ValueSome s -> s
            | ValueNone ->
                let s = Aardvark.SceneGraph.Heap.HeapStorage(runtime)
                storage <- ValueSome s
                s)

    interface ISceneNode with
        member _.GetRenderObjects(state : TraversalState) =
            Aardvark.SceneGraph.Heap.ofRenderObjects (getStorage state.Runtime) (child.GetRenderObjects state)

        member _.GetObjects(state : TraversalState) =
            match state.PickContext with
            | Some ctx when state.PixelPick ->
                // The heap picks per-slot in the pixel buffer; BVH (`Intersectable`) picks are
                // DROPPED on the heap path, so FORCE pixel picking for the whole heap subtree —
                // otherwise anything with a bounding box (e.g. `Primitives.Box`) would silently
                // become unpickable. `Sg.NoEvents` children still set `PixelPick = false` on the
                // way down (NoEvents wins over ForcePixelPick) and stay rendered-but-unpickable.
                let pickState = { state with ForcePixelPick = true; PixelPick = true }
                let renders, _picks = child.GetObjects pickState
                // O(1) heap picking: for each child render object register its TraversalState
                // → a per-slot dom pick id, compose the heap pick chain onto its effect and
                // carry the id as a "HeapPickId" uniform (the heap rewrites that name into its
                // per-slot HeapPickIds gather), heapify via the picking path, and DROP the
                // per-object pick objects (the heap's per-slot write replaces them).
                let pickSem = Symbol.Create "PickId"
                let wrap (ro : IRenderObject) : IRenderObject =
                    match ro with
                    | :? RenderObject as o ->
                        match RenderObject.traversalStates.TryGetValue o with
                        | true, ts when ts.PixelPick ->
                            match o.Surface with
                            | Surface.Effect eff ->
                                let id = ctx.Register ts
                                let geomHas (sem : string) =
                                    ValueOption.isSome (o.VertexAttributes.TryGetAttribute (Symbol.Create sem)) ||
                                    ValueOption.isSome (o.InstanceAttributes.TryGetAttribute (Symbol.Create sem))
                                let choice = PickShader.chooseChain eff geomHas
                                let pre = if choice.InjectVsn then [ PickShader.viewSpaceNormalEffect ] else []
                                let chain = pre @ [ PickShader.pickDepthBeforeEffect; eff; PickShader.pickFinalHeapEffect ]
                                let r = RenderObject.Clone o
                                r.Surface <- Surface.Effect (FShade.Effect.compose chain)
                                r.Uniforms <- UniformProvider.union o.Uniforms (UniformProvider.ofList [ "HeapPickId", AVal.constant id :> IAdaptiveValue ])
                                r.BlendState <-
                                    { o.BlendState with
                                        AttachmentMode      = o.BlendState.AttachmentMode      |> AVal.map (Map.add pickSem BlendMode.None)
                                        AttachmentWriteMask = o.BlendState.AttachmentWriteMask |> AVal.map (Map.add pickSem ColorMask.All) }
                                r :> IRenderObject
                            | _ -> o :> IRenderObject
                        | _ -> o :> IRenderObject   // no TraversalState or NoEvents → rendered, unpickable
                    | _ -> ro
                let wrapped = renders |> ASet.map wrap
                // deregister a part's pick id when the heap frees its slot (ref-counted in the SceneHandler).
                // DEFERRED: links against the real pick signature (user sems + PickId) at compile time.
                Aardvark.SceneGraph.Heap.ofRenderObjectsPicking (getStorage state.Runtime) ctx.Deregister wrapped, ASet.empty
            | _ ->
                // non-dom / whole-heap NoEvents: plain heap collapse, original picks passed through.
                let renders, picks = child.GetObjects state
                Aardvark.SceneGraph.Heap.ofRenderObjects (getStorage state.Runtime) renders, picks

/// `heap { ... }` — exactly like `sg { ... }`, but its children render through the heap.
/// Reuses every `SceneNodeBuilder` Yield/Combine/Delay/Zero overload and only overrides
/// `Run` to wrap the built node in `HeapNode`. Drop-in inside a pure view:
///     renderControl { ...; heap { m.Nodes |> IndexList.map heapNodeView } }
type HeapBuilder() =
    inherit SceneNodeBuilder()
    member inline x.Run([<InlineIfLambda>] action : SceneBuilder<unit>) : ISceneNode =
        let s = SceneNodeBuilderState()
        action s
        HeapNode(s.Build()) :> ISceneNode

[<AutoOpen>]
module HeapBuilders =
    /// the `heap { ... }` computation expression
    let heap = HeapBuilder()

    /// non-CE form: `HeapSg.ofChild (sg { ... })`
    [<RequireQualifiedAccess>]
    module HeapSg =
        let ofChild (child : ISceneNode) : ISceneNode = HeapNode child :> ISceneNode
