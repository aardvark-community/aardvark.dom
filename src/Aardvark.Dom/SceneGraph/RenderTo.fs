namespace Aardvark.Dom

open Aardvark.Base
open Aardvark.Application
open Aardvark.Rendering
open FSharp.Data.Adaptive

// ============================================================================
//  Offscreen rendering + picking (RenderTo / IRenderPickContext)
//
//  These wrap the `PickProducer` (the "compile a scene into a RenderTask +
//  provide picking" half of SceneHandler) so a scene can be rendered offscreen
//  into adaptive textures WHILE staying pickable through the very same patched
//  pipeline the window uses. `IRuntime.RenderTo` is the plain (non-pick)
//  offscreen primitive every post pass (blur / AO / warp / composite) uses;
//  `RenderToPickable` additionally returns an `IRenderPickContext`.
//
//  NOTE: the context is `IRenderPickContext` (View/Proj/Pick/ref-count), which
//  is distinct from the heap's minimal `IPickContext` (Register/Deregister,
//  TraversalState.fs) — different concern, deliberately not merged.
// ============================================================================

/// A resolved pick — everything in the innermost context's frame (the one that
/// actually rendered the hit), with the trafos FORCED at pick time (World is
/// meaningless without the stack that produced it, and those are all avals that
/// move). Local = World |> ModelTrafo.Backward, clip via View*Proj, etc.
type PickResult =
    {
        /// world-space hit position
        World          : V3d
        /// innermost picked node's model trafo (forced @ pick time)
        ModelTrafo     : Trafo3d
        /// innermost context's view (forced @ pick time)
        ViewTrafo      : Trafo3d
        /// innermost context's proj (forced @ pick time)
        ProjTrafo      : Trafo3d
        /// innermost node's traversal state — the dispatcher's input
        TraversalState : TraversalState
    }

/// A rendered scene's picking surface. NOT adaptive — just ref-counted: hold it
/// (or any texture it produced) and the offscreen render stays live; dispose it
/// to release the producer. Hides the bvh / traversal map / mode-A|B internals;
/// exposes an imperative pixel pick.
type IRenderPickContext =
    inherit System.IDisposable
    /// the minimal handle the pick patcher/dispatcher recurse through (Size + PickAt)
    inherit IPickSubContext
    /// ref-count only (no aval semantics) — keeps the underlying render live
    abstract member Acquire : unit -> unit
    abstract member Release : unit -> unit
    /// camera it rendered with (live)
    abstract member View : aval<Trafo3d>
    abstract member Proj : aval<Trafo3d>
    /// world hit + forced M/V/P + TraversalState. None = far plane / no object.
    abstract member Pick : px : V2i -> voption<PickResult>

type RenderResult =
    {
        Textures : Map<Symbol, IAdaptiveResource<IBackendTexture>>
        Pick     : IRenderPickContext
    }

[<AutoOpen>]
module RuntimeRenderToExtensions =

    type IRuntime with

        /// Non-pick offscreen render: compile the scene's render objects and
        /// return the requested semantics as adaptive textures. This is the
        /// primitive every post-processing pass (blur / AO / warp / composite)
        /// is built from — plain scenegraphs, no picking.
        member runtime.RenderTo(scene : ISceneNode, size : aval<V2i>, clear : ClearValues, semantics : Map<Symbol, TextureFormat>) : Map<Symbol, IAdaptiveResource<IBackendTexture>> =
            let signature = runtime.CreateFramebufferSignature(Map.toList semantics)
            let ros, _ = scene.GetObjects(TraversalState.empty runtime)
            let task = runtime.CompileRender(signature, ros)
            let output = semantics |> Map.toSeq |> Seq.map fst |> Set.ofSeq
            RenderTask.renderSemanticsWithClear output size clear task

        /// Pickable offscreen render: same compilation, but through the patched
        /// pick pipeline (PickProducer). Returns the requested textures plus an
        /// `IRenderPickContext` whose `Pick` reads the produced pick buffer.
        /// `view`/`proj` are the inner camera used for the pick reconstruction
        /// (the scene itself still carries its own camera uniforms).
        member runtime.RenderToPickable(scene : ISceneNode, view : aval<Trafo3d>, proj : aval<Trafo3d>, size : aval<V2i>, clear : ClearValues, semantics : Map<Symbol, TextureFormat>) : RenderResult =
            // PickProducer adds the PickId attachment onto this signature itself.
            let signature = runtime.CreateFramebufferSignature(Map.toList semantics)
            // offscreen ⇒ no DOM trigger; its own size cval (the task fills it in).
            let producer = new PickProducer(signature, ignore, scene, view, proj, cval V2i.Zero)

            // honour the requested background: the producer clears its OWN buffer and then blits the
            // result over the output, so the output-side clear never shows — map the `clear` color
            // onto the producer's ClearColor instead (this is what the offscreen background becomes).
            match clear.[DefaultSemantic.Colors] with
            | Some cc ->
                let c = cc.Float
                transact (fun () -> producer.ClearColor.Value <- C4f(c.X, c.Y, c.Z, c.W))
            | None -> ()

            // The producer's task (a `PickRenderTask`) reports its runtime +
            // output signature, so the standard renderTo can allocate a target
            // and drive it. The producer clears its own buffers and blits the
            // color out; pulling the returned texture keeps the whole render
            // live and refreshes `producer.Pick`.
            let color = producer.RenderTask |> RenderTask.renderToColorWithClear size clear

            // shared resolver — the innermost frame, forced at pick time.
            let resolve (px : V2i) =
                match producer.Pick(px, SceneEventKind.Click) with
                | Some (scope, viewPos, _normal, _partIndex, next) ->
                    let target = match next with | Some n -> n | None -> scope
                    let v = AVal.force producer.View
                    let p = AVal.force producer.Proj
                    let world = v.Backward.TransformPos viewPos
                    let model = TraversalState.modelTrafo target |> AVal.force
                    ValueSome (struct (world, model, v, p, target))
                | None -> ValueNone

            let ctx =
                { new IRenderPickContext with
                    member _.Acquire() = color.Acquire()
                    member _.Release() = color.Release()
                    member _.View = producer.View
                    member _.Proj = producer.Proj
                    // IPickSubContext — used by the outer pick resolver to recurse a portal.
                    member _.Size = size
                    member _.PickAt(px : V2i) = resolve px
                    member _.Pick(px : V2i) =
                        match resolve px with
                        | ValueSome (struct (world, model, v, p, target)) ->
                            ValueSome {
                                World = world
                                ModelTrafo = model
                                ViewTrafo = v
                                ProjTrafo = p
                                TraversalState = target
                            }
                        | ValueNone -> ValueNone
                    member _.Dispose() = (producer :> System.IDisposable).Dispose() }

            { Textures = Map.ofList [ DefaultSemantic.Colors, color ]
              Pick     = ctx }
