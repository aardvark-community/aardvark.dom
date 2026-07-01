### 1.2.0-prerelease0008
* Offscreen `IRuntime.RenderTo` / `RenderToPickable` + `IRenderPickContext`; pick THROUGH a post-process warp via `Sg.PickContext` + the `PickContextCoord` rewrite (portal picking recurses into the offscreen scene).
* Heap picking composes with mixed pickable / `Sg.NoEvents` children — the `heap { }` builder forces pixel picking for its subtree, and non-pick buckets stay rendered-but-inert (needs Aardvark.Rendering 5.7.0-prerelease0030).
* macOS: zero-copy IOSurface sharing pushes the surface to the consumer (producer-as-client mach-port direction) instead of publishing as a server the browser never queried — the shared frame no longer stays black.
* RemoteHtmlBackend renders at device-pixel resolution (respects `devicePixelRatio`).

### 1.2.0-prerelease0007
* PickArgmin: drop `[<Literal>]` from `ResultFloats`. A literal in a `[<ReflectedDefinition>]` module compiles to a const field with no backing property, which poisons `Expr.TryGetReflectedDefinition` for EVERY method in the assembly — so heap-pick `Normal24.encode` (and any reflected function) became "not reflectable". Plain `let` = a bindable property.

### 1.2.0-prerelease0006
* SharedTexture: Y-flip the FBO→shared-image blit (`recordBlitInto` + `recordBlitIntoKeyed`, so Linux/dma-buf, macOS/IOSurface, and Windows/D3D11 all) — the zero-copy frame came out upside down because the render target is top-left origin but the shared texture is sampled bottom-left.

### 1.2.0-prerelease0005
* pick: run the pick-buffer readback on the render thread (`RenderMarshal` → `RunRender`) instead of the Kestrel worker — fixes an `AccessViolation` in `vkCmdCopyImageToBuffer` when a click's `Download` raced the render loop's resolve/encode. Staged a GPU argmin snap shader (`PickArgmin`) for a future constant-size readback.
* merged zero-copy shared-texture streaming (Windows D3D11 keyed-mutex transport) — orthogonal to picking.

### 1.2.0-prerelease0004
* heap picking: a `HeapNode` registers its pickable bucket with the pick context so one `HeapRenderObject` (N parts → 1 draw) still resolves clicks per part — via the heap's per-slot `HeapPickId` SSBO — and `SceneHandler` routes the pickable bundle through the PickId attachment pass, gating the decoder by pick mode (`pickModes`). Deregistration is wired through the bucket's per-slot free callback.
* bumped to Aardvark.Rendering/SceneGraph 5.7.0-prerelease0027 (heap O(1) picking: `ofRenderObjectsPicking` + `HeapRenderObject.IsPickable` + per-slot deregister).

### 1.2.0-prerelease0003
* memoize `ViewProjTrafo` per `(View, Proj)` like its sibling camera uniforms. It was the one camera uniform built as a fresh `AVal.map2` on every `TryGetUniform`, so every render object got a DISTINCT view-proj aval over the one camera — consumers that dedup global uniforms by aval identity (e.g. the heap arena) then saw N distinct sources and an O(N) fan-out per camera move. Now all render objects in a render control share ONE aval.
* depend on `Aardvark.Rendering 5.7.0-prerelease0018` (heap: unified ref-counted-per-aval uniforms, real double-precision uniforms, camera composites derived from `View`/`Proj`).

### 1.2.0-prerelease0002
* expose the UNFOLDED model-trafo stack as a well-known `ModelTrafoStack` uniform (`aval<aval<Trafo3d>[]>`, root->leaf `Trafo3d *` compose order) ALONGSIDE the folded `ModelTrafo`. A GPU-chain consumer (the heap) forces it ONCE and composes the per-leaf chain on the GPU — deduping shared/constant ancestor links across leaves and re-folding only on the GPU when one link is edited — while every non-chain consumer is untouched (they never request the name). Exposed unconditionally by `RenderObject.ofTraversalState`; the text/shape path keeps it consistent by prepending its own render trafo as the new outermost link.

### 1.2.0-prerelease0001
* updated to Aardvark.Rendering 5.7.0-prerelease0002 (SceneGraph, Rendering.GL/Vulkan, Application.Slim.GL/Vulkan, Rendering.Text)
* FShade.Core minimum bumped to 5.7.9
* `NodeBuilder.Yield`: replaced the flexible `#seq<DomNode>` overload with explicit overloads for `seq`, `list`, array, `IndexList` and `HashSet`. The flex constraint blocked F# CE overload resolution when several other `Yield` overloads exist (e.g. nesting an `if/else -> DomNode` inside a parent CE); concrete collection shapes resolve unambiguously.

### 1.1.9
* fix: picking was broken on HiDPI / Retina displays after the renderer started honoring `devicePixelRatio` (Aardworx.WebAssembly 1.2.7). `SceneHandler.HandlePointerEvent` / `HandleTapEvent` / `HandleWheelEvent` computed `pixel = original.ClientPosition - V2i original.ClientRect.Min` — CSS pixels from DOM events — and passed that straight to the pick-buffer read. With a 3× framebuffer (iPhone) the read happened at 1/3 the click location → wrong scope or none. `SceneEventLocation` also got the mismatched `pixel` (CSS) and `viewportSize` (fb), breaking any consumer doing `pixel / viewportSize` for NDC.
* convert at the input boundary instead: a new `toFbPixel` helper in `SceneHandler` scales the event position via the per-event `ClientRect.Size` vs. `viewportSize` ratio — self-consistent under any `RenderControl.PixelRatio` override (no `window.devicePixelRatio` lookup needed). Pixel space stays sub-pixel `V2d` end-to-end; rounding to `V2i` happens only at the GL read-edge.
* added `RenderControlInfo.ClientSize : aval<V2i>` (CSS pixel size of the canvas) alongside the existing `ViewportSize` (framebuffer pixels). Apps positioning HTML overlays via `Left`/`Top` should bind `let! cs = RenderControl.ClientSize` instead of `ViewportSize`. Shader / projection / NDC math keeps using `ViewportSize`. The split makes the unit explicit in the type — no DPR multiplication in app code. `ClientSize` is populated lazily from the first DOM event (`ClientRect.Size`); it's `V2i.II` before any event arrives, which is fine for the typical case where overlays are conditional on user interaction.

### 1.1.8
* fix: MSAA pick decoder could return the wrong scope at silhouettes when scopes of different modes shared an id space. Specifically — at a silhouette where MS resolve averaged a mode-A pickId with the cleared `0` background (e.g. Green `+2` and `0`), the result `+1` would round to a clean integer and look up `scopes.[1]`. If id 1 happened to belong to a *mode B* scope (e.g. Blue, encoded `-1`), the decoder returned Blue's scope but interpreted the pick attachment with mode A's layout — giving a garbage view-position to the wrong scope. Cross-mode silhouettes (mode A vs mode B pickable) had a symmetric flaw.
* introduces a `pickModes : Dict<int, bool>` populated at wrap time from `chooseChain.Final` (`true` = mode A, `false` = mode B). The decoder now rejects any candidate whose `sign(pixIdRaw)` doesn't match the registered scope's mode, before the layout-dependent decoding runs.
* `readIdAt` now also rejects non-integral reads — anything more than `0.01` from `round(f)` decodes to `0`. MSAA-resolved pickIds are only trustworthy when all 4 samples agreed (→ exact integer); averaged silhouette mixes that happen to land on a clean integer for the *center* pixel are caught instead by the neighbour validator (since adjacent pixels with different coverage produce different averages).
* same-id neighbour validation tightened from ≥ 2 to ≥ 3 of 8. Two adjacent silhouette pixels with similar coverage can produce the same near-integer averaged id; three in a 3×3 essentially never do.
* `pickModes` is cleaned up in `releaseId` alongside the other id-state dicts.

### 1.1.7
* **MSAA picking finally works**. Previously enabling `Samples > 1` broke picking entirely: the pick attachment was an MS-resolved `Rgba32f` buffer, and the `IntBitsToFloat`-encoded ids were denormals (1, 2, 3, …) which NVIDIA's MS resolve flushes to zero — every pickId came back as 0. Fixed by going to a single MS framebuffer with `Rgba32f` pick attachment, plain-float storage end-to-end (no bit-cast), and ≥2-of-8 same-id neighbour validation in a 33×33 spiral on the CPU. Silhouette pixels (which average two distinct ids into a non-integer) fail validation and the spiral moves on; interior pixels (all 4 samples agree) round-trip exactly.
* **`Sg.PixelSnapRadius : aval<int>`** — per-scope snap radius. Default `1` (absorbs the 1-pixel silhouette feather the validator rejects). Values are clamped to the global cap (16) at pick time. Snap walks a static disc of `(2R+1)²` offsets sorted by squared distance from center; first valid candidate wins. Pickable pixel hits and BVH-pickable ray hits compete homogeneously per offset (closer-in-depth wins).
* **BVH cone cull**: `BvhTree3d.GetIntersecting(hull : FastHull3d)` added. Built per-`Read` from the snap window's NDC sub-rect via `ViewProjection.toHull3d` so the spiral only ever ray-tests the small candidate set whose bounding boxes intersect the cone.
* **Pick attachment encoding rewritten** — all four channels are plain `float32`, never bit-cast:
  * Mode A: `(id, n24, depth, pi)` where `n24` is `Normal24.encode` (12+12 octahedron packed into ≤ 2²⁴, round-trips exactly through float32 mantissa).
  * Mode B: `(-id, pvp.x, pvp.y, pvp.z)` — direct per-component float32 storage of the user's `PickViewPosition`, no `(dir, length)` decomposition. Mode-B's surface normal is now estimated CPU-side from the same-id neighbours already gathered for validation (cross-product of the largest-area tangent pair, oriented toward camera).
  * Pick-effect cache prefix bumped from `fpick_` → `pickv3_` so older shader caches invalidate by name.
* **Id recycling**: `acquireId` / `releaseId` with `freeIds : SortedSet<int>` (smallest-first reuse). Per-`IRenderObject` `acquiredFor` tracks the multiset of scopes acquired during wrapping so a `MultiRenderObject` releases exactly N times when removed. Keeps the live id space dense well below the 2²⁴ float32 ceiling regardless of scope churn.
* **Hot path is allocation-free**: spiral inner loop (~805 iters per pointer move) uses mutable locals + a sentinel for "no winner yet" instead of `option<tuple>` chaining; `cullSet` pre-flattened to `CullEntry[]` (struct); `vp.Backward`, `proj.Backward`, `view.Forward/Backward` cached as locals; `PixelSnapRadius` `AVal.force`'d once per scope per `Read` via a small per-call cache; CPU normal estimation runs only after a winner is decided. Big win on the WebAssembly interpreter.
* **`ReadPickRegion`**: now returns `voption<PickRegion>` (raw `float32[]` + strides) for the new fast path, with the legacy closure-returning overload preserved for external callers.

### 1.1.6
* refactored pick-chain selector to use FShade 5.7.4's `Effect.Dependencies`. The old logic gated mode A vs B on `Map.containsKey "PickViewPosition" eff.Outputs` (a coarse "does the user effect declare it?" check) and used `hasAllInputs` over a fully-linked module to test whether the with-normal variant was viable (one `Effect.toModule` call per draw setup). Both replaced by a pure-map analysis: for each pick-relevant semantic (`ViewSpaceNormal`, `PickViewPosition`, `PickPartIndex`) we ask "can the user's effect produce this with the geometry's actual vertex attributes?" via `EffectDeps.resolveTop`.
* normal handling now has two paths, mirroring how pi/pvp work:
  1. user effect produces `ViewSpaceNormal` (per-pixel pick normals — e.g. normal mapping) → use theirs, skip injection
  2. geometry has `Normals` attribute → inject `viewSpaceNormalEffect` to synthesise vsn from the model normal
  3. neither → no-normal variant
* user effects that declare a pick output (e.g. `PickPartIndex`) but whose dep closure isn't satisfied by the geometry (e.g. they read `[<InstanceId>]` from a non-instanced render object) now degrade gracefully to the no-pi variant — previously this caused a silent attribute demand and "no buffer is bound to enabled attribute" at draw time.
* extracted `PickShader.chooseChain` / `PickShader.composePickChain` as pure, public helpers; added `Aardvark.Dom.Tests` project with unit tests covering each branch of the selector + the FShade `Positions0` frag-input-renaming quirk.
* requires FShade 5.7.4 (deps tracking is upstream).

### 1.1.5
* fix: `pickDepthBefore`'s input record declared `[<Depth>] d : float32` purely for record-shape reasons. FShade saw Depth as a fragment input with no upstream producer and routed it backwards as a required vertex attribute. Since real geometry never has a "Depth" attribute, `hasAllInputs` returned false for every with-normal pick chain — every RenderObject silently fell back to the no-normal variant which writes a constant 0 in the encoded-normal slot. Picked normals were always V3d.Zero. Replaced the input record with `{ [<FragCoord>] fc : V4f }` (FragCoord is `gl_FragCoord`, a GPU built-in that never becomes a varying), so depth seeding no longer pollutes the vertex input set.

### 1.1.4
* refactor: rewrote the PickShader chain so each effect declares only the fields it actually reads/writes. Previously every variant funneled through a fat `Vertex` record with `pi`/`pvp` baked in — composing with a user vertex shader that didn't carry those fields caused FShade to re-emit them as required vertex attributes (the same class of bug we'd been patching mode-by-mode in 1.1.1–1.1.3). Now: a small `viewSpaceNormalEffect` produces `vn` from `Normal`; `pickDepthBefore` writes only `Depth`; and each `pickFinal*` reads only `c`/`vn`/`d`/`pi`/`pvp` as needed. Mode B drops `pi` from the chain entirely (it was never read there). Mode A defaults `PartIndex` to `0` in the fragment when the user effect doesn't output `PickPartIndex` — never to `gl_InstanceID`. Net result: `pi` and `pvp` can only become required vertex inputs if the user's own effect already required them.

### 1.1.3
* fix: `pickIdBefore` also propagated `pi` because it took a `Vertex` record and `return { v with d = d }` carries every field forward — leaking `PickPartIndex` back into the chain even after switching to NoPi `pickIdNoPi`. Added `pickIdBeforeNoPi` (takes `VertexNoPi`) and use it in the same NoPi branches. Now PickPartIndex truly only appears as a vertex attribute when the user effect explicitly outputs it.

### 1.1.2
* `pickIdNoNormal` no longer reads `v.pi` — it writes a constant 0 in the alpha slot instead. The previous `vertexPickEffect`-based default broke when an intermediate user vertex shader (e.g. `DefaultSurfaces.stableTrafo`) sat between the injected `pickVertex` and the fragment chain, making FShade re-emit `PickPartIndex` as a real vertex attribute. The new fragment-side default makes the no-normal path always work without any vertex-attribute requirement; per-feature pick distinction in this fallback path is dropped (use the with-normal `pickEffect` if you need it).
* reverted the 1.1.1 chain change — `pickEffectNoNormal` no longer prepends `vertexPickEffect`.

### 1.1.1
* always inject `PickShader.vertexPickEffect` into the no-normal pick chain so `pi` (PickPartIndex) defaults to `gl_InstanceID`. Previously the no-normal path omitted it, causing FShade to emit `PickPartIndex` as a real vertex attribute that custom-built RenderObjects (e.g. batched tile renderers) had no easy way to supply, breaking the draw entirely.

### 1.1.0
* updated to Aardvark.Rendering 5.6.4 / FShade 5.7.3
* migrated all shader code to explicit float32/V*f types (FShade no longer silently lowers double to float)
* adopted Rendering 5.6 API changes (HashMap.tryFindV, DrawCallInfo[], OutputDescription.Framebuffer, Buffer.create uint64, Image.create parameter order)
* added Aardvark.FontProvider dependency (no longer transitive from Rendering.Text)

### 1.0.25
* SceneEventLocation/SceneEvent now carry a PartIndex (sub-entity id for batched picks)
* pick shader writes PartIndex to the previously-unused alpha slot in Mode A; user shaders can override via Semantic("PickPartIndex"), default is gl_InstanceID
* BVH-based picking and Mode-B (real-position) picking report PartIndex = 0

### 1.0.24
* fixed BVH picking not dispatching events when pixel readback returns nothing

### 1.0.23
* FreeFlyState.Enabled flag 
* FreeFlyController AnimationFinished message
* AnimationFinished Event for SimpleFreeFlyController

### 1.0.22
* fixed FreeFlyController bug with hanging keys

### 1.0.21
* fixed gizmo overlay event handling for non-fullscreen canvases

### 1.0.20
* added simple Gizmo-Overlay to Aardvark.Dom.Utilities

### 1.0.19
* enhanced FreeFlyController

### 1.0.18
* FreeFlyController now installs touchsticks for touch-devices

### 1.0.17
* improved Gamepad support in FreeFlyController

### 1.0.16
* added basic Gamepad support 

### 1.0.15
* fixed aardvark.registerSelector again

### 1.0.14
* fixed aardvark.registerSelector

### 1.0.13
* added IHtmlBackend.NewId/Register for better interop with existing HTML elements

### 1.0.12
* SceneHandler again uses RGBA32F for rendering Pick Information (better compatibility)

### 1.0.11
* again using integer pick-buffer to better support WebGL/GLES

### 1.0.10
* disabled blending for PickBuffer

### 1.0.9
* SceneEvent pick-rays are now really independent of pick-depth

### 1.0.8
* added SimpleFreeFlyController

### 1.0.7
* fixed `Aardvark.Dom.Utilites` package name typo

### 1.0.6
* added simple utility for starting a server
* added `Aardvark.Dom.Utilites` providing a simple OrbitController
* added PickRays to SceneEvent

### 1.0.5
* fixed Effect-IDs avoiding wrong cache-hits

### 1.0.4
* SceneHandler now used RGBA32F for rendering Pick Information (better compatibility)

### 1.0.3
* MS rendering now uses Renderbuffers (better compatibility)

### 1.0.2
* geometric (IIntersectable) picking now respects `Sg.Active`
* Fixed render control samples attribute
* Fixed multisampled raw download

### 1.0.1
* net8.0 updates 

### 1.0.0-prerelease0040
* updated rendering packages
* added `App.startAndGetEnv`
* added several `Sg.VertexAttribute` overloads for single-values
* pinned dotnet to 6.0.100

### 1.0.0-prerelease0039
* IIntersectable can now set explicit hit-position

### 1.0.0-prerelease0038
* added Aardvark.Dom.Bootstrap

### 1.0.0-prerelease0036
* added extension library for dom builders

### 1.0.0-prerelease0035
* added new yield overloads for dom builders

### 1.0.0-prerelease0034
* moved worker to Aardvark.Dom.Core

### 1.0.0-prerelease0033
* added Worker-API sketch

### 1.0.0-prerelease0032
* yet another pointerCapture/Tap problem

### 1.0.0-prerelease0031
* fixed another pointerCapture/Tap problem

### 1.0.0-prerelease0030
* fixed composition problem with pointerCapture and Tap

### 1.0.0-prerelease0029
* made PixelPick-Shader creation lazy

### 1.0.0-prerelease0028
* updated packages

### 1.0.0-prerelease0027
* fixed `Sg.PickThrough` problem

### 1.0.0-prerelease0026
* added `Sg.PickThrough` currently only working on intersectable nodes

### 1.0.0-prerelease0025
* simplified `Sg.Active`

### 1.0.0-prerelease0024
* several small improvements
* added support for modified pixel-picking via returning `PickViewPosition`. user-shaders may return a replacement for the view-position which will be respected by the picking system (note that it may also differ in the XY-coordinates)

### 1.0.0-prerelease0023
* added Env.RunModal (allowing for temporary UI in root-node)

### 1.0.0-prerelease0022
* added RenderControl.OnReady event

### 1.0.0-prerelease0021
* proper constant propagation for NodeList/AttributeTable

### 1.0.0-prerelease0020
* fixed InputEvent parser for non-text inputs

### 1.0.0-prerelease0019
* inlined all builder methods

### 1.0.0-prerelease0018
* several Sg mode helpers (BlendMode, DepthBias, etc.)
* attachment-states (WriteMask & BlendMode) are now additive (can be set separately per attachment)
* Sg.Shader can now yield Effects directly

### 1.0.0-prerelease0017
* DisableImplicitFSharpCoreReference for all projects

### 1.0.0-prerelease0016
* added strongly typed PointerType
* Event inheritance now reflects the one in HTML/JS

### 1.0.0-prerelease0015
* SceneEvents are now wrapping real HTML events and therefore include all their properties
* Events now include the (optional) source JSON data (allowing for access to missing properties)

### 1.0.0-prerelease0014
* added Tap, DoubleTap and LongPress events

### 1.0.0-prerelease0013
* RenderControl now uses mouse and touch events instead of PointerEvents (experimental)

### 1.0.0-prerelease0012
* removed accidental print of TouchEvents

### 1.0.0-prerelease0011
* fixed getting `TouchList` type in aardvark-dom.js (breaks on safari)

### 1.0.0-prerelease0010
* Dom.OnTouchStart/Move/End/Cancel

### 1.0.0-prerelease0009
* Sg PointerEvents get Button instead of int
* Sg.Text no longer uses Aardvark.SceneGraph
* added Sg.Shape
* Sg.Text now includes `pickBounds : bool` option

### 1.0.0-prerelease0008
* Sg.Cursor now takes a string
* Dom KeyboardEvents and InputEvents
* Sg KeyboardEvents now take HTML `Key`, `KeyLocation` and `Code` directly

### 1.0.0-prerelease0007
* Sg.Delay
* Dom.OnBeforeInput
* delayed loading of FontSquirrel.Hack and exposed Text-RenderStyle

### 1.0.0-prerelease0006
* OnChange/OnInput

### 1.0.0-prerelease0005
* respecting depth-writes

### 1.0.0-prerelease0004
* Improved normal-detection

### 1.0.0-prerelease0003
* Normal32 int encoding

### 1.0.0-prerelease0002
* removed stopPropagation from event-listeners

### 1.0.0-prerelease0001
* initial package 