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