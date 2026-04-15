# AGENTS.md

Orientation for AI coding agents working in this repository. Humans should
read `README.md` first for the framing; this document focuses on things that
are non-obvious from reading the source and on conventions that are easy to
violate by accident.

## What this repository contains

Aardvark.Dom is an F# framework that unifies HTML-style DOM construction and
Aardvark 3D scene graphs under a single computation-expression DSL. A program
is a tree of `body { }`, `div { }`, `input { }`, `renderControl { }`, and
`sg { }` nodes. Nodes compose by lexical nesting, attributes flow from
parents to children, and a single event system routes pointer, keyboard,
wheel, touch, and focus events through both the DOM part and the 3D part of
the tree.

The framework itself is host-agnostic. Hosts differ in how rendered frames
and events reach the client: over a websocket plus video stream
(`Aardvark.Dom.Server`), over a local shared-memory transfer to a Chromium
shell (Aardium), or entirely in the browser via WebGL and Blazor
WebAssembly (`aardworx.webassembly`).

## Project layout

| Project                          | Role                                                                                                          |
|----------------------------------|---------------------------------------------------------------------------------------------------------------|
| `Aardvark.Dom.Core`              | Low-level worker and core abstractions shared by all hosts.                                                   |
| `Aardvark.Dom`                   | The DSL, the scene graph, the event system, and the pick infrastructure.                                      |
| `Aardvark.Dom.Bootstrap`         | Pre-styled HTML components built on Bootstrap (accordion, dropdown, modal, numeric input, select, slider, ...).|
| `Aardvark.Dom.Elm`               | Elm-style `App<'Model, 'Msg>` scaffolding for update-loop applications.                                       |
| `Aardvark.Dom.Remote`            | Abstract remote HTML backend; defines the protocol used by remote hosts.                                      |
| `Aardvark.Dom.Server`            | Concrete remote host with JPEG streaming and shared-memory image transfer.                                    |
| `Aardvark.Dom.Giraffe`           | Giraffe/ASP.NET integration for the remote host.                                                              |
| `Aardvark.Dom.Utilities`         | Free-fly controller, orbit controller, gizmo overlay, other common scene helpers.                             |
| `Demo`                           | Runnable example exercising most of the framework. Start here when exploring behavior.                       |

Key subdirectories inside `Aardvark.Dom`:

* `Frontend.fs` — top-level DSL entry points. `NodeBuilder` instances for
  each HTML tag, `RenderControlBuilder`, and the `Dom.*` auto-open module.
* `Updater.fs` — `IHtmlBackend<'a>` abstraction plus the `Updater` that
  reconciles the adaptive view against whichever backend it is running on.
  Every host implements `IHtmlBackend`.
* `Events.fs` — DOM event record types (`PointerEvent`, `WheelEvent`,
  `KeyboardEvent`, `InputEvent`, `TapEvent`, ...).
* `Gamepad.fs` — gamepad input plumbing.
* `AdaptiveUtilities.fs` — helpers on top of `FSharp.Data.Adaptive`.
* `UI/` — attribute system: `Attribute.fs`, `AttributeMap.fs`, `DomNode.fs`,
  `Extensions.fs`.
* `SceneGraph/` — all the 3D pieces:
  * `SceneGraph.fs` — immutable scene-graph data structures.
  * `SceneFrontend.fs` — the `sg { }` DSL.
  * `SceneEvent.fs` — `SceneEvent`, `SceneEventLocation`, `IEventHandler`.
  * `SceneHandler.fs` — the evaluator that turns a scene graph into
    draw commands, runs pick readbacks, fuses pixel- and BVH-based picks,
    and dispatches events. **The most subtle file in the repository.**
  * `TraversalState.fs` — per-scope state that walks with the traversal;
    owns capture/bubble routing.
  * `BVH.fs`, `Intersectable.fs` — CPU-side picking against `IIntersectable`
    nodes.

Key subdirectories inside `Aardvark.Dom.Server`:

* `Jpeg/` — `TurboJpeg.fs`, `JpegTransfer.fs`, `JpegDownload.fs`. Streams
  encoded frames to the remote client.
* `SharedMemory/` — `SharedMemory.fs`, `SharedMemoryTransfer.fs`,
  `RawDownload.fs`. Implements the shared-memory image transfer used by
  Aardium.

## Build and development

```sh
./build.sh          # dotnet tool restore, paket restore, dotnet build
./build.cmd         # Windows equivalent
dotnet run --project src/Demo/Demo.fsproj
```

* **Dependencies are managed with Paket**, not NuGet directly. Edit
  `paket.dependencies` at the repository root and `paket.references` inside
  each project, then run `dotnet paket install` or `dotnet paket update`.
* **The SDK is pinned** in `global.json` to .NET 8.0 with
  `rollForward: latestFeature`. Do not add a different TFM without first
  verifying every project still builds.
* **Adaptify-generated files** (`*.g.fs`) are checked in. They are
  regenerated from their adjacent `.fs` model sources during the build. Do
  not hand-edit a `.g.fs` file; edit the source model and rebuild. Check the
  regenerated file in alongside the source change.
* **Demo is the canonical smoke test.** If you change the DSL, event system,
  or pick infrastructure, run the Demo project and confirm that pointer
  events, free-fly navigation, gizmo, and scene picking still behave.

## Release process

Releases are cut by adding an entry at the top of `RELEASE_NOTES.md` and
pushing to `master`. CI picks up the new top-most version string and
publishes the NuGet packages automatically.

Entry format, preserving the existing style exactly:

```
### 1.0.N
* short description of change 1
* short description of change 2
```

Guidelines:

* Bump the patch number for bug fixes and additive backward-compatible
  changes. Bump the minor number for larger additions that warrant more
  attention.
* Commit the release-notes entry together with the code changes it
  describes. One commit per release is the norm; the commit message should
  include the version.
* **Never publish packages manually.** CI is the only publisher.

## Scene graph and pick system

This section covers the parts of the framework that are most easily broken
by a well-intentioned refactor. Read it before changing `SceneHandler.fs`,
`SceneEvent.fs`, or any pick shader code.

### Pick buffer modes

The pick target is a single `Rgba32f` framebuffer. Three shader effects
write into it with three different slot layouts:

* **Mode A: `pickEffect` / `pickEffectNoNormal`.** Used by default and by
  batched picking.
  * `R = PickId` (int bits reinterpreted to float)
  * `G = oct-encoded normal32` (zero for the "no normal" variant)
  * `B = depth` (linearized)
  * `A = PartIndex` (int bits reinterpreted to float). Added in 1.0.25;
    the slot was previously unused. It is sourced from
    `Semantic("PickPartIndex")` on the pick vertex and defaults to
    `gl_InstanceID`.
* **Mode B: `pickIdWithRealPosition`.** Used when the caller needs the
  pick position in view space without relying on depth reconstruction.
  * `R = -PickId` (note the sign)
  * `G = oct-encoded normal32`
  * `B = oct-encoded view-space direction`
  * `A = distance along that direction`

All four Mode-B slots are occupied, so Mode B cannot carry `PartIndex`.
Batched picking that needs sub-entity identity must use Mode A.

**The sign of the first slot is the mode discriminator.** Positive means
Mode A, negative means Mode B. `ReadPickInfo` dispatches on this sign. Do
not change the discriminator semantics; if you need more fields, add a
second pick target.

### Effect composition

The pick path builds an FShade `Effect` by composing four pieces in order:

```
Effect.compose [
    vertexPickEffect       // reads VertexIn including gl_InstanceID, produces the through-Vertex
    pickEffectBefore       // captures depth via FragCoord
    userEffect             // the caller's own effect; may override reserved semantics
    pickEffect             // writes the final V4d into the pick buffer
]
```

User shaders opt into pick semantics by outputting them. Reserved names:

* `PickViewPosition : V3d` — view-space position, consumed by Mode B.
* `PickPartIndex : int` — sub-entity index written to Mode A's alpha slot.
* `ViewSpaceNormal : V3d` — view-space normal written to the G slot.

Pick-private semantics are intentionally prefixed with `Pick` so they cannot
collide with user shader semantics. Preserve the prefix when adding new
ones.

### BVH fallback and fusion

`SceneHandler.Read` runs **both** a pixel-based pick (framebuffer readback)
and a BVH-based pick (CPU ray intersection against `IIntersectable` nodes)
and fuses them by depth. Invariants that are easy to miss:

* BVH hits cannot carry `PartIndex`; they always report `0`. If you need
  sub-entity identity for a BVH-picked object, split it into multiple
  `IIntersectable` children.
* `PickThrough` allows an intersectable scope to pass a hit to the next
  closest scope, but only for BVH hits. A pick-through on a pixel-picked
  scope logs a warning and is otherwise a no-op.
* `capturedScope` (pointer capture) overrides routing. When a pointer is
  captured, `capturedResult` preserves `state :> obj` as the original target
  even though the dispatched event is re-parented onto the capturing scope.
  This is easy to break in refactors: do not silently replace `state :> obj`
  with `c :> obj`.
* The synthetic "no hit" locations used by focus-leave, key-without-pixel,
  and release-pointer-capture paths pass
  `viewPos = V3d(0, 0, -1e8)`, `normal = V3d.Zero`, `partIndex = 0` by
  convention. Match that convention if you introduce more such paths.

### Event routing and `SceneEventLocation`

`TraversalState` walks the scene-graph tree to dispatch capture and bubble
phases mirroring DOM semantics. `SceneEvent.WithKind` and
`SceneEvent.WithLocation` create derived events while preserving the rest
of the state including `PartIndex`, because the whole `SceneEventLocation`
is passed forward by reference.

**Rule of thumb.** Do not reconstruct a `SceneEventLocation` from individual
fields unless you genuinely need to change one. If you do, audit every
field: the current constructor takes
`(modelTrafo, local2World, viewTrafo, projTrafo, pixel, viewportSize, viewPos, viewNormal, partIndex)`.
Backward-compatible constructor overloads exist for the old 7- and 8-arg
forms and default `partIndex = 0`, but they silently lose the part index.
Prefer `event.WithLocation(...)` and `location.Transformed(...)` to the
full constructor whenever possible.

## Hosting backends

Aardvark.Dom runs under three backends. The application tree is portable
between them; the entry point and the `IHtmlBackend` implementation are
not.

* **Server-rendered** via `Aardvark.Dom.Remote` plus `Aardvark.Dom.Server`
  (and `Aardvark.Dom.Giraffe` for ASP.NET routing). The browser receives a
  video stream plus an event channel. Rendered frames are delivered either
  by JPEG streaming (`Jpeg/`) or, when the client is local, by shared-memory
  transfer (`SharedMemory/`).
* **In-browser WebAssembly** via
  [`aardworx.webassembly`](https://github.com/aardworx/aardworx.webassembly).
  A separate repository provides a WebGL-backed Aardvark runtime; Aardvark.Dom
  sits on top unchanged. Good for static hosting with no backend.
* **Native desktop** via
  [Aardium](https://github.com/aardvark-platform/aardium). Aardium is a
  small Chromium shell; Aardvark.Dom's server host hands it rendered frames
  through shared memory so there is no network round-trip.

Changes to the DSL, `Updater`, `IHtmlBackend`, or the event system must not
break any backend. When in doubt, build and smoke-test the Demo project
under the server backend and, if you have it, under Aardium.

## Conventions and constraints

* **F# style follows the rest of the Aardvark Platform.** PascalCase public
  names, camelCase locals, four-space indentation, no trailing whitespace.
* **Adaptive values everywhere.** State is `cval` / `cset` / `cmap`; views
  read `aval<'T>` and mutations happen inside `transact`. Do not introduce
  synchronous imperative state into the view layer.
* **No global mutable state** in the framework projects. `Demo` is allowed
  to cheat for illustration purposes.
* **Shader semantic names are contracts.** Renaming a semantic is a breaking
  change for any user effect that references it. Call out renames in the
  release notes.
* **Prefer editing over adding.** The codebase has a consistent shape; when
  a change fits an existing module, extend it rather than creating a new
  one.
* **Do not commit generated files by hand.** `*.g.fs` files are regenerated
  by Adaptify during the build. Edit the `.fs` source and rebuild.
* **Do not skip pre-commit or other hooks.** If a hook fails, fix the
  underlying issue rather than bypassing the hook.
* **Do not touch Mode-B pick layout or the sign discriminator** without a
  deliberate design discussion; it is relied on by downstream code in other
  repositories.

## When something is unclear

* `src/Demo/Program.fs` is the single best source of truth for DSL usage
  and runtime behavior.
* `src/Aardvark.Dom/SceneGraph/SceneHandler.fs` is the single best source of
  truth for how picking and event dispatch actually work.
* `RELEASE_NOTES.md` is the single best source of truth for recent
  user-visible behavior changes; scan it before assuming something is old.
* When in doubt about a host-specific behavior, read the relevant
  `IHtmlBackend` implementation under `Aardvark.Dom.Remote` /
  `Aardvark.Dom.Server`.
