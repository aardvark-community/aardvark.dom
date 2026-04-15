# Aardvark.Dom

Aardvark.Dom is an F# framework for building interactive 3D applications
using the same mental model as the HTML DOM: a nested tree of elements with
attributes, events that capture and bubble, and a reactive data model that
re-renders when state changes. In addition to ordinary HTML nodes, the tree
can contain real 3D scenes, and those scenes follow the same nesting,
attribute, and event semantics as the rest of the page.

```fsharp
body {
    Style [ Background "#202124"; Color "white" ]

    h1 { "hello" }

    renderControl {
        Style [ Width "100%"; Height "600px" ]
        Samples 4

        SimpleFreeFlyController {
            Location = V3d(3,4,5)
            LookAt = V3d.Zero
            Sky = V3d.OOI
            Config = None
            AnimationFinished = None
        }
        Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }

        sg {
            Sg.Translate(1.0, 0.0, 0.0)
            Sg.OnClick (fun e -> printfn "clicked at %A" e.WorldPosition)
            Primitives.box
        }

        sg {
            Sg.Scale 0.5
            Sg.Cursor "pointer"
            Primitives.sphere
        }
    }
}
```

`body`, `h1`, `renderControl`, and `sg` are all nodes in one tree. Attributes
are set with keywords like `Style`, `Samples`, and `Scale`, event handlers are
attached with `OnClick` or `OnPointerEnter`, and children nest by lexical
containment. The element that happens to be 3D is `renderControl`, and the
scene inside it is built from `sg { }` blocks that transform and compose
exactly like HTML.

## Features

* **Unified DSL for DOM and 3D.** Computation expressions such as `body { }`,
  `div { }`, `input { }`, `renderControl { }` and `sg { }` all compose in a
  single program. No HTML, CSS, or JavaScript is required.
* **Consistent event model.** Pointer, keyboard, wheel, touch, tap, and focus
  events work on both HTML elements and 3D scene nodes. 3D events carry the
  picked world, view, and local positions, the surface normal, the pick ray,
  and (as of 1.0.25) a sub-entity index for routing picks inside batched
  geometry. Events capture and bubble along the combined tree.
* **Adaptive state.** Application state lives in `cval`, `cset`, and `cmap`
  values from `FSharp.Data.Adaptive`. Views read `aval<'T>` values and the
  framework re-runs only the parts that depend on what actually changed.
* **CSS-like scene-graph attributes.** `Sg.Scale`, `Sg.Translate`, `Sg.Shader`,
  `Sg.BlendMode`, `Sg.Cursor`, `Sg.PickThrough`, and others attach to `sg`
  nodes and flow down to their children the same way `color` or `font-family`
  flow down in CSS.
* **Utility library.** `Aardvark.Dom.Utilities` provides free-fly and orbit
  controllers, a gizmo overlay, text, gamepad and touch input, and an
  Elm-style update-loop helper.

## Hosting Options

Aardvark.Dom is host-agnostic. The same `body { … renderControl { … } }` tree
can run in several environments, and application code is usually portable
between them.

### Server-rendered (Giraffe / ASP.NET)

Application code runs on a server and rendering is performed on the server's
GPU. The browser receives a live video stream and a thin event channel. This
is what `Aardvark.Dom.Remote` and `Aardvark.Dom.Server` provide, and what the
`Demo` project demonstrates. It is a good fit for data-heavy applications
where the dataset should not be shipped to the client.

### In-browser via [`aardworx.webassembly`](https://github.com/aardworx/aardworx.webassembly)

The same DSL is compiled to Blazor WebAssembly and runs entirely in the
browser on WebGL. `aardworx.webassembly` provides the Aardvark rendering
backend for WASM; Aardvark.Dom sits on top and supplies the HTML-plus-3D
tree. This is a good fit for statically hostable applications (GitHub Pages,
object storage, CDNs) where a backend is undesirable.

### Native desktop via [Aardium](https://github.com/aardvark-platform/aardium)

Aardium is a small Chromium-based native shell. An Aardvark.Dom application
runs as a local process while Aardium hosts the window and the Chrome view.
Rendered images are delivered to the browser side via shared-memory image
transfer rather than a network socket: the GPU writes into a shared buffer
and the view reads from it directly. The result is native-application
responsiveness combined with the layout and input handling of the web stack.

## Choosing a Host

| Requirement                                                         | Recommended host                          |
|---------------------------------------------------------------------|-------------------------------------------|
| Zero-install web app with private or large data served from a GPU  | Giraffe/ASP.NET (`Aardvark.Dom.Server`)   |
| Statically hosted web app with no backend                           | `aardworx.webassembly` + Aardvark.Dom     |
| Native desktop tool in F# without a traditional UI framework        | Aardium + Aardvark.Dom                    |

All three hosts share the same application code. Moving between them is
typically a matter of swapping the entry point.

## Getting Started

* See `src/Demo/Program.fs` for a running example that exercises most of the
  framework: styled HTML, controllers, shaders, nested `sg` blocks, pointer
  and keyboard events, gizmos, and gamepad input.
* The main NuGet package is `Aardvark.Dom`; additional packages cover the
  hosting options listed above.
* Aardvark.Dom is part of the [Aardvark Platform](https://github.com/aardvark-platform),
  an open-source F# stack for visual computing and real-time graphics.

## License

Apache-2.0.
