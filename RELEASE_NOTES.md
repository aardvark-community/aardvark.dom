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