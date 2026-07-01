module DemoCubes.Program

// A tiny aardvark.dom desktop demo that exercises the picking/hover machinery:
// an 8x8 grid of cubes, driven by a SimpleFreeFlyController. Hovering a cube
// highlights it (pointer enter/leave); clicking it deletes it (Sg.Active gate).
// Half the cubes force PIXEL picking (Sg.ForcePixelPicking), the other half use
// the default BVH ray-pick — so a single scene tests both pick paths + the
// enter/leave cursor state machine at once.
//
// Hosting mirrors CadSceneDemo: Server.Start (the Aardvark.Dom.Giraffe dom
// server) serves the app on localhost:5000; Aardium (a browser) opens it as a
// desktop window. `--server` skips Aardium and just serves for a real browser.

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application.Slim
open Aardvark.Dom
open Aardvark.Dom.Remote
open Aardvark.Dom.Utilities
open Aardium.Shared

let cubesApp (_runtime : IRuntime) =

    let view (_env : Env<unit>) (_ : unit) =
        // one alive/hovered pair per cube; `alive=false` removes it from render
        // AND picking (Sg.Active gates both), so delete-on-click just flips it.
        let cubes =
            [ for x in 0 .. 7 do
                for y in 0 .. 7 ->
                    let alive = cval true
                    let hovered = cval false
                    let forcePixel = (x + y) % 2 = 0   // checkerboard: half pixel-pick, half BVH
                    (x, y, alive, hovered, forcePixel) ]

        let remaining =
            let alives = cubes |> List.map (fun (_, _, a, _, _) -> a :> aval<_>)
            AVal.custom (fun t -> alives |> List.sumBy (fun a -> if a.GetValue t then 1 else 0))

        body {
            Style [
                Background "#202124"; Color "white"; FontFamily "monospace"
                Margin "0"; Padding "0"; Overflow "hidden"; Width "100vw"; Height "100vh"
            ]

            div {
                Style [
                    Position "fixed"; Top "8px"; Left "8px"; ZIndex 10
                    FontSize "13px"; Padding "6px 10px"
                    Background "rgba(0,0,0,0.45)"; BorderRadius "6px"
                ]
                remaining |> AVal.map (fun n ->
                    sprintf "8x8 cubes — hover to highlight, click to delete. Blue cubes force PIXEL picking, orange use BVH.  (%d left)" n)
            }

            renderControl {
                Style [Width "100%"; Height "100%"; Background "#202124"; Outline "none"]
                Samples 4
                TabIndex 0

                SimpleFreeFlyController {
                    Location = V3d(-4.0, -6.0, 8.0)
                    LookAt   = V3d(3.5, 3.5, 0.0)
                    Sky      = V3d.OOI
                    Config   = None
                    AnimationFinished = None
                }

                let! size = RenderControl.ViewportSize
                let proj =
                    size |> AVal.map (fun s ->
                        Frustum.perspective 60.0 0.1 200.0 (float s.X / float s.Y)
                        |> Frustum.projTrafo)
                Sg.Proj proj

                Dom.Style [Css.Cursor "crosshair"]
                Sg.Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }

                sg {
                    Sg.Cursor "pointer"
                    [
                        for (x, y, alive, hovered, forcePixel) in cubes ->
                            let baseColor =
                                if forcePixel then C4b(90uy, 140uy, 220uy, 255uy)
                                else C4b(220uy, 150uy, 90uy, 255uy)
                            let color =
                                hovered |> AVal.map (fun h -> if h then C4b.White else baseColor)
                            sg {
                                Sg.Active alive
                                Sg.Translate(float x, float y, 0.0)
                                if forcePixel then Sg.ForcePixelPicking
                                Sg.OnPointerEnter(fun _ -> transact (fun () -> hovered.Value <- true))
                                Sg.OnPointerLeave(fun _ -> transact (fun () -> hovered.Value <- false))
                                Sg.OnClick(fun _ -> transact (fun () -> alive.Value <- false))
                                Primitives.Box(V3d(0.8, 0.8, 0.8), color)
                            }
                    ]
                }
            }
        }

    {
        initial = ()
        update = fun _ () () -> ()
        view = view
        unpersist =
            {
                init = fun () -> ()
                update = fun () () -> ()
            }
    }


[<EntryPoint>]
let main argv =
    Aardvark.Init()
    Aardium.Init(null)

    use application = new VulkanApplication(false)
    let runtime = application.Runtime :> IRuntime

    let task = Server.Start(runtime, fun ctx -> App.start ctx (cubesApp runtime))

    if argv |> Array.contains "--server" then
        Log.line "serving on http://localhost:5000/ (open in a browser)"
        task.Wait()
    else
        Aardium.run {
            url "http://localhost:5000/"
            fullscreen true
        }
    0
