module DemoCubes.Program

// aardvark.dom portal demo: the 8x8 pickable cube grid is rendered OFFSCREEN via
// `IRuntime.RenderToPickable`, then composited onto a fullscreen quad through a
// warping shader (animated bulge + ripple). The composite writes the source-uv
// it sampled into `PickContextCoord`, and mounts the offscreen render's
// `IRenderPickContext` via `Sg.PickContext` — so picking RECURSES through the
// warp into the inner scene: hover-highlight and click-to-delete still land on
// the correct cube even though it's visually distorted. The free-fly controller
// drives the INNER camera. Half the cubes force pixel picking, half use BVH.
//
// Hosting mirrors CadSceneDemo: Server.Start (the dom server) serves on :5000;
// Aardium (a browser) opens it as a desktop window. `--server` skips Aardium.

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application.Slim
open Aardvark.Dom
open Aardvark.Dom.Remote
open Aardvark.Dom.Utilities
open Aardium.Shared

/// The composite: sample the offscreen color at a WARPED uv and emit that uv as
/// `PickContextCoord`, so the pick patcher records where each screen pixel came
/// from in the inner image (that's what makes picking follow the warp).
module CompositeShader =
    open FShade

    let private colorSampler =
        sampler2d {
            texture uniform?ColorTex
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    type UniformScope with
        member x.WarpTime : float32 = uniform?WarpTime

    type Vertex = { [<Position>] pos : V4f; [<TexCoord>] tc : V2f }
    type FragIn = { [<TexCoord>] tc : V2f }
    type FragOut = { [<Color>] color : V4f; [<Semantic("PickContextCoord")>] uv : V2f }

    // pass NDC position + texcoord straight through (the quad is already fullscreen NDC).
    let vert (v : Vertex) = vertex { return v }

    let frag (v : FragIn) =
        fragment {
            let t = float32 uniform.WarpTime
            let c = v.tc - V2f(0.5f, 0.5f)
            let r2 = Vec.dot c c
            // PINCUSHION (pulls edges INWARD, so every sample stays in [0,1] — no
            // clamp smear) + a strong animated RIPPLE so the warp is clearly
            // visible and obviously non-affine.
            let warped =
                V2f(0.5f, 0.5f)
                + c * (1.0f - 0.28f * r2)
                + V2f(0.035f * sin(v.tc.Y * 14.0f + t),
                      0.035f * sin(v.tc.X * 14.0f + t))
            let col = colorSampler.Sample warped
            let r : FragOut = { color = col; uv = warped }
            return r
        }


let cubesApp (runtime : IRuntime) =

    let view (_env : Env<unit>) (_ : unit) =
        // one alive/hovered pair per cube; `alive=false` removes it from render AND picking.
        let cubes =
            [ for x in 0 .. 7 do
                for y in 0 .. 7 ->
                    let alive = cval true
                    let hovered = cval false
                    // checkerboard: half the cubes are pickable, half are Sg.NoEvents.
                    // both carry click/hover handlers — NoEvents must SUPPRESS them so only
                    // the pickable half ever hovers/deletes (tests both combos through dom).
                    let pickable = (x + y) % 2 = 0
                    (x, y, alive, hovered, pickable) ]

        // Two counters: blue (pickable) and total. Every click that lands must delete a BLUE
        // cube, so Δtotal must always equal Δblue — if a NoEvents (orange) cube ever got picked,
        // total would drop faster than blue. That invariant is the automated NoEvents proof.
        let countAlive (filter : bool -> bool) =
            let alives = cubes |> List.choose (fun (_, _, a, _, p) -> if filter p then Some (a :> aval<_>) else None)
            AVal.custom (fun t -> alives |> List.sumBy (fun a -> if a.GetValue t then 1 else 0))
        let remaining = countAlive id            // blue only
        let remainingTotal = countAlive (fun _ -> true)   // blue + orange

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
                (remaining, remainingTotal) ||> AVal.map2 (fun blue total ->
                    sprintf "distorted heap cubes — blue = pickable (hover/click to delete), orange = Sg.NoEvents (inert). picking follows the warp.  blue=%d total=%d" blue total)
            }

            renderControl {
                Style [Width "100%"; Height "100%"; Background "#202124"; Outline "none"]
                Samples 1
                TabIndex 0

                let! size = RenderControl.ViewportSize
                let! time = RenderControl.Time
                // the free-fly drives the INNER camera (the cubes we render offscreen).
                let! camView =
                    SimpleFreeFlyController {
                        Location = V3d(-4.0, -6.0, 8.0)
                        LookAt   = V3d(3.5, 3.5, 0.0)
                        Sky      = V3d.OOI
                        Config   = None
                        AnimationFinished = None
                    }

                // The outer scene is a fullscreen NDC composite — give it an identity camera so the
                // pick machinery doesn't warn about a missing view/proj. The REAL camera is the
                // inner one (camView, passed to RenderToPickable below).
                Sg.View (AVal.constant Trafo3d.Identity)
                Sg.Proj (AVal.constant Trafo3d.Identity)

                let innerProj =
                    size |> AVal.map (fun s ->
                        Frustum.perspective 60.0 0.1 200.0 (float s.X / float s.Y)
                        |> Frustum.projTrafo)

                // ---- INNER offscreen scene: the pickable cube grid ----
                let innerScene =
                    sg {
                        Sg.View camView
                        Sg.Proj innerProj
                        Sg.Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }
                        Sg.Cursor "pointer"
                        heap {
                            [
                                for (x, y, alive, hovered, pickable) in cubes ->
                                    let baseColor =
                                        if pickable then C4b(90uy, 140uy, 220uy, 255uy)   // blue = pickable
                                        else C4b(220uy, 150uy, 90uy, 255uy)               // orange = NoEvents
                                    let color =
                                        hovered |> AVal.map (fun h -> if h then C4b.White else baseColor)
                                    sg {
                                        Sg.Active alive
                                        Sg.Translate(float x, float y, 0.0)
                                        // NoEvents on the non-pickable half — even with handlers attached,
                                        // the heap must leave these rendered-but-unpickable (PixelPick=false).
                                        // The pickable half needs NO ForcePixelPicking anymore: the heap
                                        // builder forces pixel picking for its whole subtree.
                                        if not pickable then Sg.NoEvents
                                        Sg.OnPointerEnter(fun _ -> transact (fun () -> hovered.Value <- true))
                                        Sg.OnPointerLeave(fun _ -> transact (fun () -> hovered.Value <- false))
                                        Sg.OnClick(fun _ -> transact (fun () -> alive.Value <- false))
                                        Primitives.Box(V3d(0.8, 0.8, 0.8), color)
                                    }
                            ]
                        }
                    }

                let clearVals = clear { color C4f.Black; depth 1.0 }
                let result =
                    runtime.RenderToPickable(
                        innerScene, camView, innerProj, size, clearVals,
                        Map.ofList [
                            DefaultSemantic.Colors, TextureFormat.Rgba8
                            DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
                        ])
                let colorTex = result.Textures.[DefaultSemantic.Colors]
                let ctx = result.Pick

                // wrap to [0, 2π) BEFORE the shader — DateTime.Ticks/1e7 is ~6e10 for
                // 2026, and a float32 uniform that large drowns the spatial sin term
                // (ULP ~8000) → a frozen, spatially-constant ripple. mod 2π·1e7 keeps
                // it small AND seamless (sin has period 2π).
                let warpTime = time |> AVal.map (fun t -> float (t.Ticks % 62_831_853L) / 1.0e7)

                // ---- OUTER composite: warp the inner render, keep picking alive ----
                sg {
                    Sg.PickContext ctx
                    // crosshair over the portal's own background (inner-miss falls through to here);
                    // hovering a cube shows the inner cube's "pointer" cursor instead.
                    Sg.Cursor "crosshair"
                    Sg.DepthTest (AVal.constant DepthTest.None)
                    // pass the offscreen color as the IAdaptiveResource ITSELF (not AVal.map'd) so
                    // the render pipeline acquires/releases it with this quad — no manual leak.
                    Sg.Uniform("ColorTex", colorTex)
                    Sg.Uniform("WarpTime", warpTime)
                    Sg.Shader { CompositeShader.vert; CompositeShader.frag }
                    Primitives.ScreenQuad 0.0
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
