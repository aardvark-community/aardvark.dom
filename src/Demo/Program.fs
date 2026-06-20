module Program

open System.Threading
open System.Threading.Tasks
open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Giraffe
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Application
open Aardvark.Dom
open Aardvark.Dom.Remote
open Aardvark.Rendering
open Aardvark.Application.Slim
open Demo
open Aardvark.Dom.Utilities

// L2: a rotating shaded cube rendered offscreen, streamed zero-copy into the browser.
// In its own module so `open Aardvark.SceneGraph` (whose `Sg` clashes with Aardvark.Dom's)
// stays local. Renders to a Bgra8 colour texture so the copy into the BGRA ring is a
// straight vkCmdCopyImage (no R/B swizzle).
module ST = Aardvark.Dom.Remote.SharedTexture.FdHandoff

module CubeRender =
    open Aardvark.Base
    open Aardvark.Rendering
    open Aardvark.SceneGraph
    open FShade
    open FSharp.Data.Adaptive

    type T = { angle : cval<float>; color : IAdaptiveResource<IBackendTexture>; dispose : unit -> unit }

    let create (runtime : IRuntime) (w : int) (h : int) : T =
        Aardvark.Base.IntrospectionProperties.CustomEntryAssembly <- System.Reflection.Assembly.GetAssembly(typeof<ISg>)
        let signature =
            runtime.CreateFramebufferSignature([
                DefaultSemantic.Colors, TextureFormat.Bgra8
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
            ])
        let angle = cval 0.0
        let sg =
            Sg.box' (C4b(255uy, 150uy, 30uy, 255uy)) (Box3d.FromCenterAndSize(V3d.Zero, V3d(2.0, 2.0, 2.0)))
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.trafo (angle |> AVal.map (fun a -> Trafo3d.RotationZ a * Trafo3d.RotationX (a * 0.5)))
            |> Sg.viewTrafo (CameraView.lookAt (V3d(5.0, 5.0, 4.0)) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> AVal.constant)
            |> Sg.projTrafo (Frustum.perspective 60.0 0.1 100.0 (float w / float h) |> Frustum.projTrafo |> AVal.constant)
        let task = sg |> Sg.compile runtime signature
        // opaque dark background so the cube is high-contrast (and unambiguous in a screenshot)
        let clearValues = clear { color (C4b(10uy, 12uy, 34uy, 255uy)); depth 1.0 }
        let color = task |> RenderTask.renderToColorWithClear (AVal.constant (V2i(w, h))) clearValues
        color.Acquire()
        { angle = angle; color = color
          dispose = fun () -> (try color.Release() with _ -> ()); (try task.Dispose() with _ -> ()) }

    /// Advance the rotation and render the frame; returns the freshly-rendered colour texture.
    let render (t : T) (frame : int) : IBackendTexture =
        transact (fun () -> t.angle.Value <- float frame * 0.04)
        t.color.GetValue()

    /// Free the offscreen FBO/render task (used when the surface is reallocated on resize).
    let destroy (t : T) = t.dispose ()

module Shader =
    open FShade
    
    type Fragment =
        {
            [<Semantic("PickViewPosition")>] vp : V3f
        }

    let withViewPos (v : Effects.Vertex) =
        fragment {
            let vp = uniform.ProjTrafoInv * v.pos
            let vp = vp.XYZ / vp.W
            let vp = vp + V3f(0.1f, 0.0f, 0.0f)
            return { vp = vp.XYZ }
        }
    
/// Gradient fidelity check for the OPAQUE_FD tiling test: expects R=x-ramp, G=y-ramp, B=128
/// (what OpaqueFd.fillGradient wrote) at 9 spread-out points. A scrambled OPTIMAL tiling fails.
module OpaqueGrad =
    let check (tag : string) (buf : byte[]) (w : int) (h : int) : bool =
        let pts = [ (0,0); (w-1,0); (0,h-1); (w-1,h-1); (w/4,h/4); (3*w/4,h/4); (w/4,3*h/4); (3*w/4,3*h/4); (w/2,h/2) ]
        let mutable allok = true
        for (x, y) in pts do
            let (r, g, b, a) = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.pixelAt buf w x y
            let er, eg, eb, ea = x * 255 / (w - 1), y * 255 / (h - 1), 128, 255
            let ok = abs (r-er) <= 4 && abs (g-eg) <= 4 && abs (b-eb) <= 4 && abs (a-ea) <= 4
            if not ok then allok <- false
            printfn "[%s] (%d,%d) got=(%d,%d,%d,%d) exp=(%d,%d,%d,%d) %s" tag x y r g b a er eg eb ea (if ok then "ok" else "BAD")
        printfn "[%s] gradient fidelity: %s" tag (if allok then "PASS — tiling preserved" else "FAIL — scrambled")
        allok

let testApp (_runtime : IRuntime) =
    let content = cval 0
    let text = cval ""
    //let click (evt : MouseEvent) =
    //    transact (fun () -> 
    //        content.Value <- int (round evt.ClientX)
    //        text.Value <- JsonSerializer.Serialize(evt, JsonSerializerOptions(WriteIndented = true))
    //    )

    let down = cset[]
    let pos = cval V2d.Zero
    let frameTime = cval ""

    let view (env : Env<unit>) (_) = 
        body {
            Style [ 
                Background "#36363A"
                Color "white"
                FontFamily "monospace"
            ]
        
        
            Dom.OnGamepadAxisChange(fun e ->
                printfn "%s/%s: %.3f" e.ControllerId  e.AxisName e.Value 
            )

            input {
                Type "checkbox"
                Checked true
                OnChange (fun e -> printfn "%A" e.Checked)
            }   
            
            input {
                Type "text"
                Value "hans"
                OnChange (fun e -> printfn "%A" e.Value)
                OnInput (fun e -> printfn "%A %A %A" e.InputType e.Data e.Value)
            }   
            
            h1 {
                Id "foo"
                Class "bar"
                content |> AVal.map string


                Dom.OnTouchStart(fun evt -> 
                    transact (fun () -> 
                        text.Value <- JsonSerializer.Serialize(evt, JsonSerializerOptions(WriteIndented = true))
                    )
                )

                Dom.OnTouchMove(fun evt -> 
                    transact (fun () -> 
                        text.Value <- JsonSerializer.Serialize(evt, JsonSerializerOptions(WriteIndented = true))
                    )
                )
            }

            ul {
                Dom.OnTap((fun e -> printfn "capture tap 0 %A %A" e.DeltaTime e.Movement), true)
                Dom.OnTap((fun e -> printfn "bubble tap 0 %A %A" e.DeltaTime e.Movement), false)
                Dom.OnDoubleTap((fun e -> printfn "capture doubletap 0 %A %A" e.DeltaTime e.Movement), true)
                Dom.OnDoubleTap((fun e -> printfn "bubble doubletap 0 %A %A" e.DeltaTime e.Movement), false)
                Dom.OnLongPress((fun e -> printfn "capture longpress 0"), true)
                Dom.OnLongPress((fun e -> printfn "bubble longpress 0"), false)
                li { "Hans" }
                li {
                    frameTime
                    Dom.OnTap((fun e -> printfn "capture tap 1 %A %A" e.DeltaTime e.Movement), true)
                    Dom.OnTap((fun e -> printfn "bubble tap 1 %A %A" e.DeltaTime e.Movement), false)
                    Dom.OnDoubleTap((fun e -> printfn "capture doubletap 1 %A %A" e.DeltaTime e.Movement), true)
                    Dom.OnDoubleTap((fun e -> printfn "bubble doubletap 1 %A %A" e.DeltaTime e.Movement), false)
                    Dom.OnLongPress((fun e -> printfn "capture longpress 1"), true)
                    Dom.OnLongPress((fun e -> printfn "bubble longpress 1"), false)
                    ul {
                        li { 
                            "Sepp" 
                            OnMouseEnter(fun e -> printfn "enter 2")
                            OnMouseLeave(fun e -> printfn "leave 2")
                            Dom.OnClick((fun e -> printfn "capture 2"; true), true)
                            Dom.OnClick((fun e -> printfn "bubble 2"; true), false)
                        }
                        li { "Franz" }
                    }
                }
            }

            
            let rand = RandomSystem()
            let markerColor = cval (rand.UniformC3f().ToC4b())
            let marker = cval (Ray3d(V3d.NaN, V3d.Zero))

            pre {
                
                Style [
                    Background "#202124"
                    Color "#00C800"
                    BorderRadius "20px"
                    Padding "15px"
                    PaddingLeft "20px"
                ]
                
                down |> ASet.isEmpty |> AVal.map (function
                    | true -> None
                    | false -> Some (Style [FontWeight "bold"])
                )


                Dom.OnPointerDown(fun e ->
                    transact (fun () -> 
                        text.Value <- "POINTER DOWN"
                    )

                )
                
                Dom.OnPointerMove(fun e ->
                    transact (fun () -> 
                        text.Value <- string e.ClientX
                    )

                )
                
                //Dom.OnTouchStart(fun e ->
                //    transact (fun () -> 
                //        text.Value <- "TOUCH DOWN"
                //    )

                //)
                
                //Dom.OnMouseMove(fun e ->
                //    transact (fun () -> 
                //        text.Value <- "MOUSE MOVE"
                //    )

                //)
                //OnBoot "console.log('hi there');"
                //OnShutdown "console.log('bye');"

                

                //OnContextMenu(click, useCapture = true, preventDefault = true)
                //Dom.OnClick(click, true)

                //Dom.OnPointerDown (fun e ->
                //    transact (fun () -> down.Add e.PointerId |> ignore)
                //    printfn "down\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                //)
                
                //Dom.OnPointerUp (fun e ->
                //    transact (fun () -> down.Remove e.PointerId |> ignore)
                //    printfn "up\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                //)

                down |> ASet.isEmpty |> AVal.map (function
                    | true -> []
                    | false -> 
                        [
                            Dom.OnPointerMove (fun e ->
                                printfn "move %d: %f,%f" e.PointerId e.ClientX e.ClientY
                                transact (fun () -> pos.Value <- V2d(e.ClientX, e.ClientY))
                            )
                        ]
                )


                text
            }
            
            down |> ASet.isEmpty |> AVal.map (function  
                | true -> []
                | false ->
                    [h3 { pos |> AVal.map string }; h4 { "foo"} ]    
            )

            down |> ASet.sort |> AList.map (fun d ->
                pre {
                    
                    OnShutdown("__THIS__.shutdown();")

                    Attribute(
                        "boot", 
                        AttributeValue.Execute(
                            [|
                            
                                fun (c : IChannel) ->
                                    task {
                                    
                                        let sw = System.Diagnostics.Stopwatch.StartNew()

                                        let mutable t : System.Threading.Timer = null 
                                        let tick _ =
                                            try c.Send(ChannelMessage.Text (string sw.Elapsed.TotalSeconds)).Result
                                            with _ -> ()

                                        t <- new System.Threading.Timer(TimerCallback(tick), null, 1000, 1000)
                                    
                                        let run () =
                                            task {
                                                let mutable running = true
                                                while running do
                                                    let! msg = c.Receive()
                                                    match msg with
                                                    | ChannelMessage.Close -> 
                                                        running <- false
                                                    | ChannelMessage.Binary a -> 
                                                        printfn "%A" a
                                                    | ChannelMessage.Text a ->
                                                        printfn "%A" a
                                            }

                                        do! run()
                                        printfn "CLOSE"
                                        t.Dispose()
                                    }
                                
                            |], 
                            fun n ->
                                let n = n.[0]
                                [
                                    $"{n}.onmessage = (e) => {{"
                                    $"  console.log(\"got\", e.data); "
                                    $"  {n}.send(e.data);"
                                    $"  let t = parseFloat(e.data);"
                                    $"  if(t >= 10.0) {n}.close();"
                                    $"}};"
                                    $"{n}.onclose = (e) => {{"
                                    $"  console.log(\"close\"); "
                                    $"}};"
                                    $"__THIS__.shutdown = () => {{ console.warn(\"shutdown\"); }}";
                                ]
                        )
                    )


                    $"pointer{d}"
                }
            )

            h5 { marker |> AVal.map (fun r -> if Vec.AnyNaN r.Origin then "no marker" else sprintf "%.2f %.2f %.2f" r.Origin.X r.Origin.Y r.Origin.Z) }
            
            let rotationTrafo (active : aval<bool>) (time : aval<System.DateTime>) =
                let seconds = 
                    let sw = System.Diagnostics.Stopwatch.StartNew()
                    time |> AVal.map (fun t ->
                        sw.Elapsed.TotalSeconds
                    )

                AVal.integrate Trafo3d.Identity time [
                    active |> AVal.map (function
                        | true ->
                            seconds |> AVal.step (fun t dt v ->
                                if dt > 0.1 then
                                    printfn "%.3f" dt
                                v * Trafo3d.RotationZ(dt)
                            )
                        | false ->
                            AdaptiveFunc.Identity
                    )
                ]
            let rotActive = cval true
            renderControl  {
                // HTML attributes
                Style [Width "100%"; Height "600px"; Background "#202124"; Outline "none"] 
                Samples 4
                Quality 50
                TabIndex 0
                
                SimpleFreeFlyController {
                    Location = V3d(3,4,5)
                    LookAt = V3d.Zero
                    Sky = V3d.OOI
                    Config = None
                    AnimationFinished = Some (fun () ->
                        printfn "animation finished"
                    )
                }
                
                // SimpleOrbitController {
                //     Location = V3d(3,4,5)
                //     Center = V3d.Zero
                //     RotateButton = Button.Left
                //     PanButton = Button.Middle 
                // }
                
                let mutable myHandler = None
                RenderControl.OnReady (fun handler ->
                    myHandler <- Some handler
                )
                
                Dom.OnPointerDown((fun e ->
                    transact(fun () -> rotActive.Value <- false)
                ), pointerCapture = true)
                
                Dom.OnPointerUp((fun e ->
                    transact(fun () -> rotActive.Value <- true)
                ), pointerCapture = true)
                
                //Sg.ForcePixelPicking
                //Sg.OnTap (fun e ->
                //    transact (fun () ->
                //        text.Value <- (JsonSerializer.Serialize(e, JsonSerializerOptions(WriteIndented = true)))
                //    )
                //)
                
                Dom.OnKeyDown((fun e ->
                    match myHandler with
                    | Some handler ->
                        handler.Read(handler.Size / 2, SceneEventKind.KeyDown) |> printfn "%A"
                    | None ->
                        printfn "no handler"
                    //printfn "Down %s" (JsonSerializer.Serialize(e, JsonSerializerOptions(WriteIndented = true)))
                ), preventDefault = true)
                
                Dom.OnKeyUp((fun e ->
                    ()
                    //printfn "Up %s" (JsonSerializer.Serialize(e, JsonSerializerOptions(WriteIndented = true)))
                ), preventDefault = true)


                Dom.OnMouseEnter(fun e ->
                    printfn "enter rc"
                )
                Dom.OnMouseLeave(fun e ->
                    printfn "leave rc"
                    transact (fun _ -> marker.Value <- Ray3d(V3d.NaN, V3d.Zero))
                )


                Dom.On("wheel", (fun (e : WheelEvent) -> ()), preventDefault = true)
                Dom.On("wheel", (fun (e : WheelEvent) -> ()), preventDefault = true, useCapture = true)
                Dom.On("contextmenu", (fun (e : Event) -> ()), preventDefault = true)


                // react to resize of the RenderControl
                RenderControl.OnResize(fun s ->
                    Log.warn "resize: %A" s
                )
                RenderControl.OnRendered(fun s ->
                    if s.FrameTime <> MicroTime.Zero then
                        transact (fun () -> frameTime.Value <- sprintf "%.3fms"  s.FrameTime.TotalMilliseconds)
                    () // some code here
                )

                // get the changeable size/time for the control
                let! size = RenderControl.ViewportSize
                let! time = RenderControl.Time

                // apply a camera
                //let view = CameraView.lookAt (V3d(3,4,5)) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> AVal.constant
                let proj = size |> AVal.map (fun s -> Frustum.perspective 80.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                //Sg.View view
                Sg.Proj proj
                
                gizmo {
                    Gizmo.Top 0
                    Gizmo.Left 0
                    Gizmo.TextLabels("E", "W", "N", "S", "+Z", "-Z")
                }
                

                // set the cursor to "crosshair" for the entire control and to "Hand" whenever a scene-element is hovered
                Dom.Style [Css.Cursor "crosshair"]
                
                

                // setup transformation and shaders
                
                Scale 3.0
                //Trafo (rotationTrafo rotActive time)
                Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }  
  
                // scene      
                sg {
                    Sg.Cursor "none"
                    // whenever something is hovered udpate the marker-arrow
                    Sg.OnPointerEnter(fun e ->
                        printfn "enter group"
                        transact (fun () -> 
                            markerColor.Value <- C4b.Red
                            rotActive.Value <- false
                        )
                    )
                    Sg.OnPointerLeave(fun e ->
                        printfn "leave group"
                        transact (fun () -> 
                            markerColor.Value <- C4b.Gray
                            rotActive.Value <- true
                        )
                    )
                    Sg.OnPointerMove(fun e ->
                        let m = e.ModelTrafo
                        printfn "move"
                        transact (fun () ->
                            if Vec.AllTiny e.Normal then
                                marker.Value <- Ray3d(e.Position, V3d.OOI)
                            else
                                marker.Value <- Ray3d(e.Position, e.Normal)
                        )
                    )
                    Sg.OnTap(fun e ->
                        transact (fun () -> 
                            markerColor.Value <- C4b.Green
                        )
                    )

                    // render a teapot and a sphere
                    sg {
                        Sg.Shader {
                            DefaultSurfaces.trafo
                            DefaultSurfaces.simpleLighting
                            Shader.withViewPos
                        }
                        Translate(0.5, 0.0, 0.0)
                        Sg.BlendMode BlendMode.Blend
                        Primitives.Teapot(C4b.Green)
                    }
                    
                    sg {
                        Sg.Shader {
                            DefaultSurfaces.trafo
                            DefaultSurfaces.simpleLighting
                        }
                        Translate(1.5, 0.0, 0.0)
                        Primitives.Teapot(C4b.Yellow)
                    }

                    sg {
                        Primitives.WireSphere(V3d(-0.5, 0.0, 0.25), 0.25)
                        Sg.OnClick (fun _ ->
                            env.RunModal (fun destroy ->
                                div {
                                    Style [Top "0px"; Left "0px"; Width "100%"; Height "100%"; Position "fixed"; Background "red"]
                                    Dom.OnClick (fun _ ->
                                        destroy.Dispose()    
                                    )
                                    
                                }
                            )
                            |> ignore
                        )
                    }
                    
                    sg {
                        Scale 0.5
                        Translate(0.0, 0.5, 0.0)
                        Primitives.Tetrahedron()
                    }
                }
                sg {
                    Sg.PickThrough
                    Sg.Scale 0.2
                    Sg.Translate (0.0, 0.0, 0.5)
                    Sg.Text(AVal.constant "Hi There", pickBounds = true)
                    // Sg.OnPointerEnter(fun e ->
                    //     printfn "enter text"
                    //     transact (fun () -> 
                    //         markerColor.Value <- C4b.Green
                    //         rotActive.Value <- false
                    //     )
                    // )
                    // Sg.OnPointerLeave(fun e ->
                    //     printfn "leave text"
                    //     transact (fun () -> 
                    //         markerColor.Value <- C4b.Gray
                    //         rotActive.Value <- true
                    //     )
                    // )
                    Sg.OnTap (fun e ->
                        transact (fun () -> text.Value <- e.WorldPosition.ToString())
                        if e.Target <> e.This then
                            e.Context.DispatchPointerEvent(e.Target, e) |> ignore
                            false
                        else
                            true
                    )
                }
                
                sg {
                    Sg.NoEvents
                    Sg.Shader {
                        DefaultSurfaces.trafo
                        DefaultSurfaces.constantColor C4f.Red
                        DefaultSurfaces.simpleLighting
                    }
                    Primitives.ScreenQuad -0.1
                    
                }
                
                // render the arrow
                sg {
                    Sg.NoEvents
                    let len = 0.1
                    let h = 0.03
                    let radius = 0.005
                    Primitives.Cone(marker |> AVal.map (fun r -> Cone3d(r.GetPointOnRay len, -r.Direction * h, Constant.PiQuarter / 2.0)), markerColor)
                    Primitives.Cylinder(marker |> AVal.map (fun r -> Cylinder3d(r.Origin, r.GetPointOnRay (len - h), radius)), markerColor)
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


    
let testApp2 (_runtime : IRuntime) =
    body {
        Style [ Position "fixed"; Overflow "hidden"; StyleProperty("touch-action", "none")]
        let content = cval "asdasd"
        div {
            Style [Width "400px"; Height "400px"; Background "green"; StyleProperty("-webkit-user-select", "none"); Position "fixed"; Overflow "hidden"]
            //Dom.OnPointerDown((fun e ->
            //    transact (fun () ->
            //        content.Value <- System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
            //    )
            //), pointerCapture = false, preventDefault = true)
            //Dom.OnPointerMove((fun e ->
            //    transact (fun () ->
            //        content.Value <- System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
            //    )
            //), preventDefault = true)
            
            Dom.OnTap(fun e ->
                transact (fun () ->
                    content.Value <- "TAP"
                    //content.Value <- System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
                )
            )
            Dom.OnDoubleTap(fun e ->
                transact (fun () ->
                    content.Value <- "DOUBLETAP"
                    //content.Value <- System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true))
                )
            )

            content
        }
    }


module Elm =
    open Adaptify
    open FSharp.Data.Adaptive

    type Message =
        | Inc
        | Dec
        | Down of Button
        | Up of Button


    let update (env : Env<Message>) (model : Model) (msg : Message) =
        match msg with
        | Down b -> { model with PressedButtons = HashSet.add b model.PressedButtons }
        | Up b -> { model with PressedButtons = HashSet.remove b model.PressedButtons }
        | Inc -> 

            task {
                let! v = env.StartTask "return document.body.getBoundingClientRect();"
                match v.TryGetProperty "x", v.TryGetProperty "y" with
                | (true, x), (true, y) -> printfn "%A %A" x y
                | _ -> printfn "NABSJKANs"
            } |> ignore

            { model with Count = model.Count + 1 }
        | Dec -> 
            { model with Count = model.Count - 1 }

    let view (env : Env<Message>) (model : AdaptiveModel) =
        body {
            h1 { model.Count |> AVal.map (sprintf "Count: %d") }

            div {
                Dom.OnMouseDown(fun e -> env.Emit (Down e.Button))
                Dom.OnMouseUp(fun e -> env.Emit (Up e.Button))
                Dom.OnContextMenu(ignore, preventDefault = true)

                
                //model.Count |> AVal.map (fun v ->
                //    div {
                //        Attribute(
                //            "boot", AttributeValue.Execute([|
                //                fun (c : IChannel) ->
                //                    task {
                //                        match! c.Receive() with
                //                        | ChannelMessage.Text str ->
                //                            env.Emit Inc
                //                        | _ ->
                //                            ()
                //                    }
                //            |], fun names ->
                //                [
                //                    $"{names.[0]}.onopen = function() {{"
                //                    $"    {names.[0]}.send(\"{v}\");"
                //                    $"}}"
                //                ]
                //            )
                //        )
                //    }
                //)


                button {
                    Dom.OnClick (fun _ -> env.Emit Inc)
                    Dom.OnContextMenu ((fun _ -> env.Emit Inc), preventDefault = true)
                    "+"
                }
            
                button {
                    Dom.OnClick (fun _ -> env.Emit Dec)
                    Dom.OnContextMenu ((fun _ -> env.Emit Dec), preventDefault = true)
                    "-"
                }
            
            
                ASet.channel 
                    model.PressedButtons 
                    (fun name -> [ $"console.log('add: ', {name});"])
                    (fun name -> [ $"console.log('rem: ', {name});"])
                    
                h3 { model.PressedButtons |> ASet.count |> AVal.map (sprintf "Count: %d") }
                ul {
                    model.PressedButtons |> ASet.sort |> AList.map (fun b ->
                        li { 
                            
                            AVal.channel model.Count (fun name ->
                                [
                                    $"console.log('count is: ', {name});"
                                ]
                            )
                            string b 
                        }
                    )
                }
            }
        }

    let app =
        {
            initial = { Count = 0; PressedButtons = HashSet.empty }
            update = update
            view = view
            unpersist = Unpersist.instance
        }

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FSharp.Core.CompilerServices
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop

[<AbstractClass; Sealed; Extension>]
type ReaderExtensions private() = 
    [<Extension>]
    static member GetSpliceOperations(x : IOpReader<IndexList<'a>, IndexListDelta<'a>>, token : AdaptiveToken) =
        let mutable old = x.State
        let ops = x.GetChanges(token)
        
        let mutable result = ListCollector()
        let mutable pendingIndex = -1
        let mutable pendingRemoves = 0
        let mutable pendingInserts = ListCollector()
        
        let flush() =
            if pendingIndex >= 0 then
                result.Add (pendingIndex, pendingRemoves, pendingInserts.Close())
                pendingIndex <- -1
                pendingRemoves <- 0
                pendingInserts <- ListCollector()
                
        let mutable index = 0
        for idx, op in ops do

            let l, s, r = IndexList.split idx old
            match op with
            | Remove ->
                if IndexList.isEmpty l then 
                    if pendingIndex < 0 then pendingIndex <- index
                else
                    flush()
                    index <- index + l.Count
                    pendingIndex <- index

                pendingRemoves <- pendingRemoves + 1
            | Set v ->
                let replaced = Option.isSome s
                if IndexList.isEmpty l then 
                    if pendingIndex < 0 then pendingIndex <- index
                else
                    flush()
                    index <- index + l.Count
                    pendingIndex <- index
                if replaced then pendingRemoves <- pendingRemoves + 1
                pendingInserts.Add v
                
            old <- r

        flush()

        result.Close()


module List =
    let applySplice (index : int) (count : int) (repl : list<'a>) (l : list<'a>) =
        let rec applySplice (i : int) (index : int) (count : int) (repl : list<'a>) (l : list<'a>) =
            if i = index then
                repl @ List.skip count l
            else
                match l with
                | h :: t ->
                    h :: applySplice (i+1) index count repl t
                | [] ->
                    failwith ""
        applySplice 0 index count repl l
        
    let applySplices (ops : list<int * int * list<'a>>) (l : list<'a>) =
        let mutable res = l
        for o,c,r in ops do
            res <- applySplice o c r res
        res



/// Three teapots side-by-side with pixel-snap radii 0 / 8 / 16 to
/// illustrate `Sg.PixelSnapRadius`. Hovering near (but not on) a teapot
/// only "snaps" the pick to it within its declared radius.
let snapDemo (_runtime : IRuntime) =
    let hovered = cval ""
    let active : cval<string> = cval ""

    let view (_env : Env<unit>) (_) =
        body {
            Style [
                Background "#202124"; Color "white"; FontFamily "monospace"
                Margin "0"; Padding "0"
            ]

            div {
                Style [Padding "10px"; FontSize "14px"]
                "Hover near each teapot — only inside the declared radius does the cursor snap to it. Active teapot brightens."
            }

            div {
                Style [Padding "10px"; FontSize "16px"; FontWeight "bold"]
                hovered |> AVal.map (fun s -> if s = "" then "(no snap)" else s)
            }

            renderControl {
                Style [Width "100%"; Height "600px"; Background "#36363A"; Outline "none"]
                Samples 4
                TabIndex 0

                SimpleFreeFlyController {
                    Location = V3d(0.0, -8.0, 2.0)
                    LookAt = V3d.Zero
                    Sky = V3d.OOI
                    Config = None
                    AnimationFinished = None
                }

                let! size = RenderControl.ViewportSize
                let proj =
                    size |> AVal.map (fun s ->
                        Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y)
                        |> Frustum.projTrafo
                    )
                Sg.Proj proj

                Dom.Style [Css.Cursor "crosshair"]
                Sg.Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }
                Sg.Scale 5.0

                let teapot (label : string) (baseColor : C4b) (snap : int) (x : float) =
                    let dyn =
                        active |> AVal.map (fun a ->
                            if a = label then
                                // bright tint when active
                                C4b(
                                    byte (min 255 (int baseColor.R + 100)),
                                    byte (min 255 (int baseColor.G + 100)),
                                    byte (min 255 (int baseColor.B + 100)),
                                    255uy)
                            else baseColor
                        )
                    sg {
                        Sg.PixelSnapRadius snap
                        Sg.Translate(x, 0.0, 0.0)
                        Sg.OnPointerEnter (fun _ ->
                            transact (fun () ->
                                active.Value <- label
                                hovered.Value <- sprintf "%s — snap radius %d" label snap)
                        )
                        Sg.OnPointerLeave (fun _ ->
                            transact (fun () ->
                                if active.Value = label then
                                    active.Value <- ""
                                    hovered.Value <- "")
                        )
                        Primitives.Teapot dyn
                    }

                teapot "Red"   C4b.Red   0  -0.4
                teapot "Green" C4b.Green 8   0.0
                teapot "Blue"  C4b.Blue  16  0.4
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

    // Cross-process consumer: reopen a dma-buf published by another process and
    // EGL-validate it. No Vulkan needed here (mirrors Electron's GPU process).
    if Array.contains "dmabuf-recv" argv then
        printfn "[dmabuf-recv] waiting for a dma-buf on /tmp/dmabuf.sock ..."
        let img = Aardvark.Dom.Remote.SharedTexture.FdHandoff.recvFd "/tmp/dmabuf.sock"
        let data = Aardvark.Dom.Remote.SharedTexture.EglDmaBufImport.readbackRGBA img
        let i = ((128 * 256) + 128) * 4
        let got = (int data.[i], int data.[i+1], int data.[i+2], int data.[i+3])
        let exp = (51, 102, 153, 255)
        let close (a,b,c,d) (e,f,g,h) = abs(a-e)<=3 && abs(b-f)<=3 && abs(c-g)<=3 && abs(d-h)<=3
        let ok = close got exp
        printfn "[dmabuf-recv] cross-process dma-buf center RGBA=%A expected~%A %s" got exp (if ok then "PASS" else "FAIL")
        exit (if ok then 0 else 1)

    // macOS milestone (analog of dmabuf-test): create an IOSurface-backed image via
    // VK_EXT_metal_objects (enabled on a Headless runtime) and export the IOSurface.
    if Array.contains "metal-test" argv then
        let metalExt = System.Func<Aardvark.Rendering.Vulkan.PhysicalDevice, seq<string>>(fun _ -> Seq.singleton "VK_EXT_metal_objects")
        let metalApp = new Aardvark.Rendering.Vulkan.HeadlessVulkanApplication(deviceExtensions = metalExt)
        match metalApp.Runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            printfn "[metal-test] VK_EXT_metal_objects enabled: %b" (dev.IsExtensionEnabled "VK_EXT_metal_objects")
            let img = Aardvark.Dom.Remote.SharedTexture.MetalExport.create dev 256 256
            printfn "[metal-test] IOSurface=0x%X  %dx%d  offset=%d stride=%d" (int64 img.IOSurface) img.Width img.Height img.Offset img.Stride
            Aardvark.Dom.Remote.SharedTexture.MetalExport.destroy dev img
            exit (if img.IOSurface <> 0n then 0 else 1)
        | r -> eprintfn "[metal-test] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // macOS milestone (analog of dmabuf-gpu-test): GPU-fill the IOSurface-backed image
    // and verify via map-readback.
    if Array.contains "metal-gpu-test" argv then
        let metalExt = System.Func<Aardvark.Rendering.Vulkan.PhysicalDevice, seq<string>>(fun _ -> Seq.singleton "VK_EXT_metal_objects")
        let metalApp = new Aardvark.Rendering.Vulkan.HeadlessVulkanApplication(deviceExtensions = metalExt)
        match metalApp.Runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let dst = Aardvark.Dom.Remote.SharedTexture.MetalExport.create dev 256 256
            let (src, srcMem) = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.createColorSource dev 256 256
            let evt = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.clearAndCopyMetal dev src dst (V4f(0.2f, 0.4f, 0.6f, 1.0f))
            let got = Aardvark.Dom.Remote.SharedTexture.MetalExport.readbackCenter dev dst
            let exp = (51, 102, 153, 255)
            let close (a,b,c,d) (e,f,g,h) = abs(a-e)<=3 && abs(b-f)<=3 && abs(c-g)<=3 && abs(d-h)<=3
            let ok = close got exp
            printfn "[metal-gpu-test] IOSurface=0x%X  MTLSharedEvent=0x%X  center RGBA=%A expected~%A %s"
                (int64 dst.IOSurface) (int64 evt) got exp (if ok then "PASS" else "FAIL")
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.destroyImage dev src srcMem
            Aardvark.Dom.Remote.SharedTexture.MetalExport.destroy dev dst
            exit (if ok then 0 else 1)
        | r -> eprintfn "[metal-gpu-test] runtime is not Vulkan: %A" (r.GetType()); exit 2
    let lib = Aardvark.LoadLibrary(typeof<Aardvark.Dom.Remote.Jpeg.JpegTransfer>.Assembly, "turbojpeg")
    use tj = new Aardvark.Dom.Remote.Jpeg.TJCompressor()

    // Texture-sharing work targets Vulkan (dma-buf / D3D11 / IOSurface export).
    // GL can do it too on Linux via an EGL context + EGL_MESA_image_dma_buf_export,
    // but we standardize on Vulkan for now.
    let app = new VulkanApplication()

    // dma-buf export needs VK_EXT_image_drm_format_modifier (not in Aardvark's default
    // device extension set) so the exported buffer's DRM layout matches what a Vulkan
    // importer (Chromium's ExternalVkImageBacking) reconstructs. Use a Headless runtime
    // with it enabled for the dma-buf hooks.
    let dmaBufRuntime =
        if [ "dmabuf-test"; "dmabuf-gpu-test"; "dmabuf-send"; "opaquefd-send"; "opaquefd-recv-vk"; "opaquefd-selftest"; "opaquefd-stream"; "opaque-rewrite-send"; "opaque-rewrite-recv" ] |> List.exists (fun h -> Array.contains h argv) then
            let ext = System.Func<Aardvark.Rendering.Vulkan.PhysicalDevice, seq<string>>(fun _ -> Seq.singleton "VK_EXT_image_drm_format_modifier")
            (new Aardvark.Rendering.Vulkan.HeadlessVulkanApplication(deviceExtensions = ext)).Runtime
        else app.Runtime

    // REPEATED-IN-PLACE cross-instance write test (standalone, no Chromium): does NVIDIA
    // surface REPEATED writes to ONE OPAQUE_FD buffer the consumer imported ONCE, when the
    // consumer re-acquires from EXTERNAL (+ optional fence) every iteration? Isolates the
    // question from Chromium viz caching. Producer rewrites one buffer with distinct colors.
    if Array.contains "opaque-rewrite-send" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let W, H = 256, 256
            let opaque = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.create dev W H
            let (src, srcMem) = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.createColorSource dev W H
            let conn = Aardvark.Dom.Remote.SharedTexture.FdHandoff.streamConnect "/tmp/opqrw.sock"
            Aardvark.Dom.Remote.SharedTexture.FdHandoff.streamHello conn (sprintf "%d %d %d" W H opaque.Size) [| opaque.MemFd |]
            printfn "[rw-send] HELLO memfd=%d %dx%d size=%d" opaque.MemFd W H opaque.Size
            for k in 0 .. 30 do
                let r = (k * 37) % 256
                let g = (k * 53) % 256
                let col = V4f(float32 r / 255.0f, float32 g / 255.0f, 0.5f, 1.0f)
                let dst : Aardvark.Dom.Remote.SharedTexture.DmaBufImage =
                    { Fd = -1; Width = W; Height = H; Fourcc = 0u; Modifier = 0UL; Offset = 0UL; Stride = 0UL
                      Image = opaque.Image; Memory = opaque.Memory; Size = opaque.Size }
                let fenceFd = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.clearAndCopy dev src dst col
                printfn "[rw-send] k=%d wrote expect~(%d,%d,128) fence=%d" k r g fenceFd
                Aardvark.Dom.Remote.SharedTexture.FdHandoff.streamFrameFd conn (sprintf "F %d\n" k) fenceFd |> ignore
                System.Threading.Thread.Sleep 120
            let (pr, pg, pb, pa) = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.readCenterLocal dev opaque
            printfn "[rw-send] FINAL producer same-instance center=(%d,%d,%d,%d) (= last color)" pr pg pb pa
            Aardvark.Dom.Remote.SharedTexture.FdHandoff.streamFrame conn "BYE\n" |> ignore
            exit 0
        | r -> eprintfn "[rw-send] runtime is not Vulkan: %A" (r.GetType()); exit 2

    if Array.contains "opaque-rewrite-recv" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let useSem = not (Array.contains "nosem" argv)
            let conn = Aardvark.Dom.Remote.SharedTexture.FdHandoff.streamServerAccept "/tmp/opqrw.sock"
            let (hello, memFd) = Aardvark.Dom.Remote.SharedTexture.FdHandoff.streamRecv conn
            let parts = hello.Trim().Split(' ')
            let W, H = int parts.[0], int parts.[1]
            printfn "[rw-recv] HELLO memfd=%d %dx%d useSem=%b" memFd W H useSem
            let imp = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.importOnce dev memFd W H
            let mutable go = true
            while go do
                let (msg, fenceFd) = Aardvark.Dom.Remote.SharedTexture.FdHandoff.streamRecv conn
                if msg = "" || msg.StartsWith "BYE" then go <- false
                else
                    let (r, g, b, a) = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.readCenterCross dev imp fenceFd useSem
                    printfn "[rw-recv] %-7s CONSUMER center=(%d,%d,%d,%d)" (msg.Trim()) r g b a
            printfn "[rw-recv] done"
            exit 0
        | r -> eprintfn "[rw-recv] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // Milestone 0 smoke test: prove the device can export a LINEAR color image as
    // a dma-buf fd (the real unknown on NVIDIA). Run with `Demo.dll dmabuf-test`.
    if Array.contains "dmabuf-test" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let img = Aardvark.Dom.Remote.SharedTexture.DmaBufExport.create dev 256 256
            Aardvark.Dom.Remote.SharedTexture.DmaBufExport.fillTestPattern dev img
            printfn "[dmabuf-test] exported fd=%d  %dx%d  fourcc=0x%08X  modifier=%d  offset=%d  stride=%d  size=%d"
                img.Fd img.Width img.Height img.Fourcc img.Modifier img.Offset img.Stride img.Size
            // Milestone 0b: re-import via EGL (the Chromium NativePixmap path) and verify pixels.
            let ok = Aardvark.Dom.Remote.SharedTexture.EglDmaBufImport.validate img
            printfn "[dmabuf-test] EGL re-import validation: %s" (if ok then "PASS" else "FAIL")
            Aardvark.Dom.Remote.SharedTexture.DmaBufExport.destroy dev img
            exit (if img.Fd >= 0 && ok then 0 else 1)
        | r -> eprintfn "[dmabuf-test] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // Milestone 0c: fill the shared dma-buf purely on the GPU (clear a source image
    // + vkCmdCopyImage into it, no CPU map) and verify the colour survives via EGL.
    if Array.contains "dmabuf-gpu-test" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let dst = Aardvark.Dom.Remote.SharedTexture.DmaBufExport.create dev 256 256
            let struct (src, srcMem) =
                let (a, b) = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.createColorSource dev 256 256
                struct (a, b)
            let color = V4f(0.2f, 0.4f, 0.6f, 1.0f) // (R,G,B,A)
            let fenceFd = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.clearAndCopy dev src dst color
            printfn "[dmabuf-gpu-test] exported sync_fd fence=%d (%s)" fenceFd
                (if fenceFd >= 0 then "real fence fd" elif fenceFd = -1 then "already-signaled sentinel (valid)" else "INVALID")
            let data = Aardvark.Dom.Remote.SharedTexture.EglDmaBufImport.readbackRGBA dst
            let i = ((128 * 256) + 128) * 4
            let got = (int data.[i], int data.[i+1], int data.[i+2], int data.[i+3])
            let exp = (51, 102, 153, 255)
            let close (a,b,c,d) (e,f,g,h) = abs(a-e)<=3 && abs(b-f)<=3 && abs(c-g)<=3 && abs(d-h)<=3
            let ok = close got exp
            printfn "[dmabuf-gpu-test] GPU-copied dma-buf center RGBA=%A expected~%A %s" got exp (if ok then "PASS" else "FAIL")
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.destroyImage dev src srcMem
            Aardvark.Dom.Remote.SharedTexture.DmaBufExport.destroy dev dst
            exit (if ok then 0 else 1)
        | r -> eprintfn "[dmabuf-gpu-test] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // Cross-process producer: GPU-fill a dma-buf, publish its descriptor, and hold
    // it alive so a separate `dmabuf-recv` process can reopen + validate it.
    if Array.contains "dmabuf-send" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let dst = Aardvark.Dom.Remote.SharedTexture.DmaBufExport.create dev 256 256
            let (src, srcMem) = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.createColorSource dev 256 256
            let fenceFd = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.clearAndCopy dev src dst (V4f(0.2f, 0.4f, 0.6f, 1.0f))
            printfn "[dmabuf-send] sending dma-buf fd=%d fence=%d (pid=%d) over /tmp/dmabuf.sock"
                dst.Fd fenceFd (System.Diagnostics.Process.GetCurrentProcess().Id)
            Aardvark.Dom.Remote.SharedTexture.FdHandoff.sendFd "/tmp/dmabuf.sock" dst fenceFd
            System.Threading.Thread.Sleep 500 // let the consumer recvmsg before we free
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.destroyImage dev src srcMem
            Aardvark.Dom.Remote.SharedTexture.DmaBufExport.destroy dev dst
            exit 0
        | r -> eprintfn "[dmabuf-send] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // OPAQUE_FD same-instance CONTROL: fill + read back in ONE instance. Proves the
    // fill/usage/readback path is correct before trusting a cross-instance zero.
    if Array.contains "opaquefd-selftest" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let opaque = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.create dev 256 256
            Aardvark.Dom.Remote.SharedTexture.OpaqueFd.fillGradient dev opaque
            let buf = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.readAllLocal dev opaque
            let ok = OpaqueGrad.check "opaquefd-selftest SAME-INSTANCE" buf 256 256
            Aardvark.Dom.Remote.SharedTexture.OpaqueFd.destroy dev opaque
            exit (if ok then 0 else 1)
        | r -> eprintfn "[opaquefd-selftest] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // OPAQUE_FD cross-instance test producer: GPU-fill an OPTIMAL image exported as
    // VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD and hand its memory fd + sync_fd to a
    // separate-instance consumer (the Vulkan-native path, vs dma-buf interop).
    if Array.contains "opaquefd-send" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let opaque = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.create dev 256 256
            // fill SOLID TEAL (51,102,153) via the proven GPU clear+copy (releases to
            // VK_QUEUE_FAMILY_EXTERNAL in GENERAL, matching Chromium's import acquire).
            let (src, srcMem) = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.createColorSource dev 256 256
            let dst : Aardvark.Dom.Remote.SharedTexture.DmaBufImage =
                { Fd = opaque.MemFd; Width = 256; Height = 256; Fourcc = 0u
                  Modifier = 0xFFFFFFFFFFFFFFFEUL   // OPAQUE_FD sentinel — consumer imports as OPAQUE_FD
                  Offset = 0UL; Stride = 0UL; Image = opaque.Image; Memory = opaque.Memory; Size = opaque.Size }
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.clearAndCopy dev src dst (V4f(0.2f, 0.4f, 0.6f, 1.0f)) |> ignore
            printfn "[opaquefd-send] sending SOLID TEAL (OPAQUE_FD) memfd=%d 256x256 size=%d -> /tmp/dmabuf.sock" opaque.MemFd opaque.Size
            Aardvark.Dom.Remote.SharedTexture.FdHandoff.sendFd "/tmp/dmabuf.sock" dst -1
            System.Threading.Thread.Sleep 2000
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.destroyImage dev src srcMem
            Aardvark.Dom.Remote.SharedTexture.OpaqueFd.destroy dev opaque
            exit 0
        | r -> eprintfn "[opaquefd-send] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // L1 STREAM: a ring of 3 OPAQUE_FD buffers, streamed continuously to the Chromium
    // painter. HELLO sends the 3 memfds once; each frame GPU-fills the next ring buffer
    // (round-robin, waitIdle inside clearAndCopy so it's tear-free without a fence yet) and
    // sends a tiny "F <i>" tick. The painter swaps the composited buffer per tick.
    if Array.contains "opaquefd-stream" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let mutable W, H = 256, 256
            let mutable gen = 0
            let mutable ring = Array.init 3 (fun _ -> Aardvark.Dom.Remote.SharedTexture.OpaqueFd.create dev W H)
            let mutable cube = CubeRender.create vk W H
            let conn = ST.streamConnect "/tmp/dmabuf.sock"

            // Persistent per-slot copy resources (allocated ONCE; no per-frame pool/semaphore
            // churn, no waitIdle). Each ring slot has a reusable command buffer, an exportable
            // signal semaphore (sync_fd → consumer's acquire), and a VkFence (CPU completion).
            let devH = dev.Handle
            let qfi = uint32 dev.GraphicsFamily.Index
            let mutable poolInfo =
                Aardvark.Rendering.Vulkan.VkCommandPoolCreateInfo(
                    0n, Aardvark.Rendering.Vulkan.VkCommandPoolCreateFlags.ResetCommandBufferBit, qfi)
            let mutable pool = Unchecked.defaultof<Aardvark.Rendering.Vulkan.VkCommandPool>
            Aardvark.Rendering.Vulkan.VkRaw.vkCreateCommandPool(devH, &&poolInfo, NativePtr.zero, &&pool) |> ignore
            let cmds =
                Array.init 3 (fun _ ->
                    let mutable ai = Aardvark.Rendering.Vulkan.VkCommandBufferAllocateInfo(0n, pool, Aardvark.Rendering.Vulkan.VkCommandBufferLevel.Primary, 1u)
                    let mutable c = Unchecked.defaultof<Aardvark.Rendering.Vulkan.VkCommandBuffer>
                    Aardvark.Rendering.Vulkan.VkRaw.vkAllocateCommandBuffers(devH, &&ai, &&c) |> ignore
                    c)
            let sems = Array.init 3 (fun _ -> Aardvark.Dom.Remote.SharedTexture.DmaBufSync.createExportableSemaphore dev)
            let fences =
                Array.init 3 (fun _ ->
                    let mutable fi = Aardvark.Rendering.Vulkan.VkFenceCreateInfo(Aardvark.Rendering.Vulkan.VkFenceCreateFlags.SignaledBit)
                    let mutable f = Unchecked.defaultof<Aardvark.Rendering.Vulkan.VkFence>
                    Aardvark.Rendering.Vulkan.VkRaw.vkCreateFence(devH, &&fi, NativePtr.zero, &&f) |> ignore
                    f)
            let waitFence (f : Aardvark.Rendering.Vulkan.VkFence) =
                let mutable ff = f
                Aardvark.Rendering.Vulkan.VkRaw.vkWaitForFences(devH, 1u, &&ff, 1u, System.UInt64.MaxValue) |> ignore
            let resetFence (f : Aardvark.Rendering.Vulkan.VkFence) =
                let mutable ff = f
                Aardvark.Rendering.Vulkan.VkRaw.vkResetFences(devH, 1u, &&ff) |> ignore

            let sendHello () =
                ST.streamHello conn (sprintf "STREAM %d %d 3 %d" W H gen) (ring |> Array.map (fun o -> o.MemFd))
                printfn "[opaquefd-stream] HELLO gen=%d (3 OPAQUE_FD ring buffers %dx%d)" gen W H
            sendHello ()
            // Reallocate the ring + offscreen FBO to a new size and re-announce (HELLO).
            let reallocate (nw : int) (nh : int) =
                Aardvark.Rendering.Vulkan.VkRaw.vkDeviceWaitIdle dev.Handle |> ignore
                ring |> Array.iter (Aardvark.Dom.Remote.SharedTexture.OpaqueFd.destroy dev)
                CubeRender.destroy cube
                W <- nw; H <- nh; gen <- gen + 1
                ring <- Array.init 3 (fun _ -> Aardvark.Dom.Remote.SharedTexture.OpaqueFd.create dev W H)
                cube <- CubeRender.create vk W H
                sendHello ()
            // Consumer is authoritative for size: parse the LAST "R <w> <h>" it sent.
            let pollResize () : (int * int) option =
                let s = ST.streamPoll conn
                if s = "" then None
                else
                    let mutable result = None
                    for line in s.Split('\n') do
                        let parts = line.Trim().Split(' ')
                        if parts.Length >= 3 && parts.[0] = "R" then
                            match System.Int32.TryParse parts.[1], System.Int32.TryParse parts.[2] with
                            | (true, w), (true, h) when w > 0 && h > 0 -> result <- Some (w, h)
                            | _ -> ()
                    result
            let mutable frame = 0
            let mutable running = true
            let fpsTimer = System.Diagnostics.Stopwatch.StartNew()
            let mutable fpsCount = 0
            while running do
                match pollResize () with
                | Some (nw, nh) when (nw, nh) <> (W, H) -> reallocate nw nh
                | _ -> ()
                let i = frame % 3
                let p = (frame + 2) % 3
                // Shared cube FBO: the PREVIOUS frame's copy READS it, so it must finish before
                // we re-render into it; also wait this slot's own last copy (3 frames ago) so its
                // command buffer / semaphore / ring image are free to reuse. NO device-wide waitIdle.
                if frame > 0 then waitFence fences.[p]
                waitFence fences.[i]
                resetFence fences.[i]
                let tex = CubeRender.render cube frame
                let cimg = unbox<Aardvark.Rendering.Vulkan.Image> tex
                Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.recordCopyInto dev cmds.[i] cimg.Handle cimg.Layout ring.[i].Image W H sems.[i] fences.[i]
                let fenceFd = Aardvark.Dom.Remote.SharedTexture.DmaBufSync.exportSyncFd dev sems.[i]
                // streamFrameFd blocks when the consumer's socket buffer fills → natural backpressure
                // pacing the producer to the consumer's consumption rate.
                if not (ST.streamFrameFd conn (sprintf "F %d %d\n" i gen) fenceFd) then
                    running <- false
                ST.closeFd fenceFd
                fpsCount <- fpsCount + 1
                if fpsTimer.Elapsed.TotalSeconds >= 1.0 then
                    printfn "[opaquefd-stream] %d fps (gen=%d %dx%d)" fpsCount gen W H
                    fpsCount <- 0; fpsTimer.Restart()
                frame <- frame + 1
            printfn "[opaquefd-stream] streamed %d frames; closing" frame
            Aardvark.Rendering.Vulkan.VkRaw.vkDeviceWaitIdle devH |> ignore
            ST.streamClose conn
            fences |> Array.iter (fun f -> Aardvark.Rendering.Vulkan.VkRaw.vkDestroyFence(devH, f, NativePtr.zero))
            sems |> Array.iter (Aardvark.Dom.Remote.SharedTexture.DmaBufSync.destroySemaphore dev)
            Aardvark.Rendering.Vulkan.VkRaw.vkDestroyCommandPool(devH, pool, NativePtr.zero)
            ring |> Array.iter (Aardvark.Dom.Remote.SharedTexture.OpaqueFd.destroy dev)
            exit 0
        | r -> eprintfn "[opaquefd-stream] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // OPAQUE_FD cross-instance test consumer (SEPARATE process + VkInstance): import the
    // memory + sync_fd, acquire from EXTERNAL, copy to host, read the centre pixel.
    // Variants: `no-sem` skips the semaphore wait; `undef-layout` acquires from UNDEFINED.
    if Array.contains "opaquefd-recv-vk" argv then
        match dmaBufRuntime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let (img, syncFd) = Aardvark.Dom.Remote.SharedTexture.FdHandoff.recvFd2 "/tmp/opaque.sock"
            let useSem = not (Array.contains "no-sem" argv)
            let useGeneral = not (Array.contains "undef-layout" argv)
            printfn "[opaquefd-recv-vk] received memfd=%d sync=%d %dx%d useSem=%b generalLayout=%b"
                img.Fd syncFd img.Width img.Height useSem useGeneral
            let buf = Aardvark.Dom.Remote.SharedTexture.OpaqueFd.importAndReadAll dev img.Fd syncFd img.Width img.Height useSem useGeneral
            let ok = OpaqueGrad.check "opaquefd-recv-vk CROSS-INSTANCE" buf img.Width img.Height
            exit (if ok then 0 else 1)
        | r -> eprintfn "[opaquefd-recv-vk] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // Windows milestone (analog of dmabuf-test): export a color image's memory as a
    // Win32 NT handle and confirm it's valid.
    if Array.contains "win32-test" argv then
        match app.Runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let img = Aardvark.Dom.Remote.SharedTexture.Win32Export.create dev 256 256
            printfn "[win32-test] exported NT handle=0x%X  %dx%d  size=%d" (int64 img.Handle) img.Width img.Height img.Size
            Aardvark.Dom.Remote.SharedTexture.Win32Export.destroy dev img
            exit (if img.Handle <> 0n then 0 else 1)
        | r -> eprintfn "[win32-test] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // Windows milestone (analog of dmabuf-gpu-test): GPU-fill the Win32 shared image
    // and verify via map-readback; also export a Win32 fence handle.
    if Array.contains "win32-gpu-test" argv then
        match app.Runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let dst = Aardvark.Dom.Remote.SharedTexture.Win32Export.create dev 256 256
            let (src, srcMem) = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.createColorSource dev 256 256
            let fence = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.clearAndCopyWin dev src dst (V4f(0.2f, 0.4f, 0.6f, 1.0f))
            let got = Aardvark.Dom.Remote.SharedTexture.Win32Export.readbackCenter dev dst
            let exp = (51, 102, 153, 255)
            let close (a,b,c,d) (e,f,g,h) = abs(a-e)<=3 && abs(b-f)<=3 && abs(c-g)<=3 && abs(d-h)<=3
            let ok = close got exp
            printfn "[win32-gpu-test] NT=0x%X fence=0x%X  center RGBA=%A expected~%A %s"
                (int64 dst.Handle) (int64 fence) got exp (if ok then "PASS" else "FAIL")
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.destroyImage dev src srcMem
            Aardvark.Dom.Remote.SharedTexture.Win32Export.destroy dev dst
            exit (if ok then 0 else 1)
        | r -> eprintfn "[win32-gpu-test] runtime is not Vulkan: %A" (r.GetType()); exit 2

    // List available device extensions (e.g. check MoltenVK VK_EXT_metal_objects on macOS).
    if Array.contains "vk-exts" argv then
        match app.Runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let exts = vk.Device.PhysicalDevice.AvailableExtensions |> Seq.map string |> Seq.toList
            printfn "[vk-exts] %d device extensions available" (List.length exts)
            for e in exts do
                let l = e.ToLowerInvariant()
                if l.Contains "metal" || l.Contains "external" then printfn "  %s" e
            printfn "[vk-exts] VK_EXT_metal_objects present: %b" (exts |> List.exists (fun e -> e = "VK_EXT_metal_objects"))
            exit 0
        | r -> eprintfn "[vk-exts] runtime is not Vulkan: %A" (r.GetType()); exit 2
    let noDisposable = { new System.IDisposable with member x.Dispose() = () }


    let run (ctx : DomContext) =
        App.start ctx (snapDemo ctx.Runtime)
        //App.start ctx (testApp ctx.Runtime)
        //App.start ctx Elm.app


    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseSockets()
                    .Configure(fun b -> b.UseWebSockets().UseGiraffe (DomNode.toRoute app.Runtime run))
                    .ConfigureServices(fun s -> s.AddGiraffe() |> ignore)
                    |> ignore
        )
        .Build()
        .Run()
       
        
    0


    

//[<Struct>]
//type HMap(store : HashMap<Symbol, obj>) =
//    static member Empty = HMap HashMap.empty

//    member x.Add(key : TypedSymbol<'a>, value : 'a) =
//        HMap(HashMap.add key.Symbol (box value) store)

//    member x.Remove(key : TypedSymbol<'a>) =
//        HMap(HashMap.remove key.Symbol store)

//    member x.TryRemove(key : TypedSymbol<'a>) =
//        match HashMap.tryRemove key.Symbol store with
//        | Some(value, rest) ->
//            Some(value :?> 'a, HMap rest)
//        | _ ->
//            None

//    member x.TryFind(key : TypedSymbol<'a>) =
//        match HashMap.tryFind key.Symbol store with
//        | Some r -> Some (r :?> 'a)
//        | None -> None

//module HMap =
//    let empty = HMap.Empty

//    let add (key : TypedSymbol<'a>) (value : 'a) (map : HMap) =
//        map.Add(key, value)
        
//    let remove (key : TypedSymbol<'a>) (map : HMap) =
//        map.Remove(key)

//    let tryRemove (key : TypedSymbol<'a>) (map : HMap) =
//        map.TryRemove(key)

//    let tryFind (key : TypedSymbol<'a>) (map : HMap) =
//        map.TryFind(key)
