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
    let lib = Aardvark.LoadLibrary(typeof<Aardvark.Dom.Remote.Jpeg.JpegTransfer>.Assembly, "turbojpeg")
    use tj = new Aardvark.Dom.Remote.Jpeg.TJCompressor()

    // Texture-sharing work targets Vulkan (dma-buf / D3D11 / IOSurface export).
    // GL can do it too on Linux via an EGL context + EGL_MESA_image_dma_buf_export,
    // but we standardize on Vulkan for now.
    let app = new VulkanApplication()

    // Milestone 0 smoke test: prove the device can export a LINEAR color image as
    // a dma-buf fd (the real unknown on NVIDIA). Run with `Demo.dll dmabuf-test`.
    if Array.contains "dmabuf-test" argv then
        match app.Runtime with
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
        match app.Runtime with
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
        match app.Runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            let dev = vk.Device
            let dst = Aardvark.Dom.Remote.SharedTexture.DmaBufExport.create dev 256 256
            let (src, srcMem) = Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.createColorSource dev 256 256
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.clearAndCopy dev src dst (V4f(0.2f, 0.4f, 0.6f, 1.0f)) |> ignore
            printfn "[dmabuf-send] sending dma-buf fd=%d (pid=%d) over /tmp/dmabuf.sock"
                dst.Fd (System.Diagnostics.Process.GetCurrentProcess().Id)
            Aardvark.Dom.Remote.SharedTexture.FdHandoff.sendFd "/tmp/dmabuf.sock" dst
            System.Threading.Thread.Sleep 500 // let the consumer recvmsg before we free
            Aardvark.Dom.Remote.SharedTexture.DmaBufGpu.destroyImage dev src srcMem
            Aardvark.Dom.Remote.SharedTexture.DmaBufExport.destroy dev dst
            exit 0
        | r -> eprintfn "[dmabuf-send] runtime is not Vulkan: %A" (r.GetType()); exit 2

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
