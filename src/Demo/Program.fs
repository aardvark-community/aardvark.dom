﻿open System.Threading
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

let testApp (_runtime : IRuntime) =
    let content = cval 0
    let text = cval ""
    let click (evt : MouseEvent) =
        transact (fun () -> 
            content.Value <- int (round evt.ClientX)
            text.Value <- JsonSerializer.Serialize(evt, JsonSerializerOptions(WriteIndented = true))
        )

    let down = cset[]
    let pos = cval V2d.Zero
    let frameTime = cval ""

    let view = 
        body {
            Style [ 
                Background "#36363A"
                Color "white"
                FontFamily "monospace"
            ]

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
                Dom.OnClick(click, true)
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
                OnMouseEnter(fun e -> printfn "enter 0")
                OnMouseLeave(fun e -> printfn "leave 0")
                Dom.OnClick((fun e -> printfn "capture 0"; true), true)
                Dom.OnClick((fun e -> printfn "bubble 0"; true), false)
                li { "Hans" }
                li {
                    frameTime
                    OnMouseEnter(fun e -> printfn "enter 1")
                    OnMouseLeave(fun e -> printfn "leave 1")
                    Dom.OnClick((fun e -> printfn "capture 1"; true), true)
                    Dom.OnClick((fun e -> printfn "bubble 1"; false), false)
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

                OnBoot "console.log('hi there');"
                OnShutdown "console.log('bye');"

                

                OnContextMenu(click, useCapture = true, preventDefault = true)
                Dom.OnClick(click, true)

                Dom.OnPointerDown (fun e ->
                    transact (fun () -> down.Add e.PointerId |> ignore)
                    printfn "down\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                )
                
                Dom.OnPointerUp (fun e ->
                    transact (fun () -> down.Remove e.PointerId |> ignore)
                    printfn "up\n%s" (System.Text.Json.JsonSerializer.Serialize(e, System.Text.Json.JsonSerializerOptions(WriteIndented = true)))
                )

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
                                        let tick(_) =
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

            renderControl  {
                // HTML attributes
                Style [Width "100%"; Height "600px"; Background "#202124"] 
                Samples 4
                Quality 50
                TabIndex 0
                
                Dom.OnKeyDown((fun e ->
                    printfn "Down %s" (JsonSerializer.Serialize(e, JsonSerializerOptions(WriteIndented = true)))
                ), preventDefault = true)
                
                Dom.OnKeyUp((fun e ->
                    printfn "Up %s" (JsonSerializer.Serialize(e, JsonSerializerOptions(WriteIndented = true)))
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
                let view = CameraView.lookAt (V3d(3,4,5)) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> AVal.constant
                let proj = size |> AVal.map (fun s -> Frustum.perspective 80.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                Sg.View view
                Sg.Proj proj

                // set the cursor to "crosshair" for the entire control and to "Hand" whenever a scene-element is hovered
                Dom.Style [Css.Cursor "crosshair"]
                Sg.Cursor "none"
                

                // setup transformation and shaders
                let rotActive = cval true
                Scale 3.0
                Trafo (rotationTrafo rotActive time)
                Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }  
  
                // scene      
                sg {
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
                            marker.Value <- Ray3d(e.Position, e.Normal)
                        )
                    )

                    // render a teapot and a sphere
                    sg {
                        Translate(0.5, 0.0, 0.0)
                        Primitives.Teapot(C4b.Green)
                    }

                    sg {
                        Primitives.WireSphere(V3d(-0.5, 0.0, 0.25), 0.25)
                    }
                    
                    sg {
                        Scale 0.5
                        Translate(0.0, 0.5, 0.0)
                        Primitives.Tetrahedron()
                    }
                }
                sg {
                    Sg.Scale 0.2
                    Sg.Text(AVal.constant "Hi There")
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
        
    view


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

            let (l, s, r) = IndexList.split idx old
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
        for (o,c,r) in ops do
            res <- applySplice o c r res
        res



[<EntryPoint>]
let main _ =
    Aardvark.Init()
    let app = new OpenGlApplication()
    let noDisposable = { new System.IDisposable with member x.Dispose() = () }


    let run (ctx : DomContext) = 
        testApp ctx.Runtime, noDisposable
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
