namespace Aardvark.Dom.Utilities

open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.Base
open Aardvark.Dom
open Aardvark.Dom.Utilities
open Aardvark.Dom.Utilities.OrbitController

type SimpleOrbitControllerConfig =
    {
        Location : V3d
        Center : V3d
        RotateButton : Button
        PanButton : Button
    }

type SimpleOrbitController = SimpleOrbitController of SimpleOrbitControllerConfig

[<AutoOpen>]
module RenderControlOrbitExtensions =
    
    let private orbit (SimpleOrbitController config) : NodeBuilderHelpers.RenderControlBuilder<aval<Trafo3d>>  =
    
        let mutable state =
            let dist = config.Location - config.Center
            let r = Vec.length dist
            let dir = dist / r
            
            let theta = asin dir.Z
            let phi = atan2 dir.Y dir.X
            
            OrbitState.create config.Center phi theta r config.RotateButton config.PanButton
        let astate = AdaptiveOrbitState state

        let coll = AsyncBlockingCollection<_>()

        let env = 
            { new Env<OrbitMessage> with
                member this.Emit(messages: OrbitMessage seq): unit = 
                    coll.Add messages
                member this.Run(js: string, arg1: (System.Text.Json.JsonElement -> unit) option): unit = 
                    raise (System.NotImplementedException())
                member this.RunModal(modal: System.IDisposable -> DomNode): System.IDisposable = 
                    raise (System.NotImplementedException())
                member this.Runtime: IRuntime = 
                    failwith ""
                member this.StartWorker(): System.Threading.Tasks.Task<WorkerInstance<'b,'a>> = 
                    raise (System.NotImplementedException())
            }
        let runner =
            task {
                while true do
                    let! msgs = coll.Take()
                    for msg in msgs do
                        state <- OrbitController.update state msg
                        transact (fun () -> astate.Update state)

                    
            }
            
        let view = astate.view |> AVal.map CameraView.viewTrafo
        let run = 
            renderControlExt {
                RenderControl.OnRendered (fun _ -> env.Emit [OrbitMessage.Rendered])
                OrbitController.getAttributes env
                Sg.View view
                Sg.OnDoubleTap(fun e -> env.Emit [OrbitMessage.SetTargetCenter(true, AnimationKind.Tanh, e.WorldPosition)]; false)
            }
        fun a -> run a; view
    
    type RenderControlBuilder with
    
        
        member x.Yield(config : SimpleOrbitController) =
            x.Yield (orbit config >> ignore)
          
        member x.Bind(config : SimpleOrbitController, cont : aval<Trafo3d> -> _) =
            x.Bind(orbit config, cont)
            
    
    
    
    
    
    
    

