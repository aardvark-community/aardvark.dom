namespace Aardvark.Dom.Utilities

open System
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.Base
open Aardvark.Dom
open Aardvark.Dom.Utilities
open Aardvark.Dom.Utilities.FreeFlyController

type SimpleFreeFlyConfig =
    {
        Location : V3d
        LookAt : V3d
        Sky : V3d
        Config : option<FreeFlyConfig>
    }

type SimpleFreeFlyController = SimpleFreeFlyController of SimpleFreeFlyConfig

[<AutoOpen>]
module RenderControlFreeFlyExtensions =
    
    let private freeFly (SimpleFreeFlyController config) : NodeBuilderHelpers.RenderControlBuilder<aval<Trafo3d>>  =
    
        let mutable state =
            {
                Config = defaultArg config.Config FreeFlyConfig.Default
                LastRender = TimeSpan.Zero
                Position = config.Location
                Sky = config.Sky
                Forward = Vec.normalize (config.LookAt - config.Location)
                
                SprintFactor = 1.0
                MoveVectors = HashMap.empty
                TurnVectors = HashMap.empty
                Momentum = V3d.Zero
                TargetTurn = V2d.Zero
                TargetMoveLocal = V3d.Zero
                TargetMoveGlobal = V3d.Zero
                Camera = CameraView.lookAt config.Location config.LookAt config.Sky
                Handler = None
            }
        let astate = AdaptiveFreeFlyState state

        let coll = AsyncBlockingCollection<_>()

        let env = 
            { new Env<FreeFlyMessage> with
                member this.Emit(messages: FreeFlyMessage seq): unit = 
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
                        state <- FreeFlyController.update state msg
                        transact (fun () -> astate.Update state)

                    
            }
            
        let view = astate.Camera |> AVal.map CameraView.viewTrafo
        let run = 
            renderControlExt {
                FreeFlyController.getAttributes astate env
                
                Dom.OnKeyDown(fun e ->
                    if e.Key = " " then
                        env.Emit [ FreeFlyMessage.FlyTo(config.Location, Vec.normalize (config.LookAt - config.Location)) ]
                        
                )
                
                Dom.OnGamepadButtonDown(fun e ->
                    if e.ButtonName = "Select" || e.ButtonName = "Start" then
                        env.Emit [ FreeFlyMessage.FlyTo(config.Location, Vec.normalize (config.LookAt - config.Location)) ]
                        
                        
                )
                
                Sg.View view
            }
        fun a -> run a; view
    
    type RenderControlBuilder with
    
        
        member x.Yield(config : SimpleFreeFlyController) =
            x.Yield (freeFly config >> ignore)
          
        member x.Bind(config : SimpleFreeFlyController, cont : aval<Trafo3d> -> _) =
            x.Bind(freeFly config, cont)
            
    
    
    
    
    
    
    

