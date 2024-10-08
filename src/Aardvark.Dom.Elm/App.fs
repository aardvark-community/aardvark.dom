﻿namespace Aardvark.Dom

open System
open System.Threading
open System.Threading.Tasks
open Aardvark.Base
open Aardvark.Rendering
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive
open Adaptify
open System.Collections.Generic

type Env<'msg> =
    abstract Runtime : IRuntime
    abstract Emit : messages : seq<'msg> -> unit
    abstract Run : js : string * option<System.Text.Json.JsonElement -> unit> -> unit
    
    abstract RunModal : modal : (IDisposable -> DomNode) -> IDisposable
    
    abstract StartWorker<'t, 'a, 'b when 't :> AbstractWorker<'a, 'b> and 't : (new : unit -> 't)> : unit -> Task<WorkerInstance<'b, 'a>>
    
    //abstract Start : IAsyncEnumerable<'msg> -> IDisposable

module Env =
    let map (mapping : 'a -> 'b) (env : Env<'b>) : Env<'a> =
        { new Env<'a> with
            member x.Runtime = env.Runtime
            member x.Run(js, cb) = env.Run(js, cb)
            member x.Emit msg = env.Emit (Seq.map mapping msg)
            member x.RunModal(modal) = env.RunModal(modal)
            member x.StartWorker<'t, 'a, 'b when 't :> AbstractWorker<'a, 'b> and 't : (new : unit -> 't)>() = env.StartWorker<'t, 'a, 'b>()
        }

type App<'model, 'amodel, 'msg> =   
    {
        initial     : 'model
        update      : Env<'msg> -> 'model -> 'msg -> 'model
        view        : Env<'msg> -> 'amodel -> DomNode
        unpersist   : Unpersist<'model, 'amodel>
    }

[<AbstractClass; Sealed; Extension>]
type EnvExtensions private() =
    [<Extension>]
    static member Emit(this : Env<'msg>, messages : #seq<'msg>) =
        this.Emit(messages :> seq<_>)
        
    [<Extension>]
    static member Emit(this : Env<'msg>, message : 'msg) =
        this.Emit (Seq.singleton message)
         
    [<Extension>]
    static member Start(this : Env<'msg>, js : string) =
        this.Run (js, None)

    [<Extension>]
    static member Start(this : Env<'msg>, js : list<string>) =
        this.Run (String.concat "\n" js, None)

       
    [<Extension>]
    static member StartTask(this : Env<'msg>, js : string) =
        let tcs = System.Threading.Tasks.TaskCompletionSource<_>()
        this.Run (js, Some tcs.SetResult)
        tcs.Task

    [<Extension>]
    static member StartTask(this : Env<'msg>, js : list<string>) =
        let js = String.concat "\n" js
        this.StartTask(js)


module App =    
    let startAndGetEnv (ctx : DomContext) (app : App<'model, 'amodel, 'msg>) =
        let mutable model = app.initial
        let amodel = app.unpersist.init model

        let queue = new System.Collections.Concurrent.BlockingCollection<seq<'msg>>()

        let customChildren = clist<DomNode>()
        
        let env =
            { new Env<'msg> with
                member x.Runtime = ctx.Runtime
                member x.Emit (messages : seq<'msg>) =
                    if not queue.IsAddingCompleted then
                        queue.Add messages
                member x.Run(js, cb) =
                    if not queue.IsAddingCompleted then
                        ctx.Execute js cb
                member x.RunModal(modal) =
                    let v = customChildren.Value
                    let idx = v.NewIndexAfter v.MaxIndex
                    
                    let destroy =
                        { new IDisposable with
                            member x.Dispose() =
                                transact (fun () -> customChildren.Remove idx |> ignore)
                        }
                    
                    let ui = modal destroy
                    transact(fun () -> customChildren.Perform(IndexListDelta.single idx (Set ui)))
                    destroy
                
                member x.StartWorker<'t, 'a, 'b when 't :> AbstractWorker<'a, 'b> and 't : (new : unit -> 't)>() =
                    ctx.StartWorker<'t, 'a, 'b>()
            }

        let updateThread = 
            startThread <| fun () ->
                for msgs in queue.GetConsumingEnumerable() do
                    for m in msgs do 
                        model <- app.update env model m
                    transact (fun () ->
                        app.unpersist.update amodel model
                    )

        let shutdown =
            { new IDisposable with
                member x.Dispose() =
                    queue.CompleteAdding()
                    updateThread.Join()
            }

        let view = app.view env amodel
        
        let view =
            match view with
            | DomNode.Element(tag, att, cs) ->
                DomNode.Element(tag, att, AList.append cs customChildren)
            | _ -> view
        
        view, shutdown, env
 
    let start (ctx : DomContext) (app : App<'model, 'amodel, 'msg>) =
        let view, shutdown, _ = startAndGetEnv ctx app
        view, shutdown