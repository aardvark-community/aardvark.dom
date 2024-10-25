namespace Aardvark.Dom.Utilities

open System
open System.Threading
open Aardvark.Dom

[<AutoOpen>]
module Dom =
    let renderControlExt = RenderControlBuilderExt()
    
    

type internal AsyncBlockingCollection<'a>() =
    let store = System.Collections.Generic.Queue<'a>()

    let mutable next : option<Tasks.TaskCompletionSource<unit>> = None //Tasks.TaskCompletionSource<unit>()

    member x.Add(value : 'a) =
        lock store (fun () ->
            store.Enqueue value
            match next with
            | Some n -> 
                n.SetResult()
                next <- None
            | None -> 
                ()
        )

    member x.Take() =
        Async.FromContinuations (fun (success, error, cancel) ->
            Monitor.Enter store
            try
                if store.Count > 0 then
                    let value = store.Dequeue()
                    Monitor.Exit store
                    success value
                else
                    let tcs = 
                        match next with
                        | Some n -> n
                        | None ->
                            let n = Tasks.TaskCompletionSource<unit>()
                            next <- Some n
                            n
                    Monitor.Exit store
                    tcs.Task.ContinueWith (fun (_t : Tasks.Task<unit>) ->
                        Async.StartWithContinuations(x.Take(), success, error, cancel)
                    ) |> ignore
            with 
                | :? OperationCanceledException as e ->
                    Monitor.Exit store
                    cancel e
                | e ->
                    Monitor.Exit store
                    error e
        )

    member x.Clear() =
        lock store (fun () ->
            store.Clear()
        )
