namespace Aardvark.Dom

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open FSharp.Data.Adaptive
open System.Runtime.InteropServices

[<AutoOpen>]
module private TaskHelpers =

    type AsyncSignal(isSet : bool) =
        static let finished = Task.FromResult()
    
        let mutable isSet = isSet
        let mutable tcs : option<TaskCompletionSource<unit>> = None

        member x.Pulse() =
            lock x (fun () -> 
                match tcs with
                | Some t -> 
                    t.SetResult()
                    tcs <- None
                    isSet <- false
                | None -> 
                    isSet <- true
            )
            
        member x.Poll() =
            lock x (fun () ->
                if isSet then
                    isSet <- false
                    true
                else
                    false
            )
            
        member x.Wait(ct : CancellationToken) =
            lock x (fun () ->
                if isSet then
                    isSet <- false
                    finished
                else
                    match tcs with
                    | Some tcs -> 
                        tcs.Task
                    | None -> 
                        let t = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
                        tcs <- Some t

                        if ct.CanBeCanceled then
                            t.Task.WaitAsync(ct)
                        else
                            t.Task
            )
                
        member x.Wait() = x.Wait(CancellationToken.None)
   
    let randomString() =
        let str = System.Convert.ToBase64String(Guid.NewGuid().ToByteArray())
        "_" + str.TrimEnd('=').Replace("/", "_").Replace("+", "$")
 

module AVal =
    let channel (value : aval<'a>) (js : string -> list<string>) =

        let server (c : IChannel) =
            let s = AsyncSignal(true)
            let sub = value.AddMarkingCallback s.Pulse
            let cancel = new CancellationTokenSource()
            

            let csub = 
                c.OnClose.Subscribe (fun () ->
                    cancel.Cancel()
                )
            
            task {
                try
                    try
                        while not cancel.IsCancellationRequested do
                            if not (s.Poll()) then
                                do! s.Wait(cancel.Token)
                            let v = AVal.force value
                            let str = System.Text.Json.JsonSerializer.Serialize v
                            do! c.Send(ChannelMessage.Text str)
                    finally
                        sub.Dispose()
                        csub.Dispose()
                        cancel.Dispose()
                        printfn "channel shutdown"
                with _ ->
                    ()
            }

        let name = randomString()
        let code = js name
        let client (cname : string) =
            String.concat "\n" [
                yield $"{cname}.onmessage = (e) => {{"
                yield $"  let {name} = JSON.parse(e.data);"
                yield $"  {{"
                yield! code
                yield $"  }}"
                yield $"}}"
            ]

        Dom.OnBoot(server, client)
        
module ASet =
    let channel (value : aset<'a>) (add : string -> list<string>) (remove : string -> list<string>) =

        let server (c : IChannel) =
            let s = AsyncSignal(true)
            let reader = value.GetReader()
            let sub = reader.AddMarkingCallback s.Pulse
            let cancel = new CancellationTokenSource()
            

            let csub = 
                c.OnClose.Subscribe (fun () ->
                    cancel.Cancel()
                )
            
            task {
                try
                    try
                        while not cancel.IsCancellationRequested do
                            if not (s.Poll()) then
                                do! s.Wait(cancel.Token)

                            let ops = reader.GetChanges()
                            for op in ops do
                                match op with
                                | Add(_, v) ->
                                    let str = System.Text.Json.JsonSerializer.Serialize v
                                    do! c.Send(ChannelMessage.Text ("+" + str))
                                | Rem(_, v) ->
                                    let str = System.Text.Json.JsonSerializer.Serialize v
                                    do! c.Send(ChannelMessage.Text ("-" + str))
                    finally
                        sub.Dispose()
                        csub.Dispose()
                        cancel.Dispose()
                        printfn "channel shutdown"
                with _ ->
                    ()
            }

        let name = randomString()
        let add = add name
        let rem = remove name
        let client (cname : string) =
            String.concat "\n" [
                yield $"{cname}.onmessage = (e) => {{"
                yield $"  let {name} = JSON.parse(e.data.substr(1));"
                yield $"  if(e.data.startsWith('+')) {{"
                yield $"    {{"
                yield! add
                yield $"    }}"
                yield $"  }}"
                yield $"  else {{"
                yield $"    {{"
                yield! rem
                yield $"    }}"
                yield $"  }}"
                yield $"}}"
            ]

        Dom.OnBoot(server, client)
      
      


    
