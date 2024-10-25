namespace Aardvark.Dom

open System
open System.Threading
open Aardvark.Dom.Remote
open Aardvark.Rendering
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Giraffe

[<AbstractClass; Sealed>]
type Server private() =
    
    static member Start (rt : IRuntime, content : DomContext -> DomNode * IDisposable, ?ct : CancellationToken) =
        let ct = defaultArg ct CancellationToken.None
        let run (ctx : DomContext) = 
            content ctx


        let host = 
            Host.CreateDefaultBuilder()
                .ConfigureWebHostDefaults(
                    fun webHostBuilder ->
                        webHostBuilder
                            .UseSockets()
                            .Configure(fun b -> b.UseWebSockets().UseGiraffe (DomNode.toRoute rt run))
                            .ConfigureServices(fun s -> s.AddGiraffe() |> ignore)
                            |> ignore
                )
                .Build()

        host.Start()
        host.WaitForShutdownAsync(ct)

    static member Start(rt : IRuntime, content : DomContext -> DomNode, ?ct : CancellationToken) =
        let getContent (ctx : DomContext) = content ctx, { new IDisposable with member x.Dispose() = () }
        Server.Start(rt, getContent, ?ct = ct)

    static member Start(rt : IRuntime, content : DomNode, ?ct : CancellationToken) =
        let getContent (_ctx : DomContext) = content, { new IDisposable with member x.Dispose() = () }
        Server.Start(rt, getContent, ?ct = ct)

