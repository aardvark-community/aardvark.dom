namespace Aardvark.Dom

open System.Threading.Tasks

type WorkerInstance<'cmd, 'msg> =
    {
        Receive         : unit -> Task<'cmd>
        Send            : seq<'msg> -> Task
        FinishSending   : unit -> unit
    }

[<AbstractClass>]
type AbstractWorker<'cmd, 'msg>() =
    abstract Run : instance : WorkerInstance<'cmd, 'msg> -> Task
        
