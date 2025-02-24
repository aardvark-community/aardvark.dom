//d6b01853-8ed8-fec6-6e70-88b4653de635
//a214c7ec-ccbd-f7d2-1b53-04d099431617
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec Demo

open System
open FSharp.Data.Adaptive
open Adaptify
open Demo
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveModel(value : Model) =
    let _Count_ = FSharp.Data.Adaptive.cval(value.Count)
    let _PressedButtons_ = FSharp.Data.Adaptive.cset(value.PressedButtons)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Model) = AdaptiveModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Model) -> AdaptiveModel(value)) (fun (adaptive : AdaptiveModel) (value : Model) -> adaptive.Update(value))
    member __.Update(value : Model) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Model>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _Count_.Value <- value.Count
            _PressedButtons_.Value <- value.PressedButtons
    member __.Current = __adaptive
    member __.Count = _Count_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.PressedButtons = _PressedButtons_ :> FSharp.Data.Adaptive.aset<Aardvark.Dom.Button>

