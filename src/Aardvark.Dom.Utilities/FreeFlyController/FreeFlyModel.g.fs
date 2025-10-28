//7e59030d-0ed1-bc59-d934-9a224fa2005e
//7ced8a8f-ab67-a2c4-1bc9-35735ff70d51
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec Aardvark.Dom.Utilities.FreeFlyController

open System
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Dom.Utilities.FreeFlyController
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveFreeFlyState(value : FreeFlyState) =
    let mutable _MoveVec_ = FSharp.Data.Adaptive.cval(value.MoveVec)
    let mutable _IsAnimating_ = FSharp.Data.Adaptive.cval(value.IsAnimating)
    let mutable _TurnVec_ = FSharp.Data.Adaptive.cval(value.TurnVec)
    let _LastRender_ = FSharp.Data.Adaptive.cval(value.LastRender)
    let _Position_ = FSharp.Data.Adaptive.cval(value.Position)
    let _Sky_ = FSharp.Data.Adaptive.cval(value.Sky)
    let _Forward_ = FSharp.Data.Adaptive.cval(value.Forward)
    let _Config_ = FSharp.Data.Adaptive.cval(value.Config)
    let _SprintFactor_ = FSharp.Data.Adaptive.cval(value.SprintFactor)
    let _Momentum_ = FSharp.Data.Adaptive.cval(value.Momentum)
    let _TargetMoveLocal_ = FSharp.Data.Adaptive.cval(value.TargetMoveLocal)
    let _TargetMoveGlobal_ = FSharp.Data.Adaptive.cval(value.TargetMoveGlobal)
    let _TargetTurn_ = FSharp.Data.Adaptive.cval(value.TargetTurn)
    let _Camera_ = FSharp.Data.Adaptive.cval(value.Camera)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : FreeFlyState) = AdaptiveFreeFlyState(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : FreeFlyState) -> AdaptiveFreeFlyState(value)) (fun (adaptive : AdaptiveFreeFlyState) (value : FreeFlyState) -> adaptive.Update(value))
    member __.Update(value : FreeFlyState) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<FreeFlyState>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _MoveVec_.Value <- value.MoveVec
            _IsAnimating_.Value <- value.IsAnimating
            _TurnVec_.Value <- value.TurnVec
            _LastRender_.Value <- value.LastRender
            _Position_.Value <- value.Position
            _Sky_.Value <- value.Sky
            _Forward_.Value <- value.Forward
            _Config_.Value <- value.Config
            _SprintFactor_.Value <- value.SprintFactor
            _Momentum_.Value <- value.Momentum
            _TargetMoveLocal_.Value <- value.TargetMoveLocal
            _TargetMoveGlobal_.Value <- value.TargetMoveGlobal
            _TargetTurn_.Value <- value.TargetTurn
            _Camera_.Value <- value.Camera
            ()
    member __.Current = __adaptive
    member __.MoveVec = _MoveVec_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.IsAnimating = _IsAnimating_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.TurnVec = _TurnVec_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V2d>
    member __.LastRender = _LastRender_ :> FSharp.Data.Adaptive.aval<System.TimeSpan>
    member __.Position = _Position_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.Sky = _Sky_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.Forward = _Forward_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.Config = _Config_ :> FSharp.Data.Adaptive.aval<FreeFlyConfig>
    member __.MoveVectors = __value.MoveVectors
    member __.TurnVectors = __value.TurnVectors
    member __.SprintFactor = _SprintFactor_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.Momentum = _Momentum_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.TargetMoveLocal = _TargetMoveLocal_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.TargetMoveGlobal = _TargetMoveGlobal_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.TargetTurn = _TargetTurn_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V2d>
    member __.Camera = _Camera_ :> FSharp.Data.Adaptive.aval<Aardvark.Rendering.CameraView>
    member __.Handler = __value.Handler

