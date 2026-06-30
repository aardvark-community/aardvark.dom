namespace Aardvark.Dom

open Aardvark.Base
open Aardvark.Rendering
open FShade

/// Filled in by the server (`RenderMarshal.run <- Some (fun rt f -> rt.RunRender f)`).
/// The dom runs ALL GPU work on one render thread; the pick readback is the one piece
/// that was firing on a Kestrel worker thread, racing the render loop → the
/// `vkCmdCopyImageToBuffer` AccessViolation. `onRender` marshals that readback onto the
/// render thread (no-op/inline when the hook is unset or we're already on it).
module RenderMarshal =
    let mutable run : (IRuntime -> (unit -> obj) -> obj) option = None
    let onRender (runtime : IRuntime) (f : unit -> 'a) : 'a =
        match run with
        | Some g -> unbox<'a> (g runtime (fun () -> box (f ())))
        | None -> f ()

/// GPU pick "snap" — replaces the 33×33 region download + CPU spiral with a single
/// compute dispatch that scans the snap disc against the (already MS-resolved) pickId
/// texture and writes ONE small result the host reads back. Run on the render thread,
/// the off-thread `vkCmdCopyImageToBuffer` that raced the render loop disappears, and
/// the readback is a constant ~40 B at any radius instead of (2R+1)²·16 B.
///
/// Algorithm mirrors wombat.dom's `pickArgminCompute` (nearest valid pixel, screen-dist²,
/// disc-local-index tie-break), but as ONE invocation walking the disc serially — at the
/// radii we use (≤96) that is a few thousand texelFetches on a single GPU thread, dwarfed
/// by the frame, and it avoids FShade's signed-only storage atomics (so no key packing).
[<ReflectedDefinition>]
module PickArgmin =

    /// result buffer layout (float32 slots):
    ///   0 found (1/0)   1 px   2 py   3 dist²   4..7 winning pixel slots   8 centerSlot0   9 pad
    [<Literal>]
    let ResultFloats = 10

    let private pickSampler =
        sampler2d {
            texture uniform?PickTexture
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    /// `metaRadius.[absId]` = that scope's effective snap radius (<0 ⇒ inactive/noEvents);
    /// `metaSign.[absId]`   = registered mode sign (+1 mode-A / −1 mode-B). `result` is a
    /// `float32[]` of length `ResultFloats`.
    [<LocalSize(X = 1)>]
    let argmin (cx : int) (cy : int) (width : int) (height : int) (radius : int)
               (metaRadius : float32[]) (metaSign : float32[]) (result : float32[]) =
        compute {
            let center = pickSampler.Read(V2i(cx, cy), 0)

            let mutable bestD = 1.0e30
            let mutable bestLi = 0x7fffffff
            let mutable bpx = 0
            let mutable bpy = 0
            let mutable found = 0
            let mutable bs = V4d.Zero

            let side = 2 * radius + 1
            for dy in -radius .. radius do
                for dx in -radius .. radius do
                    let d2 = dx * dx + dy * dy
                    if d2 <= radius * radius then
                        let px = cx + dx
                        let py = cy + dy
                        if px >= 0 && py >= 0 && px < width && py < height then
                            let s = pickSampler.Read(V2i(px, py), 0)
                            let s0 = s.X
                            if s0 <> 0.0f then
                                let absId = int (abs s0)
                                let effR = float metaRadius.[absId]
                                let pixSign = if s0 < 0.0f then -1.0 else 1.0
                                let d2f = float d2
                                if effR >= 0.0 && d2f <= effR * effR && pixSign = float metaSign.[absId] then
                                    let li = (dy + radius) * side + (dx + radius)
                                    if d2f < bestD || (d2f = bestD && li < bestLi) then
                                        bestD <- d2f
                                        bestLi <- li
                                        bpx <- px
                                        bpy <- py
                                        bs <- V4d s
                                        found <- 1

            result.[0] <- float32 found
            result.[1] <- float32 bpx
            result.[2] <- float32 bpy
            result.[3] <- (if found = 1 then float32 bestD else 0.0f)
            result.[4] <- float32 bs.X
            result.[5] <- float32 bs.Y
            result.[6] <- float32 bs.Z
            result.[7] <- float32 bs.W
            result.[8] <- center.X
            result.[9] <- 0.0f
        }
