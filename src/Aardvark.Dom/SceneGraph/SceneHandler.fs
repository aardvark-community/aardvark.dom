namespace Aardvark.Dom

open System.Runtime.InteropServices
open FShade.Imperative
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.Dom
open Microsoft.FSharp.NativeInterop

#nowarn "9"

[<ReflectedDefinition>]
module internal Normal32 =
    let private sgn (v : V2f) = V2f((if v.X >= 0.0f then 1.0f else -1.0f), (if v.Y >= 0.0f then 1.0f else -1.0f))
    let private clamp (v : V2f) =
        V2f(
            (if v.X < -1.0f then -1.0f elif v.X > 1.0f then 1.0f else v.X),
            (if v.Y < -1.0f then -1.0f elif v.Y > 1.0f then 1.0f else v.Y)
        )

    let decode (v : int) : V3f =
        if v = 0 then
            V3f.Zero
        else
            let e = V2f(float32 (uint32 v >>> 16) / 65535.0f, float32 (v &&& 0xFFFF) / 65535.0f) * 2.0f - V2f.II
            let v = V3f(e, 1.0f - abs e.X - abs e.Y)
            if v.Z < 0.0f then V3f(V2f(1.0f - abs v.Y, 1.0f - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
            else v |> Vec.normalize

    let encode (v : V3f) : int =
        if v.X = 0.0f && v.Y = 0.0f && v.Z = 0.0f then
            0
        else
            let p = v.XY * (1.0f / (abs v.X + abs v.Y + abs v.Z |> float32))
            let p =
                if v.Z <= 0.0f then clamp (V2f(1.0f - abs p.Y, 1.0f - abs p.X) * sgn p)
                else clamp p

            let x0 = floor ((p.X * 0.5f + 0.5f) * 65535.0f) |> int
            let y0 = floor ((p.Y * 0.5f + 0.5f) * 65535.0f) |> int

            let mutable bestDot = 0.0f
            let mutable best = 0

            for dx in 0 .. 1 do
                for dy in 0 .. 1 do
                    let e = (((x0 + dx) <<< 16) ||| (y0 + dy))
                    let vv = decode e
                    let d = Vec.dot vv v
                    if d > bestDot then
                        bestDot <- d
                        best <- e

            best
        
/// Octahedral normal codec packed into 24 bits (12+12). Stored as a plain
/// `float32` (any int up to 2^24 round-trips exactly through float32, so the
/// value survives MS resolve-average when all 4 samples agree, with no
/// IEEE-special-value risk).
[<ReflectedDefinition>]
module internal Normal24 =
    let private sgn (v : V2f) = V2f((if v.X >= 0.0f then 1.0f else -1.0f), (if v.Y >= 0.0f then 1.0f else -1.0f))
    let private clamp (v : V2f) =
        V2f(
            (if v.X < -1.0f then -1.0f elif v.X > 1.0f then 1.0f else v.X),
            (if v.Y < -1.0f then -1.0f elif v.Y > 1.0f then 1.0f else v.Y)
        )

    let decode (v : int) : V3f =
        if v = 0 then
            V3f.Zero
        else
            let e = V2f(float32 (uint32 v >>> 12) / 4095.0f, float32 (v &&& 0xFFF) / 4095.0f) * 2.0f - V2f.II
            let v = V3f(e, 1.0f - abs e.X - abs e.Y)
            if v.Z < 0.0f then V3f(V2f(1.0f - abs v.Y, 1.0f - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
            else v |> Vec.normalize

    let encode (v : V3f) : int =
        if v.X = 0.0f && v.Y = 0.0f && v.Z = 0.0f then
            0
        else
            let p = v.XY * (1.0f / (abs v.X + abs v.Y + abs v.Z |> float32))
            let p =
                if v.Z <= 0.0f then clamp (V2f(1.0f - abs p.Y, 1.0f - abs p.X) * sgn p)
                else clamp p

            let x0 = floor ((p.X * 0.5f + 0.5f) * 4095.0f) |> int
            let y0 = floor ((p.Y * 0.5f + 0.5f) * 4095.0f) |> int

            let mutable bestDot = 0.0f
            let mutable best = 0

            for dx in 0 .. 1 do
                for dy in 0 .. 1 do
                    let e = (((x0 + dx) <<< 12) ||| (y0 + dy))
                    let vv = decode e
                    let d = Vec.dot vv v
                    if d > bestDot then
                        bestDot <- d
                        best <- e

            best


module PickShader =
    open FShade

    type UniformScope with
        member x.PickId : int = uniform?PickId
        member x.Level : int = uniform?Level
        member x.Selected : int = uniform?Selected

        member x.Levels : int = uniform?Levels
        member x.Distance : float32 = uniform?Distance
        
        member x.LevelSizes : Arr<16 N, V2i> = uniform?LevelSizes
        member x.LevelPixelSizes : Arr<16 N, V2i> = uniform?LevelPixelSizes

    // -----------------------------------------------------------------------
    // Pick chain — design notes
    //
    // Each effect declares an input record containing ONLY the fields it
    // actually reads, and an output record containing ONLY what it writes.
    // FShade composition then routes each requested fragment input to the
    // nearest upstream producer. A semantic only becomes a required vertex
    // attribute if some fragment in the chain reads it AND no upstream effect
    // produces it. With this design, neither `PickPartIndex` nor
    // `PickViewPosition` can ever leak in as a vertex attribute from OUR
    // chain — they can only appear if the user's own effect already required
    // them.
    //
    // Modes:
    //   A: id, encoded-normal, depth, PartIndex packed into 4 float slots.
    //      PartIndex = user-supplied via "PickPartIndex" semantic, or 0
    //      (we do NOT default it to gl_InstanceID — past attempts to do so
    //      broke composition with custom user vertex shaders).
    //   B: id, encoded-normal, encoded-direction, length — all four slots
    //      consumed. No room for PartIndex; pi is irrelevant in this mode.
    //   No-normal variants: same as A but write 0 instead of an encoded
    //      normal — used when the geometry has no Normal attribute.
    // -----------------------------------------------------------------------

    type Fragment =
        {
            [<Color>] c : V4f
            [<Semantic("PickId")>] id : V4f
        }

    [<GLSLIntrinsic("gl_FragCoord")>]
    let fragCoord() : V4f = onlyInShaderCode "fragcoord"

    // ---- Vertex producer for ViewSpaceNormal.
    // Only injected when a downstream pick fragment needs vn. Reads Normal,
    // writes vn — nothing more, so pi/pvp never get pulled through.
    type VnIn =
        {
            [<Position>] pos : V4f
            [<Normal>] n : V3f
        }
    type VnOut =
        {
            [<Position>] pos : V4f
            [<Semantic("ViewSpaceNormal")>] vn : V3f
        }
    let viewSpaceNormalVertex (v : VnIn) =
        vertex {
            let vn = uniform.ModelViewTrafoInv.Transposed * V4f(v.n, 0.0f) |> Vec.xyz |> Vec.normalize
            let r : VnOut = { pos = v.pos; vn = vn }
            return r
        }

    // ---- Depth seeding fragment: writes gl_FragCoord.z into the Depth
    // semantic before the user's fragment runs, so downstream pickFinal*
    // sees the natural depth even if the user fragment never touches Depth.
    // IMPORTANT: input record must NOT declare [<Depth>] — FShade would
    // then route Depth backwards as a required vertex attribute, breaking
    // hasAllInputs for any geometry that has no "Depth" attribute (i.e.,
    // every real RenderObject). Use FragCoord which is a GPU built-in
    // (gl_FragCoord) and never becomes a varying.
    type DepthSeedIn = { [<FragCoord>] fc : V4f }
    type DepthSeedOut = { [<FragCoord>] fc : V4f; [<Depth>] d : float32 }
    let pickDepthBefore (v : DepthSeedIn) =
        fragment {
            let r : DepthSeedOut = { fc = v.fc; d = v.fc.Z }
            return r
        }

    // ---- Mode A, with normal, with user PartIndex.
    type FinalA_In =
        {
            [<Color>] c : V4f
            [<Semantic("ViewSpaceNormal")>] vn : V3f
            [<Semantic("PickPartIndex")>] pi : int
            [<Depth>] d : float32
        }
    let pickFinalA (v : FinalA_In) =
        fragment {
            // All 4 channels are plain-float storage — never bit-cast. Each is
            // an integer or in-range float that survives MS resolve-average:
            //   * id  : float32 of an int < 2^24 (recycled, so live id space
            //           stays dense well under that ceiling).
            //   * n24 : float32 of a 12+12 octahedron normal packed into 24
            //           bits. Lower angular precision than the old Normal32
            //           bit-cast (12 bits/axis ≈ 0.05° vs 16 bits/axis ≈ 0.003°),
            //           but bullet-proof under MS resolve.
            //   * d   : NDC depth in [-1, 1].
            //   * pi  : float32 of an int < 2^24.
            let n24 = Normal24.encode (Vec.normalize v.vn)
            let d = (2.0f * v.d - 1.0f)
            let r : Fragment = { c = v.c; id = V4f(float32 uniform.PickId, float32 n24, d, float32 v.pi) }
            return r
        }

    // ---- Mode A, with normal, no PartIndex (defaults to 0 in fragment).
    type FinalANoPi_In =
        {
            [<Color>] c : V4f
            [<Semantic("ViewSpaceNormal")>] vn : V3f
            [<Depth>] d : float32
        }
    let pickFinalANoPi (v : FinalANoPi_In) =
        fragment {
            let n24 = Normal24.encode (Vec.normalize v.vn)
            let d = (2.0f * v.d - 1.0f)
            let r : Fragment = { c = v.c; id = V4f(float32 uniform.PickId, float32 n24, d, 0.0f) }
            return r
        }

    // ---- Mode A, no normal, with user PartIndex.
    type FinalANoNormal_In =
        {
            [<Color>] c : V4f
            [<Semantic("PickPartIndex")>] pi : int
            [<Depth>] d : float32
        }
    let pickFinalANoNormal (v : FinalANoNormal_In) =
        fragment {
            let d = (2.0f * v.d - 1.0f)
            let r : Fragment = { c = v.c; id = V4f(float32 uniform.PickId, 0.0f, d, float32 v.pi) }
            return r
        }
    // (no n24/n32 here — slot 1 already 0.0f, decoded as a zero-length normal.)

    // ---- Mode A, no normal, no PartIndex.
    type FinalANoNormalNoPi_In =
        {
            [<Color>] c : V4f
            [<Depth>] d : float32
        }
    let pickFinalANoNormalNoPi (v : FinalANoNormalNoPi_In) =
        fragment {
            let d = (2.0f * v.d - 1.0f)
            let r : Fragment = { c = v.c; id = V4f(float32 uniform.PickId, 0.0f, d, 0.0f) }
            return r
        }

    // ---- Mode B (real position). The user effect produces a precise
    // `PickViewPosition`; we store its components straight as plain float32
    // in slots 1/2/3. Mode B is signalled by NEGATIVE slot 0.
    //
    // Slot layout:
    //   slot 0 : -float32 PickId   (sign = mode tag)
    //   slot 1 : pvp.X             (plain float32)
    //   slot 2 : pvp.Y             (plain float32)
    //   slot 3 : pvp.Z             (plain float32)
    //
    // No depth slot — `pvp` IS the view-space position; the host doesn't
    // need to unproject. Per-component float32 precision is the user's
    // intended precision (they passed a `V3f`). Interior all-samples-agree
    // → exact round-trip; silhouette mixing → neighbour-validator on slot 0
    // rejects it.
    type FinalB_In =
        {
            [<Color>] c : V4f
            [<Semantic("PickViewPosition")>] pvp : V3f
        }
    let pickFinalB (v : FinalB_In) =
        fragment {
            let r : Fragment = { c = v.c; id = V4f(-(float32 uniform.PickId), v.pvp.X, v.pvp.Y, v.pvp.Z) }
            return r
        }

    let viewSpaceNormalEffect = Effect.ofFunction viewSpaceNormalVertex
    let pickDepthBeforeEffect = Effect.ofFunction pickDepthBefore
    let pickFinalAEffect = Effect.ofFunction pickFinalA
    let pickFinalANoPiEffect = Effect.ofFunction pickFinalANoPi
    let pickFinalANoNormalEffect = Effect.ofFunction pickFinalANoNormal
    let pickFinalANoNormalNoPiEffect = Effect.ofFunction pickFinalANoNormalNoPi
    let pickFinalBEffect = Effect.ofFunction pickFinalB

    /// Tag identifying which of our pick-final effects ended up at the tail
    /// of the composed chain. Exposed primarily so unit tests can assert the
    /// selection logic without round-tripping through `Effect.compose`.
    type PickFinal =
        | FinalA              // mode A, with normal,    with PartIndex
        | FinalANoPi          // mode A, with normal,    no PartIndex
        | FinalANoNormal      // mode A, no normal,      with PartIndex
        | FinalANoNormalNoPi  // mode A, no normal,      no PartIndex
        | FinalB              // mode B (real position)

    /// Result of resolving a pick chain for a given user effect + geometry.
    /// `Final` is the tail effect; `InjectVsn` says whether
    /// `viewSpaceNormalEffect` is prepended to synthesise ViewSpaceNormal
    /// from the geometry's `Normals` vertex attribute. The full chain is
    /// `(if InjectVsn then [viewSpaceNormalEffect] else []) @ [pickDepthBeforeEffect; eff; finalEffect Final]`.
    type PickChainChoice =
        {
            Final     : PickFinal
            InjectVsn : bool
        }

    /// Resolve the pick chain choice for a user effect against a callback
    /// `geomHas` reporting whether the target geometry exposes a given
    /// vertex/instance attribute. Pure function — driven entirely by
    /// FShade 5.7.4's `Effect.Dependencies`. No `Effect.toModule` calls,
    /// no speculative composition.
    ///
    /// Resolution rules:
    ///   * `PickViewPosition` produced by user (deps satisfied) → mode B
    ///   * else → mode A; pick the with-normal / no-normal · with-pi /
    ///     no-pi variant according to whether the user's effect can produce
    ///     `ViewSpaceNormal` (or geometry has `Normals` for synthesis) and
    ///     whether the user's effect can produce `PickPartIndex`.
    ///
    /// `InjectVsn` is set iff we need to synthesise vsn ourselves
    /// (geometry has `Normals` but user's effect doesn't produce vsn).
    /// FShade renames `Position` to `Positions0` (or `Positions1`, …) when
    /// the fragment shader reads `v.pos` directly — to disambiguate the
    /// fragment-input copy from the vertex-attribute output. Geometry only
    /// ever exposes the canonical `Positions` semantic, so treat any
    /// `Positions[0-9]+` as a synonym when checking attribute availability.
    let private canonicalSemantic (s : string) =
        if s.StartsWith "Positions" && s.Length > "Positions".Length then
            let suffix = s.Substring("Positions".Length)
            if suffix |> Seq.forall System.Char.IsDigit then "Positions" else s
        else s

    let chooseChain (eff : FShade.Effect) (geomHas : string -> bool) : PickChainChoice =
        let resolved : Map<string, FShade.OutputDeps> =
            FShade.EffectDeps.resolveTop eff.Dependencies

        let canEffProduce (sem : string) =
            match Map.tryFind sem resolved with
            | Some (d : FShade.OutputDeps) ->
                d.Inputs
                |> Map.toSeq
                |> Seq.forall (fun (n, _) -> geomHas (canonicalSemantic n))
            | None -> false

        let userVsn       = canEffProduce "ViewSpaceNormal"
        let userPvp       = canEffProduce "PickViewPosition"
        let userPi        = canEffProduce "PickPartIndex"
        let canSynthesise = geomHas "Normals"
        let canCarryVsn   = userVsn || canSynthesise
        let needInjectVsn = canSynthesise && not userVsn

        if userPvp then
            { Final = FinalB; InjectVsn = needInjectVsn }
        else
            let final =
                match canCarryVsn, userPi with
                | true,  true  -> FinalA
                | true,  false -> FinalANoPi
                | false, true  -> FinalANoNormal
                | false, false -> FinalANoNormalNoPi
            { Final = final; InjectVsn = needInjectVsn }

    /// Look up the actual effect for a `PickFinal` tag.
    let finalEffect = function
        | FinalA              -> pickFinalAEffect
        | FinalANoPi          -> pickFinalANoPiEffect
        | FinalANoNormal      -> pickFinalANoNormalEffect
        | FinalANoNormalNoPi  -> pickFinalANoNormalNoPiEffect
        | FinalB              -> pickFinalBEffect

    /// Build the composed pick effect for a user effect + geometry callback.
    let composePickChain (eff : FShade.Effect) (geomHas : string -> bool) : FShade.Effect =
        let choice = chooseChain eff geomHas
        let pre = if choice.InjectVsn then [viewSpaceNormalEffect] else []
        let chain = pre @ [pickDepthBeforeEffect; eff; finalEffect choice.Final]
        FShade.Effect.compose chain

    // Legacy record kept for `binary` / `outline` — they only read v.fc, but
    // share this type with their own callers. Stripped of the pick-specific
    // pi/pvp fields so it can't accidentally re-introduce them.
    type Vertex =
        {
            [<Color>] c : V4f
            [<Position>] pos : V4f
            [<Semantic("ViewSpaceNormal")>] vn : V3f
            [<Depth>] d : float32
            [<FragCoord>] fc : V4f
        }

    let pickSampler =
        intSampler2d {
            texture uniform?PickTexture
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }
        
    let binary (v : Vertex) =
        fragment {
            let mutable r = 0.0f
            let px = V2i v.fc.XY
            let s = pickSampler.Size
            if uniform.Selected >= 0 && px.X < s.X && px.Y < s.Y then
                let id = pickSampler.[px].X
                if id = uniform.Selected then
                    r <- 1.0f
            return V4f(r, r, r, r)
        }
        
    let quadTree =
        sampler2d {
            texture uniform?QuadTreeTexture
            filter Filter.MinMagMipPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }
        
    [<ReflectedDefinition>]
    let enqueue (value : V4f) (heap : ref<Arr<_, V4f>>) (cnt : int) =
        let mutable i = cnt
        let value = value
        let mutable run = true

        while run && i > 0 do
            let p = (i-1)/2
            let pv = (!heap).[p]
            if pv.X > value.X then
                (!heap).[i] <- pv
                i <- p
            else
                run <- false

        (!heap).[i] <- value

    [<ReflectedDefinition>]
    let dequeue (queue : ref<Arr<_, V4f>>) (cnt : int) =
        if cnt <= 1 then
            (!queue).[0]
        else
            let mutable cnt = cnt
            let minValue = (!queue).[0]
            let value = (!queue).[cnt - 1]
            let mutable run = true
            let mutable i = 0
            cnt <- cnt - 1
            while run && i < cnt do
                let i0 = 2*i+1
                let i1 = i0 + 1

                if i1 < cnt then
                    let v0 = (!queue).[i0]
                    let v1 = (!queue).[i1]

                    if v0.X < value.X then
                        if v1.X < value.X then
                            if v0.X < v1.X then
                                (!queue).[i] <- v0
                                i <- i0
                            else
                                (!queue).[i] <- v1
                                i <- i1
                        else
                            (!queue).[i] <- v0
                            i <- i0
                    elif v1.X < value.X then
                        (!queue).[i] <- v1
                        i <- i1
                    else
                        run <- false
                            
                elif i0 < cnt then
                    let v0 = (!queue).[i0]
                    if v0.X < value.X then
                        (!queue).[i] <- v0
                        i <- i0
                    else
                        run <- false
                else
                    run <- false
                    
            (!queue).[i] <- value

            minValue


    [<ReflectedDefinition; Inline>]
    let sqr (a : float32) = a*a

    [<ReflectedDefinition>]
    let minDistSq (px : V2i) (o : V2i) (s : V2i) : float32 =
        if s.X <= 1 && s.Y <= 1 then
            let d = V2f px - V2f o
            Vec.dot d d
        else
            let min = o
            let max = o + s - V2i.II


            let lx = px.X < min.X
            let ly = px.Y < min.Y
            let hx = px.X > max.X
            let hy = px.Y > max.Y

            let ix = not lx && not hx
            let iy = not ly && not hy

            if ix then
                if iy then
                    0.0f
                else
                    if hy then sqr (float32 (px.Y - max.Y))
                    else sqr (float32 (min.Y - px.Y))

            elif iy then
                if hx then sqr (float32 (px.X - max.X))
                else sqr (float32 (min.X - px.X))

            elif hx then
                if hy then
                    let d = (V2f px - V2f max)
                    Vec.dot d d
                else
                    let d = (V2f px - V2f(max.X, min.Y))
                    Vec.dot d d
            else
                if hy then
                    let d = (V2f px - V2f(min.X, max.Y))
                    Vec.dot d d
                else
                    let d = (V2f px - V2f min)
                    Vec.dot d d
                

            //if ix && iy then
            //    0.0
            //elif ix then
            //    if hy then sqr (float (px.Y - max.Y))
            //    else sqr (float (min.Y - px.Y))
            //elif iy then
            //    if hx then sqr (float (px.X - max.X))
            //    else sqr (float (min.X - px.X))
            //elif hx then
            //    if hy then 
            //        let d = (V2d px - V2d max)
            //        Vec.dot d d
            //    else 
            //        let d = (V2d px - V2d(max.X, min.Y))
            //        Vec.dot d d
            //else
            //    if hy then 
            //        let d = (V2d px - V2d(min.X, max.Y))
            //        Vec.dot d d
            //    else
            //        let d = (V2d px - V2d min)
            //        Vec.dot d d
            

                
            



    let outline (v : Vertex) =
        fragment {
            let px = V2i v.fc.XY
            let distanceSq = sqr uniform.Distance
            let mutable radiusSq = sqr (uniform.Distance + 1.0f)

            let queue : Arr<64 N, V4f> = Unchecked.defaultof<_>
            let mutable queueCount = 0

            //let treeSize = quadTree.Size
            let maxLevel = uniform.Levels - 1

            enqueue (V4f(0.0f, float32 maxLevel, 0.0f, 0.0f) ) &&queue queueCount
            queueCount <- queueCount + 1


            let mutable iter = 0
            while queueCount > 0 do
                let e = dequeue &&queue queueCount
                queueCount <- queueCount - 1


                iter <- iter + 1

                if e.X > radiusSq then
                    queueCount <- 0
                else
                    let level = int e.Y
                    let offset = V2i e.ZW
                    let levelPixelSize = uniform.LevelPixelSizes.[level]
                    let value = quadTree.Read(offset / levelPixelSize, level).X

                    if value >= 1.0f then
                        radiusSq <- e.X

                    elif value > 0.0f then
                        if levelPixelSize.X <= 4 && levelPixelSize.Y <= 4 then
                            for x in 0 .. levelPixelSize.X - 1 do
                                for y in 0 .. levelPixelSize.Y - 1 do
                                    let pp = offset + V2i(x,y)
                                    let value = quadTree.Read(pp, 0).X
                                    if value >= 1.0f then
                                        let d = V2f pp - V2f px
                                        let dSq = Vec.dot d d
                                        if dSq < radiusSq then
                                            radiusSq <- dSq

                        else
                            if level > 0 then
                                // has non-empty children
                                let h = levelPixelSize / 2

                                let i0 = offset
                                let i1 = offset + V2i(h.X, 0)
                                let i2 = offset + V2i(0, h.Y)
                                let i3 = offset + h

                                let c0 = V4f(minDistSq px i0 h, float32 level - 1.0f, V2f i0)
                                enqueue c0 &&queue queueCount; queueCount <- queueCount + 1

                                let c1 = V4f(minDistSq px i1 h, float32 level - 1.0f, V2f i1)
                                enqueue c1 &&queue queueCount; queueCount <- queueCount + 1

                                let c2 = V4f(minDistSq px i2 h, float32 level - 1.0f, V2f i2)
                                enqueue c2 &&queue queueCount; queueCount <- queueCount + 1

                                let c3 = V4f(minDistSq px i3 h, float32 level - 1.0f, V2f i3)
                                enqueue c3 &&queue queueCount; queueCount <- queueCount + 1


            //return V4f(float32 iter / 10.0f, 0.0f, 0.0f, 1.0f)
            //return quadTree.[px]

            let mutable a = 0.5f

            if radiusSq >= distanceSq then a <- 0.0f
            elif radiusSq <= 0.0f then a <- 0.1f


            //let t = sqrt radiusSq / uniform.Distance
            //let a = (1.0f - abs (2.0f * t - 1.0f)) ** 1.0f

            //let a = 1.0f - radius / uniform.Distance
            return V4f(0.05f, 0.5f, 0.85f, a)
                

        }
    //let selectionSampler =
    //    sampler2d {
    //        texture uniform?SelectionTexture
    //        addressU WrapMode.Clamp
    //        addressV WrapMode.Clamp
    //    }

    //let downsampleSelectionBuffer (v : Vertex) =
    //    fragment {
    //        let px = v.fc.XY
    //        let si = selectionSampler.GetSize uniform.Level
    //        let so = selectionSampler.GetSize (uniform.Level+1)
            
    //        let iMin = V2d si * (px - V2d.Half) / V2d so |> floor |> V2i
    //        let iMax = V2d si * (px + V2d.Half) / V2d so |> ceil |> V2i
            
    //        let mutable cnt = 0
    //        for x in iMin.X .. iMax.X do
    //            for y in iMin.Y .. iMax.Y do
    //                let v = selectionSampler.Read(V2i(x,y), uniform.Level).X
    //                if v > 0.0 then cnt <- cnt + 1
            
            
    //        if cnt > 0 then
    //            return V4d.IIII
    //        else
    //            return V4d.Zero
    //    }

module private PickBuffer =
    open Aardvark.SceneGraph
    
    let compileQuadTree (outputSignature : IFramebufferSignature) (selected : aval<int>) (pickTexture : IBackendTexture) =
        let runtime = pickTexture.Runtime :?> IRuntime
        use __ = runtime.ContextLock
        let srcSize = pickTexture.Size.XY

        let treeSize = 
            //let a = Fun.NextPowerOfTwo (max srcSize.X srcSize.Y)
            V2i(Fun.NextPowerOfTwo srcSize.X, Fun.NextPowerOfTwo srcSize.Y)
        let levels = 1 + int (floor (log2 (float (max treeSize.X treeSize.Y))))


        let selectionTexture = runtime.CreateTexture2D(treeSize, TextureFormat.R32f, levels)


        let signature = runtime.CreateFramebufferSignature([ DefaultSemantic.Colors, TextureFormat.R32f ])
        
        let createSelection =
            let render =
                Sg.fullScreenQuad
                |> Sg.depthTest' DepthTest.None
                |> Sg.shader { do! PickShader.binary }
                |> Sg.uniform "Selected" selected
                |> Sg.texture' "PickTexture" pickTexture
                |> Sg.compile runtime signature

            render

        let fbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, selectionTexture.[TextureAspect.Color, 0, 0] :> IFramebufferOutput])
            
        createSelection.Update(AdaptiveToken.Top, RenderToken.Empty)

        let levelSizes =
            let mutable s = treeSize
            Array.init 16 (fun _i ->
                let mine = s
                s <- V2i(max 1 (s.X / 2), max 1 (s.Y / 2))
                mine
            )

        let levelPixelSizes =
            levelSizes |> Array.map (fun s ->
                treeSize / s
            )


        let renderOutline =
            Sg.fullScreenQuad
            |> Sg.depthTest' DepthTest.None
            |> Sg.shader { do! PickShader.outline }
            |> Sg.texture' "QuadTreeTexture" selectionTexture
            |> Sg.uniform' "Distance" 10.0
            |> Sg.uniform' "Levels" levels
            |> Sg.uniform' "LevelSizes" levelSizes
            |> Sg.uniform' "LevelPixelSizes" levelPixelSizes
            |> Sg.blendMode' BlendMode.Blend
            |> Sg.compile runtime outputSignature

        let run () =
            createSelection.Run(fbo)
            runtime.GenerateMipMaps selectionTexture
  
        let release() =
            createSelection.Dispose()
            signature.Dispose()
            runtime.DeleteFramebuffer fbo
            runtime.DeleteTexture selectionTexture
            
        renderOutline, run, release
        
    

    
[<AutoOpen>]
module internal BlitExtensions =
    let private pickBuffer = Symbol.Create "PickId"
    let private pickViewPosition = Symbol.Create "PickViewPosition"
    //module private GL =
    //    open OpenTK.Graphics.OpenGL4
    //    open Aardvark.Rendering.GL
    //    let blit (src : Framebuffer) (dst : Framebuffer) =
    //        let drawBuffer =
    //            if dst.Handle = 0 then DrawBufferMode.BackLeft
    //            else DrawBufferMode.ColorAttachment0
            
    //        GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, src.Handle)
    //        GL.BindFramebuffer(FramebufferTarget.DrawFramebuffer, dst.Handle)
            
    //        GL.DrawBuffer(drawBuffer)
    //        GL.ReadBuffer(ReadBufferMode.ColorAttachment0)
    //        GL.BlitFramebuffer(
    //            0, 0, src.Size.X, src.Size.Y,
    //            0, 0, dst.Size.X, dst.Size.Y,
    //            ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit ||| ClearBufferMask.StencilBufferBit,
    //            BlitFramebufferFilter.Nearest
    //        )
    //        GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, dst.Handle)
            
    //    let readPixel (src : Framebuffer) (px : V2i) =
    //        use __ = src.Context.ResourceLock
    //        GL.BindFramebuffer(FramebufferTarget.Framebuffer, src.Handle)
    //        let res = [| V4i.Zero |]
    //        let (KeyValue(slot, _)) = src.Signature.Layout.ColorAttachments |> Seq.find (fun (KeyValue(id, sem)) -> sem.Name = pickBuffer)
    //        GL.ReadBuffer(unbox (int ReadBufferMode.ColorAttachment0 + slot))
    //        GL.BindBuffer(BufferTarget.PixelPackBuffer, 0)
    //        GL.BindBuffer(BufferTarget.PixelUnpackBuffer, 0)
    //        GL.ReadPixels(px.X, src.Size.Y - 1 - px.Y, 1, 1, PixelFormat.RgbaInteger, PixelType.Int, res)
    //        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
    //        res.[0]
        
    //module Vulkan =
    //    open Aardvark.Rendering.Vulkan
    //    let blit (src : Framebuffer) (dst : Framebuffer) =
    //        let device = src.Device
            
    //        device.perform {
    //            for (KeyValue(name, srcView)) in src.Attachments do
    //                match dst.Attachments.TryGetValue name with
    //                | (true, dstView) ->
    //                    let ap = if name = DefaultSemantic.DepthStencil then TextureAspect.DepthStencil else TextureAspect.Color
    //                    let lSrc = srcView.Image.Layout
    //                    let lDst = dstView.Image.Layout
    //                    do! Command.TransformLayout(srcView.Image, VkImageLayout.TransferSrcOptimal)
    //                    do! Command.TransformLayout(dstView.Image, VkImageLayout.TransferDstOptimal)
    //                    do! Command.Copy(srcView.Image.[ap, 0, *], dstView.Image.[ap, 0, *])
    //                    do! Command.TransformLayout(srcView.Image, lSrc)
    //                    do! Command.TransformLayout(dstView.Image, lDst)
    //                | _ ->
    //                    ()
    //        }
                      
    //    let readPixel (src : Framebuffer) (px : V2i) =
    //        let att = src.Attachments.[pickBuffer].Image :> IBackendTexture
    //        let rt = src.Device.Runtime
    //        let dst = PixImage<int>(Col.Format.RGBA, V2i.II)
    //        rt.Download(att.[TextureAspect.Color, 0, 0], dst, px, V2i.II)
    //        let r = V4i dst.Volume.Data
    //        r
            
    /// Raw pick-buffer region returned by `ReadPickRegion`. Decoding is
    /// inlined at the call site to avoid per-pixel closure/option allocation
    /// in the spiral hot loop.
    [<Struct>]
    type PickRegion =
        { Data       : float32[]
          BaseIdx    : int
          Dx         : int
          Dy         : int
          Dz         : int
          OriginX    : int
          OriginY    : int
          SizeX      : int
          SizeY      : int }

    /// Pre-flattened BVH cull entry. Avoids the per-iter
    /// `struct(_,(_,_))` destructure in the spiral.
    [<Struct>]
    type CullEntry =
        { State        : TraversalState
          Intersectable: IIntersectable
          Trafo        : Trafo3d }

    type IFramebufferRuntime with
        member x.BlitFramebuffer(src : IFramebuffer, dst : IFramebuffer) =
            x.Copy(src, dst)

        /// Reads a (2*radius+1)² region of the pick buffer around `center`,
        /// clamped to framebuffer bounds. Returns the raw region data; the
        /// caller decodes pixel-by-pixel.
        member x.ReadPickRegion(src : IFramebuffer, center : V2i, radius : int) : voption<PickRegion> =
            let s = src.Size.XY
            let originX = max 0 (center.X - radius)
            let originY = max 0 (center.Y - radius)
            let endX = min s.X (center.X + radius + 1)
            let endY = min s.Y (center.Y + radius + 1)
            let sizeX = endX - originX
            let sizeY = endY - originY
            if sizeX <= 0 || sizeY <= 0 then
                ValueNone
            else
                let img = x.ReadPixels(src, pickBuffer, V2i(originX, originY), V2i(sizeX, sizeY)) :?> PixImage<float32>
                ValueSome {
                    Data    = img.Data
                    BaseIdx = int img.Volume.Origin
                    Dx      = int img.Volume.DX
                    Dy      = int img.Volume.DY
                    Dz      = int img.Volume.DZ
                    OriginX = originX
                    OriginY = originY
                    SizeX   = sizeX
                    SizeY   = sizeY
                }

        /// Legacy wrapper kept compatible with the previous closure-based API
        /// for any external callers that still want it. The inlined fast path
        /// is in `Read` below.
        member x.ReadPickRegion(src : IFramebuffer, projTrafo : Trafo3d, center : V2i, radius : int) : V2i -> option<int * int * V3d * V3d> =
            let s = src.Size.XY
            let originX = max 0 (center.X - radius)
            let originY = max 0 (center.Y - radius)
            let endX = min s.X (center.X + radius + 1)
            let endY = min s.Y (center.Y + radius + 1)
            let regionSize = V2i(endX - originX, endY - originY)
            if regionSize.X <= 0 || regionSize.Y <= 0 then
                fun _ -> None
            else
                let regionOrigin = V2i(originX, originY)
                let img = x.ReadPixels(src, pickBuffer, regionOrigin, regionSize) :?> PixImage<float32>
                let data = img.Data
                let baseIdx = int img.Volume.Origin
                let dx = int img.Volume.DX
                let dy = int img.Volume.DY
                let dz = int img.Volume.DZ

                // Read PickId-as-int at a region-local pixel, or 0 if out of
                // region bounds.
                let inline readIdAt (lx : int) (ly : int) =
                    if lx < 0 || ly < 0 || lx >= regionSize.X || ly >= regionSize.Y then 0
                    else
                        let f0 = data.[baseIdx + lx * dx + ly * dy]
                        if f0 = 0.0f then 0 else int f0

                // Compute the view-space position at a region-local pixel,
                // using the same decoding rule as the main path. Used for
                // CPU normal estimation from same-id neighbours.
                let viewPosAt (lx : int) (ly : int) (idF : int) : V3d =
                    let i0 = baseIdx + lx * dx + ly * dy
                    let f1 = data.[i0 + dz]
                    let f2 = data.[i0 + 2 * dz]
                    let f3 = data.[i0 + 3 * dz]
                    if idF > 0 then
                        let px = originX + lx
                        let py = originY + ly
                        let depth = float f2
                        let tc = (V2d(px, py) + V2d.Half) / V2d s
                        let ndc = V3d(2.0 * tc.X - 1.0, 1.0 - 2.0 * tc.Y, depth)
                        projTrafo.Backward.TransformPosProj ndc
                    else
                        V3d(float f1, float f2, float f3)

                fun (offset : V2i) ->
                    let p = center + offset
                    let lx = p.X - originX
                    let ly = p.Y - originY
                    if lx < 0 || ly < 0 || lx >= regionSize.X || ly >= regionSize.Y then None
                    else
                        let idF = readIdAt lx ly
                        if idF = 0 then None
                        else
                            // Walk the 3×3 neighbourhood once, simultaneously
                            // counting same-id neighbours (validation) and
                            // gathering their view-space positions (for CPU
                            // normal estimation when the encoded normal is
                            // missing — mode B always, mode A NoNormal variants).
                            //
                            // Silhouette pixels under MS resolve produce
                            // essentially-random averaged ids; two adjacent
                            // garbage averages matching the same int is
                            // vanishingly unlikely, so ≥2 same-id neighbours
                            // is a reliable interior-pixel test.
                            let nbrs = ResizeArray<V3d>(8)
                            for ddy in -1 .. 1 do
                                for ddx in -1 .. 1 do
                                    if (ddx <> 0 || ddy <> 0) && readIdAt (lx + ddx) (ly + ddy) = idF then
                                        nbrs.Add (viewPosAt (lx + ddx) (ly + ddy) idF)
                            if nbrs.Count < 2 then None
                            else
                                let i0 = baseIdx + lx * dx + ly * dy
                                let f1 = data.[i0 + dz]
                                let f2 = data.[i0 + 2 * dz]
                                let f3 = data.[i0 + 3 * dz]

                                let viewPos, encodedNormal, partIndex =
                                    if idF > 0 then
                                        // Mode A: id, n24, depth, pi.
                                        let depth = float f2
                                        let tc = (V2d p + V2d.Half) / V2d s
                                        let ndc = V3d(2.0 * tc.X - 1.0, 1.0 - 2.0 * tc.Y, depth)
                                        let vp = projTrafo.Backward.TransformPosProj ndc
                                        let n = if int f1 = 0 then V3d.Zero else V3d (Normal24.decode (int f1))
                                        vp, n, int f3
                                    else
                                        // Mode B: -id, pvp.x, pvp.y, pvp.z.
                                        V3d(float f1, float f2, float f3), V3d.Zero, 0

                                // CPU normal estimation when none was encoded.
                                // Pick the same-id neighbour pair whose
                                // view-space tangent vectors give the largest
                                // cross-product magnitude, then orient toward
                                // the camera (view-space camera at origin
                                // looking down -Z, so visible surfaces have
                                // normals with negative-or-zero z).
                                let normal =
                                    if encodedNormal <> V3d.Zero then
                                        encodedNormal
                                    else
                                        let mutable bestArea = 0.0
                                        let mutable bestN = V3d.Zero
                                        for i in 0 .. nbrs.Count - 2 do
                                            let vi = nbrs.[i] - viewPos
                                            for j in i + 1 .. nbrs.Count - 1 do
                                                let n = Vec.cross vi (nbrs.[j] - viewPos)
                                                let m = n.Length
                                                if m > bestArea then
                                                    bestArea <- m
                                                    bestN <- n / m
                                        if bestArea = 0.0 then V3d.Zero
                                        elif bestN.Z > 0.0 then -bestN
                                        else bestN

                                Some (abs idF, partIndex, viewPos, normal)

[<AutoOpen>]
module internal PickSnap =
    /// Pixel-snap radius for picking. `Read` reads a (2R+1)² region around
    /// the cursor and walks `offsets` from the center outward.
    let radius = 16

    /// Pixel offsets within the snap disc (|offset| <= radius), sorted by
    /// squared distance from the center. The first entry is V2i.OO.
    let offsets : V2i[] =
        let r = radius
        [|
            for dx in -r .. r do
                for dy in -r .. r do
                    let d2 = dx*dx + dy*dy
                    if d2 <= r*r then yield struct(d2, V2i(dx, dy))
        |]
        |> Array.sortBy (fun struct(d2, _) -> d2)
        |> Array.map (fun struct(_, p) -> p)


type private SceneHandlerFramebuffers =
    {
        PickableFramebuffer         : IFramebuffer
        NonPickableFramebuffer      : IFramebuffer
        PickBuffer                  : IRenderbuffer
        PickTextureResolved         : IBackendTexture
        PickFramebufferResolved     : IFramebuffer
        Disposables                 : list<System.IDisposable>
    }


type private Stats(maxCnt : int) =
    let mutable sum = 0.0
    let mutable sumSq = 0.0
    let elements = System.Collections.Generic.Queue<float>(maxCnt + 1)

    member x.Add(value : float) =
        sum <- sum + value
        sumSq <- sumSq + value * value
        elements.Enqueue(value)

        if elements.Count > maxCnt then
            let o = elements.Dequeue()
            sum <- sum - o
            sumSq <- sumSq - o * o

    member x.Average = 
        let c = elements.Count
        if c <= 0 then 0.0
        else sum / float c

    member x.StdDev = 
        let c = elements.Count
        if c > 1 then
            let avg = sum / float c
            sqrt (max 0.0 ((sumSq - avg * sum) / float (c - 1)))
        else
            0.0


type SceneHandler(signature : IFramebufferSignature, trigger : RenderControlEvent -> unit, setCursor : option<string> -> unit, scene : ISceneNode, view : aval<Trafo3d>, proj : aval<Trafo3d>, fboSize : cval<V2i>, time : cval<System.DateTime>) =
    static let pickBuffer = Symbol.Create "PickId"
    
    let runtime = signature.Runtime :?> IRuntime

    // PickId state. Ids are assigned per-scope on first use (acquireId), and
    // recycled when the last RenderObject referencing a scope is removed
    // (releaseId). The free-list is a SortedSet so reuse picks the smallest
    // available id first — keeps the live id space dense, well clear of the
    // ~8M pickId-as-float32 ceiling.
    let mutable currentId = 1
    let pickIds = Dict<TraversalState, int>()
    let scopes = Dict<int, TraversalState>()
    let pickIdRefs = Dict<TraversalState, int>()
    let freeIds = System.Collections.Generic.SortedSet<int>()
    // For each top-level IRenderObject in the `render` ASet, the multiset of
    // scopes it acquired during wrapping (one entry per pickable leaf). On
    // remove we release each. Needed because a MultiRenderObject can acquire
    // its scope N times for N pickable children but appears only once in the
    // ASet's delta stream.
    let acquiredFor = Dict<IRenderObject, ResizeArray<TraversalState>>()

    let mutable pickTexture : option<IFramebuffer> = None
    //let mutable attachments = []
    let mutable fbos : option<SceneHandlerFramebuffers> = None
    let mutable viewportSize = V2i.Zero
    let hoverId = cval -1
    let lastOver : cval<option<TraversalState>> = cval None
    let lastFocus : cval<option<TraversalState>> = cval None
    
    let cursor = 
        lastOver |> AVal.bind (function
            | Some l -> l.Cursor
            | None -> AVal.constant None
        )

    let cursorSub =
        cursor.AddCallback setCursor


    let clearColor = cval C4f.Black

    let acquireId (scope : TraversalState) =
        let id =
            match pickIds.TryGetValue scope with
            | true, id -> id
            | _ ->
                let id =
                    if freeIds.Count > 0 then
                        let i = freeIds.Min
                        freeIds.Remove i |> ignore
                        i
                    else
                        let i = currentId
                        currentId <- i + 1
                        i
                pickIds.[scope] <- id
                scopes.[id] <- scope
                id
        pickIdRefs.[scope] <-
            match pickIdRefs.TryGetValue scope with
            | true, n -> n + 1
            | _ -> 1
        id

    let releaseId (scope : TraversalState) =
        match pickIdRefs.TryGetValue scope with
        | true, 1 ->
            let id = pickIds.[scope]
            pickIds.Remove scope |> ignore
            scopes.Remove id |> ignore
            pickIdRefs.Remove scope |> ignore
            freeIds.Add id |> ignore
        | true, n ->
            pickIdRefs.[scope] <- n - 1
        | _ -> ()

    let mutable lastMousePosition = None
     
    /// Read the (2*PickSnap.radius+1)² pick-buffer region around `pixel`.
    /// Returns the raw region data; `Read` decodes inline.
    let readPickRegion (pixel : V2i) : voption<PickRegion> =
        match pickTexture with
        | Some pickFbo when pixel.AllGreaterOrEqual 0 && pixel.AllSmaller pickFbo.Size.XY ->
            runtime.ReadPickRegion(pickFbo, pixel, PickSnap.radius)
        | _ ->
            ValueNone
             

    let mutable renderTask, pickObjects, dispose =
        let runtime = signature.Runtime :?> IRuntime

        // Single MS FBO: extend the user's signature with the pick attachment
        // (`Rgba32f`, MS) so pickable geometry writes color, depth, and the
        // PickId record in one render pass. After render the pick attachment
        // is resolved to a single-sample texture for `ReadPixels`.
        //
        // Encoding caveat: MS resolve averages samples for float formats. At
        // silhouette pixels the averaged PickId lands on a non-integer that
        // (almost always) won't map to any registered scope — so the snap
        // spiral skips it. The default `PixelSnapRadius = 1` absorbs that
        // 1-pixel feather.
        let pickId = signature.ColorAttachmentSlots

        let colorAttachments =
            Map.add pickId { Name = pickBuffer; Format = TextureFormat.Rgba32f } signature.ColorAttachments

        let newSignature =
            runtime.CreateFramebufferSignature(colorAttachments, signature.DepthStencilAttachment, signature.Samples, signature.LayerCount, signature.PerLayerUniforms)

        let render, pick = scene.GetObjects(TraversalState.empty runtime)


        let rec wrapObject (acquired : ResizeArray<TraversalState>) (t : TraversalState) (o : IRenderObject) =
            if t.PixelPick then
                match o with
                | :? RenderObject as o ->
                    let pickId = acquireId t
                    acquired.Add t
                    match o.Surface with
                    | Surface.Effect eff ->
                        let newShaders =
                            lazy (
                                let geomHas (sem : string) =
                                    ValueOption.isSome (o.VertexAttributes.TryGetAttribute (Symbol.Create sem)) ||
                                    ValueOption.isSome (o.InstanceAttributes.TryGetAttribute (Symbol.Create sem))
                                let newShader = PickShader.composePickChain eff geomHas
                                newShader.Shaders
                            )

                        // `pickv3_` — bump when the pick chain encoding changes
                        // so cached compiled shaders from older builds get
                        // rejected by name instead of silently rebound.
                        let newEffect = FShade.Effect("pickv3_" + eff.Id, newShaders, [])

                        let r = RenderObject.Clone o

                        let newBlendState =
                            let newModes =
                                o.BlendState.AttachmentMode |> AVal.map (fun map ->
                                    Map.add pickBuffer BlendMode.None map
                                )
                            let newWrites =
                                o.BlendState.AttachmentWriteMask |> AVal.map (fun map ->
                                    Map.add pickBuffer ColorMask.All map
                                )

                            {
                                Mode = o.BlendState.Mode
                                AttachmentMode = newModes
                                AttachmentWriteMask = newWrites
                                ConstantColor = o.BlendState.ConstantColor
                                ColorWriteMask = o.BlendState.ColorWriteMask
                            }


                        r.Uniforms <- UniformProvider.union o.Uniforms (UniformProvider.ofList ["PickId", AVal.constant pickId :> IAdaptiveValue])
                        r.Surface <- Surface.Effect newEffect
                        r.BlendState <- newBlendState
                        r :> IRenderObject, true
                    | s ->
                        Log.warn "cannot change surface: %A" s
                        o :> IRenderObject, true

                | :? MultiRenderObject as o ->
                    let res = o.Children |> List.map (wrapObject acquired t)
                    if res |> List.forall snd then
                        let n = MultiRenderObject(List.map fst res)
                        n :> IRenderObject, true
                    else
                        o :> IRenderObject, false
                | o ->
                    Log.warn "cannot wrap object: %A" o
                    o, false
            else
                o, false


        let objs =
            render
            |> ASet.map (fun o ->
                let acquired = ResizeArray<TraversalState>()
                let wrapped, p =
                    match RenderObject.traversalStates.TryGetValue o with
                    | true, t -> wrapObject acquired t o
                    | _ -> o, false
                if acquired.Count > 0 then
                    acquiredFor.[o] <- acquired
                wrapped, p
            )

        // Independent reader on `render` to observe removals → release ids.
        // ASet.map's mapper above only fires on Add; this reader observes Rem
        // and walks the per-IRO `acquiredFor` record so we release exactly as
        // many times as we acquired (a MultiRenderObject can acquire its
        // scope N times for N pickable children).
        let releaseTrackerReader = render.GetReader()
        let pumpReleases (token : AdaptiveToken) =
            let ops = releaseTrackerReader.GetChanges token
            for op in ops do
                match op with
                | Add _ -> ()
                | Rem(_, o) ->
                    match acquiredFor.TryGetValue o with
                    | true, scopes ->
                        acquiredFor.Remove o |> ignore
                        for s in scopes do releaseId s
                    | _ -> ()

        let pickable = objs |> ASet.choose (fun (o, p) -> if p then Some o else None)
        let nonPickable = objs |> ASet.choose (fun (o, p) -> if not p then Some o else None)

        let renderPickable = runtime.CompileRender(newSignature, pickable)
        let renderNonPickable = runtime.CompileRender(signature, nonPickable)


        let getFramebuffers (size : V2i) =
            match fbos with
            | Some o when o.PickableFramebuffer.Size = size ->
                o
            | _ ->
                match fbos with
                | Some o ->
                    runtime.DeleteFramebuffer o.PickableFramebuffer
                    runtime.DeleteFramebuffer o.NonPickableFramebuffer
                    runtime.DeleteFramebuffer o.PickFramebufferResolved
                    for t in o.Disposables do t.Dispose()
                | None -> ()

                let semantics =
                    let res =
                        newSignature.ColorAttachments
                        |> Map.toList
                        |> List.map (fun (_slot, a) -> a.Name, a.Format)
                        |> Map.ofList
                    match newSignature.DepthStencilAttachment with
                    | Some ds -> Map.add DefaultSemantic.DepthStencil ds res
                    | None -> res

                // Both framebuffers (with and without pick attachment) share
                // the same backing color/depth renderbuffers, so non-pickable
                // RenderObjects can render to the same color/depth as pickable
                // ones without paying for a second color buffer.
                let buffers =
                    semantics |> Map.map (fun _ fmt ->
                        runtime.CreateRenderbuffer(size, fmt, newSignature.Samples)
                    )
                let outputs = buffers |> Map.map (fun _ b -> b :> IFramebufferOutput)

                let nf = runtime.CreateFramebuffer(signature, outputs)
                let pf = runtime.CreateFramebuffer(newSignature, outputs)

                // Single-sample resolve target for the pick attachment so
                // `ReadPixels` has a non-MS source to read from.
                let pickResolvedTex = runtime.CreateTexture2D(size, TextureFormat.Rgba32f, 1, 1)
                let pickResolvedSig = runtime.CreateFramebufferSignature([pickBuffer, TextureFormat.Rgba32f])
                let pickResolvedFbo =
                    runtime.CreateFramebuffer(
                        pickResolvedSig,
                        [pickBuffer, pickResolvedTex.[TextureAspect.Color, 0, 0] :> IFramebufferOutput]
                    )

                let result =
                    {
                        PickableFramebuffer     = pf
                        NonPickableFramebuffer  = nf
                        PickBuffer              = buffers.[pickBuffer]
                        PickTextureResolved     = pickResolvedTex
                        PickFramebufferResolved = pickResolvedFbo
                        Disposables =
                            (pickResolvedTex :> System.IDisposable)
                            :: (buffers |> Map.toList |> List.map (fun (_, b) -> b :> System.IDisposable))
                    }

                fbos <- Some result
                result


        let clear =
            let clearValues =
                clearColor
                |> AVal.map (fun color ->
                    ClearValues.empty
                    |> ClearValues.colors (Map.ofList [
                        DefaultSemantic.Colors, ClearColor.op_Implicit color
                        pickBuffer, ClearColor.op_Implicit V4f.Zero
                    ])
                    |> ClearValues.depth 1.0
                    |> ClearValues.stencil 0
                )
            runtime.CompileClear (newSignature, clearValues)

        let mutable idx = 0
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let frameTimeWatch = System.Diagnostics.Stopwatch()
        let mutable frameTimeStats = Stats(30)

        let getInfo (size : V2i) =
            {
                Signature = signature
                Size = size
                FrameIndex = idx
                Time = sw.MicroTime
                FrameTime = MicroTime(System.TimeSpan(int64 frameTimeStats.Average))
            }


        let task =
            RenderTask.custom (fun (t, _rt, o) ->
                frameTimeWatch.Restart()
                let size = o.Framebuffer.Size
                let evtInfo = getInfo size

                if o.Framebuffer.Size <> fboSize.Value then
                    transact (fun () -> fboSize.Value <- o.Framebuffer.Size)
                    trigger (RenderControlEvent.Resize evtInfo)

                let s = fboSize.GetValue t
                let rt = RenderToken.Empty
                let outputInfo = getFramebuffers s

                // Recycle ids for any RenderObjects that were removed since
                // last frame.
                pumpReleases t

                trigger (RenderControlEvent.PreRender evtInfo)

                clear.Run(t, rt, outputInfo.PickableFramebuffer)
                renderPickable.Run(t, rt, outputInfo.PickableFramebuffer)
                renderNonPickable.Run(t, rt, outputInfo.NonPickableFramebuffer)

                let pickRb = outputInfo.PickBuffer
                if pickRb.Samples > 1 then
                    runtime.ResolveMultisamples(pickRb, outputInfo.PickTextureResolved)
                else
                    runtime.Copy(pickRb, outputInfo.PickTextureResolved.[TextureAspect.Color, 0, *])

                pickTexture <- Some outputInfo.PickFramebufferResolved
                viewportSize <- outputInfo.PickTextureResolved.Size.XY

                trigger (RenderControlEvent.PostRender evtInfo)


                runtime.BlitFramebuffer(outputInfo.NonPickableFramebuffer, o.Framebuffer)
                idx <- idx + 1
                frameTimeWatch.Stop()
                frameTimeStats.Add (float frameTimeWatch.Elapsed.Ticks)

            )

        let dispose() =
            clear.Dispose()
            renderPickable.Dispose()
            renderNonPickable.Dispose()
            task.Dispose()
            newSignature.Dispose()
            match fbos with
            | Some o ->
                runtime.DeleteFramebuffer o.PickableFramebuffer
                runtime.DeleteFramebuffer o.NonPickableFramebuffer
                runtime.DeleteFramebuffer o.PickFramebufferResolved
                for t in o.Disposables do t.Dispose()
                fbos <- None
            | None ->
                ()

        task, pick, dispose
    
    
    
    let mutable bvh =
        let transformedPickObject =
            pickObjects |> ASet.mapA (fun o ->
                o.Trafo |> AVal.map (fun t -> o.Intersectable, o.State, t)    
            )
            
        let reader = transformedPickObject.GetReader()
        let mutable tree = Aardvark.Dom.BvhTree3d.empty
        AVal.custom (fun token ->
            let ops = reader.GetChanges token
            
            for op in ops do
                match op with
                | Add _ -> ()
                | Rem(_, (_, s, _)) -> tree <- Aardvark.Dom.BvhTree3d.remove s tree

            for op in ops do
                match op with
                | Add(_, (i, s, t)) ->
                    let bb = i.BoundingBox.Transformed t
                    tree <- Aardvark.Dom.BvhTree3d.add s bb (i, t) tree
                | Rem _ ->
                    ()
            tree
        )
   

    let capturedScopes = Dict<int, TraversalState>()
    let mutable lastMouseInfo = None
    let mutable lastRealMouseInfo = None
    
    member private x.Dispose(disposing : bool) =
        printfn "SceneHandler died: %A" disposing
        if disposing then System.GC.SuppressFinalize x
        pickTexture <- None
        lastMouseInfo <- None
        lastRealMouseInfo <- None
        capturedScopes.Clear()
        dispose()
        renderTask <- RenderTask.empty
        pickObjects <- ASet.empty
        bvh <- AVal.constant BvhTree3d.empty
        cursorSub.Dispose()

    member x.Dispose() = x.Dispose true
    override x.Finalize() = x.Dispose false
    interface System.IDisposable with
        member x.Dispose() = x.Dispose()


    member x.Time = time
    member x.ClearColor = clearColor
    member x.Runtime = runtime
    member x.FramebufferSignature = signature

    member x.Cursor = cursor

    member x.RenderTask = renderTask
  

    member x.Read(pixel : V2i, pointerId : int, kind : SceneEventKind, ?evt : PointerEvent) =

        let capturedScope =
            match capturedScopes.TryGetValue pointerId with
            | true, s -> Some s
            | _ -> None

        let result =
            if pixel.AllGreaterOrEqual 0 && pixel.AllSmaller viewportSize then
                let v = AVal.force view
                let p = AVal.force proj
                lastMousePosition <- Some pixel

                let s =
                    match pickTexture with
                    | Some fbo -> fbo.Size.XY
                    | None -> V2i.II

                let vp = v * p
                // Hoist matrix references — Trafo3d.Backward is a property
                // recompute on each access; keep one M44d struct on the stack.
                let vpBwd = vp.Backward
                let vpFwd = vp.Forward
                let pBwd  = p.Backward      // proj⁻¹: NDC → VIEW space (mode A unproject)
                let vBwd  = v.Backward
                let vFwd  = v.Forward
                let sX    = float s.X
                let sY    = float s.Y

                // Inline rayFor — used at most ~805 times per Read.
                let inline rayFor (pxX : int) (pxY : int) =
                    let ndcX = 2.0 * float pxX / sX - 1.0
                    let ndcY = 1.0 - 2.0 * float pxY / sY
                    let p0 = vpBwd.TransformPosProj(V3d(ndcX, ndcY, -1.0))
                    let p1 = vpBwd.TransformPosProj(V3d(ndcX, ndcY,  0.0))
                    Ray3d(p0, p1 - p0 |> Vec.normalize)

                // ---- Cone hull over the snap window for BVH culling.
                let r = PickSnap.radius
                let xMin = 2.0 * float (max 0 (pixel.X - r))         / sX - 1.0
                let xMax = 2.0 * float (min s.X (pixel.X + r + 1))   / sX - 1.0
                let yMin = 1.0 - 2.0 * float (min s.Y (pixel.Y + r + 1)) / sY
                let yMax = 1.0 - 2.0 * float (max 0 (pixel.Y - r))       / sY
                let cx = (xMin + xMax) * 0.5
                let cy = (yMin + yMax) * 0.5
                let sx = 2.0 / (xMax - xMin)
                let sy = 2.0 / (yMax - yMin)
                let tSub = Trafo3d.Translation(-cx, -cy, 0.0) * Trafo3d.Scale(sx, sy, 1.0)
                let coneHull = ViewProjection.toHull3d (vp * tSub) |> FastHull3d
                let bvh = AVal.force bvh
                // Pre-flatten BVH cull set into a typed struct array. Each
                // `CullEntry` is a struct so iteration over the array is
                // pointer-chase-free and the per-iter destructure of nested
                // `struct(_, (_,_))` is gone.
                let cullSet =
                    let m = bvh.GetIntersecting coneHull
                    let arr = Array.zeroCreate<CullEntry> m.Count
                    let mutable i = 0
                    for state, struct(_, (it, trafo)) in m do
                        arr.[i] <- { State = state; Intersectable = it; Trafo = trafo }
                        i <- i + 1
                    arr

                // Pick-buffer region — raw float32 array + strides. No closure.
                let regionOpt = readPickRegion pixel
                let hasRegion = regionOpt.IsSome
                let region    = if hasRegion then regionOpt.Value else Unchecked.defaultof<_>
                let rData     = if hasRegion then region.Data else null
                let rBase     = if hasRegion then region.BaseIdx else 0
                let rDx       = if hasRegion then region.Dx else 0
                let rDy       = if hasRegion then region.Dy else 0
                let rDz       = if hasRegion then region.Dz else 0
                let rOriginX  = if hasRegion then region.OriginX else 0
                let rOriginY  = if hasRegion then region.OriginY else 0
                let rSizeX    = if hasRegion then region.SizeX else 0
                let rSizeY    = if hasRegion then region.SizeY else 0

                // Read the (signed) PickId at a region-local pixel, or 0.
                let inline readIdAt (lx : int) (ly : int) =
                    if not hasRegion || lx < 0 || ly < 0 || lx >= rSizeX || ly >= rSizeY then 0
                    else
                        let f0 = rData.[rBase + lx * rDx + ly * rDy]
                        if f0 = 0.0f then 0 else int f0

                // Resolve view-space position at a same-id neighbour. Only
                // called once per accepted candidate (lazy normal estimation).
                let viewPosAt (lx : int) (ly : int) (idF : int) =
                    let i0 = rBase + lx * rDx + ly * rDy
                    let f1 = rData.[i0 + rDz]
                    let f2 = rData.[i0 + 2 * rDz]
                    let f3 = rData.[i0 + 3 * rDz]
                    if idF > 0 then
                        let pxX = rOriginX + lx
                        let pxY = rOriginY + ly
                        let depth = float f2
                        let tcX = (float pxX + 0.5) / sX
                        let tcY = (float pxY + 0.5) / sY
                        let ndc = V3d(2.0 * tcX - 1.0, 1.0 - 2.0 * tcY, depth)
                        pBwd.TransformPosProj ndc      // NDC → VIEW
                    else
                        V3d(float f1, float f2, float f3)

                // Per-scope snap-radius-squared cache. Forces `PixelSnapRadius`
                // (an `aval<int>`) once per scope per Read instead of per
                // (offset × scope).
                let snapR2Cache = Dict<TraversalState, int>()
                let inline snapR2 (state : TraversalState) =
                    let mutable r = 0
                    if snapR2Cache.TryGetValue(state, &r) then r
                    else
                        let r0 = min PickSnap.radius (max 0 (AVal.force state.PixelSnapRadius))
                        let r2 = r0 * r0
                        snapR2Cache.[state] <- r2
                        r2

                // Spiral state — all mutable, no per-iter allocations.
                let mutable winnerOffX = System.Int32.MinValue
                let mutable winnerOffY = 0
                let mutable winnerIsPixel = false
                let mutable winnerScope : TraversalState = Unchecked.defaultof<_>
                let mutable winnerVp = V3d.Zero
                let mutable winnerN  = V3d.Zero
                let mutable winnerPi = 0
                let mutable winnerHoverIdValue = 0

                let inline reuseNormalIfMissing (lx : int) (ly : int) (idF : int) (centerVP : V3d) (encodedNormal : V3d) =
                    if encodedNormal.X <> 0.0 || encodedNormal.Y <> 0.0 || encodedNormal.Z <> 0.0 then
                        encodedNormal
                    else
                        // Walk the 3×3 neighbourhood again, this time gathering
                        // view-space positions, and take the cross-product of
                        // the two same-id tangent vectors with largest area.
                        let mutable bestArea = 0.0
                        let mutable bestN = V3d.Zero
                        // up to 8 neighbours; small enough to keep on the stack
                        let n0 = Array.zeroCreate<V3d> 8
                        let mutable count = 0
                        for ddy in -1 .. 1 do
                            for ddx in -1 .. 1 do
                                if (ddx <> 0 || ddy <> 0) && readIdAt (lx + ddx) (ly + ddy) = idF then
                                    n0.[count] <- viewPosAt (lx + ddx) (ly + ddy) idF
                                    count <- count + 1
                        for i in 0 .. count - 2 do
                            let vi = n0.[i] - centerVP
                            for j in i + 1 .. count - 1 do
                                let n = Vec.cross vi (n0.[j] - centerVP)
                                let m = n.Length
                                if m > bestArea then
                                    bestArea <- m
                                    bestN <- n / m
                        // Aardvark's right-handed view space has the camera at
                        // the origin looking down -Z, so a *visible* surface's
                        // normal points toward +Z. Flip if the cross-product
                        // ended up pointing away from the camera.
                        if bestArea = 0.0 then V3d.Zero
                        elif bestN.Z < 0.0 then -bestN
                        else bestN

                let cullCount = cullSet.Length
                let offsets = PickSnap.offsets
                let mutable idx = 0
                while winnerOffX = System.Int32.MinValue && idx < offsets.Length do
                    let off = offsets.[idx]
                    let d2 = off.X * off.X + off.Y * off.Y
                    let pxX = pixel.X + off.X
                    let pxY = pixel.Y + off.Y
                    let lx  = pxX - rOriginX
                    let ly  = pxY - rOriginY

                    // ---- Pixel candidate ----
                    let pixIdRaw = readIdAt lx ly
                    let mutable pixOk = false
                    let mutable pixId = 0
                    let mutable pixScope : TraversalState = Unchecked.defaultof<_>
                    let mutable pixDepth = 0.0
                    let mutable pixVp = V3d.Zero
                    let mutable pixN  = V3d.Zero
                    let mutable pixPi = 0

                    if pixIdRaw <> 0 then
                        let absId = if pixIdRaw < 0 then -pixIdRaw else pixIdRaw
                        // capture hover even if scope mapping fails
                        if winnerHoverIdValue = 0 && off.X = 0 && off.Y = 0 then
                            winnerHoverIdValue <- absId
                        let mutable scope = Unchecked.defaultof<_>
                        if scopes.TryGetValue(absId, &scope) && d2 <= snapR2 scope then
                            // Validate by ≥ 2 same-id neighbours in the 3×3 block.
                            let mutable matches = 0
                            let mutable ddy = -1
                            while matches < 2 && ddy <= 1 do
                                let mutable ddx = -1
                                while matches < 2 && ddx <= 1 do
                                    if (ddx <> 0 || ddy <> 0) && readIdAt (lx + ddx) (ly + ddy) = pixIdRaw then
                                        matches <- matches + 1
                                    ddx <- ddx + 1
                                ddy <- ddy + 1
                            // After confirming validity, walk the rest of the
                            // neighbourhood is unnecessary for validation; we
                            // only re-walk it inside `reuseNormalIfMissing`
                            // which runs once on the winner.
                            if matches >= 2 then
                                let i0 = rBase + lx * rDx + ly * rDy
                                let f1 = rData.[i0 + rDz]
                                let f2 = rData.[i0 + 2 * rDz]
                                let f3 = rData.[i0 + 3 * rDz]
                                if pixIdRaw > 0 then
                                    pixDepth <- float f2
                                    let tcX = (float pxX + 0.5) / sX
                                    let tcY = (float pxY + 0.5) / sY
                                    let ndc = V3d(2.0 * tcX - 1.0, 1.0 - 2.0 * tcY, pixDepth)
                                    pixVp <- pBwd.TransformPosProj ndc      // NDC → VIEW
                                    pixN  <- if int f1 = 0 then V3d.Zero else V3d (Normal24.decode (int f1))
                                    pixPi <- int f3
                                else
                                    pixVp <- V3d(float f1, float f2, float f3)
                                    pixDepth <- vpFwd.TransformPosProj(pixVp).Z
                                    pixN <- V3d.Zero
                                    pixPi <- 0
                                pixId <- absId
                                pixScope <- scope
                                pixOk <- true

                    // ---- BVH candidate ----
                    let mutable bvhOk = false
                    let mutable bvhScope : TraversalState = Unchecked.defaultof<_>
                    let mutable bvhDepth = 0.0
                    let mutable bvhVp = V3d.Zero
                    let mutable bvhN  = V3d.Zero
                    if cullCount > 0 then
                        let ray = rayFor pxX pxY
                        let mutable bestT = System.Double.PositiveInfinity
                        let mutable k = 0
                        while k < cullCount do
                            let entry = cullSet.[k]
                            if AVal.force entry.State.Active && d2 <= snapR2 entry.State then
                                let mutable rt  = 0.0
                                let mutable hit = V3d.Zero
                                let mutable n   = V3d.Zero
                                let trafo = entry.Trafo
                                let localRay = ray.Transformed(trafo.Backward)
                                if entry.Intersectable.Intersects(localRay, 0.0, bestT, &rt, &hit, &n) then
                                    bestT <- rt
                                    let worldPoint = trafo.Forward.TransformPos hit
                                    let viewPoint  = vFwd.TransformPos worldPoint
                                    bvhVp    <- viewPoint
                                    bvhDepth <- vpFwd.TransformPosProj(worldPoint).Z
                                    bvhN     <- Vec.normalize (vBwd.TransposedTransformDir (trafo.Backward.TransposedTransformDir n))
                                    bvhScope <- entry.State
                                    bvhOk    <- true
                            k <- k + 1

                    // ---- Decide winner for this offset ----
                    let pickPixel = pixOk && (not bvhOk || pixDepth <= bvhDepth)
                    if pickPixel then
                        winnerOffX    <- off.X
                        winnerOffY    <- off.Y
                        winnerIsPixel <- true
                        winnerScope   <- pixScope
                        winnerVp      <- pixVp
                        winnerN       <- if pixN = V3d.Zero then reuseNormalIfMissing lx ly pixIdRaw pixVp pixN else pixN
                        winnerPi      <- pixPi
                        winnerHoverIdValue <- pixId
                    elif bvhOk then
                        winnerOffX    <- off.X
                        winnerOffY    <- off.Y
                        winnerIsPixel <- false
                        winnerScope   <- bvhScope
                        winnerVp      <- bvhVp
                        winnerN       <- bvhN
                        winnerPi      <- 0
                    else
                        idx <- idx + 1

                transact (fun () ->
                    hoverId.Value <- if winnerIsPixel then winnerHoverIdValue else 0
                )

                if winnerOffX = System.Int32.MinValue then
                    None
                else
                    let scope = winnerScope
                    let viewPos = winnerVp
                    let viewNormal = winnerN
                    let partIndex = winnerPi
                    if scope.PickThrough then
                        if winnerIsPixel then
                            Log.warn "cannot pick-through pixel-picked objects"
                            Some (scope, viewPos, viewNormal, partIndex, None)
                        else
                            // PickThrough rerun against the same offset's ray,
                            // skipping pick-through scopes. Inline the same loop
                            // as bvhAt(true) to avoid a second closure.
                            let pxX = pixel.X + winnerOffX
                            let pxY = pixel.Y + winnerOffY
                            let ray = rayFor pxX pxY
                            let mutable bestT = System.Double.PositiveInfinity
                            let mutable nFound = false
                            let mutable nScope : TraversalState = Unchecked.defaultof<_>
                            let mutable nViewPos = V3d.Zero
                            let mutable nViewNormal = V3d.Zero
                            let mutable k = 0
                            while k < cullCount do
                                let entry = cullSet.[k]
                                if AVal.force entry.State.Active && not entry.State.PickThrough then
                                    let mutable rt = 0.0
                                    let mutable hit = V3d.Zero
                                    let mutable n = V3d.Zero
                                    let trafo = entry.Trafo
                                    let localRay = ray.Transformed(trafo.Backward)
                                    if entry.Intersectable.Intersects(localRay, 0.0, bestT, &rt, &hit, &n) then
                                        bestT <- rt
                                        let worldPoint = trafo.Forward.TransformPos hit
                                        nViewPos    <- vFwd.TransformPos worldPoint
                                        nViewNormal <- Vec.normalize (vBwd.TransposedTransformDir (trafo.Backward.TransposedTransformDir n))
                                        nScope      <- entry.State
                                        nFound      <- true
                                k <- k + 1

                            if nFound then
                                let rec hasEventHandler (kind : SceneEventKind) (scope : TraversalState) =
                                    HashMap.containsKey kind (AMap.force scope.EventHandlers) ||
                                    (Option.isSome scope.Parent && hasEventHandler kind (Option.get scope.Parent))

                                if hasEventHandler kind scope then
                                    Some (scope, nViewPos, nViewNormal, 0, Some nScope)
                                else
                                    Some (nScope, nViewPos, nViewNormal, 0, None)
                            else
                                Some (scope, viewPos, viewNormal, partIndex, None)
                    else
                        Some (scope, viewPos, viewNormal, partIndex, None)
            else
                None

        let capturedResult =
            match capturedScope with
            | None ->
                match result with
                | Some (state, viewPos, normal, partIndex, nextScope) ->
                    let target =
                        match nextScope with
                        | Some n -> n
                        | None -> state
                    Some (state, target :> obj, viewPos, normal, partIndex)
                | None ->
                    None
            | Some c ->
                match result with
                | Some (state, viewPos, normal, partIndex, _nextScope) -> Some (c, state :> obj, viewPos, normal, partIndex)
                | None -> Some (c, null, V3d(0.0, 0.0, -1000000000.0), V3d.Zero, 0)

        lastMouseInfo <- Some (pixel, capturedResult)
        lastRealMouseInfo <- Some (pixel, result, evt)

        capturedResult

    member x.DispatchPointerEvent(target : obj, evt : ScenePointerEvent) =
        if isNull target then
            match evt.Kind with
            | SceneEventKind.Click when Option.isSome lastFocus.Value ->
                let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, evt.ViewTrafo, evt.ProjTrafo, evt.Pixel, evt.ViewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)
                let evt = ScenePointerEvent(x, lastFocus.Value.Value, null, evt.Kind, loc, evt.Original)
                TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
            | _ -> ()


            if Option.isSome lastOver.Value then
                let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, evt.ViewTrafo, evt.ProjTrafo, evt.Pixel, evt.ViewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)
                let evt = ScenePointerEvent(x, lastOver.Value.Value, null, evt.Kind, loc, evt.Original)
                TraversalState.handleMove lastOver evt None

            true
        else
            let view = evt.ViewTrafo
            let proj = evt.ProjTrafo
            let best = target :?> TraversalState
            let viewPos = evt.ViewPosition
            let viewNormal = evt.ViewNormal
            let partIndex = evt.PartIndex
            let model = TraversalState.modelTrafo best
            let loc = SceneEventLocation(model, view, proj, evt.Pixel, evt.ViewportSize, viewPos, viewNormal, partIndex)
            let evt = ScenePointerEvent(x, best, target, evt.Kind, loc, evt.Original)

            TraversalState.handleMove lastOver evt (Some best)

            let rec findFocusable (state : TraversalState) =
                if state.CanFocus then
                    Some state
                else
                    match state.Parent with
                    | Some p -> findFocusable p
                    | None -> None


            match evt.Kind with
            | SceneEventKind.Click ->
                match findFocusable best with
                | Some f -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt (Some f)
                | None -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
            | _ ->
                ()

            TraversalState.handleEvent true evt best
    
    member x.HandlePointerEvent(kind : SceneEventKind, original : PointerEvent) : bool =
        let s = viewportSize
        if s.AllGreater 0 then
            let view = AVal.force view 
            let proj = AVal.force proj
            let scrollDelta = V2d.Zero

            let pixel = original.ClientPosition - V2i original.ClientRect.Min

            match x.Read(pixel, original.PointerId, kind, original) with
            | Some (best, target, viewPos, viewNormal, partIndex) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, viewPos, viewNormal, partIndex)
                let evt = ScenePointerEvent(x, best, target, kind, loc, original)

                TraversalState.handleMove lastOver evt (Some best)

                let rec findFocusable (state : TraversalState) =
                    if state.CanFocus then
                        Some state
                    else
                        match state.Parent with
                        | Some p -> findFocusable p
                        | None -> None


                match kind with
                | SceneEventKind.Click ->
                    match findFocusable best with
                    | Some f -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt (Some f)
                    | None -> TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
                | _ ->
                    ()

                TraversalState.handleEvent true evt best
            | None ->
                match kind with
                | SceneEventKind.Click when Option.isSome lastFocus.Value ->
                    let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d pixel, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)
                    let evt = ScenePointerEvent(x, lastFocus.Value.Value, null, kind, loc, original)
                    TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt None
                | _ -> ()


                if Option.isSome lastOver.Value then
                    let loc = SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d pixel, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)
                    let evt = ScenePointerEvent(x, lastOver.Value.Value, null, kind, loc, original)
                    TraversalState.handleMove lastOver evt None

                true
        else
            true
        
    member x.HandleTapEvent(kind : SceneEventKind, original : TapEvent) : bool =
        let s = viewportSize
        if s.AllGreater 0 then
            let view = AVal.force view 
            let proj = AVal.force proj

            let pixel = original.ClientPosition - V2i original.ClientRect.Min

            match x.Read(pixel, original.PointerId, kind) with
            | Some (best, target, viewPos, viewNormal, partIndex) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, viewPos, viewNormal, partIndex)
                let evt = SceneTapEvent(x, best, target, kind, loc, original)
                TraversalState.handleEvent true evt best
            | None ->
                true
        else
            true
        
    member x.HandleWheelEvent(kind : SceneEventKind, original : WheelEvent) : bool =
        let s = viewportSize
        if s.AllGreater 0 then
            let view = AVal.force view 
            let proj = AVal.force proj

            let pixel = original.ClientPosition - V2i original.ClientRect.Min

            match x.Read(pixel, -1, kind) with
            | Some (best, target, viewPos, viewNormal, partIndex) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, viewPos, viewNormal, partIndex)
                let evt = SceneWheelEvent(x, best, target, kind, loc, original)
                TraversalState.handleEvent true evt best
            | None ->
                true
        else
            true
        
    member x.HandleKeyEvent(kind : SceneEventKind, original : KeyboardEvent) : bool =
        let s = viewportSize

        match lastFocus.Value with
        | Some best ->
            let model = TraversalState.modelTrafo best
            let view = AVal.force view 
            let proj = AVal.force proj
            let evtLocation =
                match lastMouseInfo with
                | Some (px, Some (_, _, viewPos, viewNormal, partIndex)) ->  SceneEventLocation(model, view, proj, V2d px, s, viewPos, viewNormal, partIndex)
                | _ ->
                    match lastMousePosition with
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)

            let evt = SceneKeyboardEvent(x, best, best, kind, evtLocation, original)
            TraversalState.handleEvent true evt best

        | None ->
            true
                
    member x.HandleInputEvent(kind : SceneEventKind, original : InputEvent) : bool =
        let s = viewportSize

        match lastFocus.Value with
        | Some best ->
            let model = TraversalState.modelTrafo best
            let view = AVal.force view 
            let proj = AVal.force proj
            let evtLocation =
                match lastMouseInfo with
                | Some (px, Some (_, _, viewPos, viewNormal, partIndex)) ->  SceneEventLocation(model, view, proj, V2d px, s, viewPos, viewNormal, partIndex)
                | _ ->
                    match lastMousePosition with
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, s, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)

            let evt = SceneInputEvent(x, best, best, kind, evtLocation, original)
            TraversalState.handleEvent true evt best

        | None ->
            true

    member x.SetFocus(newTarget : option<TraversalState>) =
        if lastFocus.Value <> newTarget then
            let view = AVal.force view 
            let proj = AVal.force proj

            let model =
                match newTarget with
                | Some t -> TraversalState.modelTrafo t
                | None -> AVal.constant Trafo3d.Identity


            let evtLocation =
                match lastMouseInfo with
                | Some (px, Some (_, _, viewPos, viewNormal, partIndex)) ->  SceneEventLocation(model, view, proj, V2d px, viewportSize, viewPos, viewNormal, partIndex)
                | _ ->
                    match lastMousePosition with
                    | Some px -> SceneEventLocation(model, view, proj, V2d px, viewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)
                    | None -> SceneEventLocation(model, view, proj, V2d.NN, viewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0)

            let target =
                match newTarget with
                | Some s -> s :> obj
                | None -> null

            let original = Event(None, "", 0.0, true, "focusout", Box2d.Invalid)
            let evt = PlainSceneEvent(x, target, target, SceneEventKind.FocusLeave, evtLocation, original)
            TraversalState.handleDifferential lastFocus SceneEventKind.FocusEnter SceneEventKind.FocusLeave evt newTarget
                
    interface IEventHandler with
        member x.Read(pixel : V2i, kind : SceneEventKind) =
            let s = viewportSize
            let view = AVal.force view 
            let proj = AVal.force proj
            match x.Read(pixel, -1, kind) with
            | Some (best, _, viewPos, viewNormal, partIndex) ->
                let model = TraversalState.modelTrafo best
                let loc = SceneEventLocation(model, view, proj, V2d pixel, s, viewPos, viewNormal, partIndex)
                Some loc
            | None ->
                None
            
        member x.Size = viewportSize
            
        member x.SetFocus(dst : option<obj>) =
            match dst with
            | Some (:? TraversalState as s) -> x.SetFocus(Some s)
            | _ -> x.SetFocus None
            
        member x.DispatchPointerEvent(target : obj, e : ScenePointerEvent) =
            x.DispatchPointerEvent(target, e)
            
        member x.HandlePointerEvent(kind : SceneEventKind, original : PointerEvent) =
            x.HandlePointerEvent(kind, original)
              
        member x.HandleKeyEvent(kind : SceneEventKind, original : KeyboardEvent) =
            x.HandleKeyEvent(kind, original)
   
        member x.HandleInputEvent(kind : SceneEventKind, original : InputEvent) =
            x.HandleInputEvent(kind, original)
   
        member x.HandleTapEvent(kind : SceneEventKind, original : TapEvent) =
            x.HandleTapEvent(kind, original)
            
        member x.HandleWheelEvent(kind : SceneEventKind, original : WheelEvent) =
            x.HandleWheelEvent(kind, original)
   
        member x.HasPointerCapture(state : obj, pointerId : int) =
            match capturedScopes.TryGetValue pointerId with
            | (true, s) -> s :> obj = state
            | _ -> false

        member x.SetPointerCapture(state : obj, pointerId : int) =
            let state = state :?> TraversalState
            capturedScopes.[pointerId] <- state
            
        member x.ReleasePointerCapture(state : obj, pointerId : int) =
            capturedScopes.Remove pointerId |> ignore
            match lastRealMouseInfo with
            | Some (px, scope, originalEvt) ->
                let view = AVal.force view 
                let proj = AVal.force proj
                let loc, target = 
                    match scope with
                    | Some (scope, viewPos, viewNormal, partIndex, _) -> SceneEventLocation(TraversalState.modelTrafo scope, view, proj, V2d px, viewportSize, viewPos, viewNormal, partIndex), Some scope
                    | None -> SceneEventLocation(AVal.constant Trafo3d.Identity, view, proj, V2d px, viewportSize, V3d(0.0, 0.0, -100000000.0), V3d.Zero, 0), None

                let eventTarget =
                    match target with
                    | Some t -> t :> obj
                    | None -> null

                let original = 
                    match originalEvt with
                    | Some e -> e
                    | None -> PointerEvent(None, "", 0.0, true, "pointermove", Box2d.Invalid, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, false, false, false, false, Button.None, Buttons.None, -1, 0.0, 0.0, 0.0, 0.0, 0.0, PointerType.Mouse)

                let evt = ScenePointerEvent(x, eventTarget, eventTarget, SceneEventKind.PointerMove, loc, original)
                TraversalState.handleMove lastOver evt target
                match target with
                | Some t -> TraversalState.handleEvent true evt t |> ignore
                | None -> ()
            | None ->
                ()

            
        member x.Cursor = x.Cursor