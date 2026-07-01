module PickChainTests

open System
open NUnit.Framework
open FsUnit
open Aardvark.Base
open FShade
open Aardvark.Dom

// ----------------------------------------------------------------------------
// Test fixtures: synthetic user effects exercising every relevant combination
// of pick-related fragment outputs and vertex-attribute requirements.
// ----------------------------------------------------------------------------

type V_Std =
    {
        [<Position>] pos    : V4f
        [<Color>]    color  : V4f
    }

type V_Normalled =
    {
        [<Position>]                pos    : V4f
        [<Color>]                   color  : V4f
        [<Semantic("Normals")>]     n      : V3f
    }

type V_Instanced =
    {
        [<Position>]                pos    : V4f
        [<Color>]                   color  : V4f
        [<Semantic("Normals")>]     n      : V3f
        [<InstanceId>]              iid    : int
    }

/// Plain user effect: writes Color, no pick-specific outputs.
let private plainShader (v : V_Std) =
    fragment { return { v with color = v.color } }

/// User effect that produces PickPartIndex from gl_InstanceID.
type FragWithPi =
    {
        [<Color>] color : V4f
        [<Semantic("PickPartIndex")>] pi : int
    }
let private piShader (v : V_Instanced) =
    fragment {
        return { color = v.color; pi = v.iid }
    }

/// User effect that produces PickViewPosition (forces mode B).
type FragWithPvp =
    {
        [<Color>] color : V4f
        [<Semantic("PickViewPosition")>] pvp : V3f
    }
let private pvpShader (v : V_Normalled) =
    fragment {
        return { color = v.color; pvp = v.pos.XYZ }
    }

/// User effect that produces ViewSpaceNormal directly (per-pixel pick normal).
type FragWithVsn =
    {
        [<Color>] color : V4f
        [<Semantic("ViewSpaceNormal")>] vsn : V3f
    }
let private vsnShader (v : V_Normalled) =
    fragment {
        return { color = v.color; vsn = Vec.normalize v.n }
    }

// ----------------------------------------------------------------------------
// helpers
// ----------------------------------------------------------------------------

/// Geometry callback: only the listed semantics are reported as available.
let private geomWith (attrs : seq<string>) =
    let s = Set.ofSeq attrs
    fun (sem : string) -> Set.contains sem s

/// Run the chain-selection logic for an effect + geometry attribute set,
/// returning the variant tag and whether the synthetic vsn vertex effect is
/// injected. Pure — no rendering, no `Effect.toModule`.
let private choose (eff : Effect) (attrs : seq<string>) =
    PickShader.chooseChain eff (geomWith attrs)

// ----------------------------------------------------------------------------
// tests
// ----------------------------------------------------------------------------

[<Test>]
let ``plain effect, geometry has Normals → mode A with-normal no-pi, vsn injected``() =
    let eff = Effect.ofFunction plainShader
    let c = choose eff ["Positions"; "Colors"; "Normals"]
    c.Final |> should equal PickShader.FinalANoPi
    c.InjectVsn |> should equal true

[<Test>]
let ``plain effect, geometry has no Normals → no-normal no-pi, no vsn injection``() =
    let eff = Effect.ofFunction plainShader
    let c = choose eff ["Positions"; "Colors"]
    c.Final |> should equal PickShader.FinalANoNormalNoPi
    c.InjectVsn |> should equal false

[<Test>]
let ``user pi-shader + Normals → mode A with-normal with-pi, vsn injected``() =
    let eff = Effect.ofFunction piShader
    let c = choose eff ["Positions"; "Colors"; "Normals"; "InstanceId"]
    c.Final |> should equal PickShader.FinalA
    c.InjectVsn |> should equal true

[<Test>]
let ``user pi-shader without Normals → no-normal with-pi``() =
    let eff = Effect.ofFunction piShader
    let c = choose eff ["Positions"; "Colors"; "InstanceId"]
    c.Final |> should equal PickShader.FinalANoNormal
    c.InjectVsn |> should equal false

[<Test>]
let ``user pvp-shader → mode B (regardless of pi support)``() =
    let eff = Effect.ofFunction pvpShader
    let c = choose eff ["Positions"; "Colors"; "Normals"]
    c.Final |> should equal PickShader.FinalB
    // Mode B still benefits from synthesised vsn for the encoded-normal slot.
    c.InjectVsn |> should equal true

[<Test>]
let ``user vsn-shader → no vsn injection (path 1 — user provides vsn)``() =
    let eff = Effect.ofFunction vsnShader
    let c = choose eff ["Positions"; "Colors"; "Normals"]
    // user provides vsn → we don't inject; final is no-pi (user didn't output PickPartIndex)
    c.Final |> should equal PickShader.FinalANoPi
    c.InjectVsn |> should equal false

[<Test>]
let ``geometry missing pi-required attribute → graceful fallback to no-pi``() =
    // piShader needs InstanceId. If geometry lacks it, pi is unsupplied →
    // canEffProduce("PickPartIndex") returns false → no-pi variant is selected.
    let eff = Effect.ofFunction piShader
    let c = choose eff ["Positions"; "Colors"; "Normals"]   // deliberately no InstanceId
    c.Final |> should equal PickShader.FinalANoPi
    c.InjectVsn |> should equal true

[<Test>]
let ``Positions0 (FShade frag-input renaming) treated as Positions``() =
    // When the user fragment reads v.pos, FShade renames Position to
    // Positions0 in the deps map. Geometry only ever exposes "Positions".
    // canonicalSemantic must collapse Positions0 → Positions for the
    // availability check, otherwise pvp shaders that read pos would
    // wrongly fall back to no-pvp (mode A).
    let pvpReadingPos (v : V_Normalled) =
        fragment { return ({ color = v.color; pvp = v.pos.XYZ } : FragWithPvp) }
    let eff = Effect.ofFunction pvpReadingPos
    let c = choose eff ["Positions"; "Colors"; "Normals"]   // no Positions0 in attrs
    c.Final |> should equal PickShader.FinalB

[<Test>]
let ``vsn-shader without Normals at all → no-normal no-pi``() =
    // vsnShader's vsn output reads Normals (n.Normalized). If geometry has
    // no Normals, canEffProduce("ViewSpaceNormal") fails AND canSynthesise
    // is false. No carryable normal → no-normal variant.
    let eff = Effect.ofFunction vsnShader
    let c = choose eff ["Positions"; "Colors"]
    c.Final |> should equal PickShader.FinalANoNormalNoPi
    c.InjectVsn |> should equal false

[<Test>]
let ``composePickChain is consistent with chooseChain``() =
    // Smoke test: composing must emit a non-empty Effect with at least
    // a Color + PickId output.
    let eff = Effect.ofFunction piShader
    let composed = PickShader.composePickChain eff (geomWith ["Positions"; "Colors"; "Normals"; "InstanceId"])
    composed.Outputs |> Map.containsKey "Colors" |> should equal true
    composed.Outputs |> Map.containsKey "PickId" |> should equal true

// ----------------------------------------------------------------------------
// Step 2 invariant: the generalized per-semantic loop, requesting only
// "PickId", must reproduce EXACTLY the old inline chooseChain/finalEffect
// chain and pick-mode tag — for every fixture. Locks identical pick output.
// ----------------------------------------------------------------------------

/// The pre-generalization reference chain + mode tag, kept here verbatim so a
/// drift in `planChain` is caught against the original definition.
let private referenceChain (eff : Effect) (geomHas : string -> bool) =
    let choice = PickShader.chooseChain eff geomHas
    let pre = if choice.InjectVsn then [PickShader.viewSpaceNormalEffect] else []
    let chain = pre @ [PickShader.pickDepthBeforeEffect; eff; PickShader.finalEffect choice.Final]
    chain, (choice.Final <> PickShader.FinalB)

let private planChainCases : (string * Effect * string list) list =
    [ "plain+Normals",     Effect.ofFunction plainShader, ["Positions"; "Colors"; "Normals"]
      "plain",             Effect.ofFunction plainShader, ["Positions"; "Colors"]
      "pi+Normals+iid",    Effect.ofFunction piShader,    ["Positions"; "Colors"; "Normals"; "InstanceId"]
      "pi+iid",            Effect.ofFunction piShader,    ["Positions"; "Colors"; "InstanceId"]
      "pvp+Normals",       Effect.ofFunction pvpShader,   ["Positions"; "Colors"; "Normals"]
      "vsn+Normals",       Effect.ofFunction vsnShader,   ["Positions"; "Colors"; "Normals"]
      "vsn",               Effect.ofFunction vsnShader,   ["Positions"; "Colors"] ]

[<Test>]
let ``planChain [PickId] reproduces the reference chain and mode for every fixture``() =
    for (name, eff, attrs) in planChainCases do
        let geomHas = geomWith attrs
        let refChain, refModeA = referenceChain eff geomHas
        let chain, isModeA = PickShader.planChain ["PickId"] eff geomHas
        // same effect list, same order, same references
        chain.Length |> should equal refChain.Length
        List.zip chain refChain
        |> List.iter (fun (a, b) -> Object.ReferenceEquals(a, b) |> should equal true)
        // same pick-mode tag
        isModeA |> should equal (Some refModeA)
