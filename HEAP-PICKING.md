# Heap picking (model B) — implementation tracker

Goal: make `HeapNode` (the GPU-heap scene node) pickable through dom's normal
`Sg.OnTap`/`OnEnter`/… without per-object pick objects (O(1) pick, not O(N)).

## Design (settled)
- dom-side `HeapNode` brokers picking (heap is BELOW dom, can't name `TraversalState`).
- Per input RO: register its `TraversalState` with the SceneHandler → a dom-sourced
  pick `id` (`IPickContext.Register`, carried on `TraversalState.PickContext`).
- The heap stores a **per-slot `HeapPickId : int`** (a per-slot MirrorBuffer, the
  `HeapMeshlets` pattern — NOT the interned field machinery) and exposes it as a
  uniform read by the pick shader via `gl_InstanceIndex`.
- dom composes a **heap pick-shader variant** that writes `HeapPickId[slot]` into the
  pick buffer's PickId slot. `HeapNode` drops the per-object picks.
- Dispatch is free: `Register`=`acquireId` already populates `scopes` (id→TraversalState).
- FIRST CUT: all-pickable (skip the pickability mode-key split / NoEvents); add the
  split as a refinement once the basic path renders + resolves.

## Status
- [x] dom: `IPickContext` interface + `PickContext` field on `TraversalState`, set on root
- [x] dom: SceneHandler registry (`acquireId`/`releaseId`-backed `pickContext`)
- [x] dom: `HeapNode` moved demo→dom (`SceneGraph/HeapNode.fs`), paket bumped 0018→0026
- [ ] heap: `ofRenderObjects` gains optional per-RO pick-id param; per-slot `HeapPickId` buffer + uniform
- [ ] heap: `HeapRenderObject.IsPickable` (+ pickability mode-key split — refinement)
- [ ] dom: heap pick-shader variant (reads `HeapPickId[gl_InstanceIndex]`)
- [ ] dom: `HeapNode.GetObjects` registers children, passes ids, drops picks
- [ ] demo: use dom's `heap` (local-ref dom+heap to test), verify pick resolves

## Log
- (start) foundation + move landed & compiling; beginning heap-side per-slot id buffer.
- mapped heap internals: `slotPage` per-slot int[] + `slotPageBuf` MirrorBuffer + `HeapSlotPage` uniform is the exact pattern to clone as `pickIds`/`pickIdBuf`/`HeapPickId`. Threading point = the per-slot ingest where slotPage.[slot] is set.
- heap side DONE: per-slot `pickIds` int[] + `pickIdBuf` MirrorBuffer + `HeapPickIds` SSBO + `HeapPickIds` shader accessor, ALL gated on a new `picking:bool` ctor flag. `ofRenderObjects` signature unchanged; added `ofRenderObjectsPicking` (=`ofRenderObjectsCore true`). AddInternal captures the per-RO "HeapPickId" uniform → pickIds[slot] only when picking. NoEvents gate goes in HeapNode (state.PixelPick).
- ✅ HEAP CORE COMPILES (Aardvark.SceneGraph builds): per-slot pick-id buffer + picking gate + ofRenderObjectsPicking. Riskiest part done.

## Next (precise resume)
1. **Heap pick-id varying** (HeapPool.fs:3553-3557, `picking` true): compose a vertex effect that
   outputs `[<Semantic("HeapPickIdV"); Interpolation Flat>] int = uniform.HeapPickIds.[%slotE]` into
   `baseE`. MUST use the bucket's `slotE` (2406: `getDrawId()` on GL / gl_InstanceIndex on Vulkan) —
   a naive `[<InstanceId>]` may mismatch the heap handle (baseInstance) → wrong ids. Build via the
   heap's Expr style (cf. `rewrite`, `bindlessGatherFlat`). slotE is in scope at 3554.
2. **dom heap pick-fragment** (SceneHandler PickShader): like `pickFinalANoPi` but
   `id = V4f(float32 v.heapPickId, n24, d, 0)` reading the `HeapPickIdV` flat varying instead of
   `uniform.PickId`. Compose chain = [vsn?; pickDepthBefore; userEffect; heapPickFinal].
3. **HeapNode.GetObjects** (aardvark.dom/SceneGraph/HeapNode.fs): 
   `let picking = state.PickContext.IsSome && state.PixelPick` (NoEvents → false → plain path);
   if picking: per child render RO look up TraversalState via `RenderObject.traversalStates`, 
   `ctx.Register ts → id`, wrap the RO adding uniform `"HeapPickId" = AVal.constant id`, call
   `Heap.ofRenderObjectsPicking`, return (heapified, ASet.empty) [drop per-object picks]; 
   else `Heap.ofRenderObjects`, return (heapified, picks). Deregister on aset Rem.
4. demo: use dom `heap`, local-ref dom+heap, verify pick resolves to the right part.

## Status (checkpoint)
- ✅ dom foundation (IPickContext/PickContext/registry) — compiles
- ✅ HeapNode moved to dom, dom bumped 0026 — compiles
- ✅ heap per-slot HeapPickId buffer + picking gate + ofRenderObjectsPicking — compiles (riskiest part DONE)
- ⏳ pick shader (1+2), HeapNode wiring (3), demo (4)
- heap HeapPickId rewrite rule added (heapRewrite, gated): `uniform.HeapPickId` → `HeapPickIds[slotE]` by name. Heap builds. Next: dom pickFinalHeap fragment + HeapNode wiring (incl pick-attachment in DCE sig).
- dom pickFinalHeap fragment + effect added; HeapNode.fs moved after SceneHandler (PickShader dep); HeapNode.GetObjects picking logic written (register+compose chain+HeapPickId uniform+pickSignature+drop picks, gated on PickContext+PixelPick). NOTE: deregistration not yet wired (ids leak on re-traversal; fine for static).

## ✅ ALL LAYERS COMPILE END-TO-END (2026-06-28)
dom (TraversalState/SceneHandler/HeapNode/pickFinalHeap) builds against the local heap
(per-slot HeapPickId buffer + heapRewrite rule + ofRenderObjectsPicking). Implementation
code-complete. Remaining: (a) publish heap 0027 → bump dom → drop the dev ProjectReference;
(b) demo: use dom `heap`, add an Sg.OnTap handler, click-test that pick resolves to the right part;
(c) wire deregistration (currently ids leak on re-traversal — fine for static scenes).
- ✅ DEMO BUILDS against local picking chain (dom heap + Sg.OnTap pick handler). Ready for click-test.

## ✅✅ WORKS END-TO-END (click-test passed, 2026-06-28)
Clicking buildings in the live demo logs distinct world hits:
  [pick] tap world=[227.45,-56.06,-43.85]   (one building)
  [pick] tap world=[263.92,-64.60,-43.84]   (another)
11,654 parts → 1 heap draw, pick still resolves to the exact part via normal Sg.OnTap (O(1)).

Three bugs beyond the core impl:
1. wrapObject dropped the HeapRenderObject bundle to non-pickable (no TraversalState) → never
   entered the PickId pass. FIX: HeapRenderObject.IsPickable (set in ofRenderObjectsPicking),
   wrapObject routes a pickable bundle into the pickable set.
2. The resolver gates on pickModes (mode A/B sign tag); the heap's ctx.Register→acquireId path
   set scopes but not pickModes → modeOk=false → every heap pick rejected. FIX: IPickContext.Register
   sets pickModes.[id] <- true (heap is mode A).
3. Runtime dll shadowing: package Aardvark.Rendering/SceneGraph (net8.0) shadowed the local
   netstandard2.0 builds → MissingMethod/no-IsPickable. Dev-fix: copy fresh local dlls into demo bin.

REMAINING (cleanup, not correctness): publish heap 0027 + dom → drop dev ProjectReferences + dll-copy.
