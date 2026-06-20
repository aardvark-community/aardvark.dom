namespace Aardvark.Dom.Remote.SharedTexture

// R1 — the REAL aardvark.dom integration of the zero-copy texture path: a proper
// IImageTransfer modeled EXACTLY on SharedMemoryTransfer (../SharedMemory/SharedMemoryTransfer.fs),
// but mapping the shared-memory mechanism onto the validated zero-copy OPAQUE_FD ring:
//
//   SharedMemory (the reference)            SharedTexture (this file)
//   ----------------------------            ------------------------------------------
//   one shm mapping (host bytes)            a ring of N=3 OPAQUE_FD images (GPU memory)
//   realloc shm on size change              realloc ring + offscreen FBO on size change
//   task.Run -> FBO                         task.Run -> FBO   (SAME — the GIVEN scene)
//   downloader.Download FBO -> shm ptr      DmaBufGpu.recordCopyInto FBO color -> ring slot
//   message {name,size}                     message {id,size}   (id = textureId of the slot)
//   client: openMapping(name)+putImageData  client: openSharedTexture(id) composites the handle
//
// The genuinely-new transport is the fd SIDE-CHANNEL: on every (re)allocation of the
// ring, each slot's OPAQUE_FD memfd is handed to the browser's native layer ONCE via
// SCM_RIGHTS (FdHandoff), keyed by a textureId. The frame channel message then only
// names which textureId holds the freshly-rendered frame — the heavy buffer never
// travels over the channel. The browser's release callback sends `FREE <textureId>`
// back over the side-channel, recycling the slot (R2).
//
// ============================================================================
//  CLIENT CONTRACT  (R2 / the browser native layer implements to this)
// ============================================================================
//
//  (A) Frame channel message  (server -> client, existing IChannel, from RenderFrame):
//        JSON  { "id": "<textureId>", "size": { "width": W, "height": H } }
//      (direct analog of shm's { "name": ..., "size": ... }).
//
//  (B) Boot JS:  create an <aardvark-surface> element, append to __THIS__
//      (analog of shm creating a <canvas>).
//
//  (C) ClientCode JS (run per channel message):  parse { id, size } -> set the
//      surface size -> aardvark.openSharedTexture(msg.id) to composite that handle
//      (analog of shm's aardvark.openMapping(name) + ctx.putImageData).
//
//  (D) Shutdown JS:  release the surface.
//
//  (E) fd SIDE-CHANNEL  (unix-domain SCM_RIGHTS, FdHandoff):
//        - The SERVER is the socket CLIENT (FdHandoff.streamConnect); the browser
//          native layer is the socket SERVER (FdHandoff.streamServerAccept) listening
//          on the per-renderer path  /tmp/aardvark-sharedtexture-<channelName>.sock .
//          The channelName is passed to the client in Boot so it can bind the path.
//        - REGISTER (server -> browser), ONCE per slot per (re)allocation:
//            ASCII  "REG <textureId> <w> <h>\n"  + the slot's OPAQUE_FD memfd via SCM_RIGHTS.
//          The browser imports the memfd as an external VkImage / GL texture and stores
//          it under <textureId>; aardvark.openSharedTexture(<textureId>) resolves to it.
//          Image params: B8G8R8A8_UNORM, OPTIMAL tiling, GENERAL layout, released to
//          QUEUE_FAMILY_EXTERNAL (the producer re-releases each frame). w/h are in the msg.
//        - FREE (browser -> server), from the release callback:
//            ASCII  "FREE <textureId>\n"  (no fd).  -> calls target.MarkFree(textureId),
//          returning the slot to the free pool so the FIFO 2-in-flight cap can reuse it.
//
//  textureId format (this file):  "<gen>-<slot>"  (e.g. "3-1"), stable for the life of
//  an allocation generation; a new generation is minted on every resize-realloc.
//
// ============================================================================

open System
open System.Collections.Generic
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.Dom
open Aardvark.Dom.Remote
open Aardvark.Rendering.Vulkan
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

// One ring slot: its OPAQUE_FD image plus the persistent per-slot copy resources
// (command buffer + exportable signal semaphore + CPU fence) — allocated ONCE per
// (re)allocation, reused every frame (NO per-frame pool/semaphore churn, NO waitIdle),
// exactly the L4 opaquefd-stream producer pattern.
type private RingSlot =
    {
        TextureId : string
        Opaque    : OpaqueImage
        Cmd       : VkCommandBuffer
        Sem       : VkSemaphore
        Fence     : VkFence
        /// busy = a copy into this slot has been handed to the client and not yet FREEd.
        mutable Busy : bool
        /// true once this slot's memfd has been registered over the side-channel.
        mutable Registered : bool
    }

type private SharedTextureRenderTarget(runtime : IRuntime, signature : IFramebufferSignature, task : IRenderTask, size : aval<V2i>, channelName : string) =
    inherit AdaptiveObject()

    static let RING_N = 3            // ring depth
    // socket path the browser native layer binds; the server connects lazily.
    let sockPath = sprintf "/tmp/aardvark-sharedtexture-%s.sock" channelName

    let vk =
        match runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk -> vk
        | _ -> failwith "[SharedTexture] runtime is not Vulkan"
    let device = vk.Device
    let devH = device.Handle
    let qfi = uint32 device.GraphicsFamily.Index

    // one reset-capable command pool for all slot command buffers (created once).
    let pool =
        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.ResetCommandBufferBit, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(devH, &&poolInfo, NativePtr.zero, &&pool) |> ignore
        pool

    let waitFence (f : VkFence) =
        let mutable ff = f
        VkRaw.vkWaitForFences(devH, 1u, &&ff, 1u, UInt64.MaxValue) |> ignore
    let resetFence (f : VkFence) =
        let mutable ff = f
        VkRaw.vkResetFences(devH, 1u, &&ff) |> ignore
    // non-blocking: has this slot's last submitted copy finished on the GPU?
    let fenceSignaled (f : VkFence) =
        let mutable ff = f
        VkRaw.vkGetFenceStatus(devH, ff) = VkResult.Success

    let newCmd () =
        let mutable ai = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable c = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(devH, &&ai, &&c) |> ignore
        c
    let newFence () =
        let mutable fi = VkFenceCreateInfo(VkFenceCreateFlags.SignaledBit)
        let mutable f = Unchecked.defaultof<VkFence>
        VkRaw.vkCreateFence(devH, &&fi, NativePtr.zero, &&f) |> ignore
        f

    // ---- offscreen FBO (Bgra8 color + depth) — matches the ring's B8G8R8A8_UNORM ----
    let mutable fbo : option<IFramebuffer * IRenderbuffer * IRenderbuffer> = None

    let getFramebuffer (s : V2i) =
        match fbo with
        | Some (f, _, _) when f.Size = s -> f
        | _ ->
            match fbo with
            | Some (f, c, d) ->
                runtime.DeleteFramebuffer f
                runtime.DeleteRenderbuffer c
                runtime.DeleteRenderbuffer d
            | None -> ()
            let c = runtime.CreateRenderbuffer(s, TextureFormat.Bgra8, signature.Samples)
            let d = runtime.CreateRenderbuffer(s, TextureFormat.Depth24Stencil8, signature.Samples)
            let f =
                runtime.CreateFramebuffer(signature, [
                    DefaultSemantic.Colors, c :> IFramebufferOutput
                    DefaultSemantic.DepthStencil, d :> IFramebufferOutput
                ])
            fbo <- Some (f, c, d)
            f

    // ---- the ring of OPAQUE_FD slots ----
    let mutable ringSize = AVal.force size
    let mutable gen = 0
    let mutable ring : RingSlot[] = [||]
    let mutable frame = 0

    // ---- lazy side-channel connection to the browser native layer ----
    let mutable conn = -1                 // -1 until connected
    let mutable triedConnect = false

    let tryConnect () =
        if conn < 0 && not triedConnect then
            triedConnect <- true
            try conn <- FdHandoff.streamConnect sockPath
            with _ -> conn <- -1          // browser not up yet — fall back to round-robin
        conn >= 0

    let disconnect () =
        if conn >= 0 then (try FdHandoff.streamClose conn with _ -> ()); conn <- -1
        triedConnect <- false

    let destroySlot (s : RingSlot) =
        OpaqueFd.destroy device s.Opaque
        DmaBufSync.destroySemaphore device s.Sem
        VkRaw.vkDestroyFence(devH, s.Fence, NativePtr.zero)
        // command buffers freed with the pool at Dispose; nothing per-slot here.

    let allocRing (s : V2i) =
        // tear down the previous generation
        if ring.Length > 0 then
            VkRaw.vkDeviceWaitIdle devH |> ignore
            ring |> Array.iter destroySlot
        gen <- gen + 1
        frame <- 0
        ring <-
            Array.init RING_N (fun i ->
                {
                    TextureId  = sprintf "%d-%d" gen i
                    Opaque     = OpaqueFd.create device s.X s.Y
                    Cmd        = newCmd ()
                    Sem        = DmaBufSync.createExportableSemaphore device
                    Fence      = newFence ()
                    Busy       = false
                    Registered = false
                })

    // Register every slot's memfd with the browser over the side-channel ONCE per
    // (re)allocation. No-op (deferred) if the side-channel isn't connected yet — the
    // slots carry Registered=false and will be registered as soon as a connection exists.
    let registerRing () =
        if tryConnect () then
            for s in ring do
                if not s.Registered then
                    // REG <textureId> <w> <h>\n  + the slot's memfd via SCM_RIGHTS.
                    let ok = FdHandoff.streamFrameFd conn (sprintf "REG %s %d %d\n" s.TextureId s.Opaque.Width s.Opaque.Height) s.Opaque.MemFd
                    if ok then s.Registered <- true
                    else disconnect ()

    // Drain any FREE <textureId> messages the browser sent back; clear the slot's Busy.
    let pollFree () =
        if conn >= 0 then
            let txt = try FdHandoff.streamPoll conn with _ -> ""
            if txt <> "" then
                for line in txt.Split('\n') do
                    let parts = line.Trim().Split(' ')
                    if parts.Length >= 2 && parts.[0] = "FREE" then
                        let id = parts.[1]
                        match ring |> Array.tryFind (fun s -> s.TextureId = id) with
                        | Some s -> s.Busy <- false
                        | None -> ()

    // Pick the next FREE slot for this frame. FIFO 2-in-flight: prefer a slot whose
    // last copy is GPU-complete (fence signaled) AND not Busy (client still holding it).
    // If a release callback has populated Busy, this respects it; if no release has
    // arrived yet (R1 / browser absent), this degrades to round-robin — matching the
    // validated opaquefd-stream behavior.
    let pickSlot () =
        let i0 = frame % RING_N
        // try round-robin start, then scan for a non-busy + completed slot.
        let mutable chosen = -1
        let mutable k = 0
        while chosen < 0 && k < RING_N do
            let i = (i0 + k) % RING_N
            let s = ring.[i]
            if not s.Busy && fenceSignaled s.Fence then chosen <- i
            k <- k + 1
        // all busy / in-flight: fall back to round-robin slot and wait its fence (cap).
        if chosen < 0 then chosen <- i0
        chosen

    member x.Size = size

    /// Side-channel release entry point — the FREE <textureId> handler calls this
    /// (also drained inline in Run via pollFree). Returns the slot to the free pool.
    member x.MarkFree(textureId : string) =
        match ring |> Array.tryFind (fun s -> s.TextureId = textureId) with
        | Some s -> s.Busy <- false
        | None -> ()

    member x.Dispose() =
        VkRaw.vkDeviceWaitIdle devH |> ignore
        disconnect ()
        if ring.Length > 0 then ring |> Array.iter destroySlot
        ring <- [||]
        ringSize <- V2i.Zero
        match fbo with
        | Some (f, c, d) ->
            runtime.DeleteFramebuffer f
            runtime.DeleteRenderbuffer c
            runtime.DeleteRenderbuffer d
            fbo <- None
        | None -> ()
        VkRaw.vkDestroyCommandPool(devH, pool, NativePtr.zero)
        // the browser native layer owns/binds sockPath (it is the socket server); nothing to unlink here.

    /// Render the GIVEN scene task into the next free ring slot and return its frame
    /// descriptor (textureId, size). Render-when-dirty falls out of being an
    /// AdaptiveObject: EvaluateAlways re-runs whenever `task` or `size` are outdated.
    member x.Run(token : AdaptiveToken) : string * V2i =
        x.EvaluateAlways token (fun token ->
            let s = size.GetValue token

            // (re)allocate ring + FBO on size change (like shm reallocs its mapping).
            if ring.Length = 0 || ringSize <> s then
                ringSize <- s
                allocRing s
            registerRing ()       // (re)hand the memfds to the browser if/when connected
            pollFree ()           // recycle any slots the browser released

            let fbo = getFramebuffer s
            let i = pickSlot ()
            let slot = ring.[i]

            // ensure this slot's previous copy is done before reusing its cmd/sem/image.
            waitFence slot.Fence
            resetFence slot.Fence

            // render the GIVEN scene into the offscreen FBO.
            task.Run(token, RenderToken.Empty, fbo)

            // FBO color attachment -> the underlying Vulkan image (handle + current layout).
            let fbo = unbox<Framebuffer> fbo
            let img = fbo.Attachments.[DefaultSemantic.Colors].Image

            // GPU copy FBO color -> ring slot; per-slot fence (CPU) + semaphore (GPU acquire).
            DmaBufGpu.recordCopyInto device slot.Cmd img.Handle img.Layout slot.Opaque.Image s.X s.Y slot.Sem slot.Fence

            // mark the slot in-flight; the browser FREEs it after compositing.
            slot.Busy <- true
            frame <- frame + 1

            (slot.TextureId, s)
        )

    /// Self-test hook (R1 verification): read a named slot's content back via the
    /// validated producer-same-instance OPAQUE_FD readback. Returns BGRA bytes (w*h*4).
    member x.DebugReadSlot(textureId : string) : byte[] =
        match ring |> Array.tryFind (fun s -> s.TextureId = textureId) with
        | Some s ->
            waitFence s.Fence          // ensure the copy into this slot finished
            OpaqueFd.readAllLocal device s.Opaque
        | None -> [||]

    interface IDisposable with
        member x.Dispose() = x.Dispose()


// R1 SELF-TEST (server-side, no browser): build a trivial scene, drive a
// SharedTextureRenderTarget for a few frames, then read the just-written ring slot
// back via the validated OPAQUE_FD same-instance readback and confirm the rendered
// content landed. Proves the server half end-to-end independent of R2.
module SharedTextureSelfTest =
    open Aardvark.SceneGraph

    /// Returns true if the ring received the expected rendered content.
    let run (runtime : IRuntime) : bool =
        Aardvark.Base.IntrospectionProperties.CustomEntryAssembly <- System.Reflection.Assembly.GetAssembly(typeof<ISg>)
        let w, h = 256, 256
        let signature =
            runtime.CreateFramebufferSignature([
                DefaultSemantic.Colors, TextureFormat.Bgra8
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
            ])

        // a rotating shaded box on an opaque dark background — same content the
        // validation path used, so the readback assertions are unambiguous.
        let angle = cval 0.0
        let sg =
            Sg.box' (C4b(255uy, 150uy, 30uy, 255uy)) (Box3d.FromCenterAndSize(V3d.Zero, V3d(2.0, 2.0, 2.0)))
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.trafo (angle |> AVal.map (fun a -> Trafo3d.RotationZ a * Trafo3d.RotationX (a * 0.5)))
            |> Sg.viewTrafo (CameraView.lookAt (V3d(5.0, 5.0, 4.0)) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> AVal.constant)
            |> Sg.projTrafo (Frustum.perspective 60.0 0.1 100.0 (float w / float h) |> Frustum.projTrafo |> AVal.constant)
        // opaque dark background + depth clear, then the box (mirrors the validated path).
        let clearTask = runtime.CompileClear(signature, clear { color (C4b(10uy, 12uy, 34uy, 255uy)); depth 1.0 })
        let task = RenderTask.ofList [ clearTask; Sg.compile runtime signature sg ]

        let size = cval (V2i(w, h))
        use target = new SharedTextureRenderTarget(runtime, signature, task, size, "selftest")

        // drive a few frames (each re-evaluates because `angle` is dirty).
        let mutable lastId = ""
        for f in 0 .. 5 do
            transact (fun () -> angle.Value <- float f * 0.04)
            let (id, _) = target.Run(AdaptiveToken.Top)
            lastId <- id

        // read the last-written slot back from the ring.
        let buf = target.DebugReadSlot lastId
        let ok =
            if buf.Length <> w * h * 4 then
                Log.warn "[SharedTexture selftest] readback wrong length %d (expected %d)" buf.Length (w*h*4)
                false
            else
                // sample a 3x3 grid; the dark opaque background (B~34,G~12,R~10) must be
                // present, and somewhere on the image the orange box (high R) must appear.
                let pixel x y =
                    let o = (y * w + x) * 4
                    // BGRA layout
                    (int buf.[o+2], int buf.[o+1], int buf.[o+0], int buf.[o+3])   // (R,G,B,A)
                let pts = [ for gy in 0..2 do for gx in 0..2 -> (gx * (w-1) / 2, gy * (h-1) / 2) ]
                let samples = pts |> List.map (fun (x,y) -> pixel x y)
                let allOpaque = samples |> List.forall (fun (_,_,_,a) -> a >= 250)
                let center =
                    let (r,_,_,_) = pixel (w/2) (h/2) in r
                let anyOrange = samples |> List.exists (fun (r,g,b,_) -> r > 120 && r > b + 30)
                for (x,y) in pts do
                    let (r,g,b,a) = pixel x y
                    Log.line "[SharedTexture selftest] (%d,%d) RGBA=(%d,%d,%d,%d)" x y r g b a
                Log.line "[SharedTexture selftest] center R=%d opaque=%b anyOrange=%b" center allOpaque anyOrange
                allOpaque && anyOrange
        if ok then Log.line "[SharedTexture selftest] PASS (textureId=%s)" lastId
        else Log.warn "[SharedTexture selftest] FAIL (textureId=%s)" lastId
        task.Dispose()
        ok


type SharedTextureTransfer() =

    // IsSupported: a Vulkan runtime that can actually export an OPAQUE_FD image.
    // Probe by creating+exporting a tiny image and disposing it.
    let supported (runtime : IRuntime) =
        match runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            try
                let img = OpaqueFd.create vk.Device 16 16
                let ok = img.MemFd >= 0
                OpaqueFd.destroy vk.Device img
                ok
            with _ -> false
        | _ -> false

    [<OnAardvarkInit>]
    static member Init() = RemoteHtmlBackend.RegisterImageTransfer<SharedTextureTransfer>(200)

    interface IImageTransfer with
        member x.Requirements = []

        member x.ClientCheck = ["return (aardvark.openSharedTexture != undefined);"]

        member x.IsSupported(runtime : IRuntime) = supported runtime

        member x.CreateRenderer(signature : IFramebufferSignature, scene : IRenderTask, size : aval<V2i>, _requestData : amap<string, string>) =
            let runtime = signature.Runtime :?> IRuntime
            // a per-renderer channel name keys the side-channel socket path; reuse the
            // channel-name surrogate (the renderer is 1:1 with a SetupRenderer channel).
            let channelName = System.Guid.NewGuid().ToString("N")
            let target = new SharedTextureRenderTarget(runtime, signature, scene, size, channelName)

            { new TransferImageRenderer() with
                member x.RenderFrame(token) =
                    let (id, size) = target.Run(token)
                    let json = $"{{ \"id\": \"{id}\", \"channel\": \"{channelName}\", \"size\": {{ \"width\": {size.X}, \"height\": {size.Y} }} }}"
                    ChannelMessage.Text json

                member x.Destroy() =
                    target.Dispose()
            }

        member x.Boot(_channelName) =
            [
                $"const surface = document.createElement(\"aardvark-surface\");"
                $"surface.style.userSelect = \"none\";"
                $"surface.style.pointerEvents = \"none\";"
                $"let sharedTextureChannel = null;"   // side-channel path, learned from the first message
                $"__THIS__.appendChild(surface);"
            ]

        member x.Shutdown(_channelName) =
            [
                $"if(surface.releaseSharedTexture) {{ surface.releaseSharedTexture(); }}"
                $"surface.remove();"
            ]

        member x.ClientCode(message) =
            [
                $"let msg = JSON.parse({message});"
                // the browser native layer binds /tmp/aardvark-sharedtexture-<channel>.sock
                // (see CONTRACT) the first time it sees the channel name; the server connects
                // to it lazily and REGisters each slot's memfd there.
                $"if(sharedTextureChannel != msg.channel) {{"
                $"    sharedTextureChannel = msg.channel;"
                $"    if(aardvark.bindSharedTextureChannel) {{ aardvark.bindSharedTextureChannel(msg.channel); }}"
                $"}}"
                $"surface.width = msg.size.width;"
                $"surface.height = msg.size.height;"
                $"aardvark.openSharedTexture(msg.id);"
            ]
