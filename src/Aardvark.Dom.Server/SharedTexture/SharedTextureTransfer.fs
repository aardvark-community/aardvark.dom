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
open System.Runtime.InteropServices

#nowarn "9"
#nowarn "51"

// ── Per-platform strategy ───────────────────────────────────────────────────
// Everything in SharedTextureRenderTarget is platform-agnostic (the ring, the offscreen
// FBO + scene render, render-when-dirty, the 2-in-flight FIFO, resize/realloc, the
// REG/FREE protocol + gen tags, the {id,channel,size} message) EXCEPT four things that
// genuinely differ by OS — factored out here:
//   - ALLOC a w×h shared backing,
//   - COPY the rendered FBO into it (incl. the cross-process sync),
//   - DESTROY it,
//   - HANDOFF / REGister its handle with the browser.
// Mapping:
//   Linux  : OPAQUE_FD dma-buf + EXTERNAL-release blit + SCM_RIGHTS fd handoff.   [tested on airtop]
//   macOS  : IOSurface (VK_EXT_metal_objects) + EXTERNAL-release blit + mach-port. [compile-only; runtime follow-on]
//   Windows: Silk.NET D3D11 keyed-mutex texture imported into Vulkan + keyed-mutex
//            blit (single key 0) + Win32 NT-handle file handoff.                   [compile-only; runtime follow-on]

/// One ring-slot backing: the Vulkan image to render into + the platform record (Tag).
type private PlatformBuffer =
    { Image  : VkImage
      Width  : int
      Height : int
      Tag    : obj }   // OpaqueImage | MetalSharedImage | (SilkD3D11Texture * D3D11Export.ImportedD3D11Image)

type private PlatformStrategy =
    { /// allocate a w×h shared backing.
      Alloc    : int -> int -> PlatformBuffer
      /// blit the rendered FBO image (src/srcLayout) into the slot buffer, with the platform's
      /// cross-process sync, signaling the per-slot semaphore (GPU) + fence (CPU).
      CopyInto : VkCommandBuffer -> VkImage -> VkImageLayout -> PlatformBuffer -> int -> int -> VkSemaphore -> VkFence -> unit
      /// free the backing.
      Destroy  : PlatformBuffer -> unit
      /// publish the slot's handle + send the REG line over the side-channel `conn`. textureId,
      /// w, h. returns true on success. (Linux: SCM_RIGHTS memfd; macOS: mach name; Windows:
      /// NT-handle value folded into the REG line over the named pipe.)
      SendReg  : int64 -> PlatformBuffer -> string -> int -> int -> bool
      /// SIDE-CHANNEL TRANSPORT (the only genuinely-new Windows piece). The browser native layer
      /// is the LISTENER/server; the producer connects as the client. The connection is an opaque
      /// int64 token (Linux: socket fd; Windows: named-pipe HANDLE). -1 / throw until the browser
      /// is up — the render target retries with a backoff. SideChannelTick sends the per-frame
      /// "F <textureId>" swap tick; SideChannelPoll drains "FREE <id>" backpressure messages.
      SideChannelConnect : string -> int64
      SideChannelPoll    : int64 -> string
      SideChannelClose   : int64 -> unit
      /// Windows keyed-mutex: WAIT the slot's copy fence before publishing the frame to the
      /// browser, so the keyed-mutex release(0) is GPU-complete before the compositor's
      /// AcquireKeyedMutex(0) — otherwise the browser acquires a key the producer's pending
      /// submit still holds and the compositor stalls. Linux/macOS (EXTERNAL release) don't
      /// need it (the per-frame acquire fence carries availability).
      SyncAfterCopy : bool
      /// self-test readback (BGRA bytes); [||] where unsupported.
      Readback : PlatformBuffer -> byte[] }

module private Platform =
    /// build the strategy for the current OS over `device`.
    let strategy (device : Device) : PlatformStrategy =
        let linux () : PlatformStrategy =
            { Alloc = fun w h ->
                let o = OpaqueFd.create device w h
                { Image = o.Image; Width = o.Width; Height = o.Height; Tag = box o }
              CopyInto = fun cmd src srcLayout buf w h sem fence ->
                DmaBufGpu.recordBlitInto device cmd src srcLayout buf.Image w h sem fence
              Destroy = fun buf -> OpaqueFd.destroy device (buf.Tag :?> OpaqueImage)
              SendReg = fun conn buf id w h ->
                let o = buf.Tag :?> OpaqueImage
                FdHandoff.streamFrameFd (int conn) (sprintf "REG %s %d %d\n" id w h) o.MemFd
              SideChannelConnect = fun ch -> int64 (FdHandoff.streamConnect (sprintf "/tmp/aardvark-sharedtexture-%s.sock" ch))
              SideChannelPoll    = fun conn -> FdHandoff.streamPoll (int conn)
              SideChannelClose   = fun conn -> FdHandoff.streamClose (int conn)
              SyncAfterCopy = false
              Readback = fun buf -> OpaqueFd.readAllLocal device (buf.Tag :?> OpaqueImage) }

        let macos () : PlatformStrategy =
            { Alloc = fun w h ->
                let m = MetalExport.createImported device w h
                { Image = m.Image; Width = m.Width; Height = m.Height; Tag = box m }
              CopyInto = fun cmd src srcLayout buf w h sem fence ->
                // IOSurface-backed image: the same EXTERNAL-release blit (Vulkan-generic).
                DmaBufGpu.recordBlitInto device cmd src srcLayout buf.Image w h sem fence
              Destroy = fun buf -> MetalExport.destroy device (buf.Tag :?> MetalSharedImage)
              SendReg = fun conn buf id w h ->
                let m = buf.Tag :?> MetalSharedImage
                // publish the IOSurface under a per-textureId mach service name; REG names it so
                // the browser does IOSurfaceLookupFromMachPort(name). (Streaming handoff: follow-on.)
                let name = sprintf "%s.%s" MetalExport.MachServiceName id
                MetalExport.aardvark_publish(name, m.IOSurface) |> ignore
                FdHandoff.streamFrame (int conn) (sprintf "REG %s %d %d %s\n" id w h name)
              SideChannelConnect = fun ch -> int64 (FdHandoff.streamConnect (sprintf "/tmp/aardvark-sharedtexture-%s.sock" ch))
              SideChannelPoll    = fun conn -> FdHandoff.streamPoll (int conn)
              SideChannelClose   = fun conn -> FdHandoff.streamClose (int conn)
              SyncAfterCopy = false
              Readback = fun _ -> [||] }

        let windows () : PlatformStrategy =
            { Alloc = fun w h ->
                let (_, _, luidHex) = D3D11Export.deviceLUID device
                let tex = SilkD3D11Alloc.alloc luidHex w h
                let imp = D3D11Export.importD3D11 device tex.Handle w h
                { Image = imp.Image; Width = w; Height = h; Tag = box (tex, imp) }
              CopyInto = fun cmd src srcLayout buf w h _sem fence ->
                let (_, imp) = buf.Tag :?> (SilkD3D11Texture * D3D11Export.ImportedD3D11Image)
                // keyed-mutex blit, single key 0 (Chromium's D3DImageBacking owns the mutex).
                // Pass a NULL signal semaphore: nothing waits it here (the keyed mutex is the
                // cross-process sync) and re-signaling the same per-slot BINARY semaphore every
                // RING_N frames without a wait would hang the queue. The per-slot CPU fence
                // alone tracks copy completion.
                DmaBufGpu.recordBlitIntoKeyed device cmd src srcLayout imp.Image imp.Memory w h 0UL 0UL
                    Unchecked.defaultof<VkSemaphore> fence
              Destroy = fun buf ->
                let (tex, imp) = buf.Tag :?> (SilkD3D11Texture * D3D11Export.ImportedD3D11Image)
                D3D11Export.destroyImported device imp
                SilkD3D11Alloc.destroy tex
              SendReg = fun conn buf id w h ->
                let (tex, _) = buf.Tag :?> (SilkD3D11Texture * D3D11Export.ImportedD3D11Image)
                // Windows: the DXGI shared NT handle can't be sent down the pipe (it's process-
                // local); fold its VALUE + the producer PID into the REG line so the browser
                // reconstitutes it via OpenProcess(PROCESS_DUP_HANDLE)+DuplicateHandle (the exact
                // mechanism the single-frame Win32 painter uses). Per-textureId REG over the pipe.
                let pid = System.Diagnostics.Process.GetCurrentProcess().Id
                NamedPipeHandoff.send (nativeint conn)
                    (sprintf "REG %s %d %d %d %d\n" id w h (int64 tex.Handle) pid)
              SideChannelConnect = fun ch -> int64 (NamedPipeHandoff.connect ch)
              SideChannelPoll    = fun conn -> NamedPipeHandoff.poll (nativeint conn)
              SideChannelClose   = fun conn -> NamedPipeHandoff.close (nativeint conn)
              SyncAfterCopy = true
              Readback = fun _ -> [||] }

        if RuntimeInformation.IsOSPlatform OSPlatform.OSX then macos ()
        elif RuntimeInformation.IsOSPlatform OSPlatform.Windows then windows ()
        else linux ()

// One ring slot: its shared backing plus the persistent per-slot copy resources
// (command buffer + exportable signal semaphore + CPU fence) — allocated ONCE per
// (re)allocation, reused every frame (NO per-frame pool/semaphore churn, NO waitIdle),
// exactly the L4 opaquefd-stream producer pattern.
type private RingSlot =
    {
        TextureId : string
        Buf       : PlatformBuffer
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

    static let RING_N = 4            // ring depth (compositor holds <=2 in-flight + producer 1 + 1 spare)
    // the side-channel path/pipe is keyed by channelName inside the platform strategy
    // (Linux /tmp/aardvark-sharedtexture-<channel>.sock; Windows \\.\pipe\aardvark-<channel>).

    let vk =
        match runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk -> vk
        | _ -> failwith "[SharedTexture] runtime is not Vulkan"
    let device = vk.Device
    let devH = device.Handle
    let qfi = uint32 device.GraphicsFamily.Index
    // platform alloc/copy/destroy/handoff selected by OS (Linux tested; macOS/Windows wired).
    let strategy = Platform.strategy device

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
            // Match the GIVEN signature's color format (the framework's DOM render task is
            // compiled for it — typically Rgba8). The ring image is BGRA, so the copy into
            // it is a vkCmdBlitImage (format-aware R/B swizzle), NOT a raw vkCmdCopyImage.
            let colorFormat =
                match signature.ColorAttachments |> Map.tryFind 0 with
                | Some att -> att.Format
                | None -> TextureFormat.Rgba8
            let c = runtime.CreateRenderbuffer(s, colorFormat, signature.Samples)
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

    // ---- lazy side-channel connection to the browser native layer (transport via the
    //      strategy: Linux AF_UNIX socket / Windows named pipe; opaque int64 token) ----
    let mutable conn = -1L                // -1 until connected
    let mutable nextConnectAttempt = DateTime.MinValue   // backoff clock for retries

    // The browser binds/listens for `channelName`; the server connects as the client.
    // The browser may not be up at the first frame, so RETRY with a short backoff
    // (the browser begins listening only once the page calls bindSharedTextureChannel).
    let tryConnect () =
        if conn < 0L && DateTime.UtcNow >= nextConnectAttempt then
            try conn <- strategy.SideChannelConnect channelName
            with _ ->
                conn <- -1L               // browser not up yet — retry after the backoff
                nextConnectAttempt <- DateTime.UtcNow.AddMilliseconds 200.0
        conn >= 0L

    let disconnect () =
        if conn >= 0L then (try strategy.SideChannelClose conn with _ -> ()); conn <- -1L
        nextConnectAttempt <- DateTime.UtcNow.AddMilliseconds 200.0

    let destroySlot (s : RingSlot) =
        strategy.Destroy s.Buf
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
                    Buf        = strategy.Alloc s.X s.Y
                    Cmd        = newCmd ()
                    // plain (non-exportable) semaphore: it is signaled by the per-slot copy
                    // submit but NEVER exported in this integration — the cross-process sync is
                    // carried by the platform's release primitive (Linux/macOS QUEUE_FAMILY_EXTERNAL
                    // release, Windows keyed-mutex acquire/release). createExportableSemaphore is
                    // sync_fd-backed (Linux-only) and would fail at runtime on Windows/macOS, so use
                    // a plain cross-platform semaphore here.
                    Sem        = DmaBufSync.createSemaphore device
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
                    // REG <textureId> <w> <h> [+ platform handle]: Linux SCM_RIGHTS memfd,
                    // macOS mach-port name, Windows NT-handle file — via the strategy.
                    let ok = strategy.SendReg conn s.Buf s.TextureId s.Buf.Width s.Buf.Height
                    if ok then s.Registered <- true
                    else disconnect ()

    // Drain any FREE <textureId> messages the browser sent back; clear the slot's Busy.
    let pollFree () =
        if conn >= 0L then
            let txt = try strategy.SideChannelPoll conn with _ -> ""
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

            // GPU blit FBO color -> ring slot via the platform strategy; per-slot fence (CPU)
            // + semaphore (GPU acquire). Blit (not copy) because the FBO is the framework's
            // Rgba8 while the ring is BGRA — the format-aware blit swizzles R/B for free. The
            // strategy supplies the cross-process sync (EXTERNAL release on Linux/macOS,
            // keyed-mutex acquire/release on Windows).
            strategy.CopyInto slot.Cmd img.Handle img.Layout slot.Buf s.X s.Y slot.Sem slot.Fence

            // Windows keyed-mutex: block until the copy (incl. release(0)) is GPU-complete so
            // the browser's AcquireKeyedMutex(0) won't race the producer's pending release. The
            // fence is reset at the top of the slot's NEXT use, so re-wait it next cycle is a
            // no-op fast path. (Linux/macOS skip this — their per-frame acquire fence handles it.)
            if strategy.SyncAfterCopy then waitFence slot.Fence

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
            strategy.Readback s.Buf
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

    // IsSupported: a Vulkan runtime whose per-OS strategy can actually allocate a shared
    // backing. Probe by allocating + destroying a tiny one through the platform strategy
    // (Linux OPAQUE_FD / macOS IOSurface / Windows D3D11 keyed-mutex).
    let supported (runtime : IRuntime) =
        match runtime with
        | :? Aardvark.Rendering.Vulkan.Runtime as vk ->
            try
                let strat = Platform.strategy vk.Device
                let buf = strat.Alloc 16 16
                strat.Destroy buf
                true
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
