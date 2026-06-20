namespace Aardvark.Dom.Remote.SharedTexture

// Milestone 0c: populate the shared dma-buf image purely on the GPU — no host
// download, no CPU map of the shared image. Stands in for the real path (Aardvark
// renders a scene into a device-local optimal target, then we vkCmdCopyImage it
// into the exported dma-buf). Here the source is GPU-cleared to a known colour so
// the EGL re-import can verify the copy without any CPU pixel writes.
//
// Raw command-buffer plumbing against the public Vulkan Device.

open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

module DmaBufGpu =

    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then failwithf "[DmaBufGpu] %s failed: %A" what res

    let private QUEUE_FAMILY_IGNORED = 0xFFFFFFFFu

    /// device-local optimal colour image (clearable + copy source) — stands in
    /// for an Aardvark render target.
    let createColorSource (device : Device) (w : int) (h : int) : VkImage * VkDeviceMemory =
        let dev = device.Handle
        let mutable info =
            VkImageCreateInfo(
                0n, VkImageCreateFlags.None, VkImageType.D2d, VkFormat.B8g8r8a8Unorm,
                VkExtent3D(uint32 w, uint32 h, 1u), 1u, 1u, VkSampleCountFlags.D1Bit,
                VkImageTiling.Optimal,
                VkImageUsageFlags.TransferSrcBit ||| VkImageUsageFlags.TransferDstBit,
                VkSharingMode.Exclusive, 0u, NativePtr.zero, VkImageLayout.Undefined)
        let mutable image = Unchecked.defaultof<VkImage>
        VkRaw.vkCreateImage(dev, &&info, NativePtr.zero, &&image) |> check "vkCreateImage(src)"

        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType =
            device.PhysicalDevice.MemoryTypes
            |> Array.find (fun mt ->
                (req.memoryTypeBits &&& (1u <<< mt.index)) <> 0u &&
                (int mt.flags &&& int VkMemoryPropertyFlags.DeviceLocalBit) <> 0)
            |> fun mt -> uint32 mt.index

        let mutable alloc = VkMemoryAllocateInfo(0n, req.size, memType)
        let mutable mem = Unchecked.defaultof<VkDeviceMemory>
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&mem) |> check "vkAllocateMemory(src)"
        VkRaw.vkBindImageMemory(dev, image, mem, 0UL) |> check "vkBindImageMemory(src)"
        image, mem

    let destroyImage (device : Device) (image : VkImage) (mem : VkDeviceMemory) =
        VkRaw.vkDestroyImage(device.Handle, image, NativePtr.zero)
        VkRaw.vkFreeMemory(device.Handle, mem, NativePtr.zero)

    let private barrier (cmd : VkCommandBuffer) (image : VkImage)
                        (srcAccess : VkAccessFlags) (dstAccess : VkAccessFlags)
                        (oldLayout : VkImageLayout) (newLayout : VkImageLayout)
                        (srcStage : VkPipelineStageFlags) (dstStage : VkPipelineStageFlags) =
        let range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        let mutable b =
            VkImageMemoryBarrier(0n, srcAccess, dstAccess, oldLayout, newLayout,
                                 QUEUE_FAMILY_IGNORED, QUEUE_FAMILY_IGNORED, image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, srcStage, dstStage, VkDependencyFlags.None,
                                   0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b)

    // VK_QUEUE_FAMILY_EXTERNAL — release ownership of the shared image to an
    // external consumer (another Vulkan instance / API). Without this, cross-instance
    // content visibility is undefined and NVIDIA samples blank in Chromium.
    let private QUEUE_FAMILY_EXTERNAL = 0xFFFFFFF1u

    let private barrierReleaseExternal (cmd : VkCommandBuffer) (image : VkImage)
                                       (srcAccess : VkAccessFlags)
                                       (oldLayout : VkImageLayout) (newLayout : VkImageLayout)
                                       (srcStage : VkPipelineStageFlags) (srcQfi : uint32) =
        let range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        let mutable b =
            VkImageMemoryBarrier(0n, srcAccess, VkAccessFlags.None, oldLayout, newLayout,
                                 srcQfi, QUEUE_FAMILY_EXTERNAL, image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, srcStage, VkPipelineStageFlags.BottomOfPipeBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b)

    /// Records "GPU-clear `src` to `color` + copy into `dstImage`" and submits it,
    /// signaling `signalSem` on completion. Returns the queue + one-shot pool so the
    /// caller can export a fence before draining and tearing down. Platform-agnostic.
    let private recordClearCopySubmit (device : Device) (src : VkImage) (dstImage : VkImage)
                                      (dstW : int) (dstH : int) (color : V4f) (signalSem : VkSemaphore) =
        let dev = device.Handle
        let qfi = uint32 device.GraphicsFamily.Index

        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)

        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.None, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(dev, &&poolInfo, NativePtr.zero, &&pool) |> check "vkCreateCommandPool"

        let mutable allocInfo = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable cmd = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(dev, &&allocInfo, &&cmd) |> check "vkAllocateCommandBuffers"

        let mutable beginInfo = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&beginInfo) |> check "vkBeginCommandBuffer"

        // src: UNDEFINED -> TRANSFER_DST, clear, -> TRANSFER_SRC
        barrier cmd src VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit
        let mutable cc = VkClearColorValue.Float32 color
        let mutable range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        VkRaw.vkCmdClearColorImage(cmd, src, VkImageLayout.TransferDstOptimal, &&cc, 1u, &&range)
        barrier cmd src VkAccessFlags.TransferWriteBit VkAccessFlags.TransferReadBit
                VkImageLayout.TransferDstOptimal VkImageLayout.TransferSrcOptimal
                VkPipelineStageFlags.TransferBit VkPipelineStageFlags.TransferBit

        // dst: UNDEFINED -> TRANSFER_DST
        barrier cmd dstImage VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit

        // copy src -> dst
        let layers = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkImageCopy(layers, VkOffset3D(0, 0, 0), layers, VkOffset3D(0, 0, 0),
                        VkExtent3D(uint32 dstW, uint32 dstH, 1u))
        VkRaw.vkCmdCopyImage(cmd, src, VkImageLayout.TransferSrcOptimal,
                             dstImage, VkImageLayout.TransferDstOptimal, 1u, &&region)

        // dst -> GENERAL + RELEASE ownership to an external (cross-instance) consumer,
        // so the producer's writes are visible to Chromium's importing instance.
        barrierReleaseExternal cmd dstImage VkAccessFlags.TransferWriteBit
                               VkImageLayout.TransferDstOptimal VkImageLayout.General
                               VkPipelineStageFlags.TransferBit qfi

        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer"

        let mutable psem = signalSem
        let mutable pcmd = cmd
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 1u, &&psem)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit"
        struct (queue, pool)

    /// Linux: GPU-clear `src` + copy into the dma-buf `dst`. Returns a sync_fd fence.
    let clearAndCopy (device : Device) (src : VkImage) (dst : DmaBufImage) (color : V4f) : int =
        let sem = DmaBufSync.createExportableSemaphore device
        let struct (queue, pool) = recordClearCopySubmit device src dst.Image dst.Width dst.Height color sem
        let syncFd = DmaBufSync.exportSyncFd device sem
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"
        DmaBufSync.destroySemaphore device sem
        VkRaw.vkDestroyCommandPool(device.Handle, pool, NativePtr.zero)
        syncFd

    // Like recordClearCopySubmit but the source ALREADY holds rendered content (in
    // `srcLayout`, e.g. an Aardvark FBO colour image): transition src -> TRANSFER_SRC,
    // copy into dst, restore src to `srcLayout` (so Aardvark's layout tracking stays
    // valid), then release dst to EXTERNAL. GPU is idle when this runs (caller waits).
    let private recordRenderCopySubmit (device : Device) (src : VkImage) (srcLayout : VkImageLayout)
                                       (dstImage : VkImage) (dstW : int) (dstH : int) (signalSem : VkSemaphore) =
        let dev = device.Handle
        // The cube render is a SEPARATE submit on the same queue; same-queue submits may
        // overlap, so wait for it to finish before copying its output.
        VkRaw.vkDeviceWaitIdle(dev) |> ignore
        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.None, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(dev, &&poolInfo, NativePtr.zero, &&pool) |> check "vkCreateCommandPool"
        let mutable allocInfo = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable cmd = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(dev, &&allocInfo, &&cmd) |> check "vkAllocateCommandBuffers"
        let mutable beginInfo = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&beginInfo) |> check "vkBeginCommandBuffer"

        // src: srcLayout -> TRANSFER_SRC
        barrier cmd src VkAccessFlags.MemoryWriteBit VkAccessFlags.TransferReadBit
                srcLayout VkImageLayout.TransferSrcOptimal
                VkPipelineStageFlags.AllCommandsBit VkPipelineStageFlags.TransferBit
        // dst: UNDEFINED -> TRANSFER_DST
        barrier cmd dstImage VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit
        // copy src -> dst
        let layers = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkImageCopy(layers, VkOffset3D(0, 0, 0), layers, VkOffset3D(0, 0, 0),
                        VkExtent3D(uint32 dstW, uint32 dstH, 1u))
        VkRaw.vkCmdCopyImage(cmd, src, VkImageLayout.TransferSrcOptimal,
                             dstImage, VkImageLayout.TransferDstOptimal, 1u, &&region)
        // src: TRANSFER_SRC -> srcLayout (restore for Aardvark)
        barrier cmd src VkAccessFlags.TransferReadBit VkAccessFlags.MemoryReadBit
                VkImageLayout.TransferSrcOptimal srcLayout
                VkPipelineStageFlags.TransferBit VkPipelineStageFlags.AllCommandsBit
        // dst -> GENERAL + RELEASE to external consumer
        barrierReleaseExternal cmd dstImage VkAccessFlags.TransferWriteBit
                               VkImageLayout.TransferDstOptimal VkImageLayout.General
                               VkPipelineStageFlags.TransferBit qfi

        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer"
        let mutable psem = signalSem
        let mutable pcmd = cmd
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 1u, &&psem)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit"
        struct (queue, pool)

    /// Low-latency variant: record the `src`(srcLayout) -> `dst` copy into a CALLER-OWNED
    /// (persistent, reset-capable) command buffer and submit, signaling `signalSem` (GPU,
    /// for the consumer's acquire) and `fence` (CPU, so the caller knows when the copy is
    /// done and the slot/FBO can be reused). NO pool creation, NO waitIdle — the producer
    /// pipelines across a ring of these. The cube render (`GetValue`) is synchronous so its
    /// output is ready; the caller must ensure the PREVIOUS copy out of the shared FBO has
    /// completed (wait its fence) before re-rendering into the FBO.
    let recordCopyInto (device : Device) (cmd : VkCommandBuffer) (src : VkImage) (srcLayout : VkImageLayout)
                       (dstImage : VkImage) (dstW : int) (dstH : int)
                       (signalSem : VkSemaphore) (fence : VkFence) =
        let dev = device.Handle
        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        VkRaw.vkResetCommandBuffer(cmd, VkCommandBufferResetFlags.None) |> ignore
        let mutable beginInfo = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&beginInfo) |> check "vkBeginCommandBuffer"

        barrier cmd src VkAccessFlags.MemoryWriteBit VkAccessFlags.TransferReadBit
                srcLayout VkImageLayout.TransferSrcOptimal
                VkPipelineStageFlags.AllCommandsBit VkPipelineStageFlags.TransferBit
        barrier cmd dstImage VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit
        let layers = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkImageCopy(layers, VkOffset3D(0, 0, 0), layers, VkOffset3D(0, 0, 0),
                        VkExtent3D(uint32 dstW, uint32 dstH, 1u))
        VkRaw.vkCmdCopyImage(cmd, src, VkImageLayout.TransferSrcOptimal,
                             dstImage, VkImageLayout.TransferDstOptimal, 1u, &&region)
        barrier cmd src VkAccessFlags.TransferReadBit VkAccessFlags.MemoryReadBit
                VkImageLayout.TransferSrcOptimal srcLayout
                VkPipelineStageFlags.TransferBit VkPipelineStageFlags.AllCommandsBit
        barrierReleaseExternal cmd dstImage VkAccessFlags.TransferWriteBit
                               VkImageLayout.TransferDstOptimal VkImageLayout.General
                               VkPipelineStageFlags.TransferBit qfi

        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer"
        let mutable psem = signalSem
        let mutable pcmd = cmd
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 1u, &&psem)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, fence) |> check "vkQueueSubmit"

    /// Like recordCopyInto, but uses vkCmdBlitImage instead of vkCmdCopyImage. The
    /// framework's DOM render task is compiled for an Rgba8 (R8G8B8A8) signature, while
    /// the shared ring image is B8G8R8A8 (the browser imports it as kBGRA_8888). A raw
    /// vkCmdCopyImage would byte-copy and swap R/B in the browser; vkCmdBlitImage goes
    /// through the format-aware path (reads src as RGBA, writes dst as BGRA) so the R/B
    /// swizzle happens for free. src and dst are the SAME size so there is no scaling
    /// (filter is irrelevant; use Nearest). Same sync contract as recordCopyInto.
    let recordBlitInto (device : Device) (cmd : VkCommandBuffer) (src : VkImage) (srcLayout : VkImageLayout)
                       (dstImage : VkImage) (dstW : int) (dstH : int)
                       (signalSem : VkSemaphore) (fence : VkFence) =
        let dev = device.Handle
        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        VkRaw.vkResetCommandBuffer(cmd, VkCommandBufferResetFlags.None) |> ignore
        let mutable beginInfo = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&beginInfo) |> check "vkBeginCommandBuffer"

        barrier cmd src VkAccessFlags.MemoryWriteBit VkAccessFlags.TransferReadBit
                srcLayout VkImageLayout.TransferSrcOptimal
                VkPipelineStageFlags.AllCommandsBit VkPipelineStageFlags.TransferBit
        barrier cmd dstImage VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit
        let layers = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        // src/dst offset ranges [min,max] for the (full-image, same-size) blit.
        let mutable offsets = VkOffset3D_2()
        offsets.[0] <- VkOffset3D(0, 0, 0)
        offsets.[1] <- VkOffset3D(dstW, dstH, 1)
        let mutable region = VkImageBlit(layers, offsets, layers, offsets)
        VkRaw.vkCmdBlitImage(cmd, src, VkImageLayout.TransferSrcOptimal,
                             dstImage, VkImageLayout.TransferDstOptimal, 1u, &&region, VkFilter.Nearest)
        barrier cmd src VkAccessFlags.TransferReadBit VkAccessFlags.MemoryReadBit
                VkImageLayout.TransferSrcOptimal srcLayout
                VkPipelineStageFlags.TransferBit VkPipelineStageFlags.AllCommandsBit
        barrierReleaseExternal cmd dstImage VkAccessFlags.TransferWriteBit
                               VkImageLayout.TransferDstOptimal VkImageLayout.General
                               VkPipelineStageFlags.TransferBit qfi

        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer"
        let mutable psem = signalSem
        let mutable pcmd = cmd
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 1u, &&psem)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, fence) |> check "vkQueueSubmit"

    /// Copy an already-rendered source image (currently in `srcLayout`) into the
    /// OPAQUE/dma-buf `dst`, releasing dst to EXTERNAL. Returns a sync_fd fence;
    /// restores the source layout so Aardvark can keep rendering into it.
    let renderCopyExternal (device : Device) (src : VkImage) (srcLayout : VkImageLayout) (dst : DmaBufImage) : int =
        let sem = DmaBufSync.createExportableSemaphore device
        let struct (queue, pool) = recordRenderCopySubmit device src srcLayout dst.Image dst.Width dst.Height sem
        let syncFd = DmaBufSync.exportSyncFd device sem
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"
        DmaBufSync.destroySemaphore device sem
        VkRaw.vkDestroyCommandPool(device.Handle, pool, NativePtr.zero)
        syncFd

    /// Windows: GPU-clear `src` + copy into the Win32 shared image `dst`. Returns an
    /// exportable Win32 semaphore handle (consumer waits via a D3D/KMT fence).
    let clearAndCopyWin (device : Device) (src : VkImage) (dst : WinSharedImage) (color : V4f) : nativeint =
        let sem = DmaBufSync.createExportableSemaphoreWin32 device
        let struct (queue, pool) = recordClearCopySubmit device src dst.Image dst.Width dst.Height color sem
        let h = DmaBufSync.exportSemaphoreWin32 device sem
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"
        DmaBufSync.destroySemaphore device sem
        VkRaw.vkDestroyCommandPool(device.Handle, pool, NativePtr.zero)
        h

    /// Windows OPAQUE_WIN32 cross-instance test: GPU-clear `src` to `color` + copy into the
    /// shared OPTIMAL image `dstImage`, releasing it to VK_QUEUE_FAMILY_EXTERNAL (GENERAL
    /// layout), then waitIdle. NO semaphore export — the AMD-permissive no-sync baseline that
    /// the OpaqueWin32 test relies on. Caller paces with a CPU sleep between iterations.
    let clearCopyExternalNoSem (device : Device) (src : VkImage) (dstImage : VkImage)
                               (dstW : int) (dstH : int) (color : V4f) =
        let dev = device.Handle
        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.None, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(dev, &&poolInfo, NativePtr.zero, &&pool) |> check "vkCreateCommandPool(win32rw)"
        let mutable allocInfo = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable cmd = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(dev, &&allocInfo, &&cmd) |> check "vkAllocateCommandBuffers(win32rw)"
        let mutable beginInfo = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&beginInfo) |> check "vkBeginCommandBuffer(win32rw)"

        barrier cmd src VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit
        let mutable cc = VkClearColorValue.Float32 color
        let mutable range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        VkRaw.vkCmdClearColorImage(cmd, src, VkImageLayout.TransferDstOptimal, &&cc, 1u, &&range)
        barrier cmd src VkAccessFlags.TransferWriteBit VkAccessFlags.TransferReadBit
                VkImageLayout.TransferDstOptimal VkImageLayout.TransferSrcOptimal
                VkPipelineStageFlags.TransferBit VkPipelineStageFlags.TransferBit
        // dst: GENERAL (after first iter) or UNDEFINED — use GENERAL is unsafe on iter0 because
        // the image starts UNDEFINED; UNDEFINED->TRANSFER_DST discards but that's fine since we
        // overwrite the whole image each iteration.
        barrier cmd dstImage VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit
        let layers = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkImageCopy(layers, VkOffset3D(0, 0, 0), layers, VkOffset3D(0, 0, 0),
                        VkExtent3D(uint32 dstW, uint32 dstH, 1u))
        VkRaw.vkCmdCopyImage(cmd, src, VkImageLayout.TransferSrcOptimal,
                             dstImage, VkImageLayout.TransferDstOptimal, 1u, &&region)
        barrierReleaseExternal cmd dstImage VkAccessFlags.TransferWriteBit
                               VkImageLayout.TransferDstOptimal VkImageLayout.General
                               VkPipelineStageFlags.TransferBit qfi
        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer(win32rw)"
        let mutable pcmd = cmd
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit(win32rw)"
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle(win32rw)"
        VkRaw.vkDestroyCommandPool(dev, pool, NativePtr.zero)

    /// macOS: GPU-clear `src` + copy into the IOSurface-backed image `dst`. Returns the
    /// exported MTLSharedEvent handle the consumer waits on.
    /// NOTE: with a binary semaphore MoltenVK returns 0 — MTLSharedEvent is value-based and
    /// needs a TIMELINE semaphore (+ timelineSemaphore device feature + value-based submit).
    /// Deferred follow-up; the GPU fill + IOSurface export/readback are fully validated.
    let clearAndCopyMetal (device : Device) (src : VkImage) (dst : MetalSharedImage) (color : V4f) : nativeint =
        let sem = DmaBufSync.createExportableSemaphoreMetal device
        let struct (queue, pool) = recordClearCopySubmit device src dst.Image dst.Width dst.Height color sem
        let sharedEvent = DmaBufSync.exportSharedEventMetal device sem
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"
        DmaBufSync.destroySemaphore device sem
        VkRaw.vkDestroyCommandPool(device.Handle, pool, NativePtr.zero)
        sharedEvent
