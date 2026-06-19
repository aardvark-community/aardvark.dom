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

        // dst -> GENERAL, make memory available to the external consumer
        barrier cmd dstImage VkAccessFlags.TransferWriteBit VkAccessFlags.MemoryReadBit
                VkImageLayout.TransferDstOptimal VkImageLayout.General
                VkPipelineStageFlags.TransferBit VkPipelineStageFlags.BottomOfPipeBit

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

    /// macOS: GPU-clear `src` + copy into the IOSurface-backed image `dst`. Fence via
    /// MTLSharedEvent is a follow-up; for now drains on the queue.
    let clearAndCopyMetal (device : Device) (src : VkImage) (dst : MetalSharedImage) (color : V4f) : unit =
        let sem = DmaBufSync.createSemaphore device
        let struct (queue, pool) = recordClearCopySubmit device src dst.Image dst.Width dst.Height color sem
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"
        DmaBufSync.destroySemaphore device sem
        VkRaw.vkDestroyCommandPool(device.Handle, pool, NativePtr.zero)
