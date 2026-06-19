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

    /// GPU-clear `src` to `color` (R,G,B,A) and copy it into the dma-buf `dst`.
    /// Fully on the GPU; the dma-buf is never mapped on the CPU. Returns a sync_fd
    /// fence that signals when the GPU work completes (-1 = already-signaled).
    let clearAndCopy (device : Device) (src : VkImage) (dst : DmaBufImage) (color : V4f) : int =
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

        // dst dma-buf: UNDEFINED -> TRANSFER_DST
        barrier cmd dst.Image VkAccessFlags.None VkAccessFlags.TransferWriteBit
                VkImageLayout.Undefined VkImageLayout.TransferDstOptimal
                VkPipelineStageFlags.TopOfPipeBit VkPipelineStageFlags.TransferBit

        // copy src -> dst
        let layers = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkImageCopy(layers, VkOffset3D(0, 0, 0), layers, VkOffset3D(0, 0, 0),
                        VkExtent3D(uint32 dst.Width, uint32 dst.Height, 1u))
        VkRaw.vkCmdCopyImage(cmd, src, VkImageLayout.TransferSrcOptimal,
                             dst.Image, VkImageLayout.TransferDstOptimal, 1u, &&region)

        // dst -> GENERAL, make memory available to the external (EGL) consumer
        barrier cmd dst.Image VkAccessFlags.TransferWriteBit VkAccessFlags.MemoryReadBit
                VkImageLayout.TransferDstOptimal VkImageLayout.General
                VkPipelineStageFlags.TransferBit VkPipelineStageFlags.BottomOfPipeBit

        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer"

        // exportable fence: the submit signals `sem` on completion; we export a
        // sync_fd from it (while the GPU work is still pending → a real fence fd)
        // so the consumer can GPU-wait instead of us stalling.
        let sem = DmaBufSync.createExportableSemaphore device
        let mutable psem = sem
        let mutable pcmd = cmd
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 1u, &&psem)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit"

        let syncFd = DmaBufSync.exportSyncFd device sem

        // (test only) drain so we can safely tear down the one-shot pool; the real
        // path drops this and lets the consumer wait on `syncFd` instead.
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"
        DmaBufSync.destroySemaphore device sem
        VkRaw.vkDestroyCommandPool(dev, pool, NativePtr.zero)
        syncFd
