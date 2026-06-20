namespace Aardvark.Dom.Remote.SharedTexture

// Standalone OPAQUE_FD cross-process Vulkan sharing test. OPAQUE_FD is the Vulkan-native,
// same-driver external-memory handle (vs dma-buf = cross-API interop). On NVIDIA the dma-buf
// import into a FOREIGN Vulkan instance reads blank even with the full ownership+semaphore
// handshake (driver wall), while EGL/CPU reads work. This checks whether OPAQUE_FD surfaces
// the content cross-instance when GPU-sampled — the Vulkan-to-Vulkan path NVIDIA actually
// supports (CUDA/Vulkan interop, multi-process Vulkan).
//
// OPAQUE_FD is OPAQUE: no modifier/layout negotiation, so producer and consumer MUST create
// the image with byte-identical VkImageCreateInfo — enforced here via the single `makeImage`
// helper used by both sides.

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

/// An OPTIMAL device-local colour image exported via OPAQUE_FD.
type OpaqueImage =
    {
        MemFd  : int
        Width  : int
        Height : int
        Size   : uint64
        Image  : VkImage
        Memory : VkDeviceMemory
    }

module OpaqueFd =

    let private OpaqueFdMem : VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlags.OpaqueFdBit
    let private QUEUE_FAMILY_EXTERNAL = 0xFFFFFFF1u
    let private QUEUE_FAMILY_IGNORED = 0xFFFFFFFFu

    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then failwithf "[OpaqueFd] %s failed: %A" what res

    let private deviceLocalType (device : Device) (typeBits : uint32) =
        device.PhysicalDevice.MemoryTypes
        |> Array.find (fun mt ->
            (typeBits &&& (1u <<< mt.index)) <> 0u &&
            (int mt.flags &&& int VkMemoryPropertyFlags.DeviceLocalBit) <> 0)
        |> fun mt -> uint32 mt.index

    let private hostVisibleType (device : Device) (typeBits : uint32) =
        device.PhysicalDevice.MemoryTypes
        |> Array.find (fun mt ->
            (typeBits &&& (1u <<< mt.index)) <> 0u &&
            (int mt.flags &&& int VkMemoryPropertyFlags.HostVisibleBit) <> 0 &&
            (int mt.flags &&& int VkMemoryPropertyFlags.HostCoherentBit) <> 0)
        |> fun mt -> uint32 mt.index

    /// SHARED: the EXACT VkImageCreateInfo both producer and consumer use — OPTIMAL, marked
    /// for OPAQUE_FD external memory, with TRANSFER_SRC so it can be copied out for readback.
    /// Byte-identical on both sides (OPAQUE_FD has no layout negotiation).
    let private makeImage (device : Device) (w : int) (h : int) : VkImage =
        let dev = device.Handle
        let mutable ext = VkExternalMemoryImageCreateInfo(OpaqueFdMem)
        let mutable info =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&ext, VkImageCreateFlags.None, VkImageType.D2d, VkFormat.B8g8r8a8Unorm,
                VkExtent3D(uint32 w, uint32 h, 1u), 1u, 1u, VkSampleCountFlags.D1Bit, VkImageTiling.Optimal,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit ||| VkImageUsageFlags.SampledBit,
                VkSharingMode.Exclusive, 0u, NativePtr.zero, VkImageLayout.Undefined)
        let mutable image = VkImage.Null
        VkRaw.vkCreateImage(dev, &&info, NativePtr.zero, &&image) |> check "vkCreateImage"
        image

    /// Acquire `image` (oldLayout, from queue family `srcQF` to `dstQF`), optionally waiting
    /// `sem`, copy it to a host buffer and return the centre pixel (R,G,B,A). Shared by the
    /// cross-instance consumer and the producer's same-instance control readback.
    let private copyCenterToHost (device : Device) (image : VkImage) (w : int) (h : int)
                                 (srcQF : uint32) (dstQF : uint32) (oldLayout : VkImageLayout)
                                 (sem : VkSemaphore) (wait : bool) : int * int * int * int =
        let dev = device.Handle
        let bufSize = uint64 (w * h * 4)
        let mutable bufInfo =
            VkBufferCreateInfo(VkBufferCreateFlags.None, bufSize, VkBufferUsageFlags.TransferDstBit,
                               VkSharingMode.Exclusive, 0u, NativePtr.zero)
        let mutable buffer = VkBuffer.Null
        VkRaw.vkCreateBuffer(dev, &&bufInfo, NativePtr.zero, &&buffer) |> check "vkCreateBuffer"
        let mutable breq = VkMemoryRequirements()
        VkRaw.vkGetBufferMemoryRequirements(dev, buffer, &&breq)
        let mutable balloc = VkMemoryAllocateInfo(0n, breq.size, hostVisibleType device breq.memoryTypeBits)
        let mutable bmem = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&balloc, NativePtr.zero, &&bmem) |> check "vkAllocateMemory(buf)"
        VkRaw.vkBindBufferMemory(dev, buffer, bmem, 0UL) |> check "vkBindBufferMemory"

        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.None, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(dev, &&poolInfo, NativePtr.zero, &&pool) |> check "vkCreateCommandPool"
        let mutable ai = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable cmd = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(dev, &&ai, &&cmd) |> check "vkAllocateCommandBuffers"
        let mutable bi = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&bi) |> check "vkBeginCommandBuffer"

        let range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        let mutable acq =
            VkImageMemoryBarrier(0n, VkAccessFlags.None, VkAccessFlags.TransferReadBit,
                                 oldLayout, VkImageLayout.TransferSrcOptimal, srcQF, dstQF, image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TopOfPipeBit, VkPipelineStageFlags.TransferBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&acq)

        let subres = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkBufferImageCopy(0UL, 0u, 0u, subres, VkOffset3D(0, 0, 0), VkExtent3D(uint32 w, uint32 h, 1u))
        VkRaw.vkCmdCopyImageToBuffer(cmd, image, VkImageLayout.TransferSrcOptimal, buffer, 1u, &&region)
        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer"

        let mutable pcmd = cmd
        if wait then
            let mutable psem = sem
            let mutable waitStage = VkPipelineStageFlags.TransferBit
            let mutable submit = VkSubmitInfo(0n, 1u, &&psem, &&waitStage, 1u, &&pcmd, 0u, NativePtr.zero)
            VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit"
        else
            let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
            VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit"
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"

        let mutable ptr = 0n
        VkRaw.vkMapMemory(dev, bmem, 0UL, bufSize, VkMemoryMapFlags.None, &&ptr) |> check "vkMapMemory"
        let off = ((h / 2) * w + (w / 2)) * 4
        let b = int (Marshal.ReadByte(ptr, off + 0))
        let g = int (Marshal.ReadByte(ptr, off + 1))
        let r = int (Marshal.ReadByte(ptr, off + 2))
        let a = int (Marshal.ReadByte(ptr, off + 3))
        VkRaw.vkUnmapMemory(dev, bmem)
        (r, g, b, a)

    /// PRODUCER: create an OPTIMAL device-local image exported as OPAQUE_FD.
    let create (device : Device) (w : int) (h : int) : OpaqueImage =
        let dev = device.Handle
        let image = makeImage device w h
        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = deviceLocalType device req.memoryTypeBits
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable export = VkExportMemoryAllocateInfo(OpaqueFdMem)
        export.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&export, req.size, memType)
        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&memory) |> check "vkAllocateMemory"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory"
        let mutable getFd = KHRExternalMemoryFd.VkMemoryGetFdInfoKHR(memory, OpaqueFdMem)
        let mutable fd = -1
        KHRExternalMemoryFd.VkRaw.vkGetMemoryFdKHR(dev, &&getFd, &&fd) |> check "vkGetMemoryFdKHR"
        { MemFd = fd; Width = w; Height = h; Size = uint64 req.size; Image = image; Memory = memory }

    let destroy (device : Device) (img : OpaqueImage) =
        VkRaw.vkDestroyImage(device.Handle, img.Image, NativePtr.zero)
        VkRaw.vkFreeMemory(device.Handle, img.Memory, NativePtr.zero)

    /// CONTROL: same-instance readback of the producer's own (already filled, GENERAL-layout)
    /// image. If this is correct but the cross-instance consumer is zero, the zero is a REAL
    /// cross-instance result; if this is also zero, the fill/usage/readback path has a bug.
    let readCenterLocal (device : Device) (img : OpaqueImage) : int * int * int * int =
        copyCenterToHost device img.Image img.Width img.Height
                         QUEUE_FAMILY_IGNORED QUEUE_FAMILY_IGNORED VkImageLayout.General
                         Unchecked.defaultof<VkSemaphore> false

    /// CONSUMER (separate VkInstance): import the OPAQUE_FD memory into a byte-identical image,
    /// acquire from VK_QUEUE_FAMILY_EXTERNAL (optionally waiting the producer's sync_fd), copy
    /// to host and return the centre pixel. `useSemaphore` toggles the acquire-semaphore wait;
    /// `useGeneralLayout` toggles the acquire oldLayout (GENERAL preserves, UNDEFINED discards).
    let importAndRead (device : Device) (memFd : int) (syncFd : int) (w : int) (h : int)
                      (useSemaphore : bool) (useGeneralLayout : bool) : int * int * int * int =
        let dev = device.Handle
        let image = makeImage device w h
        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = deviceLocalType device req.memoryTypeBits

        // import the opaque memory (dedicated): alloc.pNext -> importFd -> dedicated
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable importInfo = KHRExternalMemoryFd.VkImportMemoryFdInfoKHR(OpaqueFdMem, memFd)
        importInfo.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&importInfo, req.size, memType)
        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&memory) |> check "vkAllocateMemory(import)"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory(import)"

        // optionally import the sync_fd as a binary semaphore (TEMPORARY import for SYNC_FD)
        let mutable sem = Unchecked.defaultof<VkSemaphore>
        let wait = useSemaphore && syncFd >= 0
        if wait then
            let mutable sinfo = VkSemaphoreCreateInfo(VkSemaphoreCreateFlags.None)
            VkRaw.vkCreateSemaphore(dev, &&sinfo, NativePtr.zero, &&sem) |> check "vkCreateSemaphore"
            let mutable imp =
                KHRExternalSemaphoreFd.VkImportSemaphoreFdInfoKHR(
                    sem, VkSemaphoreImportFlags.TemporaryBit,
                    VkExternalSemaphoreHandleTypeFlags.SyncFdBit, syncFd)
            KHRExternalSemaphoreFd.VkRaw.vkImportSemaphoreFdKHR(dev, &&imp) |> check "vkImportSemaphoreFdKHR"

        let oldLayout = if useGeneralLayout then VkImageLayout.General else VkImageLayout.Undefined
        copyCenterToHost device image w h QUEUE_FAMILY_EXTERNAL (uint32 device.GraphicsFamily.Index)
                         oldLayout sem wait
