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
    /// `sem`, copy it to a host buffer and return the FULL BGRA bytes. Shared by the
    /// cross-instance consumer and the producer's same-instance control readback.
    let private copyToHost (device : Device) (image : VkImage) (w : int) (h : int)
                           (srcQF : uint32) (dstQF : uint32) (oldLayout : VkImageLayout)
                           (sem : VkSemaphore) (wait : bool) : byte[] =
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
        let buf = Array.zeroCreate<byte> (int bufSize)
        Marshal.Copy(ptr, buf, 0, int bufSize)
        VkRaw.vkUnmapMemory(dev, bmem)
        buf

    /// extract pixel (x,y) as (R,G,B,A) from a BGRA buffer.
    let pixelAt (buf : byte[]) (w : int) (x : int) (y : int) : int * int * int * int =
        let o = (y * w + x) * 4
        (int buf.[o + 2], int buf.[o + 1], int buf.[o + 0], int buf.[o + 3])

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
        let buf =
            copyToHost device img.Image img.Width img.Height
                       QUEUE_FAMILY_IGNORED QUEUE_FAMILY_IGNORED VkImageLayout.General
                       Unchecked.defaultof<VkSemaphore> false
        pixelAt buf img.Width (img.Width / 2) (img.Height / 2)

    /// Same-instance full readback (for the producer-side gradient fidelity control).
    let readAllLocal (device : Device) (img : OpaqueImage) : byte[] =
        copyToHost device img.Image img.Width img.Height
                   QUEUE_FAMILY_IGNORED QUEUE_FAMILY_IGNORED VkImageLayout.General
                   Unchecked.defaultof<VkSemaphore> false

    /// PRODUCER fill: CPU-write a 2-axis gradient (R=x-ramp, G=y-ramp, B=128) into a staging
    /// buffer and copy it into the OPAQUE export image, leaving it GENERAL + released to
    /// EXTERNAL (matches the consumer's acquire). A solid colour can't reveal a scrambled
    /// tiling; this gradient can.
    let fillGradient (device : Device) (img : OpaqueImage) =
        let dev = device.Handle
        let w, h = img.Width, img.Height
        let bufSize = uint64 (w * h * 4)
        let mutable bufInfo =
            VkBufferCreateInfo(VkBufferCreateFlags.None, bufSize, VkBufferUsageFlags.TransferSrcBit,
                               VkSharingMode.Exclusive, 0u, NativePtr.zero)
        let mutable buffer = VkBuffer.Null
        VkRaw.vkCreateBuffer(dev, &&bufInfo, NativePtr.zero, &&buffer) |> check "vkCreateBuffer(grad)"
        let mutable breq = VkMemoryRequirements()
        VkRaw.vkGetBufferMemoryRequirements(dev, buffer, &&breq)
        let mutable balloc = VkMemoryAllocateInfo(0n, breq.size, hostVisibleType device breq.memoryTypeBits)
        let mutable bmem = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&balloc, NativePtr.zero, &&bmem) |> check "vkAllocateMemory(grad)"
        VkRaw.vkBindBufferMemory(dev, buffer, bmem, 0UL) |> check "vkBindBufferMemory(grad)"

        let mutable ptr = 0n
        VkRaw.vkMapMemory(dev, bmem, 0UL, bufSize, VkMemoryMapFlags.None, &&ptr) |> check "vkMapMemory(grad)"
        for y in 0 .. h - 1 do
            for x in 0 .. w - 1 do
                let o = (y * w + x) * 4
                Marshal.WriteByte(ptr, o + 0, 128uy)                          // B
                Marshal.WriteByte(ptr, o + 1, byte (y * 255 / (h - 1)))       // G
                Marshal.WriteByte(ptr, o + 2, byte (x * 255 / (w - 1)))       // R
                Marshal.WriteByte(ptr, o + 3, 255uy)                          // A
        VkRaw.vkUnmapMemory(dev, bmem)

        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.None, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(dev, &&poolInfo, NativePtr.zero, &&pool) |> check "vkCreateCommandPool(grad)"
        let mutable cai = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable cmd = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(dev, &&cai, &&cmd) |> check "vkAllocateCommandBuffers(grad)"
        let mutable cbi = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&cbi) |> check "vkBeginCommandBuffer(grad)"

        let range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        let mutable b1 =
            VkImageMemoryBarrier(0n, VkAccessFlags.None, VkAccessFlags.TransferWriteBit,
                                 VkImageLayout.Undefined, VkImageLayout.TransferDstOptimal,
                                 QUEUE_FAMILY_IGNORED, QUEUE_FAMILY_IGNORED, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TopOfPipeBit, VkPipelineStageFlags.TransferBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b1)
        let subres = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkBufferImageCopy(0UL, 0u, 0u, subres, VkOffset3D(0, 0, 0), VkExtent3D(uint32 w, uint32 h, 1u))
        VkRaw.vkCmdCopyBufferToImage(cmd, buffer, img.Image, VkImageLayout.TransferDstOptimal, 1u, &&region)
        let mutable b2 =
            VkImageMemoryBarrier(0n, VkAccessFlags.TransferWriteBit, VkAccessFlags.None,
                                 VkImageLayout.TransferDstOptimal, VkImageLayout.General,
                                 qfi, QUEUE_FAMILY_EXTERNAL, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TransferBit, VkPipelineStageFlags.BottomOfPipeBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b2)
        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer(grad)"
        let mutable pcmd = cmd
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit(grad)"
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle(grad)"
        VkRaw.vkDestroyCommandPool(dev, pool, NativePtr.zero)
        VkRaw.vkDestroyBuffer(dev, buffer, NativePtr.zero)
        VkRaw.vkFreeMemory(dev, bmem, NativePtr.zero)

    /// CONSUMER (separate VkInstance): import the OPAQUE_FD memory into a byte-identical image,
    /// acquire from VK_QUEUE_FAMILY_EXTERNAL (optionally waiting the producer's sync_fd), copy
    /// to host and return the centre pixel. `useSemaphore` toggles the acquire-semaphore wait;
    /// `useGeneralLayout` toggles the acquire oldLayout (GENERAL preserves, UNDEFINED discards).
    let private importImage (device : Device) (memFd : int) (syncFd : int) (w : int) (h : int)
                            (useSemaphore : bool) : VkImage * VkSemaphore * bool =
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
        (image, sem, wait)

    let importAndRead (device : Device) (memFd : int) (syncFd : int) (w : int) (h : int)
                      (useSemaphore : bool) (useGeneralLayout : bool) : int * int * int * int =
        let (image, sem, wait) = importImage device memFd syncFd w h useSemaphore
        let oldLayout = if useGeneralLayout then VkImageLayout.General else VkImageLayout.Undefined
        let buf =
            copyToHost device image w h QUEUE_FAMILY_EXTERNAL (uint32 device.GraphicsFamily.Index)
                       oldLayout sem wait
        pixelAt buf w (w / 2) (h / 2)

    /// Cross-instance full readback (for the gradient tiling-fidelity check).
    let importAndReadAll (device : Device) (memFd : int) (syncFd : int) (w : int) (h : int)
                         (useSemaphore : bool) (useGeneralLayout : bool) : byte[] =
        let (image, sem, wait) = importImage device memFd syncFd w h useSemaphore
        let oldLayout = if useGeneralLayout then VkImageLayout.General else VkImageLayout.Undefined
        copyToHost device image w h QUEUE_FAMILY_EXTERNAL (uint32 device.GraphicsFamily.Index)
                   oldLayout sem wait

    /// A cross-instance imported OPAQUE_FD image kept alive for REPEATED re-acquire reads.
    type ImportedImage = { Image : VkImage; Memory : VkDeviceMemory; W : int; H : int }

    /// CONSUMER: import the OPAQUE_FD memory ONCE into a byte-identical image and keep it.
    let importOnce (device : Device) (memFd : int) (w : int) (h : int) : ImportedImage =
        let dev = device.Handle
        let image = makeImage device w h
        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = deviceLocalType device req.memoryTypeBits
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable importInfo = KHRExternalMemoryFd.VkImportMemoryFdInfoKHR(OpaqueFdMem, memFd)
        importInfo.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&importInfo, req.size, memType)
        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&memory) |> check "vkAllocateMemory(importOnce)"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory(importOnce)"
        { Image = image; Memory = memory; W = w; H = h }

    /// CONSUMER per-iteration (the crux of the repeated-in-place-write test): RE-acquire the
    /// already-imported `img` from VK_QUEUE_FAMILY_EXTERNAL (oldLayout GENERAL — what the
    /// producer re-released each iteration), optionally waiting a FRESH per-frame sync_fd,
    /// copy out and return the centre pixel. Re-runs the queue-family acquire (availability
    /// op) EVERY call — that is precisely what should surface the producer's newest write.
    let readCenterCross (device : Device) (img : ImportedImage) (syncFd : int) (useSemaphore : bool) : int * int * int * int =
        let dev = device.Handle
        let mutable sem = Unchecked.defaultof<VkSemaphore>
        let wait = useSemaphore && syncFd >= 0
        if wait then
            let mutable sinfo = VkSemaphoreCreateInfo(VkSemaphoreCreateFlags.None)
            VkRaw.vkCreateSemaphore(dev, &&sinfo, NativePtr.zero, &&sem) |> check "vkCreateSemaphore(rd)"
            let mutable imp =
                KHRExternalSemaphoreFd.VkImportSemaphoreFdInfoKHR(
                    sem, VkSemaphoreImportFlags.TemporaryBit,
                    VkExternalSemaphoreHandleTypeFlags.SyncFdBit, syncFd)
            KHRExternalSemaphoreFd.VkRaw.vkImportSemaphoreFdKHR(dev, &&imp) |> check "vkImportSemaphoreFdKHR(rd)"
        let buf =
            copyToHost device img.Image img.W img.H QUEUE_FAMILY_EXTERNAL (uint32 device.GraphicsFamily.Index)
                       VkImageLayout.General sem wait
        if wait then VkRaw.vkDestroySemaphore(dev, sem, NativePtr.zero)
        pixelAt buf img.W (img.W / 2) (img.H / 2)
