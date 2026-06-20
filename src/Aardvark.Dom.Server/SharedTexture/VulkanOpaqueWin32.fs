namespace Aardvark.Dom.Remote.SharedTexture

// Standalone OPAQUE_WIN32 cross-process / cross-VkInstance sharing test for Windows —
// the de-risk before the Chromium Windows port, mirroring VulkanOpaqueFd.fs (the OPAQUE_FD
// test that cracked Linux). OPAQUE_WIN32 is the Vulkan-native, same-driver external-memory
// handle on Windows (an NT handle), analog of OPAQUE_FD on Linux. Question: does a separate
// process with a separate VkInstance read a producer's REPEATED in-place writes to ONE image
// shared via VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT?
//
// OPAQUE_WIN32, like OPAQUE_FD, is OPAQUE: no modifier/layout negotiation, so producer and
// consumer MUST create the image with byte-identical VkImageCreateInfo — enforced here via
// the single `makeImage` helper used by both sides. OPTIMAL device-local, GPU copy-out
// readback — the real test, identical methodology to the OPAQUE_FD one.

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

/// An OPTIMAL device-local colour image exported via OPAQUE_WIN32 (an NT handle).
type OpaqueWin32Image =
    {
        Handle : nativeint
        Width  : int
        Height : int
        Size   : uint64
        Image  : VkImage
        Memory : VkDeviceMemory
    }

module OpaqueWin32 =

    let private OpaqueWin32Mem : VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlags.OpaqueWin32Bit
    let private QUEUE_FAMILY_EXTERNAL = 0xFFFFFFF1u
    let private QUEUE_FAMILY_IGNORED = 0xFFFFFFFFu

    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then failwithf "[OpaqueWin32] %s failed: %A" what res

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
    /// for OPAQUE_WIN32 external memory, usage TRANSFER_SRC|TRANSFER_DST|SAMPLED. Byte-identical
    /// on both sides (OPAQUE_WIN32 has no layout negotiation).
    let private makeImage (device : Device) (w : int) (h : int) : VkImage =
        let dev = device.Handle
        let mutable ext = VkExternalMemoryImageCreateInfo(OpaqueWin32Mem)
        let mutable info =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&ext, VkImageCreateFlags.None, VkImageType.D2d, VkFormat.B8g8r8a8Unorm,
                VkExtent3D(uint32 w, uint32 h, 1u), 1u, 1u, VkSampleCountFlags.D1Bit, VkImageTiling.Optimal,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit ||| VkImageUsageFlags.SampledBit,
                VkSharingMode.Exclusive, 0u, NativePtr.zero, VkImageLayout.Undefined)
        let mutable image = VkImage.Null
        VkRaw.vkCreateImage(dev, &&info, NativePtr.zero, &&image) |> check "vkCreateImage"
        image

    /// Acquire `image` (oldLayout, from queue family `srcQF` to `dstQF`), copy it to a host
    /// buffer and return the FULL BGRA bytes. Shared by the cross-instance consumer and the
    /// producer's same-instance control readback. No semaphore (AMD-permissive baseline; the
    /// no-semaphore variant is exactly what we want to learn what's actually required).
    let private copyToHost (device : Device) (image : VkImage) (w : int) (h : int)
                           (srcQF : uint32) (dstQF : uint32) (oldLayout : VkImageLayout) : byte[] =
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
        let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit"
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle"

        let mutable ptr = 0n
        VkRaw.vkMapMemory(dev, bmem, 0UL, bufSize, VkMemoryMapFlags.None, &&ptr) |> check "vkMapMemory"
        let buf = Array.zeroCreate<byte> (int bufSize)
        Marshal.Copy(ptr, buf, 0, int bufSize)
        VkRaw.vkUnmapMemory(dev, bmem)
        VkRaw.vkDestroyCommandPool(dev, pool, NativePtr.zero)
        VkRaw.vkDestroyBuffer(dev, buffer, NativePtr.zero)
        VkRaw.vkFreeMemory(dev, bmem, NativePtr.zero)
        buf

    /// extract pixel (x,y) as (R,G,B,A) from a BGRA buffer.
    let pixelAt (buf : byte[]) (w : int) (x : int) (y : int) : int * int * int * int =
        let o = (y * w + x) * 4
        (int buf.[o + 2], int buf.[o + 1], int buf.[o + 0], int buf.[o + 3])

    /// PRODUCER: create an OPTIMAL device-local image exported as an OPAQUE_WIN32 NT handle.
    let create (device : Device) (w : int) (h : int) : OpaqueWin32Image =
        let dev = device.Handle
        let image = makeImage device w h
        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = deviceLocalType device req.memoryTypeBits
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable export = VkExportMemoryAllocateInfo(OpaqueWin32Mem)
        export.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&export, req.size, memType)
        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&memory) |> check "vkAllocateMemory"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory"
        let mutable getInfo = KHRExternalMemoryWin32.VkMemoryGetWin32HandleInfoKHR(memory, OpaqueWin32Mem)
        let mutable handle = 0n
        KHRExternalMemoryWin32.VkRaw.vkGetMemoryWin32HandleKHR(dev, &&getInfo, &&handle) |> check "vkGetMemoryWin32HandleKHR"
        { Handle = handle; Width = w; Height = h; Size = uint64 req.size; Image = image; Memory = memory }

    let destroy (device : Device) (img : OpaqueWin32Image) =
        VkRaw.vkDestroyImage(device.Handle, img.Image, NativePtr.zero)
        VkRaw.vkFreeMemory(device.Handle, img.Memory, NativePtr.zero)

    /// CONTROL: same-instance readback of the producer's own (already filled, GENERAL-layout)
    /// image. If this cycles but the cross-instance consumer freezes, the freeze is a REAL
    /// cross-instance result; if this freezes too, the fill/usage/readback path has a bug.
    let readCenterLocal (device : Device) (img : OpaqueWin32Image) : int * int * int * int =
        let buf =
            copyToHost device img.Image img.Width img.Height
                       QUEUE_FAMILY_IGNORED QUEUE_FAMILY_IGNORED VkImageLayout.General
        pixelAt buf img.Width (img.Width / 2) (img.Height / 2)

    /// A cross-instance imported OPAQUE_WIN32 image kept alive for REPEATED re-acquire reads.
    type ImportedImage = { Image : VkImage; Memory : VkDeviceMemory; W : int; H : int }

    /// CONSUMER (separate VkInstance): import the OPAQUE_WIN32 memory ONCE into a byte-identical
    /// image and keep it. `handle` must already be valid IN THIS process (DuplicateHandle'd).
    let importOnce (device : Device) (handle : nativeint) (w : int) (h : int) : ImportedImage =
        let dev = device.Handle
        let image = makeImage device w h
        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = deviceLocalType device req.memoryTypeBits
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        // alloc.pNext -> importWin32 -> dedicated. ctor: (handleType, handle, name : byte*)
        let mutable importInfo =
            KHRExternalMemoryWin32.VkImportMemoryWin32HandleInfoKHR(OpaqueWin32Mem, handle, NativePtr.zero)
        importInfo.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&importInfo, req.size, memType)
        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&memory) |> check "vkAllocateMemory(importOnce)"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory(importOnce)"
        { Image = image; Memory = memory; W = w; H = h }

    /// CONSUMER per-iteration (the crux of the repeated-in-place-write test): RE-acquire the
    /// already-imported `img` from VK_QUEUE_FAMILY_EXTERNAL (oldLayout GENERAL — what the
    /// producer re-released each iteration), copy out and return the centre pixel. Re-runs the
    /// queue-family acquire EVERY call — precisely what should surface the producer's newest write.
    let readCenterCross (device : Device) (img : ImportedImage) : int * int * int * int =
        let buf =
            copyToHost device img.Image img.W img.H QUEUE_FAMILY_EXTERNAL (uint32 device.GraphicsFamily.Index)
                       VkImageLayout.General
        pixelAt buf img.W (img.W / 2) (img.H / 2)

    /// CONSUMER per-iteration NO-EXTERNAL-ACQUIRE variant (AMD-permissive probe): acquire with
    /// srcQF=dstQF=IGNORED (NO queue-family ownership transfer from EXTERNAL) and oldLayout=GENERAL.
    /// Tells us whether AMD needs the EXTERNAL acquire at all to see fresh writes.
    let readCenterCrossNoExternal (device : Device) (img : ImportedImage) : int * int * int * int =
        let buf =
            copyToHost device img.Image img.W img.H QUEUE_FAMILY_IGNORED QUEUE_FAMILY_IGNORED
                       VkImageLayout.General
        pixelAt buf img.W (img.W / 2) (img.H / 2)
