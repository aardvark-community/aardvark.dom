namespace Aardvark.Dom.Remote.SharedTexture

// Windows producer-side export of a Vulkan image as a D3D11-openable DXGI shared NT handle —
// the W0-analog for the PRODUCTION Windows path (Chromium's default D3DImageBacking import).
//
// Unlike VulkanOpaqueWin32 (Vulkan<->Vulkan, OPAQUE_WIN32), this exports the image with
// VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT so a *D3D11* consumer
// (ID3D11Device::OpenSharedResource1) can open it. The handle is a DXGI shared NT handle.
//
//  - Image: B8G8R8A8_UNORM (-> DXGI_FORMAT_B8G8R8A8_UNORM), OPTIMAL,
//    usage TRANSFER_DST|TRANSFER_SRC|SAMPLED.
//  - VkExternalMemoryImageCreateInfo(D3d11TextureBit) on the image; dedicated alloc with
//    VkExportMemoryAllocateInfo(D3d11TextureBit); export via vkGetMemoryWin32HandleKHR(D3d11TextureBit).
//  - Optional KEYED MUTEX: D3D11 cross-device shared textures usually require a keyed mutex.
//    If `keyedMutex` is true the producer acquires key 0 / releases key `releaseKey` around the
//    GPU fill via VkWin32KeyedMutexAcquireReleaseInfoKHR in the submit; the D3D11 consumer then
//    acquires/releases the same keys via IDXGIKeyedMutex. Requires VK_KHR_win32_keyed_mutex on
//    the device (the caller enables it). We test the no-keyed-mutex path first (AMD was permissive
//    for OPAQUE_WIN32 — waitIdle sufficed) and fall back to the keyed mutex if the open/read fails.

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

/// A B8G8R8A8 OPTIMAL image exported as a D3D11-openable DXGI shared NT handle.
type D3D11SharedImage =
    {
        /// DXGI shared NT handle (openable by ID3D11Device::OpenSharedResource1)
        Handle      : nativeint
        Width       : int
        Height      : int
        Size        : uint64
        Image       : VkImage
        Memory      : VkDeviceMemory
        /// whether this image was created WITH a keyed mutex (consumer must match)
        KeyedMutex  : bool
    }

module D3D11Export =

    /// The handle type both export-image-create-info and export-alloc-info use.
    let private D3D11Mem : VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlags.D3d11TextureBit
    let private QUEUE_FAMILY_EXTERNAL = 0xFFFFFFF1u

    /// VK_IMAGE_CREATE flag that D3D11 keyed-mutex sharing on some drivers wants; not strictly
    /// required for export, but harmless. (We don't set MUTABLE/ALIAS here.)
    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then failwithf "[D3D11Export] %s failed: %A" what res

    /// Same LUID query as OpaqueWin32 — the D3D11 consumer must select the DXGI adapter with the
    /// matching LUID (IDXGIFactory1::EnumAdapters -> DXGI_ADAPTER_DESC.AdapterLuid).
    let deviceLUID (device : Device) : bool * byte[] * string =
        let phys = device.PhysicalDevice.Handle
        let mutable idProps = VkPhysicalDeviceIDProperties.Empty
        let mutable props2 = VkPhysicalDeviceProperties2.Empty
        props2.pNext <- NativePtr.toNativeInt &&idProps
        VkRaw.vkGetPhysicalDeviceProperties2(phys, &&props2)
        let luid = Array.zeroCreate<byte> 8
        let src = idProps.deviceLUID
        for i in 0 .. 7 do luid.[i] <- src.[i]
        let valid = idProps.deviceLUIDValid <> 0u
        let hex = luid |> Array.map (sprintf "%02X") |> String.concat ""
        (valid, luid, hex)

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

    /// Create the export image (B8G8R8A8_UNORM, OPTIMAL, marked for D3D11_TEXTURE external memory).
    let private makeImage (device : Device) (w : int) (h : int) : VkImage =
        let dev = device.Handle
        let mutable ext = VkExternalMemoryImageCreateInfo(D3D11Mem)
        let mutable info =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&ext, VkImageCreateFlags.None, VkImageType.D2d, VkFormat.B8g8r8a8Unorm,
                VkExtent3D(uint32 w, uint32 h, 1u), 1u, 1u, VkSampleCountFlags.D1Bit, VkImageTiling.Optimal,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit ||| VkImageUsageFlags.SampledBit,
                VkSharingMode.Exclusive, 0u, NativePtr.zero, VkImageLayout.Undefined)
        let mutable image = VkImage.Null
        VkRaw.vkCreateImage(dev, &&info, NativePtr.zero, &&image) |> check "vkCreateImage"
        image

    /// Query whether the device supports exporting B8G8R8A8 OPTIMAL images as D3D11_TEXTURE,
    /// and report the export feature flags (EXPORTABLE / DEDICATED_ONLY). Diagnostic.
    let queryExportSupport (device : Device) : bool * VkExternalMemoryFeatureFlags =
        let phys = device.PhysicalDevice.Handle
        let mutable extInfo =
            VkPhysicalDeviceExternalImageFormatInfo(D3D11Mem)
        let mutable fmtInfo =
            VkPhysicalDeviceImageFormatInfo2(
                NativePtr.toNativeInt &&extInfo, VkFormat.B8g8r8a8Unorm, VkImageType.D2d, VkImageTiling.Optimal,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit ||| VkImageUsageFlags.SampledBit,
                VkImageCreateFlags.None)
        let mutable extProps = VkExternalImageFormatProperties.Empty
        let mutable props = VkImageFormatProperties2.Empty
        props.pNext <- NativePtr.toNativeInt &&extProps
        let r = VkRaw.vkGetPhysicalDeviceImageFormatProperties2(phys, &&fmtInfo, &&props)
        let feat = extProps.externalMemoryProperties.externalMemoryFeatures
        let exportable = (r = VkResult.Success) && (int feat &&& int VkExternalMemoryFeatureFlags.ExportableBit) <> 0
        (exportable, feat)

    /// PRODUCER: create an OPTIMAL device-local image exported as a D3D11_TEXTURE DXGI shared NT
    /// handle. If `keyedMutex` is true the image is intended to be used with a keyed mutex.
    let create (device : Device) (w : int) (h : int) (keyedMutex : bool) : D3D11SharedImage =
        let dev = device.Handle
        let image = makeImage device w h
        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = deviceLocalType device req.memoryTypeBits
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        // For a D3D11_TEXTURE export, D3D11 (OpenSharedResource1) only accepts the handle if it
        // carries DXGI shared-resource access rights. Chain VkExportMemoryWin32HandleInfoKHR with
        // dwAccess = DXGI_SHARED_RESOURCE_READ (0x80000000) | DXGI_SHARED_RESOURCE_WRITE (0x1).
        // Without this the handle opens with E_INVALIDARG. NULL pAttributes/name.
        let DXGI_SHARED_RESOURCE_READ  = 0x80000000u
        let DXGI_SHARED_RESOURCE_WRITE = 0x00000001u
        let mutable exportWin32 =
            Aardvark.Rendering.Vulkan.Extensions.KHRExternalMemoryWin32.VkExportMemoryWin32HandleInfoKHR(
                NativePtr.ofNativeInt 0n, DXGI_SHARED_RESOURCE_READ ||| DXGI_SHARED_RESOURCE_WRITE, NativePtr.zero)
        exportWin32.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable export = VkExportMemoryAllocateInfo(D3D11Mem)
        export.pNext <- NativePtr.toNativeInt &&exportWin32
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&export, req.size, memType)
        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&memory) |> check "vkAllocateMemory"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory"
        let mutable getInfo =
            Aardvark.Rendering.Vulkan.Extensions.KHRExternalMemoryWin32.VkMemoryGetWin32HandleInfoKHR(memory, D3D11Mem)
        let mutable handle = 0n
        Aardvark.Rendering.Vulkan.Extensions.KHRExternalMemoryWin32.VkRaw.vkGetMemoryWin32HandleKHR(dev, &&getInfo, &&handle)
        |> check "vkGetMemoryWin32HandleKHR(D3D11_TEXTURE)"
        { Handle = handle; Width = w; Height = h; Size = uint64 req.size; Image = image; Memory = memory; KeyedMutex = keyedMutex }

    // ====================================================================================
    // REVERSE (supported) direction: D3D11 ALLOCATES the shared keyed-mutex texture and exports
    // an NT handle (IDXGIResource1::CreateSharedHandle). Vulkan IMPORTS that memory
    // (VkImportMemoryWin32HandleInfoKHR(D3d11TextureBit) — the ImportableBit the driver supports),
    // GPU-fills a gradient, and releases the keyed mutex to the key D3D will acquire. This is the
    // production Chromium D3DImageBacking path (Chromium owns the texture, Aardvark renders into it).
    // ====================================================================================

    /// A Vulkan image whose memory was IMPORTED from a D3D11-allocated shared keyed-mutex texture.
    type ImportedD3D11Image = { Image : VkImage; Memory : VkDeviceMemory; W : int; H : int }

    /// VkImageCreateInfo MUST match the D3D11 allocation: B8G8R8A8_UNORM, OPTIMAL. D3D created it
    /// with BIND_SHADER_RESOURCE|BIND_RENDER_TARGET, so the Vulkan usage covers SAMPLED|COLOR_ATTACHMENT
    /// plus TRANSFER_DST|TRANSFER_SRC for the gradient copy. Marked for D3D11_TEXTURE external memory.
    let private makeImportImage (device : Device) (w : int) (h : int) : VkImage =
        let dev = device.Handle
        let mutable ext = VkExternalMemoryImageCreateInfo(D3D11Mem)
        let mutable info =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&ext, VkImageCreateFlags.None, VkImageType.D2d, VkFormat.B8g8r8a8Unorm,
                VkExtent3D(uint32 w, uint32 h, 1u), 1u, 1u, VkSampleCountFlags.D1Bit, VkImageTiling.Optimal,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit |||
                VkImageUsageFlags.SampledBit ||| VkImageUsageFlags.ColorAttachmentBit,
                VkSharingMode.Exclusive, 0u, NativePtr.zero, VkImageLayout.Undefined)
        let mutable image = VkImage.Null
        VkRaw.vkCreateImage(dev, &&info, NativePtr.zero, &&image) |> check "vkCreateImage(import)"
        image

    /// IMPORT the D3D11-allocated shared texture's memory into a matching Vulkan image.
    /// `handle` must already be valid IN THIS process (DuplicateHandle'd from the D3D11 allocator).
    let importD3D11 (device : Device) (handle : nativeint) (w : int) (h : int) : ImportedD3D11Image =
        let dev = device.Handle
        let image = makeImportImage device w h
        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = deviceLocalType device req.memoryTypeBits
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable importInfo =
            Aardvark.Rendering.Vulkan.Extensions.KHRExternalMemoryWin32.VkImportMemoryWin32HandleInfoKHR(
                D3D11Mem, handle, NativePtr.zero)
        importInfo.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&importInfo, req.size, memType)
        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&memory) |> check "vkAllocateMemory(importD3D11)"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory(importD3D11)"
        { Image = image; Memory = memory; W = w; H = h }

    let destroyImported (device : Device) (img : ImportedD3D11Image) =
        VkRaw.vkDestroyImage(device.Handle, img.Image, NativePtr.zero)
        VkRaw.vkFreeMemory(device.Handle, img.Memory, NativePtr.zero)

    /// GPU-fill the IMPORTED D3D11 texture with a 2-axis gradient, WRAPPED in a keyed-mutex
    /// acquire/release: acquire `acquireKey` (0 — the initial key D3D released the texture at),
    /// release `releaseKey` (1 — what the D3D reader acquires). The texture was allocated by D3D
    /// so its initial layout to Vulkan is UNDEFINED; we transition UNDEFINED->TRANSFER_DST, copy
    /// the staging gradient, then TRANSFER_DST->GENERAL. The keyed mutex (not a queue-family
    /// EXTERNAL release) is the cross-API ownership/visibility primitive here.
    let importFillRelease (device : Device) (img : ImportedD3D11Image) (acquireKey : uint64) (releaseKey : uint64) =
        let dev = device.Handle
        let w, h = img.W, img.H
        let bufSize = uint64 (w * h * 4)

        // staging buffer with the gradient
        let mutable bufInfo =
            VkBufferCreateInfo(VkBufferCreateFlags.None, bufSize, VkBufferUsageFlags.TransferSrcBit,
                               VkSharingMode.Exclusive, 0u, NativePtr.zero)
        let mutable buffer = VkBuffer.Null
        VkRaw.vkCreateBuffer(dev, &&bufInfo, NativePtr.zero, &&buffer) |> check "vkCreateBuffer(impgrad)"
        let mutable breq = VkMemoryRequirements()
        VkRaw.vkGetBufferMemoryRequirements(dev, buffer, &&breq)
        let mutable balloc = VkMemoryAllocateInfo(0n, breq.size, hostVisibleType device breq.memoryTypeBits)
        let mutable bmem = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&balloc, NativePtr.zero, &&bmem) |> check "vkAllocateMemory(impgrad)"
        VkRaw.vkBindBufferMemory(dev, buffer, bmem, 0UL) |> check "vkBindBufferMemory(impgrad)"
        let mutable ptr = 0n
        VkRaw.vkMapMemory(dev, bmem, 0UL, bufSize, VkMemoryMapFlags.None, &&ptr) |> check "vkMapMemory(impgrad)"
        for y in 0 .. h - 1 do
            for x in 0 .. w - 1 do
                let o = (y * w + x) * 4
                Marshal.WriteByte(ptr, o + 0, 128uy)                    // B
                Marshal.WriteByte(ptr, o + 1, byte (y * 255 / (h - 1))) // G
                Marshal.WriteByte(ptr, o + 2, byte (x * 255 / (w - 1))) // R
                Marshal.WriteByte(ptr, o + 3, 255uy)                    // A
        VkRaw.vkUnmapMemory(dev, bmem)

        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.None, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(dev, &&poolInfo, NativePtr.zero, &&pool) |> check "vkCreateCommandPool(impgrad)"
        let mutable cai = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable cmd = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(dev, &&cai, &&cmd) |> check "vkAllocateCommandBuffers(impgrad)"
        let mutable cbi = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&cbi) |> check "vkBeginCommandBuffer(impgrad)"

        let range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        // UNDEFINED -> TRANSFER_DST (keyed mutex gives us ownership; no queue-family transfer needed)
        let mutable b1 =
            VkImageMemoryBarrier(0n, VkAccessFlags.None, VkAccessFlags.TransferWriteBit,
                                 VkImageLayout.Undefined, VkImageLayout.TransferDstOptimal,
                                 0xFFFFFFFFu, 0xFFFFFFFFu, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TopOfPipeBit, VkPipelineStageFlags.TransferBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b1)
        let subres = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkBufferImageCopy(0UL, 0u, 0u, subres, VkOffset3D(0, 0, 0), VkExtent3D(uint32 w, uint32 h, 1u))
        VkRaw.vkCmdCopyBufferToImage(cmd, buffer, img.Image, VkImageLayout.TransferDstOptimal, 1u, &&region)
        // TRANSFER_DST -> GENERAL (leave it in a layout the D3D reader can CopyResource from)
        let mutable b2 =
            VkImageMemoryBarrier(0n, VkAccessFlags.TransferWriteBit, VkAccessFlags.None,
                                 VkImageLayout.TransferDstOptimal, VkImageLayout.General,
                                 0xFFFFFFFFu, 0xFFFFFFFFu, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TransferBit, VkPipelineStageFlags.BottomOfPipeBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b2)
        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer(impgrad)"

        // submit WRAPPED in the keyed-mutex acquire/release
        let mutable pcmd = cmd
        let acqMem = [| img.Memory |]
        let acqKeys = [| acquireKey |]
        let acqTimeouts = [| 0xFFFFFFFFu |]   // INFINITE
        let relMem = [| img.Memory |]
        let relKeys = [| releaseKey |]
        use pAcqMem = fixed acqMem
        use pAcqKeys = fixed acqKeys
        use pAcqTimeouts = fixed acqTimeouts
        use pRelMem = fixed relMem
        use pRelKeys = fixed relKeys
        let mutable km =
            Aardvark.Rendering.Vulkan.Extensions.KHRWin32KeyedMutex.VkWin32KeyedMutexAcquireReleaseInfoKHR(
                1u, pAcqMem, pAcqKeys, pAcqTimeouts, 1u, pRelMem, pRelKeys)
        let mutable submit =
            VkSubmitInfo(NativePtr.toNativeInt &&km, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit(impgrad,km)"
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle(impgrad,km)"

        VkRaw.vkDestroyCommandPool(dev, pool, NativePtr.zero)
        VkRaw.vkDestroyBuffer(dev, buffer, NativePtr.zero)
        VkRaw.vkFreeMemory(dev, bmem, NativePtr.zero)

    /// Same as importFillRelease but fills the IMPORTED D3D11 texture with a SOLID BGRA color
    /// (e.g. TEAL = B=153,G=102,R=51,A=255), wrapped in a keyed-mutex acquire(acquireKey) /
    /// release(releaseKey). Used by the standalone producer de-risk with key 0 / key 0.
    let importFillReleaseColor (device : Device) (img : ImportedD3D11Image)
                               (b : byte) (g : byte) (r : byte) (a : byte)
                               (acquireKey : uint64) (releaseKey : uint64) =
        let dev = device.Handle
        let w, h = img.W, img.H
        let bufSize = uint64 (w * h * 4)

        let mutable bufInfo =
            VkBufferCreateInfo(VkBufferCreateFlags.None, bufSize, VkBufferUsageFlags.TransferSrcBit,
                               VkSharingMode.Exclusive, 0u, NativePtr.zero)
        let mutable buffer = VkBuffer.Null
        VkRaw.vkCreateBuffer(dev, &&bufInfo, NativePtr.zero, &&buffer) |> check "vkCreateBuffer(impcol)"
        let mutable breq = VkMemoryRequirements()
        VkRaw.vkGetBufferMemoryRequirements(dev, buffer, &&breq)
        let mutable balloc = VkMemoryAllocateInfo(0n, breq.size, hostVisibleType device breq.memoryTypeBits)
        let mutable bmem = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&balloc, NativePtr.zero, &&bmem) |> check "vkAllocateMemory(impcol)"
        VkRaw.vkBindBufferMemory(dev, buffer, bmem, 0UL) |> check "vkBindBufferMemory(impcol)"
        let mutable ptr = 0n
        VkRaw.vkMapMemory(dev, bmem, 0UL, bufSize, VkMemoryMapFlags.None, &&ptr) |> check "vkMapMemory(impcol)"
        for i in 0 .. w * h - 1 do
            let o = i * 4
            Marshal.WriteByte(ptr, o + 0, b)
            Marshal.WriteByte(ptr, o + 1, g)
            Marshal.WriteByte(ptr, o + 2, r)
            Marshal.WriteByte(ptr, o + 3, a)
        VkRaw.vkUnmapMemory(dev, bmem)

        let qfi = uint32 device.GraphicsFamily.Index
        let mutable queue = Unchecked.defaultof<VkQueue>
        VkRaw.vkGetDeviceQueue(dev, qfi, 0u, &&queue)
        let mutable poolInfo = VkCommandPoolCreateInfo(0n, VkCommandPoolCreateFlags.None, qfi)
        let mutable pool = Unchecked.defaultof<VkCommandPool>
        VkRaw.vkCreateCommandPool(dev, &&poolInfo, NativePtr.zero, &&pool) |> check "vkCreateCommandPool(impcol)"
        let mutable cai = VkCommandBufferAllocateInfo(0n, pool, VkCommandBufferLevel.Primary, 1u)
        let mutable cmd = Unchecked.defaultof<VkCommandBuffer>
        VkRaw.vkAllocateCommandBuffers(dev, &&cai, &&cmd) |> check "vkAllocateCommandBuffers(impcol)"
        let mutable cbi = VkCommandBufferBeginInfo(0n, VkCommandBufferUsageFlags.OneTimeSubmitBit, NativePtr.zero)
        VkRaw.vkBeginCommandBuffer(cmd, &&cbi) |> check "vkBeginCommandBuffer(impcol)"

        let range = VkImageSubresourceRange(VkImageAspectFlags.ColorBit, 0u, 1u, 0u, 1u)
        let mutable b1 =
            VkImageMemoryBarrier(0n, VkAccessFlags.None, VkAccessFlags.TransferWriteBit,
                                 VkImageLayout.Undefined, VkImageLayout.TransferDstOptimal,
                                 0xFFFFFFFFu, 0xFFFFFFFFu, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TopOfPipeBit, VkPipelineStageFlags.TransferBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b1)
        let subres = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkBufferImageCopy(0UL, 0u, 0u, subres, VkOffset3D(0, 0, 0), VkExtent3D(uint32 w, uint32 h, 1u))
        VkRaw.vkCmdCopyBufferToImage(cmd, buffer, img.Image, VkImageLayout.TransferDstOptimal, 1u, &&region)
        let mutable b2 =
            VkImageMemoryBarrier(0n, VkAccessFlags.TransferWriteBit, VkAccessFlags.None,
                                 VkImageLayout.TransferDstOptimal, VkImageLayout.General,
                                 0xFFFFFFFFu, 0xFFFFFFFFu, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TransferBit, VkPipelineStageFlags.BottomOfPipeBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b2)
        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer(impcol)"

        let mutable pcmd = cmd
        let acqMem = [| img.Memory |]
        let acqKeys = [| acquireKey |]
        let acqTimeouts = [| 0xFFFFFFFFu |]
        let relMem = [| img.Memory |]
        let relKeys = [| releaseKey |]
        use pAcqMem = fixed acqMem
        use pAcqKeys = fixed acqKeys
        use pAcqTimeouts = fixed acqTimeouts
        use pRelMem = fixed relMem
        use pRelKeys = fixed relKeys
        let mutable km =
            Aardvark.Rendering.Vulkan.Extensions.KHRWin32KeyedMutex.VkWin32KeyedMutexAcquireReleaseInfoKHR(
                1u, pAcqMem, pAcqKeys, pAcqTimeouts, 1u, pRelMem, pRelKeys)
        let mutable submit =
            VkSubmitInfo(NativePtr.toNativeInt &&km, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
        VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit(impcol,km)"
        VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle(impcol,km)"

        VkRaw.vkDestroyCommandPool(dev, pool, NativePtr.zero)
        VkRaw.vkDestroyBuffer(dev, buffer, NativePtr.zero)
        VkRaw.vkFreeMemory(dev, bmem, NativePtr.zero)

    let destroy (device : Device) (img : D3D11SharedImage) =
        VkRaw.vkDestroyImage(device.Handle, img.Image, NativePtr.zero)
        VkRaw.vkFreeMemory(device.Handle, img.Memory, NativePtr.zero)

    /// PRODUCER fill: CPU-write a 2-axis gradient (R=x-ramp, G=y-ramp, B=128) into a staging
    /// buffer and copy it into the OPTIMAL export image. The image is released to the D3D11
    /// consumer in GENERAL layout (cross-API ownership transfer to VK_QUEUE_FAMILY_EXTERNAL).
    ///
    /// If `img.KeyedMutex`, the whole submit acquires key `acquireKey` and releases key
    /// `releaseKey` of the image's memory via VkWin32KeyedMutexAcquireReleaseInfoKHR — the
    /// D3D11 consumer then AcquireSync(releaseKey) / ReleaseSync to read.
    let fillGradient (device : Device) (img : D3D11SharedImage) (acquireKey : uint64) (releaseKey : uint64) =
        let dev = device.Handle
        let w, h = img.Width, img.Height
        let bufSize = uint64 (w * h * 4)

        // staging buffer (host visible) with the gradient
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
                Marshal.WriteByte(ptr, o + 0, 128uy)                    // B
                Marshal.WriteByte(ptr, o + 1, byte (y * 255 / (h - 1))) // G
                Marshal.WriteByte(ptr, o + 2, byte (x * 255 / (w - 1))) // R
                Marshal.WriteByte(ptr, o + 3, 255uy)                    // A
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
        // UNDEFINED -> TRANSFER_DST
        let mutable b1 =
            VkImageMemoryBarrier(0n, VkAccessFlags.None, VkAccessFlags.TransferWriteBit,
                                 VkImageLayout.Undefined, VkImageLayout.TransferDstOptimal,
                                 0xFFFFFFFFu, 0xFFFFFFFFu, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TopOfPipeBit, VkPipelineStageFlags.TransferBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b1)
        let subres = VkImageSubresourceLayers(VkImageAspectFlags.ColorBit, 0u, 0u, 1u)
        let mutable region =
            VkBufferImageCopy(0UL, 0u, 0u, subres, VkOffset3D(0, 0, 0), VkExtent3D(uint32 w, uint32 h, 1u))
        VkRaw.vkCmdCopyBufferToImage(cmd, buffer, img.Image, VkImageLayout.TransferDstOptimal, 1u, &&region)
        // TRANSFER_DST -> GENERAL, release to EXTERNAL (D3D11 consumer)
        let mutable b2 =
            VkImageMemoryBarrier(0n, VkAccessFlags.TransferWriteBit, VkAccessFlags.None,
                                 VkImageLayout.TransferDstOptimal, VkImageLayout.General,
                                 qfi, QUEUE_FAMILY_EXTERNAL, img.Image, range)
        VkRaw.vkCmdPipelineBarrier(cmd, VkPipelineStageFlags.TransferBit, VkPipelineStageFlags.BottomOfPipeBit,
                                   VkDependencyFlags.None, 0u, NativePtr.zero, 0u, NativePtr.zero, 1u, &&b2)
        VkRaw.vkEndCommandBuffer(cmd) |> check "vkEndCommandBuffer(grad)"

        let mutable pcmd = cmd
        if img.KeyedMutex then
            // pin the keyed-mutex arrays and chain the struct onto the submit's pNext.
            let acqMem = [| img.Memory |]
            let acqKeys = [| acquireKey |]
            let acqTimeouts = [| 0xFFFFFFFFu |]   // INFINITE
            let relMem = [| img.Memory |]
            let relKeys = [| releaseKey |]
            use pAcqMem = fixed acqMem
            use pAcqKeys = fixed acqKeys
            use pAcqTimeouts = fixed acqTimeouts
            use pRelMem = fixed relMem
            use pRelKeys = fixed relKeys
            let mutable km =
                Aardvark.Rendering.Vulkan.Extensions.KHRWin32KeyedMutex.VkWin32KeyedMutexAcquireReleaseInfoKHR(
                    1u, pAcqMem, pAcqKeys, pAcqTimeouts, 1u, pRelMem, pRelKeys)
            let mutable submit =
                VkSubmitInfo(NativePtr.toNativeInt &&km, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
            VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit(grad,km)"
            VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle(grad,km)"
        else
            let mutable submit = VkSubmitInfo(0n, 0u, NativePtr.zero, NativePtr.zero, 1u, &&pcmd, 0u, NativePtr.zero)
            VkRaw.vkQueueSubmit(queue, 1u, &&submit, Unchecked.defaultof<VkFence>) |> check "vkQueueSubmit(grad)"
            VkRaw.vkQueueWaitIdle(queue) |> check "vkQueueWaitIdle(grad)"

        VkRaw.vkDestroyCommandPool(dev, pool, NativePtr.zero)
        VkRaw.vkDestroyBuffer(dev, buffer, NativePtr.zero)
        VkRaw.vkFreeMemory(dev, bmem, NativePtr.zero)
