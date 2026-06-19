namespace Aardvark.Dom.Remote.SharedTexture

// Zero-copy texture sharing — exporter side (lives in Aardvark.Dom for now; migrate
// into Aardvark.Rendering once stable). Targets a Vulkan runtime: a color image is
// created as a Linux dma-buf, whose fd + DRM layout is handed to a browser/EGL
// consumer which imports it WITHOUT any host download.
//
// This module is intentionally raw: it talks to the public Vulkan `Device` of the
// Aardvark runtime via the generated wrapper, so it needs no changes to rendering.

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

/// A color image exported as a Linux dma-buf, described the way an EGL /
/// Chromium NativePixmap importer expects it.
type DmaBufImage =
    {
        /// dma-buf file descriptor (owned by this record until exported/closed)
        Fd       : int
        Width    : int
        Height   : int
        /// DRM fourcc (DRM_FORMAT_ARGB8888 for VkFormat.B8g8r8a8Unorm)
        Fourcc   : uint32
        /// DRM format modifier (DRM_FORMAT_MOD_LINEAR = 0)
        Modifier : uint64
        Offset   : uint64
        Stride   : uint64
        /// backing Vulkan handles (kept for rendering into / cleanup)
        Image    : VkImage
        Memory   : VkDeviceMemory
        Size     : uint64
    }

module DmaBufExport =

    // VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT — not a named case in the
    // generated 1.1 enum, so we use the raw value (0x200).
    let private DmaBufBit : VkExternalMemoryHandleTypeFlags = unbox 0x00000200

    /// DRM_FORMAT_ARGB8888 — little-endian B,G,R,A in memory == VkFormat.B8g8r8a8Unorm.
    [<Literal>]
    let DRM_FORMAT_ARGB8888 = 0x34325241u

    /// DRM_FORMAT_MOD_LINEAR
    [<Literal>]
    let DRM_FORMAT_MOD_LINEAR = 0UL

    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then
            failwithf "[DmaBuf] %s failed: %A" what res

    /// pick a host-visible + coherent memory type allowed by `typeBits`
    let private findHostVisibleMemoryType (device : Device) (typeBits : uint32) =
        device.PhysicalDevice.MemoryTypes
        |> Array.tryFind (fun mt ->
            (typeBits &&& (1u <<< mt.index)) <> 0u &&
            (int mt.flags &&& int VkMemoryPropertyFlags.HostVisibleBit) <> 0 &&
            (int mt.flags &&& int VkMemoryPropertyFlags.HostCoherentBit) <> 0
        )
        |> Option.map (fun mt -> uint32 mt.index)
        |> Option.defaultWith (fun () -> failwith "[DmaBuf] no host-visible/coherent memory type")

    /// Create a LINEAR, dma-buf-exportable BGRA color image and export its fd.
    /// Linear + host-visible lets us validate by writing a pattern from the CPU
    /// before wiring the GPU render path; the export semantics are identical.
    let create (device : Device) (width : int) (height : int) : DmaBufImage =
        let dev = device.Handle
        let format = VkFormat.B8g8r8a8Unorm

        // ---- image (external memory, linear tiling) ----
        let mutable extImg = VkExternalMemoryImageCreateInfo(DmaBufBit)

        let mutable imgInfo =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&extImg,
                VkImageCreateFlags.None,
                VkImageType.D2d,
                format,
                VkExtent3D(uint32 width, uint32 height, 1u),
                1u, 1u,
                VkSampleCountFlags.D1Bit,
                VkImageTiling.Linear,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit ||| VkImageUsageFlags.SampledBit,
                VkSharingMode.Exclusive,
                0u, NativePtr.zero,
                VkImageLayout.Undefined
            )

        let mutable image = VkImage.Null
        VkRaw.vkCreateImage(dev, &&imgInfo, NativePtr.zero, &&image) |> check "vkCreateImage"

        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType = findHostVisibleMemoryType device req.memoryTypeBits

        // ---- memory: dedicated + exportable as dma-buf ----
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable export = VkExportMemoryAllocateInfo(DmaBufBit)
        export.pNext <- NativePtr.toNativeInt &&dedicated

        let mutable allocInfo =
            VkMemoryAllocateInfo(NativePtr.toNativeInt &&export, req.size, memType)

        let mutable memory = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(dev, &&allocInfo, NativePtr.zero, &&memory) |> check "vkAllocateMemory"
        VkRaw.vkBindImageMemory(dev, image, memory, 0UL) |> check "vkBindImageMemory"

        // ---- query plane layout (offset / row pitch) ----
        let mutable sub = VkImageSubresource(VkImageAspectFlags.ColorBit, 0u, 0u)
        let mutable layout = VkSubresourceLayout()
        VkRaw.vkGetImageSubresourceLayout(dev, image, &&sub, &&layout)

        // ---- export dma-buf fd ----
        let mutable getFd = KHRExternalMemoryFd.VkMemoryGetFdInfoKHR(memory, DmaBufBit)
        let mutable fd = -1
        KHRExternalMemoryFd.VkRaw.vkGetMemoryFdKHR(dev, &&getFd, &&fd) |> check "vkGetMemoryFdKHR"

        {
            Fd       = fd
            Width    = width
            Height   = height
            Fourcc   = DRM_FORMAT_ARGB8888
            Modifier = DRM_FORMAT_MOD_LINEAR
            Offset   = uint64 layout.offset
            Stride   = uint64 layout.rowPitch
            Image    = image
            Memory   = memory
            Size     = uint64 req.size
        }

    /// Write a BGRA gradient into the (host-visible) dma-buf so a consumer can
    /// verify the round-trip pixel-for-pixel.
    let fillTestPattern (device : Device) (img : DmaBufImage) =
        let dev = device.Handle
        let mutable pData = 0n
        VkRaw.vkMapMemory(dev, img.Memory, 0UL, img.Size, VkMemoryMapFlags.None, &&pData) |> check "vkMapMemory"
        let baseAddr = pData + nativeint img.Offset
        let stride = int img.Stride
        for y in 0 .. img.Height - 1 do
            let row = baseAddr + nativeint (y * stride)
            for x in 0 .. img.Width - 1 do
                let p = row + nativeint (x * 4)
                Marshal.WriteByte(p, 0, byte (x * 255 / max 1 (img.Width - 1)))   // B
                Marshal.WriteByte(p, 1, byte (y * 255 / max 1 (img.Height - 1)))  // G
                Marshal.WriteByte(p, 2, 0uy)                                       // R
                Marshal.WriteByte(p, 3, 255uy)                                     // A
        VkRaw.vkUnmapMemory(dev, img.Memory)

    let destroy (device : Device) (img : DmaBufImage) =
        let dev = device.Handle
        VkRaw.vkDestroyImage(dev, img.Image, NativePtr.zero)
        VkRaw.vkFreeMemory(dev, img.Memory, NativePtr.zero)
