namespace Aardvark.Dom.Remote.SharedTexture

// Windows producer-side export — analog of VulkanDmaBuf (Linux). Exports a color
// image's memory as a Win32 NT handle. LINEAR + host-visible so the Vulkan-only
// self-validation is a plain map+read (mirrors the Linux dma-buf milestone). The
// Chromium-facing path will instead use OPTIMAL + D3D11_TEXTURE (LUID-matched),
// but the export/GPU-fill/fence plumbing is identical.

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

/// A color image exported as a Windows shared NT handle.
type WinSharedImage =
    {
        /// Win32 NT handle (owned until imported / CloseHandle'd)
        Handle : nativeint
        Width  : int
        Height : int
        Offset : uint64
        Stride : uint64
        Format : VkFormat
        Image  : VkImage
        Memory : VkDeviceMemory
        Size   : uint64
    }

module Win32Export =

    // OPAQUE_WIN32 for Vulkan<->Vulkan validation; switch to D3d11TextureBit for Chromium.
    let private HandleType = VkExternalMemoryHandleTypeFlags.OpaqueWin32Bit

    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then failwithf "[Win32Export] %s failed: %A" what res

    let private findMemoryType (device : Device) (typeBits : uint32) (want : VkMemoryPropertyFlags) =
        device.PhysicalDevice.MemoryTypes
        |> Array.tryFind (fun mt ->
            (typeBits &&& (1u <<< mt.index)) <> 0u &&
            (int mt.flags &&& int want) = int want)
        |> Option.map (fun mt -> uint32 mt.index)
        |> Option.defaultWith (fun () -> failwithf "[Win32Export] no memory type with %A" want)

    /// Create a LINEAR, host-visible color image exportable as a Win32 NT handle.
    let create (device : Device) (width : int) (height : int) : WinSharedImage =
        let dev = device.Handle
        let format = VkFormat.B8g8r8a8Unorm

        let mutable extImg = VkExternalMemoryImageCreateInfo(HandleType)
        let mutable info =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&extImg, VkImageCreateFlags.None, VkImageType.D2d, format,
                VkExtent3D(uint32 width, uint32 height, 1u), 1u, 1u, VkSampleCountFlags.D1Bit,
                VkImageTiling.Linear,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit,
                VkSharingMode.Exclusive, 0u, NativePtr.zero, VkImageLayout.Undefined)
        let mutable image = Unchecked.defaultof<VkImage>
        VkRaw.vkCreateImage(dev, &&info, NativePtr.zero, &&image) |> check "vkCreateImage"

        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType =
            findMemoryType device req.memoryTypeBits
                (VkMemoryPropertyFlags.HostVisibleBit ||| VkMemoryPropertyFlags.HostCoherentBit)

        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable export = VkExportMemoryAllocateInfo(HandleType)
        export.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&export, req.size, memType)
        let mutable mem = Unchecked.defaultof<VkDeviceMemory>
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&mem) |> check "vkAllocateMemory"
        VkRaw.vkBindImageMemory(dev, image, mem, 0UL) |> check "vkBindImageMemory"

        let mutable sub = VkImageSubresource(VkImageAspectFlags.ColorBit, 0u, 0u)
        let mutable layout = VkSubresourceLayout()
        VkRaw.vkGetImageSubresourceLayout(dev, image, &&sub, &&layout)

        let mutable getInfo = KHRExternalMemoryWin32.VkMemoryGetWin32HandleInfoKHR(mem, HandleType)
        let mutable handle = 0n
        KHRExternalMemoryWin32.VkRaw.vkGetMemoryWin32HandleKHR(dev, &&getInfo, &&handle) |> check "vkGetMemoryWin32HandleKHR"

        {
            Handle = handle; Width = width; Height = height
            Offset = uint64 layout.offset; Stride = uint64 layout.rowPitch
            Format = format; Image = image; Memory = mem; Size = uint64 req.size
        }

    /// Map the (host-visible) shared memory and read the center pixel as (R,G,B,A).
    /// Used to validate the GPU fill landed in the shared image.
    let readbackCenter (device : Device) (img : WinSharedImage) : int * int * int * int =
        let dev = device.Handle
        let mutable p = 0n
        VkRaw.vkMapMemory(dev, img.Memory, 0UL, img.Size, VkMemoryMapFlags.None, &&p) |> check "vkMapMemory"
        let px = p + nativeint img.Offset + nativeint ((img.Height / 2) * int img.Stride + (img.Width / 2) * 4)
        // B8G8R8A8 memory order: B,G,R,A -> return (R,G,B,A)
        let b = int (Marshal.ReadByte(px, 0))
        let g = int (Marshal.ReadByte(px, 1))
        let r = int (Marshal.ReadByte(px, 2))
        let a = int (Marshal.ReadByte(px, 3))
        VkRaw.vkUnmapMemory(dev, img.Memory)
        (r, g, b, a)

    let destroy (device : Device) (img : WinSharedImage) =
        VkRaw.vkDestroyImage(device.Handle, img.Image, NativePtr.zero)
        VkRaw.vkFreeMemory(device.Handle, img.Memory, NativePtr.zero)
