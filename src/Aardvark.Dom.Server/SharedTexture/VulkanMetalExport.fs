namespace Aardvark.Dom.Remote.SharedTexture

// macOS producer-side export — analog of VulkanDmaBuf/VulkanWin32Export. Via MoltenVK's
// VK_EXT_metal_objects: an image is marked for IOSurface export, then vkExportMetalObjectsEXT
// hands back the backing IOSurfaceRef — which Chromium imports through IOSurfaceImageBackingFactory.
// LINEAR + host-visible so the self-validation can map+read (mirrors the other platforms).
// NOTE: requires the device created WITH VK_EXT_metal_objects enabled (HeadlessVulkanApplication
// deviceExtensions) — it is available on MoltenVK but not in Aardvark's default extension set.

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

/// A color image exported as a macOS IOSurface.
type MetalSharedImage =
    {
        /// IOSurfaceRef (CFTypeRef; owned until released / passed to the consumer)
        IOSurface : nativeint
        Width  : int
        Height : int
        Offset : uint64
        Stride : uint64
        Format : VkFormat
        Image  : VkImage
        Memory : VkDeviceMemory
        Size   : uint64
    }

module MetalExport =

    let private IOSurfaceType = EXTMetalObjects.VkExportMetalObjectTypeFlagsEXT.MetalIosurfaceBit

    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then failwithf "[MetalExport] %s failed: %A" what res

    let private findMemoryType (device : Device) (typeBits : uint32) (want : VkMemoryPropertyFlags) =
        device.PhysicalDevice.MemoryTypes
        |> Array.tryFind (fun mt ->
            (typeBits &&& (1u <<< mt.index)) <> 0u &&
            (int mt.flags &&& int want) = int want)
        |> Option.map (fun mt -> uint32 mt.index)
        |> Option.defaultWith (fun () -> failwithf "[MetalExport] no memory type with %A" want)

    /// Create a LINEAR, host-visible color image and export its backing IOSurface.
    let create (device : Device) (width : int) (height : int) : MetalSharedImage =
        let dev = device.Handle
        let format = VkFormat.B8g8r8a8Unorm

        // mark the image (and its memory) for IOSurface export
        let mutable expImg = EXTMetalObjects.VkExportMetalObjectCreateInfoEXT(IOSurfaceType)
        let mutable info =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&expImg, VkImageCreateFlags.None, VkImageType.D2d, format,
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

        // alloc chain: metal-export -> dedicated
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable expMem = EXTMetalObjects.VkExportMetalObjectCreateInfoEXT(IOSurfaceType)
        expMem.pNext <- NativePtr.toNativeInt &&dedicated
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&expMem, req.size, memType)
        let mutable mem = Unchecked.defaultof<VkDeviceMemory>
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&mem) |> check "vkAllocateMemory"
        VkRaw.vkBindImageMemory(dev, image, mem, 0UL) |> check "vkBindImageMemory"

        let mutable sub = VkImageSubresource(VkImageAspectFlags.ColorBit, 0u, 0u)
        let mutable layout = VkSubresourceLayout()
        VkRaw.vkGetImageSubresourceLayout(dev, image, &&sub, &&layout)

        // export the IOSurface: vkExportMetalObjectsEXT walks the pNext chain and fills ioSurface
        let mutable iosInfo = EXTMetalObjects.VkExportMetalIOSurfaceInfoEXT(image, 0n)
        let mutable expInfo = EXTMetalObjects.VkExportMetalObjectsInfoEXT(NativePtr.toNativeInt &&iosInfo)
        EXTMetalObjects.VkRaw.vkExportMetalObjectsEXT(dev, &&expInfo)

        {
            IOSurface = iosInfo.ioSurface
            Width = width; Height = height
            Offset = uint64 layout.offset; Stride = uint64 layout.rowPitch
            Format = format; Image = image; Memory = mem; Size = uint64 req.size
        }

    // Read the IOSurface directly (the consumer-facing object) — its pixels live in
    // the IOSurface storage, not in our VkDeviceMemory.
    [<Literal>]
    let private framework = "/System/Library/Frameworks/IOSurface.framework/IOSurface"
    [<DllImport(framework)>] extern int IOSurfaceLock(nativeint surf, uint32 options, nativeint seed)
    [<DllImport(framework)>] extern int IOSurfaceUnlock(nativeint surf, uint32 options, nativeint seed)
    [<DllImport(framework)>] extern nativeint IOSurfaceGetBaseAddress(nativeint surf)
    [<DllImport(framework)>] extern unativeint IOSurfaceGetBytesPerRow(nativeint surf)
    [<DllImport(framework)>] extern unativeint IOSurfaceGetWidth(nativeint surf)
    [<DllImport(framework)>] extern unativeint IOSurfaceGetHeight(nativeint surf)
    // Global (per-host) IOSurface id — simplest cross-process lookup for the standalone
    // de-risk. NOTE: for the secure Chromium handoff use IOSurfaceCreateMachPort instead
    // (a mach port sent over the GPU-channel), since global ids are guessable/host-wide.
    [<DllImport(framework)>] extern uint32 IOSurfaceGetID(nativeint surf)
    [<DllImport(framework)>] extern nativeint IOSurfaceLookup(uint32 csid)

    /// The non-uniform reference gradient: R varies along X, G varies along Y, B fixed.
    /// 2-axis so a transposed / wrong-stride read would be caught (not just blank-vs-not).
    let expectedAt (w : int) (h : int) (x : int) (y : int) : int * int * int * int =
        let r = (x * 255) / (max 1 (w - 1))
        let g = (y * 255) / (max 1 (h - 1))
        (r, g, 128, 255)

    /// CPU-fill the IOSurface (LINEAR, shared storage) with the reference gradient.
    /// On unified-memory Apple Silicon this is the producer write the consumer will read
    /// from another process; it exercises the real stride/layout of the shared surface.
    let fillGradient (img : MetalSharedImage) =
        IOSurfaceLock(img.IOSurface, 0u (* read/write *), 0n) |> ignore
        let baseA = IOSurfaceGetBaseAddress(img.IOSurface)
        let bpr = int (IOSurfaceGetBytesPerRow(img.IOSurface))
        for y in 0 .. img.Height - 1 do
            let row = baseA + nativeint (y * bpr)
            for x in 0 .. img.Width - 1 do
                let (r, g, b, a) = expectedAt img.Width img.Height x y
                let px = row + nativeint (x * 4)
                // B8G8R8A8: byte order B,G,R,A
                Marshal.WriteByte(px, 0, byte b)
                Marshal.WriteByte(px, 1, byte g)
                Marshal.WriteByte(px, 2, byte r)
                Marshal.WriteByte(px, 3, byte a)
        IOSurfaceUnlock(img.IOSurface, 0u, 0n) |> ignore

    /// Lock the IOSurface and read the pixel at (x,y) as (R,G,B,A). `surf` is a raw
    /// IOSurfaceRef (works for both a producer-owned and a cross-process looked-up surface).
    let readPixel (surf : nativeint) (x : int) (y : int) : int * int * int * int =
        IOSurfaceLock(surf, 1u (* kIOSurfaceLockReadOnly *), 0n) |> ignore
        let baseA = IOSurfaceGetBaseAddress(surf)
        let bpr = int (IOSurfaceGetBytesPerRow(surf))
        let px = baseA + nativeint (y * bpr + x * 4)
        let b = int (Marshal.ReadByte(px, 0))
        let g = int (Marshal.ReadByte(px, 1))
        let r = int (Marshal.ReadByte(px, 2))
        let a = int (Marshal.ReadByte(px, 3))
        IOSurfaceUnlock(surf, 1u, 0n) |> ignore
        (r, g, b, a)

    /// Lock the IOSurface and read the center pixel as (R,G,B,A).
    let readbackCenter (_device : Device) (img : MetalSharedImage) : int * int * int * int =
        readPixel img.IOSurface (img.Width / 2) (img.Height / 2)

    let destroy (device : Device) (img : MetalSharedImage) =
        VkRaw.vkDestroyImage(device.Handle, img.Image, NativePtr.zero)
        VkRaw.vkFreeMemory(device.Handle, img.Memory, NativePtr.zero)
