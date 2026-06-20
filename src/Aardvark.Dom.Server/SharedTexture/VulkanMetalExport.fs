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
    [<DllImport(framework)>] extern uint32 IOSurfaceGetPixelFormat(nativeint surf)
    // Create an IOSurface from a CFDictionary of properties (we stamp kIOSurfacePixelFormat
    // = 'BGRA', which the MoltenVK-exported surface lacks → the property Chromium's
    // IOSurfaceImageBackingFactory validates against the SharedImage format).
    [<DllImport(framework)>] extern nativeint IOSurfaceCreate(nativeint properties)

    // --- CoreFoundation, to build the IOSurfaceCreate property dictionary ---
    [<Literal>]
    let private cf = "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"
    [<DllImport(cf)>] extern nativeint CFDictionaryCreateMutable(nativeint alloc, nativeint capacity, nativeint keyCb, nativeint valCb)
    [<DllImport(cf)>] extern void CFDictionarySetValue(nativeint dict, nativeint key, nativeint value)
    [<DllImport(cf)>] extern void CFRelease(nativeint cf)
    [<DllImport(cf)>] extern nativeint CFNumberCreate(nativeint alloc, int theType, nativeint valuePtr)
    [<DllImport(cf)>] extern nativeint CFStringCreateWithCString(nativeint alloc, string cStr, uint32 encoding)
    // kCFNumberSInt32Type = 3 ; kCFStringEncodingASCII = 0x0600
    [<Literal>]
    let private kCFNumberSInt32Type = 3
    [<Literal>]
    let private kCFStringEncodingASCII = 0x0600u

    let private cfStr (s : string) = CFStringCreateWithCString(0n, s, kCFStringEncodingASCII)
    let private cfInt (v : int) =
        let p = Marshal.AllocHGlobal 4
        Marshal.WriteInt32(p, v)
        let n = CFNumberCreate(0n, kCFNumberSInt32Type, p)
        Marshal.FreeHGlobal p
        n

    // BGRA OSType ('BGRA') — the IOSurface pixel format Chromium's IOSurfaceImageBackingFactory
    // expects for viz::SinglePlaneFormat::kBGRA_8888 (vs the property-less surface MoltenVK mints).
    [<Literal>]
    let BGRA_OSType = 0x42475241u

    // Build the CFDictionary of IOSurface properties (width/height/bpe/bpr/pixelFormat) with
    // kIOSurfacePixelFormat stamped, then IOSurfaceCreate it. Returns the IOSurfaceRef (+1).
    let private createBGRAIOSurface (width : int) (height : int) : nativeint =
        let bpe = 4
        let bpr = width * bpe   // tightly packed; IOSurface may round up internally
        let dict = CFDictionaryCreateMutable(0n, 0n, 0n, 0n)
        let set (k : string) (v : nativeint) =
            let key = cfStr k
            CFDictionarySetValue(dict, key, v)
            CFRelease key
            CFRelease v
        set "IOSurfaceWidth" (cfInt width)
        set "IOSurfaceHeight" (cfInt height)
        set "IOSurfaceBytesPerElement" (cfInt bpe)
        set "IOSurfaceBytesPerRow" (cfInt bpr)
        set "IOSurfacePixelFormat" (cfInt (int BGRA_OSType))
        let surf = IOSurfaceCreate dict
        CFRelease dict
        surf

    /// Create a properly-formatted BGRA IOSurface (with kIOSurfacePixelFormat), then IMPORT it
    /// into a VkImage via VK_EXT_metal_objects (VkImportMetalIOSurfaceInfoEXT) so MoltenVK backs
    /// the VkImage with OUR surface. This is the macOS teal-pixel handoff: the surface carries
    /// the format property Chromium validates, unlike the property-less MoltenVK-exported one.
    let createImported (device : Device) (width : int) (height : int) : MetalSharedImage =
        let dev = device.Handle
        let format = VkFormat.B8g8r8a8Unorm

        // 1) our IOSurface, format-stamped
        let surf = createBGRAIOSurface width height
        if surf = 0n then failwith "[MetalExport] IOSurfaceCreate returned null"
        let pf = IOSurfaceGetPixelFormat surf
        if pf <> BGRA_OSType then
            failwithf "[MetalExport] IOSurface pixel format=0x%X (expected 0x%X 'BGRA')" pf BGRA_OSType

        // 2) import it: chain VkImportMetalIOSurfaceInfoEXT into the image create-info pNext
        let mutable impImg = EXTMetalObjects.VkImportMetalIOSurfaceInfoEXT(surf)
        let mutable info =
            VkImageCreateInfo(
                NativePtr.toNativeInt &&impImg, VkImageCreateFlags.None, VkImageType.D2d, format,
                VkExtent3D(uint32 width, uint32 height, 1u), 1u, 1u, VkSampleCountFlags.D1Bit,
                VkImageTiling.Linear,
                VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.TransferSrcBit,
                VkSharingMode.Exclusive, 0u, NativePtr.zero, VkImageLayout.Undefined)
        let mutable image = Unchecked.defaultof<VkImage>
        VkRaw.vkCreateImage(dev, &&info, NativePtr.zero, &&image) |> check "vkCreateImage(import)"

        let mutable req = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(dev, image, &&req)
        let memType =
            findMemoryType device req.memoryTypeBits
                (VkMemoryPropertyFlags.HostVisibleBit ||| VkMemoryPropertyFlags.HostCoherentBit)
        let mutable dedicated = VkMemoryDedicatedAllocateInfo(image, VkBuffer.Null)
        let mutable alloc = VkMemoryAllocateInfo(NativePtr.toNativeInt &&dedicated, req.size, memType)
        let mutable mem = Unchecked.defaultof<VkDeviceMemory>
        VkRaw.vkAllocateMemory(dev, &&alloc, NativePtr.zero, &&mem) |> check "vkAllocateMemory(import)"
        VkRaw.vkBindImageMemory(dev, image, mem, 0UL) |> check "vkBindImageMemory(import)"

        let mutable sub = VkImageSubresource(VkImageAspectFlags.ColorBit, 0u, 0u)
        let mutable layout = VkSubresourceLayout()
        VkRaw.vkGetImageSubresourceLayout(dev, image, &&sub, &&layout)

        {
            IOSurface = surf   // OUR surface — carries the format property + (after fill) the teal
            Width = width; Height = height
            Offset = uint64 layout.offset; Stride = uint64 layout.rowPitch
            Format = format; Image = image; Memory = mem; Size = uint64 req.size
        }

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

    /// CPU-fill the whole IOSurface (LINEAR, shared storage) with a solid (R,G,B,A) color.
    /// Used for the "teal pixel" milestone: a single uniform color the browser composites.
    let fillSolid (img : MetalSharedImage) (r : int) (g : int) (b : int) (a : int) =
        IOSurfaceLock(img.IOSurface, 0u (* read/write *), 0n) |> ignore
        let baseA = IOSurfaceGetBaseAddress(img.IOSurface)
        let bpr = int (IOSurfaceGetBytesPerRow(img.IOSurface))
        for y in 0 .. img.Height - 1 do
            let row = baseA + nativeint (y * bpr)
            for x in 0 .. img.Width - 1 do
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
