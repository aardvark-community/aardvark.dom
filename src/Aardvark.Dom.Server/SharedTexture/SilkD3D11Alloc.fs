namespace Aardvark.Dom.Remote.SharedTexture

// Silk.NET-based D3D11 allocator for the PRODUCER end of the Windows zero-copy path.
//
// This is the in-process analog of tools/d3d11-consumer/d3d11_alloc.exe (the C++ allocator):
// it mints a SHARED, KEYED-MUTEX D3D11 texture (B8G8R8A8_UNORM) on a SPECIFIC DXGI adapter
// (selected by LUID, so it matches the Vulkan device the importer pins via --gpu) and exports a
// DXGI shared NT handle (IDXGIResource1::CreateSharedHandle). Vulkan then imports that handle
// in-process (D3D11Export.importD3D11) and GPU-fills it with a keyed mutex, exactly the Chromium
// D3DImageBacking ownership model (the compositor owns the texture, Aardvark renders into it).
//
// Misc flags: D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX (0x20) | _SHARED_NTHANDLE (0x800) = 0x820.
// CreateSharedHandle access: DXGI_SHARED_RESOURCE_READ (0x80000000) | _WRITE (0x1) = 0x80000001.
//
// We keep the ComPtr device + texture alive (returned in the result) — releasing them would free
// the shared texture out from under the Vulkan import / D3D11 reader.

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Silk.NET.Core.Native
open Silk.NET.DXGI
open Silk.NET.Direct3D11

#nowarn "9"
#nowarn "51"

/// A D3D11-allocated shared keyed-mutex texture + the live COM objects keeping it alive.
type SilkD3D11Texture =
    {
        /// DXGI shared NT handle (openable by Vulkan import / D3D11 OpenSharedResource1)
        Handle  : nativeint
        Width   : int
        Height  : int
        /// 16-hex-digit LUID of the adapter the texture lives on (LowPart LE then HighPart LE)
        LuidHex : string
        // live COM objects — must stay alive while the handle is in use
        Dxgi    : DXGI
        D3D11   : D3D11
        Factory : ComPtr<IDXGIFactory1>
        Device  : ComPtr<ID3D11Device>
        Context : ComPtr<ID3D11DeviceContext>
        Texture : ComPtr<ID3D11Texture2D>
    }

module SilkD3D11Alloc =

    let private check (what : string) (hr : int) =
        if hr < 0 then failwithf "[SilkD3D11Alloc] %s failed: hr=0x%08X" what hr

    /// Format the 8-byte DXGI LUID (LowPart 4 LE, HighPart 4 LE) the same way the Vulkan
    /// deviceLUID hex is printed, so the two can be compared as strings.
    let luidHex (l : Luid) : string =
        let bytes =
            Array.append
                (BitConverter.GetBytes(l.Low))           // uint32, little-endian
                (BitConverter.GetBytes(l.High))          // int32,  little-endian
        bytes |> Array.map (sprintf "%02X") |> String.concat ""

    /// Allocate a shared keyed-mutex B8G8R8A8 texture on the adapter whose LUID == `wantLuidHex`
    /// (16 hex digits, as printed by D3D11Export.deviceLUID). Returns the NT handle + live objects.
    let alloc (wantLuidHex : string) (w : int) (h : int) : SilkD3D11Texture =
        let dxgi = DXGI.GetApi()
        let d3d11 = D3D11.GetApi()

        // ---- CreateDXGIFactory1 -> IDXGIFactory1
        let mutable factory = Unchecked.defaultof<ComPtr<IDXGIFactory1>>
        check "CreateDXGIFactory1" (dxgi.CreateDXGIFactory1(&factory))

        // ---- EnumAdapters1 loop, pick the one whose AdapterLuid hex == wantLuidHex
        let want = wantLuidHex.ToUpperInvariant()
        let mutable chosen = Unchecked.defaultof<ComPtr<IDXGIAdapter1>>
        let mutable chosenHex = ""
        let mutable i = 0u
        let mutable go = true
        while go do
            let mutable adapter = Unchecked.defaultof<ComPtr<IDXGIAdapter1>>
            let hr = factory.EnumAdapters1(i, &adapter)
            if hr < 0 then
                go <- false   // DXGI_ERROR_NOT_FOUND (0x887A0002) — end of list
            else
                let mutable desc = Unchecked.defaultof<AdapterDesc1>
                check "GetDesc1" (adapter.GetDesc1(&desc))
                let hex = luidHex desc.AdapterLuid
                // Description is a fixed inline char[128]; read it for logging.
                let name =
                    let p = &&desc.Description
                    Marshal.PtrToStringUni(NativePtr.toNativeInt p)
                printfn "[silk-alloc]   adapter[%d] LUID=%s '%s'" i hex name
                if hex.ToUpperInvariant() = want && chosenHex = "" then
                    chosen <- adapter
                    chosenHex <- hex
                else
                    adapter.Dispose()
                i <- i + 1u

        if chosenHex = "" then
            factory.Dispose()
            failwithf "[silk-alloc] no DXGI adapter with LUID %s" wantLuidHex
        printfn "[silk-alloc] chosen adapter LUID=%s" chosenHex

        // ---- D3D11CreateDevice on that adapter (D3D_DRIVER_TYPE_UNKNOWN required when adapter != null).
        // Use the all-nativeptr overload: pass the adapter as IDXGIAdapter*, receive ID3D11Device*
        // / ID3D11DeviceContext* and wrap them into ComPtr afterwards.
        let adapterUnk : ComPtr<IDXGIAdapter> = chosen.QueryInterface<IDXGIAdapter>()
        let mutable devPtr = NativePtr.ofNativeInt<ID3D11Device> 0n
        let mutable ctxPtr = NativePtr.ofNativeInt<ID3D11DeviceContext> 0n
        let mutable fl = D3DFeatureLevel.Level110
        let hrDev =
            d3d11.CreateDevice(
                adapterUnk.Handle,                              // IDXGIAdapter*
                D3DDriverType.Unknown,
                0n,
                0u,                                             // flags (matches the known-good C++ allocator)
                NativePtr.ofNativeInt<D3DFeatureLevel> 0n,      // pFeatureLevels = null
                0u,                                             // FeatureLevels = 0
                uint32 D3D11.SdkVersion,
                NativePtr.toByRef &&devPtr,                     // ppDevice : ID3D11Device**
                &&fl,                                           // pFeatureLevel
                NativePtr.toByRef &&ctxPtr)                     // ppImmediateContext
        check "D3D11CreateDevice" hrDev
        adapterUnk.Dispose()
        printfn "[silk-alloc] CreateDevice OK: devPtr=0x%X ctxPtr=0x%X featureLevel=%A"
            (NativePtr.toNativeInt devPtr) (NativePtr.toNativeInt ctxPtr) fl
        let device = ComPtr<ID3D11Device>(devPtr)
        let context = ComPtr<ID3D11DeviceContext>(ctxPtr)

        // ---- CreateTexture2D: B8G8R8A8_UNORM, SHARED_KEYEDMUTEX|SHARED_NTHANDLE.
        // Silk's Texture2DDesc is a mutable struct; build it field-by-field.
        let mutable desc = Texture2DDesc()
        desc.Width <- uint32 w
        desc.Height <- uint32 h
        desc.MipLevels <- 1u
        desc.ArraySize <- 1u
        desc.Format <- Format.FormatB8G8R8A8Unorm
        desc.SampleDesc <- Silk.NET.DXGI.SampleDesc(1u, 0u)
        desc.Usage <- Usage.Default
        desc.BindFlags <- uint32 (int BindFlag.ShaderResource ||| int BindFlag.RenderTarget)
        desc.CPUAccessFlags <- 0u
        desc.MiscFlags <- uint32 (0x20 ||| 0x800)   // SHARED_KEYEDMUTEX | SHARED_NTHANDLE
        printfn "[silk-alloc] desc: %dx%d mip=%d arr=%d fmt=%A samples=%d usage=%A bind=0x%X cpu=0x%X misc=0x%X"
            desc.Width desc.Height desc.MipLevels desc.ArraySize desc.Format desc.SampleDesc.Count
            desc.Usage desc.BindFlags desc.CPUAccessFlags desc.MiscFlags
        // PROBE: try a plain (misc=0) texture first to isolate device-validity from the
        // keyed-mutex/nt-handle misc flags.
        let mutable probeDesc = desc
        probeDesc.MiscFlags <- 0u
        let mutable probePtr = NativePtr.ofNativeInt<ID3D11Texture2D> 0n
        let hrProbe =
            device.CreateTexture2D(&&probeDesc, NativePtr.ofNativeInt<SubresourceData> 0n, NativePtr.toByRef &&probePtr)
        printfn "[silk-alloc] PROBE CreateTexture2D(misc=0) hr=0x%08X" hrProbe
        if hrProbe >= 0 then (ComPtr<ID3D11Texture2D>(probePtr)).Dispose()

        let mutable texPtr = NativePtr.ofNativeInt<ID3D11Texture2D> 0n
        let hrTex =
            device.CreateTexture2D(&&desc, NativePtr.ofNativeInt<SubresourceData> 0n, NativePtr.toByRef &&texPtr)
        check "CreateTexture2D" hrTex
        let tex = ComPtr<ID3D11Texture2D>(texPtr)

        // ---- QI tex -> IDXGIResource1, CreateSharedHandle (NT handle)
        let res1 : ComPtr<IDXGIResource1> = tex.QueryInterface<IDXGIResource1>()
        let DXGI_SHARED_RESOURCE_READ  = 0x80000000u
        let DXGI_SHARED_RESOURCE_WRITE = 0x00000001u
        let mutable handlePtr : voidptr = NativePtr.toVoidPtr (NativePtr.ofNativeInt<byte> 0n)
        let hrSh =
            res1.CreateSharedHandle(
                System.ReadOnlySpan<SecurityAttributes>(),      // null pAttributes
                DXGI_SHARED_RESOURCE_READ ||| DXGI_SHARED_RESOURCE_WRITE,
                (NativePtr.ofNativeInt<char> 0n),               // lpName = null
                &handlePtr)
        check "CreateSharedHandle" hrSh
        let handle : nativeint = NativePtr.toNativeInt (NativePtr.ofVoidPtr<byte> handlePtr)
        res1.Dispose()
        chosen.Dispose()

        printfn "[silk-alloc] CreateSharedHandle -> 0x%X (%dx%d keyed-mutex shared)" (int64 handle) w h
        {
            Handle = handle; Width = w; Height = h; LuidHex = chosenHex
            Dxgi = dxgi; D3D11 = d3d11; Factory = factory
            Device = device; Context = context; Texture = tex
        }

    /// Release everything (call after the consumer is done).
    let destroy (t : SilkD3D11Texture) =
        t.Texture.Dispose()
        t.Context.Dispose()
        t.Device.Dispose()
        t.Factory.Dispose()
        t.D3D11.Dispose()
        t.Dxgi.Dispose()
