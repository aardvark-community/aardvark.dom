namespace Aardvark.Dom.Remote.SharedTexture

// Low-latency lever #2: export a Vulkan binary semaphore as a Linux sync_fd that
// is signaled when the frame's GPU work completes. The consumer (Chromium's GPU
// process, via gfx::GpuFence) does a SERVER-SIDE GPU wait on this fence before
// sampling — no CPU stall on our side, no extra "safety" frame of latency.

open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.Vulkan11

#nowarn "9"
#nowarn "51"

module DmaBufSync =

    let SyncFdBit : VkExternalSemaphoreHandleTypeFlags = VkExternalSemaphoreHandleTypeFlags.SyncFdBit

    let private check (what : string) (res : VkResult) =
        if res <> VkResult.Success then failwithf "[DmaBufSync] %s failed: %A" what res

    /// A binary semaphore created exportable as a sync_fd.
    let createExportableSemaphore (device : Device) : VkSemaphore =
        let dev = device.Handle
        let mutable exportInfo = VkExportSemaphoreCreateInfo(SyncFdBit)
        let mutable info = VkSemaphoreCreateInfo(VkSemaphoreCreateFlags.None)
        info.pNext <- NativePtr.toNativeInt &&exportInfo
        let mutable sem = Unchecked.defaultof<VkSemaphore>
        VkRaw.vkCreateSemaphore(dev, &&info, NativePtr.zero, &&sem) |> check "vkCreateSemaphore"
        sem

    /// Export a sync_fd snapshotting the semaphore's pending signal. Call AFTER the
    /// signaling submit, ideally BEFORE the GPU finishes, to get a real fence fd.
    /// fd = -1 is the valid "already signaled, no wait needed" sentinel.
    let exportSyncFd (device : Device) (sem : VkSemaphore) : int =
        let mutable getInfo = KHRExternalSemaphoreFd.VkSemaphoreGetFdInfoKHR(sem, SyncFdBit)
        let mutable fd = -1
        KHRExternalSemaphoreFd.VkRaw.vkGetSemaphoreFdKHR(device.Handle, &&getInfo, &&fd) |> check "vkGetSemaphoreFdKHR"
        fd

    // ---- Windows: OPAQUE_WIN32 semaphore handle (consumer waits via a D3D/KMT fence) ----
    let private OpaqueWin32Sem = VkExternalSemaphoreHandleTypeFlags.OpaqueWin32Bit

    let createExportableSemaphoreWin32 (device : Device) : VkSemaphore =
        let dev = device.Handle
        let mutable exportInfo = VkExportSemaphoreCreateInfo(OpaqueWin32Sem)
        let mutable info = VkSemaphoreCreateInfo(VkSemaphoreCreateFlags.None)
        info.pNext <- NativePtr.toNativeInt &&exportInfo
        let mutable sem = Unchecked.defaultof<VkSemaphore>
        VkRaw.vkCreateSemaphore(dev, &&info, NativePtr.zero, &&sem) |> check "vkCreateSemaphore(win32)"
        sem

    let exportSemaphoreWin32 (device : Device) (sem : VkSemaphore) : nativeint =
        let mutable getInfo = KHRExternalSemaphoreWin32.VkSemaphoreGetWin32HandleInfoKHR(sem, OpaqueWin32Sem)
        let mutable handle = 0n
        KHRExternalSemaphoreWin32.VkRaw.vkGetSemaphoreWin32HandleKHR(device.Handle, &&getInfo, &&handle) |> check "vkGetSemaphoreWin32HandleKHR"
        handle

    let destroySemaphore (device : Device) (sem : VkSemaphore) =
        VkRaw.vkDestroySemaphore(device.Handle, sem, NativePtr.zero)
