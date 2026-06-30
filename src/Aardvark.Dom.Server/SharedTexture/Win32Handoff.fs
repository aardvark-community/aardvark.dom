namespace Aardvark.Dom.Remote.SharedTexture

// Cross-process Win32 NT-handle handoff — the Windows analog of FdHandoff (Linux SCM_RIGHTS).
// On Linux a dma-buf/OPAQUE_FD fd is meaningless in another process, so the fd must be passed
// via SCM_RIGHTS over a unix socket. On Windows an NT handle value is likewise process-local;
// the way to move it is OpenProcess(PROCESS_DUP_HANDLE) + DuplicateHandle into the target
// process (exactly what Chromium's base::win::ScopedHandle / broker does). The handoff metadata
// (handleValue, producerPID, w, h) travels via a plain file (stand-in for the IPC channel).
//
// P/Invokes to kernel32 — these compile fine on Linux (resolve at runtime on Windows); only
// run on Windows.

open System
open System.IO
open System.Runtime.InteropServices

module Win32Handoff =

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern nativeint OpenProcess(uint32 dwDesiredAccess, bool bInheritHandle, uint32 dwProcessId)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern nativeint GetCurrentProcess()

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool DuplicateHandle(
        nativeint hSourceProcessHandle, nativeint hSourceHandle,
        nativeint hTargetProcessHandle, nativeint& lpTargetHandle,
        uint32 dwDesiredAccess, bool bInheritHandle, uint32 dwOptions)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool CloseHandle(nativeint hObject)

    let private PROCESS_DUP_HANDLE = 0x0040u
    let private DUPLICATE_SAME_ACCESS = 0x00000002u

    /// PRODUCER: write the handoff descriptor to `path`. The producer MUST stay alive holding
    /// the handle + image until the consumer has DuplicateHandle'd it.
    let writeHandoff (path : string) (handle : nativeint) (w : int) (h : int) =
        let pid = System.Diagnostics.Process.GetCurrentProcess().Id
        // "<handleValue> <producerPID> <w> <h>"
        File.WriteAllText(path, sprintf "%d %d %d %d" (int64 handle) pid w h)

    type Handoff = { Handle : nativeint; ProducerPID : int; Width : int; Height : int }

    /// CONSUMER: read the descriptor file (waits for it to appear).
    let readHandoff (path : string) : Handoff =
        let mutable tries = 0
        while not (File.Exists path) && tries < 600 do
            System.Threading.Thread.Sleep 100
            tries <- tries + 1
        if not (File.Exists path) then failwithf "[Win32Handoff] handoff file %s never appeared" path
        let parts = (File.ReadAllText path).Trim().Split(' ')
        { Handle = nativeint (int64 parts.[0]); ProducerPID = int parts.[1]
          Width = int parts.[2]; Height = int parts.[3] }

    /// CONSUMER: OpenProcess(PROCESS_DUP_HANDLE, producerPID) + DuplicateHandle the producer's
    /// handle into THIS process. Returns a handle valid in the current process (what Vulkan
    /// VkImportMemoryWin32HandleInfoKHR consumes).
    let duplicateIntoSelf (producerPID : int) (sourceHandle : nativeint) : nativeint =
        let producerProc = OpenProcess(PROCESS_DUP_HANDLE, false, uint32 producerPID)
        if producerProc = 0n then
            failwithf "[Win32Handoff] OpenProcess(pid=%d) failed (err %d)" producerPID (Marshal.GetLastWin32Error())
        let mutable dup = 0n
        let ok =
            DuplicateHandle(producerProc, sourceHandle, GetCurrentProcess(), &dup,
                            0u, false, DUPLICATE_SAME_ACCESS)
        if not ok then
            failwithf "[Win32Handoff] DuplicateHandle failed (err %d)" (Marshal.GetLastWin32Error())
        CloseHandle producerProc |> ignore
        dup
