namespace Aardvark.Dom.Remote.SharedTexture

// Windows control channel for the zero-copy SharedTexture streaming protocol — the
// Win32 analog of FdHandoff (Linux AF_UNIX SCM_RIGHTS). Linux passes the OPAQUE_FD
// memfd itself over the socket (SCM_RIGHTS) because an fd is process-local; on Windows
// an NT handle is likewise process-local, but it is moved by VALUE in the REG line and
// reconstituted in the browser via OpenProcess(PROCESS_DUP_HANDLE)+DuplicateHandle
// (exactly what the single-frame Win32Handoff path + Chromium painter already do). So
// the pipe only ever carries ASCII — no handle marshalling on the wire.
//
// Wire protocol mirrors the Linux REG/FREE side-channel, with the NT-handle value and
// producer PID folded into the REG line (replacing the SCM_RIGHTS fd):
//   producer -> browser :  "REG <textureId> <w> <h> <ntHandleValue> <producerPID>\n"  (once per slot per realloc)
//   producer -> browser :  "F <textureId>\n"                                          (per-frame tick — the swap)
//   browser  -> producer :  "FREE <textureId>\n"                                       (release-callback backpressure)
//
// TRANSPORT: a duplex named pipe  \\.\pipe\aardvark-<channel> . The BROWSER is the pipe
// SERVER (CreateNamedPipe + ConnectNamedPipe on a bg thread — mirrors the Linux browser
// being the socket LISTENER); the PRODUCER is the pipe CLIENT (CreateFile, with a short
// connect retry because the browser only creates the pipe once the page binds the channel,
// matching the Linux 200ms connect-backoff). Message-mode pipes preserve message
// boundaries; we read line-at-a-time defensively regardless.
//
// kernel32 P/Invokes resolve at runtime on Windows only (compile fine on Linux).

open System
open System.Text
open System.Runtime.InteropServices

module NamedPipeHandoff =

    [<DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)>]
    extern nativeint CreateFileW(
        string lpFileName, uint32 dwDesiredAccess, uint32 dwShareMode,
        nativeint lpSecurityAttributes, uint32 dwCreationDisposition,
        uint32 dwFlagsAndAttributes, nativeint hTemplateFile)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool ReadFile(nativeint hFile, byte[] lpBuffer, uint32 nNumberOfBytesToRead,
                         uint32& lpNumberOfBytesRead, nativeint lpOverlapped)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool WriteFile(nativeint hFile, byte[] lpBuffer, uint32 nNumberOfBytesToWrite,
                          uint32& lpNumberOfBytesWritten, nativeint lpOverlapped)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool CloseHandle(nativeint hObject)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool SetNamedPipeHandleState(nativeint hNamedPipe, uint32& lpMode,
                                        nativeint lpMaxCollectionCount, nativeint lpCollectDataTimeout)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool PeekNamedPipe(nativeint hNamedPipe, nativeint lpBuffer, uint32 nBufferSize,
                              nativeint lpBytesRead, uint32& lpTotalBytesAvail, nativeint lpBytesLeftThisMessage)

    let private GENERIC_READ  = 0x80000000u
    let private GENERIC_WRITE = 0x40000000u
    let private OPEN_EXISTING = 3u
    let private INVALID_HANDLE_VALUE = nativeint -1
    let private PIPE_READMODE_BYTE = 0x00000000u
    let private PIPE_NOWAIT = 0x00000001u
    let private PIPE_WAIT = 0x00000000u

    let pipeName (channel : string) = sprintf @"\\.\pipe\aardvark-%s" channel

    /// PRODUCER (pipe CLIENT): connect to the browser's named-pipe server for `channel`.
    /// Returns the pipe handle, or throws if the browser hasn't created the pipe yet
    /// (the caller retries with a backoff, exactly like the Linux streamConnect retry).
    let connect (channel : string) : nativeint =
        let name = pipeName channel
        let h = CreateFileW(name, GENERIC_READ ||| GENERIC_WRITE, 0u, 0n, OPEN_EXISTING, 0u, 0n)
        if h = INVALID_HANDLE_VALUE || h = 0n then
            failwithf "[NamedPipeHandoff] connect %s failed (err %d)" name (Marshal.GetLastWin32Error())
        // byte-mode reads (we frame on '\n' ourselves); leave blocking for writes.
        h

    /// Send one ASCII line (the caller includes the trailing '\n'). Returns false if the
    /// peer (browser) is gone — the producer then drops the connection and re-connects.
    let send (h : nativeint) (msg : string) : bool =
        let bytes = Encoding.ASCII.GetBytes msg
        let mutable written = 0u
        let ok = WriteFile(h, bytes, uint32 bytes.Length, &written, 0n)
        ok && int written = bytes.Length

    /// Non-blocking poll for any pending bytes from the browser (the "FREE <id>" backpressure
    /// messages). Returns "" if nothing is waiting. Uses PeekNamedPipe to avoid blocking the
    /// render thread (the producer mirrors the Linux MSG_DONTWAIT streamPoll).
    let poll (h : nativeint) : string =
        let mutable avail = 0u
        if not (PeekNamedPipe(h, 0n, 0u, 0n, &avail, 0n)) then ""
        elif avail = 0u then ""
        else
            let cap = min (int avail) 4096
            let buf = Array.zeroCreate<byte> cap
            let mutable read = 0u
            if ReadFile(h, buf, uint32 cap, &read, 0n) && read > 0u then
                Encoding.ASCII.GetString(buf, 0, int read)
            else ""

    let close (h : nativeint) =
        if h <> 0n && h <> INVALID_HANDLE_VALUE then CloseHandle h |> ignore
