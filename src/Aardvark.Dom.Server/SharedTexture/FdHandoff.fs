namespace Aardvark.Dom.Remote.SharedTexture

// Cross-process dma-buf handoff over a unix socket using SCM_RIGHTS — the same
// mechanism Chromium/Mojo uses to move a dma-buf fd into the GPU process. The
// exporter (client) sends the fd + layout to a consumer (server) which receives a
// fresh fd to the same underlying dma_buf and can EGL-import it. (Reopening via
// /proc/<pid>/fd does NOT work for dma-buf — ENXIO — hence SCM_RIGHTS.)

open System
open System.Text
open System.Runtime.InteropServices
open Aardvark.Rendering.Vulkan

module FdHandoff =

    [<DllImport("libc", SetLastError = true)>] extern int socket(int domain, int typ, int protocol)
    [<DllImport("libc", SetLastError = true)>] extern int bind(int fd, byte[] addr, int len)
    [<DllImport("libc", SetLastError = true)>] extern int listen(int fd, int backlog)
    [<DllImport("libc", SetLastError = true)>] extern int accept(int fd, nativeint addr, nativeint len)
    [<DllImport("libc", SetLastError = true)>] extern int connect(int fd, byte[] addr, int len)
    [<DllImport("libc", SetLastError = true)>] extern int close(int fd)
    [<DllImport("libc", SetLastError = true)>] extern int unlink(string path)
    [<DllImport("libc", SetLastError = true)>] extern nativeint sendmsg(int fd, nativeint msg, int flags)
    [<DllImport("libc", SetLastError = true)>] extern nativeint recvmsg(int fd, nativeint msg, int flags)

    let private AF_UNIX = 1
    let private SOCK_STREAM = 1
    let private SOL_SOCKET = 1
    let private SCM_RIGHTS = 1

    // x86_64 Linux: cmsghdr = size_t cmsg_len(8) + int level(4) + int type(4); data @16
    // one fd -> CMSG_LEN(4)=20, CMSG_SPACE(4)=24. msghdr = 56 bytes, iovec = 16.
    let private CMSG_LEN = 20L
    let private CMSG_SPACE = 24

    let private errno () = Marshal.GetLastWin32Error()

    /// sockaddr_un { sa_family(2) ; char path[108] }, returns (bytes, usedLen)
    let private sockaddr (path : string) =
        let buf = Array.zeroCreate<byte> 110
        buf.[0] <- 1uy // AF_UNIX little-endian
        buf.[1] <- 0uy
        let pb = Encoding.ASCII.GetBytes path
        Array.blit pb 0 buf 2 pb.Length
        buf, 2 + pb.Length + 1

    let private payloadOf (img : DmaBufImage) =
        sprintf "%d %d %d %d %d %d" img.Width img.Height
            (int64 img.Fourcc) (int64 img.Modifier) (int64 img.Offset) (int64 img.Stride)
        |> Encoding.ASCII.GetBytes

    /// Client: connect to `sockPath` and send the dma-buf fd + layout.
    let sendFd (sockPath : string) (img : DmaBufImage) =
        let s = socket(AF_UNIX, SOCK_STREAM, 0)
        if s < 0 then failwithf "[FdHandoff] socket failed (errno %d)" (errno())
        let addr, alen = sockaddr sockPath
        if connect(s, addr, alen) < 0 then failwithf "[FdHandoff] connect failed (errno %d)" (errno())

        let payload = payloadOf img
        let pPayload = Marshal.AllocHGlobal payload.Length
        Marshal.Copy(payload, 0, pPayload, payload.Length)
        let pIov = Marshal.AllocHGlobal 16
        Marshal.WriteIntPtr(pIov, 0, pPayload)
        Marshal.WriteInt64(pIov, 8, int64 payload.Length)

        let pControl = Marshal.AllocHGlobal CMSG_SPACE
        Marshal.WriteInt64(pControl, 0, CMSG_LEN)
        Marshal.WriteInt32(pControl, 8, SOL_SOCKET)
        Marshal.WriteInt32(pControl, 12, SCM_RIGHTS)
        Marshal.WriteInt32(pControl, 16, img.Fd)

        let pMsg = Marshal.AllocHGlobal 56
        for i in 0 .. 6 do Marshal.WriteInt64(pMsg, i * 8, 0L)
        Marshal.WriteIntPtr(pMsg, 16, pIov)
        Marshal.WriteInt64(pMsg, 24, 1L)
        Marshal.WriteIntPtr(pMsg, 32, pControl)
        Marshal.WriteInt64(pMsg, 40, int64 CMSG_SPACE)

        let n = sendmsg(s, pMsg, 0)
        if n.ToInt64() < 0L then failwithf "[FdHandoff] sendmsg failed (errno %d)" (errno())
        Marshal.FreeHGlobal pMsg; Marshal.FreeHGlobal pControl
        Marshal.FreeHGlobal pIov; Marshal.FreeHGlobal pPayload
        close s |> ignore

    /// Server: bind/listen/accept on `sockPath`, receive a dma-buf fd + layout,
    /// and return a DmaBufImage (Vulkan handles default — only the fd is needed).
    let recvFd (sockPath : string) : DmaBufImage =
        unlink sockPath |> ignore
        let s = socket(AF_UNIX, SOCK_STREAM, 0)
        if s < 0 then failwithf "[FdHandoff] socket failed (errno %d)" (errno())
        let addr, alen = sockaddr sockPath
        if bind(s, addr, alen) < 0 then failwithf "[FdHandoff] bind failed (errno %d)" (errno())
        if listen(s, 1) < 0 then failwithf "[FdHandoff] listen failed (errno %d)" (errno())
        let conn = accept(s, 0n, 0n)
        if conn < 0 then failwithf "[FdHandoff] accept failed (errno %d)" (errno())

        let cap = 256
        let pPayload = Marshal.AllocHGlobal cap
        let pIov = Marshal.AllocHGlobal 16
        Marshal.WriteIntPtr(pIov, 0, pPayload)
        Marshal.WriteInt64(pIov, 8, int64 cap)

        let pControl = Marshal.AllocHGlobal CMSG_SPACE
        for i in 0 .. 2 do Marshal.WriteInt64(pControl, i * 8, 0L)

        let pMsg = Marshal.AllocHGlobal 56
        for i in 0 .. 6 do Marshal.WriteInt64(pMsg, i * 8, 0L)
        Marshal.WriteIntPtr(pMsg, 16, pIov)
        Marshal.WriteInt64(pMsg, 24, 1L)
        Marshal.WriteIntPtr(pMsg, 32, pControl)
        Marshal.WriteInt64(pMsg, 40, int64 CMSG_SPACE)

        let n = recvmsg(conn, pMsg, 0)
        if n.ToInt64() < 0L then failwithf "[FdHandoff] recvmsg failed (errno %d)" (errno())

        let level = Marshal.ReadInt32(pControl, 8)
        let typ = Marshal.ReadInt32(pControl, 12)
        if level <> SOL_SOCKET || typ <> SCM_RIGHTS then
            failwithf "[FdHandoff] no SCM_RIGHTS in control message (level=%d type=%d)" level typ
        let fd = Marshal.ReadInt32(pControl, 16)

        let len = int (n.ToInt64())
        let bytes = Array.zeroCreate<byte> len
        Marshal.Copy(pPayload, bytes, 0, len)
        let p = (Encoding.ASCII.GetString bytes).Trim().Split(' ')

        Marshal.FreeHGlobal pMsg; Marshal.FreeHGlobal pControl
        Marshal.FreeHGlobal pIov; Marshal.FreeHGlobal pPayload
        close conn |> ignore; close s |> ignore; unlink sockPath |> ignore

        {
            Fd       = fd
            Width    = int p.[0]
            Height   = int p.[1]
            Fourcc   = uint32 (int64 p.[2])
            Modifier = uint64 (int64 p.[3])
            Offset   = uint64 (int64 p.[4])
            Stride   = uint64 (int64 p.[5])
            Image    = Unchecked.defaultof<VkImage>
            Memory   = Unchecked.defaultof<VkDeviceMemory>
            Size     = 0UL
        }
