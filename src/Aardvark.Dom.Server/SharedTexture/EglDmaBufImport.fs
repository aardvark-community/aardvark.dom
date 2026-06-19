namespace Aardvark.Dom.Remote.SharedTexture

// Milestone 0b validator: import the exported dma-buf via EGL (the same path
// Chromium uses for a NativePixmap on Linux) and read the pixels back, proving
// the fd our Vulkan exporter produces is consumable WITHOUT a host download on
// the producer side. Runs in-process (an fd is process-local); the cross-process
// SCM_RIGHTS handoff to a real browser is a later concern.
//
// NVIDIA only allows a dma-buf EGLImage to be bound as GL_TEXTURE_EXTERNAL_OES
// (and external textures are not FBO-attachable), so we sample it through a
// samplerExternalOES shader into a normal RGBA texture and read THAT back.
//
// Uses a surfaceless EGL/GLES2 context (works on NVIDIA 595, verified).

open System
open System.Text
open System.Runtime.InteropServices

module EglDmaBufImport =

    // ---- EGL / GL constants ----
    let private EGL_NONE = 0x3038
    let private EGL_TRUE = 1u
    let private EGL_DEFAULT_DISPLAY = 0n
    let private EGL_NO_CONTEXT = 0n
    let private EGL_NO_SURFACE = 0n
    let private EGL_PLATFORM_SURFACELESS_MESA = 0x31DDu
    let private EGL_OPENGL_ES_API = 0x30A0u
    let private EGL_CONTEXT_CLIENT_VERSION = 0x3098
    let private EGL_RENDERABLE_TYPE = 0x3040
    let private EGL_OPENGL_ES2_BIT = 0x0004
    let private EGL_SURFACE_TYPE = 0x3033
    let private EGL_PBUFFER_BIT = 0x0001
    let private EGL_RED_SIZE = 0x3024
    let private EGL_GREEN_SIZE = 0x3023
    let private EGL_BLUE_SIZE = 0x3022
    let private EGL_ALPHA_SIZE = 0x3021

    let private EGL_LINUX_DMA_BUF_EXT = 0x3270u
    let private EGL_WIDTH = 0x3057
    let private EGL_HEIGHT = 0x3056
    let private EGL_LINUX_DRM_FOURCC_EXT = 0x3271
    let private EGL_DMA_BUF_PLANE0_FD_EXT = 0x3272
    let private EGL_DMA_BUF_PLANE0_OFFSET_EXT = 0x3273
    let private EGL_DMA_BUF_PLANE0_PITCH_EXT = 0x3274
    let private EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT = 0x3443
    let private EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT = 0x3444

    let private GL_TEXTURE_2D = 0x0DE1u
    let private GL_TEXTURE_EXTERNAL_OES = 0x8D65u
    let private GL_TEXTURE_MIN_FILTER = 0x2801u
    let private GL_TEXTURE_MAG_FILTER = 0x2800u
    let private GL_TEXTURE_WRAP_S = 0x2802u
    let private GL_TEXTURE_WRAP_T = 0x2803u
    let private GL_CLAMP_TO_EDGE = 0x812F
    let private GL_NEAREST = 0x2600
    let private GL_FRAMEBUFFER = 0x8D40u
    let private GL_COLOR_ATTACHMENT0 = 0x8CE0u
    let private GL_FRAMEBUFFER_COMPLETE = 0x8CD5u
    let private GL_RGBA = 0x1908u
    let private GL_RGBAi = 0x1908
    let private GL_UNSIGNED_BYTE = 0x1401u
    let private GL_FRAGMENT_SHADER = 0x8B30u
    let private GL_VERTEX_SHADER = 0x8B31u
    let private GL_COMPILE_STATUS = 0x8B81u
    let private GL_LINK_STATUS = 0x8B82u
    let private GL_FLOAT = 0x1406u
    let private GL_TRIANGLE_STRIP = 0x0005u
    let private GL_TEXTURE0 = 0x84C0u
    let private GL_COLOR_BUFFER_BIT = 0x4000u

    // ---- libEGL ----
    [<DllImport("libEGL.so.1")>] extern nativeint eglGetProcAddress(string proc)
    [<DllImport("libEGL.so.1")>] extern uint32 eglInitialize(nativeint dpy, int& major, int& minor)
    [<DllImport("libEGL.so.1")>] extern uint32 eglBindAPI(uint32 api)
    [<DllImport("libEGL.so.1")>] extern uint32 eglChooseConfig(nativeint dpy, int[] attribs, nativeint[] configs, int configSize, int& numConfig)
    [<DllImport("libEGL.so.1")>] extern nativeint eglCreateContext(nativeint dpy, nativeint config, nativeint share, int[] attribs)
    [<DllImport("libEGL.so.1")>] extern uint32 eglMakeCurrent(nativeint dpy, nativeint draw, nativeint read, nativeint ctx)
    [<DllImport("libEGL.so.1")>] extern int eglGetError()

    // ---- libGLESv2 ----
    [<DllImport("libGLESv2.so.2")>] extern void glGenTextures(int n, uint32[] textures)
    [<DllImport("libGLESv2.so.2")>] extern void glBindTexture(uint32 target, uint32 texture)
    [<DllImport("libGLESv2.so.2")>] extern void glTexParameteri(uint32 target, uint32 pname, int param)
    [<DllImport("libGLESv2.so.2")>] extern void glTexImage2D(uint32 target, int level, int internalFormat, int w, int h, int border, uint32 fmt, uint32 typ, nativeint pixels)
    [<DllImport("libGLESv2.so.2")>] extern void glGenFramebuffers(int n, uint32[] fb)
    [<DllImport("libGLESv2.so.2")>] extern void glBindFramebuffer(uint32 target, uint32 fb)
    [<DllImport("libGLESv2.so.2")>] extern void glFramebufferTexture2D(uint32 target, uint32 att, uint32 textarget, uint32 tex, int level)
    [<DllImport("libGLESv2.so.2")>] extern uint32 glCheckFramebufferStatus(uint32 target)
    [<DllImport("libGLESv2.so.2")>] extern void glViewport(int x, int y, int w, int h)
    [<DllImport("libGLESv2.so.2")>] extern void glClearColor(float32 r, float32 g, float32 b, float32 a)
    [<DllImport("libGLESv2.so.2")>] extern void glClear(uint32 mask)
    [<DllImport("libGLESv2.so.2")>] extern void glReadPixels(int x, int y, int w, int h, uint32 fmt, uint32 typ, byte[] data)
    [<DllImport("libGLESv2.so.2")>] extern uint32 glGetError()
    [<DllImport("libGLESv2.so.2")>] extern void glFinish()
    [<DllImport("libGLESv2.so.2")>] extern uint32 glCreateShader(uint32 typ)
    [<DllImport("libGLESv2.so.2")>] extern void glShaderSource(uint32 shader, int count, string[] str, int[] length)
    [<DllImport("libGLESv2.so.2")>] extern void glCompileShader(uint32 shader)
    [<DllImport("libGLESv2.so.2")>] extern void glGetShaderiv(uint32 shader, uint32 pname, int& res)
    [<DllImport("libGLESv2.so.2")>] extern void glGetShaderInfoLog(uint32 shader, int maxLen, int& len, byte[] log)
    [<DllImport("libGLESv2.so.2")>] extern uint32 glCreateProgram()
    [<DllImport("libGLESv2.so.2")>] extern void glAttachShader(uint32 prog, uint32 shader)
    [<DllImport("libGLESv2.so.2")>] extern void glLinkProgram(uint32 prog)
    [<DllImport("libGLESv2.so.2")>] extern void glGetProgramiv(uint32 prog, uint32 pname, int& res)
    [<DllImport("libGLESv2.so.2")>] extern void glUseProgram(uint32 prog)
    [<DllImport("libGLESv2.so.2")>] extern int glGetAttribLocation(uint32 prog, string name)
    [<DllImport("libGLESv2.so.2")>] extern int glGetUniformLocation(uint32 prog, string name)
    [<DllImport("libGLESv2.so.2")>] extern void glUniform1i(int loc, int v)
    [<DllImport("libGLESv2.so.2")>] extern void glEnableVertexAttribArray(uint32 index)
    [<DllImport("libGLESv2.so.2")>] extern void glVertexAttribPointer(uint32 index, int size, uint32 typ, byte normalized, int stride, nativeint ptr)
    [<DllImport("libGLESv2.so.2")>] extern void glDrawArrays(uint32 mode, int first, int count)
    [<DllImport("libGLESv2.so.2")>] extern void glActiveTexture(uint32 tex)

    // extension entrypoints via eglGetProcAddress
    type private GetPlatformDisplayDel = delegate of uint32 * nativeint * int[] -> nativeint
    type private CreateImageDel = delegate of nativeint * nativeint * uint32 * nativeint * int[] -> nativeint
    type private ImageTargetTexture2DDel = delegate of uint32 * nativeint -> unit

    let private getProc<'a> (name : string) : 'a =
        let p = eglGetProcAddress name
        if p = 0n then failwithf "[EGL] eglGetProcAddress(%s) returned null" name
        Marshal.GetDelegateForFunctionPointer<'a>(p)

    let private compile (typ : uint32) (src : string) =
        let s = glCreateShader typ
        glShaderSource(s, 1, [| src |], [| src.Length |])
        glCompileShader s
        let mutable ok = 0
        glGetShaderiv(s, GL_COMPILE_STATUS, &ok)
        if ok = 0 then
            let log = Array.zeroCreate<byte> 4096
            let mutable len = 0
            glGetShaderInfoLog(s, log.Length, &len, log)
            failwithf "[GL] shader compile failed: %s" (Encoding.ASCII.GetString(log, 0, len))
        s

    let private vsSrc =
        "attribute vec2 aPos;\n\
         varying vec2 vUv;\n\
         void main(){ vUv = aPos*0.5+0.5; gl_Position = vec4(aPos,0.0,1.0); }\n"

    let private fsSrc =
        "#extension GL_OES_EGL_image_external : require\n\
         precision mediump float;\n\
         varying vec2 vUv;\n\
         uniform samplerExternalOES uTex;\n\
         void main(){ gl_FragColor = texture2D(uTex, vUv); }\n"

    /// Import the dma-buf via EGL, sample it through a samplerExternalOES shader,
    /// and read the WxH RGBA pixels back (GL_RGBA/UNSIGNED_BYTE). This is the same
    /// path Chromium's NativePixmap importer uses on Linux.
    let readbackRGBA (img : DmaBufImage) : byte[] =
        let getPlatformDisplay = getProc<GetPlatformDisplayDel> "eglGetPlatformDisplayEXT"
        let eglCreateImageKHR  = getProc<CreateImageDel> "eglCreateImageKHR"
        let glEGLImageTargetTexture2DOES = getProc<ImageTargetTexture2DDel> "glEGLImageTargetTexture2DOES"

        let dpy = getPlatformDisplay.Invoke(EGL_PLATFORM_SURFACELESS_MESA, EGL_DEFAULT_DISPLAY, [| EGL_NONE |])
        if dpy = 0n then failwith "[EGL] no surfaceless display"

        let mutable major = 0
        let mutable minor = 0
        if eglInitialize(dpy, &major, &minor) <> EGL_TRUE then failwithf "[EGL] eglInitialize failed 0x%x" (eglGetError())
        if eglBindAPI(EGL_OPENGL_ES_API) <> EGL_TRUE then failwith "[EGL] eglBindAPI failed"

        let cfgAttribs =
            [| EGL_RENDERABLE_TYPE; EGL_OPENGL_ES2_BIT
               EGL_SURFACE_TYPE;    EGL_PBUFFER_BIT
               EGL_RED_SIZE; 8; EGL_GREEN_SIZE; 8; EGL_BLUE_SIZE; 8; EGL_ALPHA_SIZE; 8
               EGL_NONE |]
        let configs = Array.zeroCreate<nativeint> 1
        let mutable num = 0
        if eglChooseConfig(dpy, cfgAttribs, configs, 1, &num) <> EGL_TRUE || num < 1 then
            failwithf "[EGL] eglChooseConfig failed 0x%x (num=%d)" (eglGetError()) num

        let ctx = eglCreateContext(dpy, configs.[0], EGL_NO_CONTEXT, [| EGL_CONTEXT_CLIENT_VERSION; 2; EGL_NONE |])
        if ctx = 0n then failwithf "[EGL] eglCreateContext failed 0x%x" (eglGetError())
        if eglMakeCurrent(dpy, EGL_NO_SURFACE, EGL_NO_SURFACE, ctx) <> EGL_TRUE then
            failwithf "[EGL] eglMakeCurrent (surfaceless) failed 0x%x" (eglGetError())

        // import the dma-buf as an EGLImage
        let modLo = int (img.Modifier &&& 0xFFFFFFFFUL)
        let modHi = int (img.Modifier >>> 32)
        let imgAttribs =
            [| EGL_WIDTH; img.Width
               EGL_HEIGHT; img.Height
               EGL_LINUX_DRM_FOURCC_EXT; int img.Fourcc
               EGL_DMA_BUF_PLANE0_FD_EXT; img.Fd
               EGL_DMA_BUF_PLANE0_OFFSET_EXT; int img.Offset
               EGL_DMA_BUF_PLANE0_PITCH_EXT; int img.Stride
               EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT; modLo
               EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT; modHi
               EGL_NONE |]
        let eglImage = eglCreateImageKHR.Invoke(dpy, EGL_NO_CONTEXT, EGL_LINUX_DMA_BUF_EXT, 0n, imgAttribs)
        if eglImage = 0n then failwithf "[EGL] eglCreateImageKHR failed 0x%x" (eglGetError())
        printfn "[egl-validate] eglCreateImageKHR ok (dma-buf accepted by NVIDIA EGL importer)"

        // bind the imported image to an EXTERNAL texture (NVIDIA requirement)
        let extTex = Array.zeroCreate<uint32> 1
        glGenTextures(1, extTex)
        glBindTexture(GL_TEXTURE_EXTERNAL_OES, extTex.[0])
        glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
        glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
        glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
        glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
        glEGLImageTargetTexture2DOES.Invoke(GL_TEXTURE_EXTERNAL_OES, eglImage)
        let glErr = glGetError()
        if glErr <> 0u then failwithf "[GL] glEGLImageTargetTexture2DOES error 0x%x" glErr

        // offscreen RGBA target (normal texture, FBO-attachable)
        let dstTex = Array.zeroCreate<uint32> 1
        glGenTextures(1, dstTex)
        glBindTexture(GL_TEXTURE_2D, dstTex.[0])
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBAi, img.Width, img.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0n)

        let fb = Array.zeroCreate<uint32> 1
        glGenFramebuffers(1, fb)
        glBindFramebuffer(GL_FRAMEBUFFER, fb.[0])
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, dstTex.[0], 0)
        let status = glCheckFramebufferStatus(GL_FRAMEBUFFER)
        if status <> GL_FRAMEBUFFER_COMPLETE then failwithf "[GL] framebuffer incomplete: 0x%x" status

        // shader program
        let prog = glCreateProgram()
        glAttachShader(prog, compile GL_VERTEX_SHADER vsSrc)
        glAttachShader(prog, compile GL_FRAGMENT_SHADER fsSrc)
        glLinkProgram prog
        let mutable linked = 0
        glGetProgramiv(prog, GL_LINK_STATUS, &linked)
        if linked = 0 then failwith "[GL] program link failed"

        // fullscreen quad (triangle strip), pinned across the draw call
        let verts = [| -1.0f; -1.0f;  1.0f; -1.0f; -1.0f; 1.0f;  1.0f; 1.0f |]
        let gch = GCHandle.Alloc(verts, GCHandleType.Pinned)
        try
            glViewport(0, 0, img.Width, img.Height)
            glClearColor(1.0f, 0.0f, 1.0f, 1.0f)
            glClear(GL_COLOR_BUFFER_BIT)
            glUseProgram prog
            glActiveTexture GL_TEXTURE0
            glBindTexture(GL_TEXTURE_EXTERNAL_OES, extTex.[0])
            glUniform1i(glGetUniformLocation(prog, "uTex"), 0)
            let aPos = glGetAttribLocation(prog, "aPos") |> uint32
            glEnableVertexAttribArray aPos
            glVertexAttribPointer(aPos, 2, GL_FLOAT, 0uy, 0, gch.AddrOfPinnedObject())
            glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)
            glFinish()
        finally
            gch.Free()

        let data = Array.zeroCreate<byte> (img.Width * img.Height * 4)
        glReadPixels(0, 0, img.Width, img.Height, GL_RGBA, GL_UNSIGNED_BYTE, data)
        data

    /// Validate the CPU-written gradient test pattern (Milestone 0b).
    let validate (img : DmaBufImage) : bool =
        let data = readbackRGBA img
        // producer wrote BGRA mem (B=x-grad, G=y-grad, R=0, A=255) -> sampled color
        // is R=0, G=y-grad, B=x-grad. glReadPixels is bottom-up so allow a Y flip.
        let sample x y = let i = (y * img.Width + x) * 4 in (int data.[i], int data.[i+1], int data.[i+2], int data.[i+3])
        let g v n = v * 255 / max 1 (n - 1)
        let close (ar,ag,ab,aa) (er,eg,eb,ea) =
            abs (ar-er) <= 3 && abs (ag-eg) <= 3 && abs (ab-eb) <= 3 && abs (aa-ea) <= 3
        let pts = [ 0,0; img.Width-1, 0; 0, img.Height-1; img.Width/2, img.Height/2; 64, 192 ]
        let mutable okDirect = true
        let mutable okFlip = true
        for (x,y) in pts do
            let got = sample x y
            let expDirect = (0, g y img.Height, g x img.Width, 255)
            let expFlip   = (0, g (img.Height-1-y) img.Height, g x img.Width, 255)
            if not (close got expDirect) then okDirect <- false
            if not (close got expFlip)   then okFlip   <- false
            printfn "[egl-validate] (%4d,%4d) got RGBA=%A  expectB(x)=%d expectG(y)=%d" x y got (g x img.Width) (g y img.Height)
        let ok = okDirect || okFlip
        printfn "[egl-validate] gradient match: %s (orientation: %s)"
            (if ok then "YES" else "NO") (if okDirect then "direct" elif okFlip then "y-flipped" else "none")
        ok
