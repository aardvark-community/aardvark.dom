// D3D11 allocator for the REVERSE (supported) Vulkan<->D3D11 sharing direction — the production
// Windows recipe: Chromium's D3DImageBacking allocates the shared texture, Aardvark (Vulkan)
// renders into it. Here a standalone D3D11 process stands in for Chromium:
//
//   1. D3D11CreateDevice on the chosen DXGI adapter (LUID match to the Vulkan device).
//   2. Create a B8G8R8A8_UNORM Texture2D with
//        D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX | D3D11_RESOURCE_MISC_SHARED_NTHANDLE.
//   3. QI IDXGIResource1 -> CreateSharedHandle(DXGI_SHARED_RESOURCE_READ|WRITE) -> NT handle.
//   4. Write the handoff {handleValue, ourPID, w, h} and WAIT for the Vulkan importer to fill +
//      release the keyed mutex (release key 1).
//   5. IDXGIKeyedMutex::AcquireSync(1) -> CopyResource -> STAGING -> Map -> verify the producer's
//      2-axis gradient at 9 points -> ReleaseSync(0).
//
// Build (zephyrus, VS 2022 x64 dev shell):
//   cl /EHsc /nologo d3d11_alloc.cpp d3d11.lib dxgi.lib
//
// Usage:
//   d3d11_alloc.exe <handoffFile> [--luid <hex16>] [--w N] [--h N]

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <d3d11_1.h>
#include <dxgi1_2.h>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>

static void fail(const char* what, HRESULT hr) {
    fprintf(stderr, "[d3d11-alloc] FAIL %s hr=0x%08lX\n", what, (unsigned long)hr);
    exit(3);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <handoffFile> [--luid <hex16>] [--w N] [--h N]\n", argv[0]);
        return 2;
    }
    const char* handoffPath = argv[1];
    bool wantLuid = false;
    uint8_t wantLuidBytes[8] = {0};
    int W = 256, H = 256;
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "--luid") == 0 && i + 1 < argc) {
            wantLuid = true;
            const char* hx = argv[++i];
            for (int b = 0; b < 8 && hx[b*2] && hx[b*2+1]; b++) {
                char two[3] = { hx[b*2], hx[b*2+1], 0 };
                wantLuidBytes[b] = (uint8_t)strtoul(two, nullptr, 16);
            }
        } else if (strcmp(argv[i], "--w") == 0 && i + 1 < argc) { W = atoi(argv[++i]); }
        else if (strcmp(argv[i], "--h") == 0 && i + 1 < argc) { H = atoi(argv[++i]); }
    }

    // ---- pick the DXGI adapter (LUID match to the Vulkan device)
    IDXGIFactory1* factory = nullptr;
    HRESULT hr = CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&factory);
    if (FAILED(hr)) fail("CreateDXGIFactory1", hr);
    auto luidHex = [](const LUID& l, char* out) {
        const uint8_t* p = (const uint8_t*)&l;
        for (int b = 0; b < 8; b++) sprintf(out + b*2, "%02X", p[b]);
    };
    IDXGIAdapter1* chosen = nullptr; IDXGIAdapter1* adapter = nullptr;
    for (UINT i = 0; factory->EnumAdapters1(i, &adapter) != DXGI_ERROR_NOT_FOUND; i++) {
        DXGI_ADAPTER_DESC1 desc; adapter->GetDesc1(&desc);
        char hx[17] = {0}; luidHex(desc.AdapterLuid, hx);
        printf("[d3d11-alloc]   adapter[%u] LUID=%s  '%ls'\n", i, hx, desc.Description);
        bool match = wantLuid ? (memcmp(&desc.AdapterLuid, wantLuidBytes, 8) == 0) : (i == 0);
        if (match && !chosen) { chosen = adapter; chosen->AddRef(); }
        adapter->Release();
    }
    if (!chosen) { fprintf(stderr, "[d3d11-alloc] no matching DXGI adapter\n"); return 3; }
    { DXGI_ADAPTER_DESC1 d; chosen->GetDesc1(&d); char hx[17]={0}; luidHex(d.AdapterLuid, hx);
      printf("[d3d11-alloc] chosen adapter LUID=%s '%ls'\n", hx, d.Description); }

    // ---- D3D11 device on that adapter
    ID3D11Device* device = nullptr; ID3D11DeviceContext* ctx = nullptr; D3D_FEATURE_LEVEL fl;
    hr = D3D11CreateDevice(chosen, D3D_DRIVER_TYPE_UNKNOWN, nullptr, 0, nullptr, 0,
                           D3D11_SDK_VERSION, &device, &fl, &ctx);
    if (FAILED(hr)) fail("D3D11CreateDevice", hr);

    // ---- create the SHARED keyed-mutex NT-handle texture
    D3D11_TEXTURE2D_DESC td = {};
    td.Width = W; td.Height = H; td.MipLevels = 1; td.ArraySize = 1;
    td.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    td.SampleDesc.Count = 1;
    td.Usage = D3D11_USAGE_DEFAULT;
    td.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET;
    td.MiscFlags = D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX | D3D11_RESOURCE_MISC_SHARED_NTHANDLE;
    ID3D11Texture2D* tex = nullptr;
    hr = device->CreateTexture2D(&td, nullptr, &tex);
    if (FAILED(hr)) fail("CreateTexture2D(shared keyedmutex nthandle)", hr);

    // ---- export an NT handle via IDXGIResource1::CreateSharedHandle
    IDXGIResource1* res1 = nullptr;
    hr = tex->QueryInterface(__uuidof(IDXGIResource1), (void**)&res1);
    if (FAILED(hr)) fail("QI IDXGIResource1", hr);
    HANDLE sharedHandle = nullptr;
    hr = res1->CreateSharedHandle(nullptr, DXGI_SHARED_RESOURCE_READ | DXGI_SHARED_RESOURCE_WRITE,
                                  nullptr, &sharedHandle);
    if (FAILED(hr)) fail("CreateSharedHandle", hr);
    int pid = (int)GetCurrentProcessId();
    printf("[d3d11-alloc] D3D11 shared keyedmutex NT handle=0x%llX pid=%d %dx%d fmt=B8G8R8A8_UNORM\n",
           (unsigned long long)(uintptr_t)sharedHandle, pid, W, H);

    // ---- write the handoff: "<handleValue> <pid> <w> <h>"
    {
        FILE* f = fopen(handoffPath, "w");
        if (!f) { fprintf(stderr, "[d3d11-alloc] cannot write handoff %s\n", handoffPath); return 3; }
        fprintf(f, "%lld %d %d %d", (long long)(intptr_t)sharedHandle, pid, W, H);
        fclose(f);
    }
    printf("[d3d11-alloc] handoff -> %s ; waiting for Vulkan to import+fill+release keyed mutex (key 1)...\n",
           handoffPath);

    // ---- acquire key 1 (the key the Vulkan importer releases after filling). Poll with a
    // generous total timeout so the Vulkan side has time to start/import/fill.
    IDXGIKeyedMutex* km = nullptr;
    hr = tex->QueryInterface(__uuidof(IDXGIKeyedMutex), (void**)&km);
    if (FAILED(hr)) fail("QI IDXGIKeyedMutex", hr);
    hr = km->AcquireSync(1, 60000);   // wait up to 60s for the producer's release(1)
    if (FAILED(hr)) { fprintf(stderr, "[d3d11-alloc] AcquireSync(1) hr=0x%08lX (Vulkan never released?)\n",
                              (unsigned long)hr); return 3; }
    printf("[d3d11-alloc] AcquireSync(1) OK — Vulkan filled the texture\n");

    // ---- copy into a CPU-readable STAGING texture
    D3D11_TEXTURE2D_DESC sd = td;
    sd.Usage = D3D11_USAGE_STAGING; sd.BindFlags = 0;
    sd.CPUAccessFlags = D3D11_CPU_ACCESS_READ; sd.MiscFlags = 0;
    ID3D11Texture2D* staging = nullptr;
    hr = device->CreateTexture2D(&sd, nullptr, &staging);
    if (FAILED(hr)) fail("CreateTexture2D(staging)", hr);
    ctx->CopyResource(staging, tex);
    ctx->Flush();

    D3D11_MAPPED_SUBRESOURCE map;
    hr = ctx->Map(staging, 0, D3D11_MAP_READ, 0, &map);
    if (FAILED(hr)) fail("Map(staging)", hr);
    const uint8_t* base = (const uint8_t*)map.pData;
    UINT pitch = map.RowPitch;

    // ---- verify the producer's gradient: R=x-ramp, G=y-ramp, B=128, A=255 (BGRA in memory)
    struct Pt { int x, y; };
    Pt pts[] = { {0,0},{W-1,0},{0,H-1},{W-1,H-1},{W/4,H/4},{3*W/4,H/4},{W/4,3*H/4},{3*W/4,3*H/4},{W/2,H/2} };
    bool allok = true;
    for (auto& p : pts) {
        const uint8_t* px = base + (size_t)p.y * pitch + (size_t)p.x * 4;
        int b = px[0], g = px[1], r = px[2], a = px[3];
        int er = p.x * 255 / (W - 1), eg = p.y * 255 / (H - 1), eb = 128, ea = 255;
        bool ok = abs(r-er)<=4 && abs(g-eg)<=4 && abs(b-eb)<=4 && abs(a-ea)<=4;
        if (!ok) allok = false;
        printf("[d3d11-alloc] (%3d,%3d) got=(%3d,%3d,%3d,%3d) exp=(%3d,%3d,%3d,%3d) %s\n",
               p.x, p.y, r, g, b, a, er, eg, eb, ea, ok ? "ok" : "BAD");
    }
    ctx->Unmap(staging, 0);
    km->ReleaseSync(0);   // hand ownership back (key 0) so a streaming producer could re-acquire

    printf("[d3d11-alloc] gradient fidelity: %s\n",
           allok ? "PASS -- D3D11-allocates / Vulkan-imports WORKS" : "FAIL -- scrambled/garbage");

    km->Release(); staging->Release(); res1->Release(); tex->Release();
    ctx->Release(); device->Release(); chosen->Release(); factory->Release();
    // NOTE: we intentionally do NOT CloseHandle(sharedHandle) before the consumer dup'd it; the
    // process exit closes it. The Vulkan side DuplicateHandle'd its own copy.
    return allok ? 0 : 1;
}
