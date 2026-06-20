// D3D11 consumer for the Vulkan->D3D11 cross-process shared-texture de-risk (the production
// Windows path: how Chromium's default D3DImageBacking imports a texture).
//
// Reads the handoff file {handleValue producerPID w h}, OpenProcess(PROCESS_DUP_HANDLE) +
// DuplicateHandle the producer's DXGI shared NT handle into this process, creates a D3D11 device
// on the SAME DXGI adapter as the producer (LUID match), opens the shared texture via
// ID3D11Device1::OpenSharedResource1, (optionally) acquires the keyed mutex, CopyResource into a
// CPU-readable STAGING texture, Map, and verifies the producer's 2-axis gradient at several points.
//
// Build (zephyrus, VS 2022 x64 dev shell):
//   cl /EHsc /nologo d3d11_consumer.cpp d3d11.lib dxgi.lib
//
// Usage:
//   d3d11_consumer.exe <handoffFile> [--luid <hex16>] [--keyedmutex] [--releasekey N]
//   --luid <hex16>   : pin the D3D11 device to the DXGI adapter whose AdapterLuid matches this
//                      16-hex-digit (8-byte, little-endian as printed by the producer) value.
//   --keyedmutex     : the producer used a keyed mutex; AcquireSync(releasekey)/ReleaseSync.
//   --releasekey N   : the key the producer released (default 1).

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <d3d11_1.h>
#include <dxgi1_2.h>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

static void fail(const char* what, HRESULT hr) {
    fprintf(stderr, "[d3d11-recv] FAIL %s hr=0x%08lX\n", what, (unsigned long)hr);
    exit(3);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <handoffFile> [--luid <hex16>] [--keyedmutex] [--releasekey N]\n", argv[0]);
        return 2;
    }
    const char* handoffPath = argv[1];
    bool wantLuid = false;
    // The producer prints VkPhysicalDeviceIDProperties.deviceLUID as 8 bytes hex left-to-right
    // (byte[0]..byte[7]). The DXGI LUID struct in memory is LowPart(4 LE) then HighPart(4 LE) —
    // the SAME 8-byte order. So parse the hex string into 8 bytes and compare raw against
    // &desc.AdapterLuid. (Do NOT strtoull as a big-endian uint64 — that mismatches the byte order.)
    uint8_t wantLuidBytes[8] = {0};
    bool keyedMutex = false;
    uint64_t releaseKey = 1;
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "--luid") == 0 && i + 1 < argc) {
            wantLuid = true;
            const char* hx = argv[++i];
            for (int b = 0; b < 8 && hx[b*2] && hx[b*2+1]; b++) {
                char two[3] = { hx[b*2], hx[b*2+1], 0 };
                wantLuidBytes[b] = (uint8_t)strtoul(two, nullptr, 16);
            }
        } else if (strcmp(argv[i], "--keyedmutex") == 0) {
            keyedMutex = true;
        } else if (strcmp(argv[i], "--releasekey") == 0 && i + 1 < argc) {
            releaseKey = strtoull(argv[++i], nullptr, 10);
        }
    }

    // ---- read the handoff file: "<handleValue> <producerPID> <w> <h>"
    long long handleVal = 0; int producerPID = 0, w = 0, h = 0;
    {
        FILE* f = nullptr;
        // wait for the file to appear (producer may start after us)
        for (int t = 0; t < 600 && !f; t++) {
            f = fopen(handoffPath, "r");
            if (!f) Sleep(100);
        }
        if (!f) { fprintf(stderr, "[d3d11-recv] handoff %s never appeared\n", handoffPath); return 3; }
        if (fscanf(f, "%lld %d %d %d", &handleVal, &producerPID, &w, &h) != 4) {
            fprintf(stderr, "[d3d11-recv] bad handoff content\n"); fclose(f); return 3;
        }
        fclose(f);
    }
    printf("[d3d11-recv] handoff handle=0x%llX producerPID=%d %dx%d keyedMutex=%d releaseKey=%llu\n",
           handleVal, producerPID, w, h, (int)keyedMutex, (unsigned long long)releaseKey);

    // ---- DuplicateHandle the producer's NT handle into THIS process
    HANDLE prodProc = OpenProcess(PROCESS_DUP_HANDLE, FALSE, (DWORD)producerPID);
    if (!prodProc) { fprintf(stderr, "[d3d11-recv] OpenProcess(pid=%d) failed err=%lu\n", producerPID, GetLastError()); return 3; }
    HANDLE dupHandle = nullptr;
    if (!DuplicateHandle(prodProc, (HANDLE)(intptr_t)handleVal, GetCurrentProcess(), &dupHandle,
                         0, FALSE, DUPLICATE_SAME_ACCESS)) {
        fprintf(stderr, "[d3d11-recv] DuplicateHandle failed err=%lu\n", GetLastError()); return 3;
    }
    CloseHandle(prodProc);
    printf("[d3d11-recv] DuplicateHandle -> %p (valid in this process)\n", dupHandle);

    // ---- select the DXGI adapter (LUID match to the producer's Vulkan device)
    IDXGIFactory1* factory = nullptr;
    HRESULT hr = CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&factory);
    if (FAILED(hr)) fail("CreateDXGIFactory1", hr);

    IDXGIAdapter1* chosen = nullptr;
    IDXGIAdapter1* adapter = nullptr;
    auto luidHex = [](const LUID& l, char* out) {
        const uint8_t* p = (const uint8_t*)&l;  // LowPart(4 LE) then HighPart(4 LE) = 8 bytes
        for (int b = 0; b < 8; b++) sprintf(out + b*2, "%02X", p[b]);
    };
    for (UINT i = 0; factory->EnumAdapters1(i, &adapter) != DXGI_ERROR_NOT_FOUND; i++) {
        DXGI_ADAPTER_DESC1 desc;
        adapter->GetDesc1(&desc);
        char hx[17] = {0}; luidHex(desc.AdapterLuid, hx);
        printf("[d3d11-recv]   adapter[%u] LUID=%s  '%ls'\n", i, hx, desc.Description);
        bool match = wantLuid ? (memcmp(&desc.AdapterLuid, wantLuidBytes, 8) == 0) : (i == 0);
        if (match && !chosen) { chosen = adapter; chosen->AddRef(); }
        adapter->Release();
    }
    if (!chosen) { fprintf(stderr, "[d3d11-recv] no matching DXGI adapter (LUID mismatch)\n"); return 3; }
    {
        DXGI_ADAPTER_DESC1 d; chosen->GetDesc1(&d);
        char hx[17] = {0}; luidHex(d.AdapterLuid, hx);
        printf("[d3d11-recv] chosen adapter LUID=%s '%ls'\n", hx, d.Description);
    }

    // ---- create a D3D11 device on that adapter
    ID3D11Device* device = nullptr;
    ID3D11DeviceContext* ctx = nullptr;
    D3D_FEATURE_LEVEL fl;
    hr = D3D11CreateDevice(chosen, D3D_DRIVER_TYPE_UNKNOWN, nullptr, 0, nullptr, 0,
                           D3D11_SDK_VERSION, &device, &fl, &ctx);
    if (FAILED(hr)) fail("D3D11CreateDevice", hr);
    ID3D11Device1* device1 = nullptr;
    hr = device->QueryInterface(__uuidof(ID3D11Device1), (void**)&device1);
    if (FAILED(hr)) fail("QI ID3D11Device1", hr);

    // ---- open the shared NT handle as an ID3D11Texture2D
    ID3D11Texture2D* tex = nullptr;
    hr = device1->OpenSharedResource1(dupHandle, __uuidof(ID3D11Texture2D), (void**)&tex);
    if (FAILED(hr)) {
        fprintf(stderr, "[d3d11-recv] OpenSharedResource1 FAILED hr=0x%08lX "
                        "(producer-export or handle-type mismatch)\n", (unsigned long)hr);
        return 3;
    }
    D3D11_TEXTURE2D_DESC td; tex->GetDesc(&td);
    printf("[d3d11-recv] OpenSharedResource1 OK: %ux%u fmt=%d misc=0x%X\n",
           td.Width, td.Height, (int)td.Format, td.MiscFlags);

    // ---- acquire keyed mutex if the producer used one
    IDXGIKeyedMutex* km = nullptr;
    if (keyedMutex) {
        hr = tex->QueryInterface(__uuidof(IDXGIKeyedMutex), (void**)&km);
        if (FAILED(hr)) fail("QI IDXGIKeyedMutex", hr);
        hr = km->AcquireSync(releaseKey, 5000);
        if (FAILED(hr)) { fprintf(stderr, "[d3d11-recv] AcquireSync(%llu) hr=0x%08lX\n",
                                  (unsigned long long)releaseKey, (unsigned long)hr); return 3; }
        printf("[d3d11-recv] AcquireSync(%llu) OK\n", (unsigned long long)releaseKey);
    }

    // ---- copy into a CPU-readable STAGING texture
    D3D11_TEXTURE2D_DESC sd = td;
    sd.Usage = D3D11_USAGE_STAGING;
    sd.BindFlags = 0;
    sd.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
    sd.MiscFlags = 0;
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
    Pt pts[] = { {0,0},{w-1,0},{0,h-1},{w-1,h-1},{w/4,h/4},{3*w/4,h/4},{w/4,3*h/4},{3*w/4,3*h/4},{w/2,h/2} };
    bool allok = true;
    for (auto& p : pts) {
        const uint8_t* px = base + (size_t)p.y * pitch + (size_t)p.x * 4;
        int b = px[0], g = px[1], r = px[2], a = px[3];
        int er = p.x * 255 / (w - 1), eg = p.y * 255 / (h - 1), eb = 128, ea = 255;
        bool ok = abs(r-er)<=4 && abs(g-eg)<=4 && abs(b-eb)<=4 && abs(a-ea)<=4;
        if (!ok) allok = false;
        printf("[d3d11-recv] (%3d,%3d) got=(%3d,%3d,%3d,%3d) exp=(%3d,%3d,%3d,%3d) %s\n",
               p.x, p.y, r, g, b, a, er, eg, eb, ea, ok ? "ok" : "BAD");
    }
    ctx->Unmap(staging, 0);
    if (km) { km->ReleaseSync(releaseKey); km->Release(); }

    printf("[d3d11-recv] gradient fidelity: %s\n", allok ? "PASS -- Vulkan->D3D11 sharing WORKS" : "FAIL -- scrambled/garbage");

    staging->Release(); tex->Release(); device1->Release(); ctx->Release(); device->Release();
    chosen->Release(); factory->Release();
    return allok ? 0 : 1;
}
