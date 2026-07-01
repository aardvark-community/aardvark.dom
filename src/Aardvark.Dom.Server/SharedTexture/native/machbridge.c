// Tiny mach-port IOSurface handoff bridge. Producer publishes an IOSurface under a
// bootstrap service name; consumer looks it up. Robust struct layout via real headers.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mach/mach.h>
#include <servers/bootstrap.h>
#include <IOSurface/IOSurface.h>
#include <pthread.h>
#include <unistd.h>

typedef struct {
    mach_msg_header_t header;
    mach_msg_body_t   body;
    mach_msg_port_descriptor_t surface;
} surface_msg_t;

typedef struct { mach_msg_header_t header; mach_msg_trailer_t trailer; } request_msg_t;

static IOSurfaceRef g_surface = NULL;
static mach_port_t  g_service = MACH_PORT_NULL;

static void* serve_thread(void* arg) {
    (void)arg;
    for (;;) {
        union { mach_msg_header_t hdr; unsigned char buf[1024]; } req; memset(&req, 0, sizeof(req));
        kern_return_t kr = mach_msg(&req.hdr, MACH_RCV_MSG, 0, sizeof(req),
                                    g_service, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        if (kr != KERN_SUCCESS) { fprintf(stderr, "[bridge] recv kr=0x%x\n", kr); continue; }
        mach_port_t reply = req.hdr.msgh_remote_port;
        mach_port_t sport = IOSurfaceCreateMachPort(g_surface);
        surface_msg_t resp; memset(&resp, 0, sizeof(resp));
        resp.header.msgh_bits =
            MACH_MSGH_BITS(MACH_MSGH_BITS_REMOTE(req.hdr.msgh_bits), 0) | MACH_MSGH_BITS_COMPLEX;
        resp.header.msgh_size = sizeof(resp);
        resp.header.msgh_remote_port = reply;
        resp.header.msgh_local_port = MACH_PORT_NULL;
        resp.header.msgh_id = 0x100;
        resp.body.msgh_descriptor_count = 1;
        resp.surface.name = sport;
        resp.surface.disposition = MACH_MSG_TYPE_MOVE_SEND;
        resp.surface.type = MACH_MSG_PORT_DESCRIPTOR;
        kr = mach_msg(&resp.header, MACH_SEND_MSG, sizeof(resp), 0, MACH_PORT_NULL,
                      MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        if (kr != KERN_SUCCESS) fprintf(stderr, "[bridge] reply kr=0x%x\n", kr);
    }
    return NULL;
}

int aardvark_publish(const char* name, IOSurfaceRef surf) {
    g_surface = surf;
    kern_return_t kr = bootstrap_check_in(bootstrap_port, name, &g_service);
    if (kr != KERN_SUCCESS) { fprintf(stderr, "[bridge] check_in kr=0x%x\n", kr); return (int)kr; }
    pthread_t t; pthread_create(&t, NULL, serve_thread, NULL);
    return 0;
}

IOSurfaceRef aardvark_lookup(const char* name) {
    mach_port_t svc = MACH_PORT_NULL;
    kern_return_t kr = bootstrap_look_up(bootstrap_port, name, &svc);
    if (kr != KERN_SUCCESS) { fprintf(stderr, "[bridge] look_up kr=0x%x\n", kr); return NULL; }
    mach_port_t reply = MACH_PORT_NULL;
    mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &reply);
    struct { mach_msg_header_t header; } req; memset(&req, 0, sizeof(req));
    req.header.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, MACH_MSG_TYPE_MAKE_SEND_ONCE);
    req.header.msgh_size = sizeof(req);
    req.header.msgh_remote_port = svc;
    req.header.msgh_local_port = reply;
    req.header.msgh_id = 1;
    union { surface_msg_t msg; unsigned char buf[1024]; } resp; memset(&resp, 0, sizeof(resp));
    kr = mach_msg(&req.header, MACH_SEND_MSG, sizeof(req), 0, MACH_PORT_NULL,
                  MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    if (kr != KERN_SUCCESS) { fprintf(stderr, "[bridge] send kr=0x%x\n", kr); return NULL; }
    kr = mach_msg(&resp.msg.header, MACH_RCV_MSG, 0, sizeof(resp), reply,
                  MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    if (kr != KERN_SUCCESS) { fprintf(stderr, "[bridge] recv kr=0x%x\n", kr); return NULL; }
    return IOSurfaceLookupFromMachPort(resp.msg.surface.name);
}

// Producer-as-CLIENT push: look up the consumer's (browser's) bootstrap service and SEND it
// the IOSurface, unsolicited, matching a consumer that does bootstrap_check_in + a recv loop
// (the patched Electron painter). Counterpart to aardvark_publish (producer-as-server, for a
// look_up-ing consumer). The consumer receives one surface_msg_t and does
// IOSurfaceLookupFromMachPort(surface.name).
int aardvark_send(const char* name, IOSurfaceRef surf) {
    mach_port_t svc = MACH_PORT_NULL;
    kern_return_t kr = KERN_FAILURE;
    // The consumer check_ins its service only after it reads our REG line off the socket, so
    // the service may not be registered yet — retry the lookup briefly (~1s) to absorb the race.
    for (int i = 0; i < 200; i++) {
        kr = bootstrap_look_up(bootstrap_port, name, &svc);
        if (kr == KERN_SUCCESS) break;
        usleep(5000);
    }
    if (kr != KERN_SUCCESS) { fprintf(stderr, "[bridge] send look_up '%s' kr=0x%x\n", name, kr); return (int)kr; }
    mach_port_t sport = IOSurfaceCreateMachPort(surf);
    surface_msg_t msg; memset(&msg, 0, sizeof(msg));
    msg.header.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, 0) | MACH_MSGH_BITS_COMPLEX;
    msg.header.msgh_size = sizeof(msg);
    msg.header.msgh_remote_port = svc;
    msg.header.msgh_local_port = MACH_PORT_NULL;
    msg.header.msgh_id = 0x100;
    msg.body.msgh_descriptor_count = 1;
    msg.surface.name = sport;
    msg.surface.disposition = MACH_MSG_TYPE_MOVE_SEND;
    msg.surface.type = MACH_MSG_PORT_DESCRIPTOR;
    kr = mach_msg(&msg.header, MACH_SEND_MSG, sizeof(msg), 0, MACH_PORT_NULL,
                  MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    if (kr != KERN_SUCCESS) { fprintf(stderr, "[bridge] send '%s' kr=0x%x\n", name, kr); return (int)kr; }
    fprintf(stderr, "[bridge] sent IOSurface -> %s\n", name);
    return 0;
}
