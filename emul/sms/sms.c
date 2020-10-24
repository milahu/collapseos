#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>

#include <xcb/xcb.h>
#define XK_MISCELLANY
#include <X11/keysymdef.h>

#include "../emul.h"
#include "vdp.h"
#include "port.h"
#include "pad.h"
#include "kbd.h"

#define RAMSTART 0xc000
#define VDP_CMD_PORT 0xbf
#define VDP_DATA_PORT 0xbe
#define PORTS_CTL_PORT 0x3f
#define PORTS_IO1_PORT 0xdc
#define PORTS_IO2_PORT 0xdd
#define MAX_ROMSIZE 0x8000

static xcb_connection_t    *conn;
static xcb_screen_t        *screen;

/* graphics contexts */
static xcb_gcontext_t       fg;
/* win */
static xcb_drawable_t       win;

// pixels to draw. We draw them in one shot.
static xcb_rectangle_t rectangles[VDP_SCREENW*VDP_SCREENH];

static Machine *m;
static VDP vdp;
static bool vdp_changed;
static Ports ports;
static Pad pad;
static Kbd kbd;
static bool use_kbd = false;

static uint8_t iord_vdp_cmd()
{
    return vdp_cmd_rd(&vdp);
}

static uint8_t iord_vdp_data()
{
    return vdp_data_rd(&vdp);
}

static uint8_t iord_ports_io1()
{
    return ports_A_rd(&ports);
}

static uint8_t iord_ports_io2()
{
    return ports_B_rd(&ports);
}

static uint8_t iord_pad()
{
    return pad_rd(&pad);
}

static uint8_t iord_kbd()
{
    return kbd_rd(&kbd);
}

static void iowr_vdp_cmd(uint8_t val)
{
    vdp_cmd_wr(&vdp, val);
}

static void iowr_vdp_data(uint8_t val)
{
    vdp_changed = true;
    vdp_data_wr(&vdp, val);
}

static void iowr_ports_ctl(uint8_t val)
{
    ports_ctl_wr(&ports, val);
}

void create_window()
{
    uint32_t mask;
    uint32_t values[2];

    /* Create the window */
    win = xcb_generate_id(conn);
    mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
    values[0] = screen->white_pixel;
    values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS |
        XCB_EVENT_MASK_KEY_RELEASE;
    xcb_create_window(
        conn,
        screen->root_depth,
        win,
        screen->root,
        0, 0,
        500, 500,
        10,
        XCB_WINDOW_CLASS_INPUT_OUTPUT,
        screen->root_visual,
        mask, values);

    fg = xcb_generate_id(conn);
    mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
    values[0] = screen->black_pixel;
    values[1] = 0;
    xcb_create_gc(conn, fg, screen->root, mask, values);

    /* Map the window on the screen */
    xcb_map_window(conn, win);
}

// To make things simple with X11, we only support monochrome display, which is
// inverted: As soon as the color of the pixel is non-black, we show a black
// pixel. If the pixel is white, we show black.
void draw_pixels()
{
    xcb_get_geometry_reply_t *geom;

    geom = xcb_get_geometry_reply(conn, xcb_get_geometry(conn, win), NULL);

    xcb_clear_area(
        conn, 0, win, 0, 0, geom->width, geom->height);
    // Figure out inner size to maximize our screen's aspect ratio
    int psize = geom->height / VDP_SCREENH;
    if (geom->width / VDP_SCREENW < psize) {
        // width is the constraint
        psize = geom->width / VDP_SCREENW;
    }
    int innerw = psize * VDP_SCREENW;
    int innerh = psize * VDP_SCREENH;
    int innerx = (geom->width - innerw) / 2;
    int innery = (geom->height - innerh) / 2;
    free(geom);
    int drawcnt = 0;
    for (int i=0; i<VDP_SCREENW; i++) {
        for (int j=0; j<VDP_SCREENH; j++) {
            if (vdp_pixel(&vdp, i, j)) {
                int x = innerx + (i*psize);
                int y = innery + (j*psize);
                rectangles[drawcnt].x = x;
                rectangles[drawcnt].y = y;
                rectangles[drawcnt].height = psize;
                rectangles[drawcnt].width = psize;
                drawcnt++;
            }
        }
    }
    if (drawcnt) {
        xcb_poly_fill_rectangle(
            conn, win, fg, drawcnt, rectangles);
    }
    vdp_changed = false;
    xcb_flush(conn);
}

// Returns true to exist event loop
static bool _handle_keypress(xcb_generic_event_t *e)
{
    xcb_key_press_event_t *ev = (xcb_key_press_event_t *)e;
    if (ev->detail == 0x09) { // ESC
        return true;
    }
    bool ispressed = e->response_type == XCB_KEY_PRESS;
    // change keycode into symbol
    xcb_get_keyboard_mapping_reply_t* km = xcb_get_keyboard_mapping_reply(
        conn, xcb_get_keyboard_mapping(conn, ev->detail, 1), NULL);
    if (km->length) {
        xcb_keysym_t* keysyms = (xcb_keysym_t*)(km + 1);
        if (use_kbd) {
            if ((keysyms[0] == XK_Shift_L) || (keysyms[0] == XK_Shift_R)) {
                kbd_pressshift(&kbd, ispressed);
            } else if (ispressed) {
                fprintf(stderr, "pressing %x\n", keysyms[0]);
                kbd_presskey(&kbd, keysyms[0]);
            }
        } else { // pad
            switch (keysyms[0]) {
                case 'w':
                    pad_setbtn(&pad, PAD_BTN_UP, ispressed);
                    break;
                case 'a':
                    pad_setbtn(&pad, PAD_BTN_LEFT, ispressed);
                    break;
                case 's':
                    pad_setbtn(&pad, PAD_BTN_DOWN, ispressed);
                    break;
                case 'd':
                    pad_setbtn(&pad, PAD_BTN_RIGHT, ispressed);
                    break;
                case 'h':
                    pad_setbtn(&pad, PAD_BTN_A, ispressed);
                    break;
                case 'j':
                    pad_setbtn(&pad, PAD_BTN_B, ispressed);
                    break;
                case 'k':
                    pad_setbtn(&pad, PAD_BTN_C, ispressed);
                    break;
                case 'l':
                    pad_setbtn(&pad, PAD_BTN_START, ispressed);
                    break;
            }
        }
    }
    free(km);
    return false;
}

void event_loop()
{
    while (1) {
        if (!emul_step()) {
            fprintf(stderr, "CPU halted, quitting\n");
            usleep(1000 * 1000);
            break;
        }
        if (vdp_changed) {
            // To avoid overdrawing, we'll let the CPU run a bit to finish its
            // drawing operation.
            emul_steps(100);
            draw_pixels();
        }
        // A low tech way of checking when the window was closed. The proper way
        // involving WM_DELETE is too complicated.
        xcb_get_geometry_reply_t *geom;
        geom = xcb_get_geometry_reply(conn, xcb_get_geometry(conn, win), NULL);
        if (geom == NULL) {
            return;     // window has been closed.
        } else {
            free(geom);
        }
        xcb_generic_event_t *e = xcb_poll_for_event(conn);
        if (!e) {
            continue;
        }
        switch (e->response_type & ~0x80) {
        /* ESC to exit */
        case XCB_KEY_RELEASE:
        case XCB_KEY_PRESS:
            if (_handle_keypress(e)) return;
            break;
        case XCB_EXPOSE: {
            draw_pixels();
            break;
        }
        default: {
            break;
        }
        }
        free(e);
    }
}

static void usage()
{
    fprintf(stderr, "Usage: ./sms [-k] /path/to/rom\n");
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        usage();
        return 1;
    }
    int ch;
    while ((ch = getopt(argc, argv, "k")) != -1) {
        switch (ch) {
            case 'k':
                use_kbd = true;
                break;
        }
    }
    if (optind != argc-1) {
        usage();
        return 1;
    }
    FILE *fp = fopen(argv[optind], "r");
    if (fp == NULL) {
        fprintf(stderr, "Can't open %s\n", argv[1]);
        return 1;
    }
    m = emul_init();
    m->ramstart = RAMSTART;
    int i = 0;
    int c;
    while ((c = fgetc(fp)) != EOF && i < MAX_ROMSIZE) {
        m->mem[i++] = c & 0xff;
    }
    pclose(fp);
    if (c != EOF) {
        fprintf(stderr, "ROM image too large.\n");
        return 1;
    }
    vdp_init(&vdp);
    vdp_changed = false;
    ports_init(&ports);
    pad_init(&pad, &ports.THA);
    kbd_init(&kbd, &ports.THA);
    if (use_kbd) {
        ports.portA_rd = iord_kbd;
    } else {
        ports.portA_rd = iord_pad;
    }
    m->iord[VDP_CMD_PORT] = iord_vdp_cmd;
    m->iord[VDP_DATA_PORT] = iord_vdp_data;
    m->iord[PORTS_IO1_PORT] = iord_ports_io1;
    m->iord[PORTS_IO2_PORT] = iord_ports_io2;
    m->iord[PORTS_CTL_PORT] = iord_noop;
    m->iowr[VDP_CMD_PORT] = iowr_vdp_cmd;
    m->iowr[VDP_DATA_PORT] = iowr_vdp_data;
    m->iowr[PORTS_CTL_PORT] = iowr_ports_ctl;
    conn = xcb_connect(NULL, NULL);
    screen = xcb_setup_roots_iterator(xcb_get_setup(conn)).data;
    create_window();
    draw_pixels();
    event_loop();
    emul_printdebug();
    return 0;
}
