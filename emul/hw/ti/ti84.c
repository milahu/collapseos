/* TI-84+
 *
 * A plain TI-84 with its built-in keyboard as an input and its LCD screen
 * as an output.
 *
 * Uses XCB to render the screen and record keystrokes.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include <xcb/xcb.h>

#include "../../emul.h"
#include "t6a04.h"
#include "kbd.h"

#define RAMSTART 0x8000
#define KBD_PORT 0x01
#define INTERRUPT_PORT 0x03
#define LCD_CMD_PORT 0x10
#define LCD_DATA_PORT 0x11
#define MAX_ROMSIZE 0x4000

static xcb_connection_t    *conn;
static xcb_screen_t        *screen;

/* graphics contexts */
static xcb_gcontext_t       fg;
/* win */
static xcb_drawable_t       win;

// pixels to draw. We draw them in one shot.
static xcb_rectangle_t rectangles[96*64];

static Machine *m;
static T6A04 lcd;
static bool lcd_changed;
static KBD kbd;
static bool on_was_pressed;

static uint8_t iord_lcd_cmd()
{
    return t6a04_cmd_rd(&lcd);
}

static uint8_t iord_lcd_data()
{
    return t6a04_data_rd(&lcd);
}

static uint8_t iord_kbd()
{
    return kbd_rd(&kbd);
}

static uint8_t iord_interrupt()
{
    return on_was_pressed ? 1 : 0;
}

static void iowr_lcd_cmd(uint8_t val)
{
    t6a04_cmd_wr(&lcd, val);
}

static void iowr_lcd_data(uint8_t val)
{
    lcd_changed = true;
    t6a04_data_wr(&lcd, val);
}

static void iowr_kbd(uint8_t val)
{
    kbd_wr(&kbd, val);
}

static void iowr_interrupt(uint8_t val)
{
    if ((val & 1) == 0) {
        on_was_pressed = false;
    }
}

// TIL: XCB doesn't have a builtin way to translate a keycode to an ASCII char.
// Using Xlib looks complicated. This will probably not work in many cases (non
// query keyboards and all...), but for now, let's go with this.
static uint8_t keycode_to_tikbd(xcb_keycode_t kc)
{
    switch (kc) {
    case 0x0a: return 0x41; // 1
    case 0x0b: return 0x31; // 2
    case 0x0c: return 0x21; // 3
    case 0x0d: return 0x42; // 4
    case 0x0e: return 0x32; // 5
    case 0x0f: return 0x22; // 6
    case 0x10: return 0x43; // 7
    case 0x11: return 0x33; // 8
    case 0x12: return 0x23; // 9
    case 0x13: return 0x40; // 0
    case 0x14: return 0x12; // -
    case 0x15: return 0x11; // +
    case 0x16: return 0x67; // DEL
    case 0x18: return 0x23; // Q
    case 0x19: return 0x12; // W
    case 0x1a: return 0x45; // E
    case 0x1b: return 0x13; // R
    case 0x1c: return 0x42; // T
    case 0x1d: return 0x41; // Y
    case 0x1e: return 0x32; // U
    case 0x1f: return 0x54; // I
    case 0x20: return 0x43; // O
    case 0x21: return 0x33; // P
    case 0x22: return 0x34; // (
    case 0x23: return 0x24; // )
    case 0x24: return 0x10; // Return
    case 0x25: return KBD_ALPHA; // LCTRL
    case 0x26: return 0x56; // A
    case 0x27: return 0x52; // S
    case 0x28: return 0x55; // D
    case 0x29: return 0x35; // F
    case 0x2a: return 0x25; // G
    case 0x2b: return 0x15; // H
    case 0x2c: return 0x44; // J
    case 0x2d: return 0x34; // K
    case 0x2e: return 0x24; // L
    case 0x2f: return 0x30; // :
    case 0x30: return 0x11; // "
    case 0x32: return KBD_2ND; // Lshift
    case 0x34: return 0x31; // Z
    case 0x35: return 0x51; // X
    case 0x36: return 0x36; // C
    case 0x37: return 0x22; // V
    case 0x38: return 0x46; // B
    case 0x39: return 0x53; // N
    case 0x3a: return 0x14; // M
    case 0x3b: return 0x44; // ,
    case 0x3c: return 0x30; // .
    case 0x3d: return 0x20; // ?
    case 0x41: return 0x40; // Space
    default: return 0;
    }
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

bool get_pixel(int x, int y)
{
    return t6a04_pixel(&lcd, x, y);
}

void draw_pixels()
{
    xcb_get_geometry_reply_t *geom;

    geom = xcb_get_geometry_reply(conn, xcb_get_geometry(conn, win), NULL);

    xcb_clear_area(
        conn, 0, win, 0, 0, geom->width, geom->height);
    // Figure out inner size to maximize a 96x64 screen (1.5 aspect ratio)
    int psize = geom->height / 64;
    if (geom->width / 96 < psize) {
        // width is the constraint
        psize = geom->width / 96;
    }
    int innerw = psize * 96;
    int innerh = psize * 64;
    int innerx = (geom->width - innerw) / 2;
    int innery = (geom->height - innerh) / 2;
    free(geom);
    int drawcnt = 0;
    for (int i=0; i<96; i++) {
        for (int j=0; j<64; j++) {
            if (get_pixel(i, j)) {
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
    lcd_changed = false;
    xcb_flush(conn);
}

void event_loop()
{
    while (1) {
        emul_steps(100);
        if (lcd_changed) {
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
        case XCB_KEY_PRESS: {
            xcb_key_press_event_t *ev = (xcb_key_press_event_t *)e;
            if (ev->detail == 0x09) return;
            if (ev->detail == 0x31 && e->response_type == XCB_KEY_PRESS) {
                // tilde, mapped to ON
                on_was_pressed = true;
                Z80INT(&m->cpu, 0);
                Z80Execute(&m->cpu); // unhalts the CPU
            }
            uint8_t key = keycode_to_tikbd(ev->detail);
            if (key) {
                kbd_setkey(&kbd, key, e->response_type == XCB_KEY_PRESS);
            }
            break;
        }
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

int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: ./ti84 /path/to/rom\n");
        return 1;
    }
    FILE *fp = fopen(argv[1], "r");
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
    if (i == MAX_ROMSIZE) {
        fprintf(stderr, "ROM image too large.\n");
        return 1;
    }
    t6a04_init(&lcd);
    kbd_init(&kbd);
    lcd_changed = false;
    on_was_pressed = false;
    m->iord[KBD_PORT] = iord_kbd;
    m->iord[INTERRUPT_PORT] = iord_interrupt;
    m->iord[LCD_CMD_PORT] = iord_lcd_cmd;
    m->iord[LCD_DATA_PORT] = iord_lcd_data;
    m->iowr[KBD_PORT] = iowr_kbd;
    m->iowr[INTERRUPT_PORT] = iowr_interrupt;
    m->iowr[LCD_CMD_PORT] = iowr_lcd_cmd;
    m->iowr[LCD_DATA_PORT] = iowr_lcd_data;
    conn = xcb_connect(NULL, NULL);
    screen = xcb_setup_roots_iterator(xcb_get_setup(conn)).data;
    create_window();
    draw_pixels();
    event_loop();
    emul_printdebug();
    return 0;
}
