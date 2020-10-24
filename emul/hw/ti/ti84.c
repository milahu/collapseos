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
#define XK_MISCELLANY
#include <X11/keysymdef.h>

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

static uint8_t keycode_to_tikbd(xcb_keycode_t kc)
{
    // First, change keycode into symbol
    xcb_get_keyboard_mapping_reply_t* km = xcb_get_keyboard_mapping_reply(
        conn, xcb_get_keyboard_mapping(conn, kc, 1), NULL);
    xcb_keysym_t* keysyms = (xcb_keysym_t*)(km + 1);
    uint8_t res = 0;
    for (int i=0; i<km->length; i++) {
        switch (keysyms[0]) {
        case XK_Shift_L: res = KBD_2ND; break;
        case XK_Control_L: res = KBD_ALPHA; break;
        case XK_Return: res = 0x10; break;
        case XK_Delete: res = 0x67; break;
        case ' ': res = 0x40; break;
        case '1': res = 0x41; break;
        case '2': res = 0x31; break;
        case '3': res = 0x21; break;
        case '4': res = 0x42; break;
        case '5': res = 0x32; break;
        case '6': res = 0x22; break;
        case '7': res = 0x43; break;
        case '8': res = 0x33; break;
        case '9': res = 0x23; break;
        case '0': res = 0x40; break;
        case '-': res = 0x12; break;
        case '+': res = 0x11; break;
        case 'q': res = 0x23; break;
        case 'w': res = 0x12; break;
        case 'e': res = 0x45; break;
        case 'r': res = 0x13; break;
        case 't': res = 0x42; break;
        case 'y': res = 0x41; break;
        case 'u': res = 0x32; break;
        case 'i': res = 0x54; break;
        case 'o': res = 0x43; break;
        case 'p': res = 0x33; break;
        case '(': res = 0x34; break;
        case ')': res = 0x24; break;
        case 'a': res = 0x56; break;
        case 's': res = 0x52; break;
        case 'd': res = 0x55; break;
        case 'f': res = 0x35; break;
        case 'g': res = 0x25; break;
        case 'h': res = 0x15; break;
        case 'j': res = 0x44; break;
        case 'k': res = 0x34; break;
        case 'l': res = 0x24; break;
        case ':': res = 0x30; break;
        case '"': res = 0x11; break;
        case 'z': res = 0x31; break;
        case 'x': res = 0x51; break;
        case 'c': res = 0x36; break;
        case 'v': res = 0x22; break;
        case 'b': res = 0x46; break;
        case 'n': res = 0x53; break;
        case 'm': res = 0x14; break;
        case ',': res = 0x44; break;
        case '.': res = 0x30; break;
        case '?': res = 0x20; break;
        }
        if (res) {
            break;
        }
    }
    free(km);
    return res;
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
