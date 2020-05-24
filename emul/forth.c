#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <curses.h>
#include <termios.h>
#include "emul.h"

#define WCOLS 80
#define WLINES 32
// in sync with glue.asm
#define RAMSTART 0x900
#define STDIO_PORT 0x00
// This binary is also used for automated tests and those tests, when
// failing, send a non-zero value to RET_PORT to indicate failure
#define RET_PORT 0x01
// Port for block reads. Write 2 bytes, MSB first, on that port and then
// read 1024 bytes from the DATA port.
#define BLK_PORT 0x03
#define BLKDATA_PORT 0x04
#define SETX_PORT 0x05
#define SETY_PORT 0x06

static FILE *fp;
static int retcode = 0;
WINDOW *bw, *dw, *w;

void debug_panel()
{
    char buf[30];
    emul_debugstr(buf);
    mvwaddnstr(dw, 0, 0, buf, 30);
    wrefresh(dw);
}

static uint8_t iord_stdio()
{
    int c;
    if (fp != NULL) {
        c = getc(fp);
    } else {
        debug_panel();
        c = wgetch(w);
    }
    if (c == EOF) {
        c = 4; // ASCII EOT
    }
    return (uint8_t)c;
}

static void iowr_stdio(uint8_t val)
{
    if (fp != NULL) {
        putchar(val);
    } else {
        if (val >= 0x20 || val == '\n') {
            wechochar(w, val);
        } else if (val == 0x08) {
            int y, x; getyx(w, y, x);
            wmove(w, y, x-1);
        }
    }
}

static void iowr_ret(uint8_t val)
{
    retcode = val;
}

static void iowr_setx(uint8_t val)
{
    int y, x; getyx(w, y, x);
    wmove(w, y, val);
}

static void iowr_sety(uint8_t val)
{
    int y, x; getyx(w, y, x);
    wmove(w, val, x);
}

int main(int argc, char *argv[])
{
    Machine *m = emul_init();
    if (m == NULL) {
        return 1;
    }
    m->ramstart = RAMSTART;
    m->iord[STDIO_PORT] = iord_stdio;
    m->iowr[STDIO_PORT] = iowr_stdio;
    m->iowr[RET_PORT] = iowr_ret;
    m->iowr[SETX_PORT] = iowr_setx;
    m->iowr[SETY_PORT] = iowr_sety;
    w = NULL;
    if (argc == 2) {
        fp = fopen(argv[1], "r");
        if (fp == NULL) {
            fprintf(stderr, "Can't open %s\n", argv[1]);
            return 1;
        }
        while (emul_step());
        fclose(fp);
    } else if (argc == 1) {
        fp = NULL;
        initscr(); cbreak(); noecho(); nl(); clear();
        // border window
        bw = newwin(WLINES+2, WCOLS+2, 0, 0);
        wborder(bw, 0, 0, 0, 0, 0, 0, 0, 0);
        wrefresh(bw);
        // debug panel
        dw = newwin(1, 30, LINES-1, COLS-30);
        w = newwin(WLINES, WCOLS, 1, 1);
        scrollok(w, 1);
        while (emul_steps(1000)) {
            debug_panel();
        }
        nocbreak(); echo(); delwin(w); delwin(bw); delwin(dw); endwin();
        printf("\nDone!\n");
        emul_printdebug();
    } else {
        fprintf(stderr, "Usage: ./forth [filename]\n");
        retcode = 1;
    }
    emul_deinit();
    return retcode;
}
