#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <curses.h>
#include <termios.h>
#include "emul.h"

#define WCOLS 80
#define WLINES 24
#define RAMSTART 0
#define BINSTART 0x3000

WINDOW *bw, *dw, *w;

void debug_panel()
{
    char buf[30];
    emul_debugstr(buf);
    mvwaddnstr(dw, 0, 0, buf, 30);
    wrefresh(dw);
}

static void pchookfunc(Machine *m)
{
    byte val;
    switch (m->cpu.R1.br.A) {
    case 0x01: // @KEY
        debug_panel();
        m->cpu.R1.br.A = wgetch(w);
        break;
    case 0x02: // @DSP
        val = m->cpu.R1.br.C;
        if (val == '\r') {
            val = '\n';
        }
        if (val >= 0x20 || val == '\n') {
            wechochar(w, val);
        } else if (val == 0x08) {
            int y, x; getyx(w, y, x);
            wmove(w, y, x-1);
        }
        break;
    case 0x03: // @GET
        break;
    case 0x04: // @PUT
        break;
    case 0x0f: // @VDCTL
        wmove(w, m->cpu.R1.br.H, m->cpu.R1.br.L);
        break;
    case 0x16: // @EXIT
        break;
    case 0x1a: // @ERROR
        break;
    case 0x28: // @DCSTAT
        break;
    case 0x31: // @RDSEC
        break;
    case 0x35: // @WRSEC
        break;
    default:
        fprintf(stderr, "Unhandled RST28: %x\n", m->cpu.R1.br.A);
    }
}

static void usage()
{
    fprintf(stderr, "Usage: ./trs80 /path/to/rom\n");
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        usage();
        return 1;
    }
    Machine *m = emul_init(argv[1], BINSTART);
    if (m == NULL) return 1;
    m->ramstart = RAMSTART;
    m->pchookfunc = pchookfunc;
    m->pchooks_cnt = 1;
    m->pchooks[0] = 0x28; // RST 28
    // Place a RET at 0x28 so that it properly returns after pchookfunc().
    m->mem[0x28] = 0xc9; // RET
    m->cpu.PC = BINSTART;
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
    printf("PC: %x\n", m->cpu.PC);
    emul_deinit();
    return 0;
}
