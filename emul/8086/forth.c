#include <stdint.h>
#include <stdio.h>
#include <curses.h>
#include "cpu.h"

#define WCOLS 80
#define WLINES 25

#ifndef FBIN_PATH
#error FBIN_PATH needed
#endif
#ifndef BLKFS_PATH
#error BLKFS_PATH needed
#endif

extern uint8_t byteregtable[8];
extern union _bytewordregs_ regs;
extern INTHOOK INTHOOKS[0x100];

static FILE *fp;
static FILE *blkfp;
static int retcode = 0;
WINDOW *bw, *dw, *w;

/* we have a fake INT API:
INT 1: EMIT. AL = char to spit
INT 2: KEY. AL = char read
INT 3: AT-XY. AL = x, BL = y
INT 4: BLKREAD. AX = blkid, BX = dest addr
INT 5: BLKWRITE. AX = blkid, BX = src addr
*/

void int1() {
    uint8_t val = regs.byteregs[regal];
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

void int2() {
    regs.byteregs[regal] = getchar();
}

void int3() {
    wmove(w, regs.byteregs[regbl], regs.byteregs[regal]);
}

void int4() {
    uint16_t blkid = getreg16(regax);
    uint16_t dest = getreg16(regbx);
    fseek(blkfp, blkid*1024, SEEK_SET);
    for (int i=0; i<1024; i++) {
        write86(dest+i, getc(blkfp));
    }
}

void int5() {
    uint16_t blkid = getreg16(regax);
    uint16_t dest = getreg16(regbx);
    fseek(blkfp, blkid*1024, SEEK_SET);
    for (int i=0; i<1024; i++) {
        putc(read86(dest+i), blkfp);
    }
}

int main(int argc, char *argv[])
{
    INTHOOKS[1] = int1;
    INTHOOKS[2] = int2;
    INTHOOKS[3] = int3;
    INTHOOKS[4] = int4;
    INTHOOKS[5] = int5;
    reset86();
    fprintf(stderr, "Using blkfs %s\n", BLKFS_PATH);
    blkfp = fopen(BLKFS_PATH, "r+");
    if (!blkfp) {
        fprintf(stderr, "Can't open\n");
        return 1;
    }
    fseek(blkfp, 0, SEEK_END);
    if (ftell(blkfp) < 100 * 1024) {
        fclose(blkfp);
        fprintf(stderr, "emul/blkfs too small, something's wrong, aborting.\n");
        return 1;
    }
    fseek(blkfp, 0, SEEK_SET);
    // initialize memory
    FILE *bfp = fopen(FBIN_PATH, "r");
    if (!bfp) {
        fprintf(stderr, "Can't open forth.bin\n");
        return 1;
    }
    int i = 0;
    int c = getc(bfp);
    while (c != EOF) {
        write86(i++, c);
        c = getc(bfp);
    }
    fclose(bfp);
    w = NULL;
    if (argc == 2) {
        fp = fopen(argv[1], "r");
        if (fp == NULL) {
            fprintf(stderr, "Can't open %s\n", argv[1]);
            return 1;
        }
        while (exec86(100));
        fclose(fp);
    } else if (argc == 1) {
        initscr(); cbreak(); noecho(); nl(); clear();
        // border window
        bw = newwin(WLINES+2, WCOLS+2, 0, 0);
        wborder(bw, 0, 0, 0, 0, 0, 0, 0, 0);
        wrefresh(bw);
        // debug panel
        dw = newwin(1, 30, LINES-1, COLS-30);
        w = newwin(WLINES, WCOLS, 1, 1);
        scrollok(w, 1);
        while (exec86(100)) {
            //debug_panel();
        }
        nocbreak(); echo(); delwin(w); delwin(bw); delwin(dw); endwin();
        printf("\nDone!\n");
        //emul_printdebug();
    }

    return 0;
}
