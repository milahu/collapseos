#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <curses.h>
#include <termios.h>
#include "emul.h"
#ifndef FBIN_PATH
#error FBIN_PATH needed
#endif
#ifndef BLKFS_PATH
#error BLKFS_PATH needed
#endif

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

static FILE *fp;
static FILE *blkfp;
static int retcode = 0;
static uint16_t blkid = 0;

static uint8_t iord_stdio()
{
    int c;
    if (fp != NULL) {
        c = getc(fp);
    } else {
        c = getch();
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
            echochar(val);
        }
    }
}

static void iowr_ret(uint8_t val)
{
    retcode = val;
}

static void iowr_blk(uint8_t val)
{
    blkid <<= 8;
    blkid |= val;
    fseek(blkfp, blkid*1024, SEEK_SET);
}

static uint8_t iord_blkdata()
{
    uint8_t res = 0;
    int c = getc(blkfp);
    if (c != EOF) {
        res = c;
    }
    return res;
}

static void iowr_blkdata(uint8_t val)
{
    putc(val, blkfp);
}

int run()
{
    fprintf(stderr, "Using blkfs %s\n", BLKFS_PATH);
    blkfp = fopen(BLKFS_PATH, "r+");
    if (!blkfp) {
        fprintf(stderr, "Can't open\n");
        return 1;
    }
    Machine *m = emul_init();
    m->ramstart = RAMSTART;
    m->iord[STDIO_PORT] = iord_stdio;
    m->iowr[STDIO_PORT] = iowr_stdio;
    m->iowr[RET_PORT] = iowr_ret;
    m->iowr[BLK_PORT] = iowr_blk;
    m->iord[BLKDATA_PORT] = iord_blkdata;
    m->iowr[BLKDATA_PORT] = iowr_blkdata;
    // initialize memory
    FILE *bfp = fopen(FBIN_PATH, "r");
    if (!bfp) {
        fprintf(stderr, "Can't open forth.bin\n");
        return 1;
    }
    int i = 0;
    int c = getc(bfp);
    while (c != EOF) {
        m->mem[i++] = c;
        c = getc(bfp);
    }
    fclose(bfp);
    // Run!
    while (emul_step());

    if (blkfp != NULL) {
        fclose(blkfp);
    }
    return retcode;
}

int main(int argc, char *argv[])
{
    if (argc == 2) {
        fp = fopen(argv[1], "r");
        if (fp == NULL) {
            fprintf(stderr, "Can't open %s\n", argv[1]);
            return 1;
        }
        int ret = run();
        fclose(fp);
        return ret;
    } else if (argc == 1) {
        fp = NULL;
        initscr(); cbreak(); noecho(); nl(); clear();
        scrollok(stdscr, 1);
        int ret = run();
        nocbreak(); echo(); endwin();
        printf("\nDone!\n");
        emul_printdebug();
        return ret;
    } else {
        fprintf(stderr, "Usage: ./forth [filename]\n");
        return 1;
    }
}
