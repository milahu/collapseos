#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include "../emul.h"
#include "forth1-bin.h"

// in sync with glue.asm
#define RAMSTART 0x900
#define STDIO_PORT 0x00
// This binary is also used for automated tests and those tests, when
// failing, send a non-zero value to RET_PORT to indicate failure
#define RET_PORT 0x01
// Port for block reads. Write 2 bytes, MSB first, on that port and then
// read 1024 bytes from the same port.
#define BLK_PORT 0x03

static int running;
static FILE *fp;
static FILE *blkfp;
static int retcode = 0;
static uint16_t blkid = 0;

static uint8_t iord_stdio()
{
    int c = getc(fp);
    if (c == EOF) {
        running = 0;
    }
    return (uint8_t)c;
}

static void iowr_stdio(uint8_t val)
{
    if (val == 0x04) { // CTRL+D
        running = 0;
    } else {
        putchar(val);
    }
}

static void iowr_ret(uint8_t val)
{
    retcode = val;
}

static uint8_t iord_blk()
{
    uint8_t res = 0;
    if (blkfp != NULL) {
        int c = getc(blkfp);
        if (c != EOF) {
            res = c;
        }
    }
    return res;
}

static void iowr_blk(uint8_t val)
{
    blkid <<= 8;
    blkid |= val;
    if (blkfp != NULL) {
        fseek(blkfp, blkid*1024, SEEK_SET);
    }
}


int main(int argc, char *argv[])
{
    bool tty = false;
    struct termios termInfo;
    if (argc == 2) {
        fp = fopen(argv[1], "r");
        if (fp == NULL) {
            fprintf(stderr, "Can't open %s\n", argv[1]);
            return 1;
        }
    } else if (argc == 1) {
        fp = stdin;
        tty = isatty(fileno(stdin));
        if (tty) {
            // Turn echo off: the shell takes care of its own echoing.
            if (tcgetattr(0, &termInfo) == -1) {
                printf("Can't setup terminal.\n");
                return 1;
            }
            termInfo.c_lflag &= ~ECHO;
            termInfo.c_lflag &= ~ICANON;
            tcsetattr(0, TCSAFLUSH, &termInfo);
        }
    } else {
        fprintf(stderr, "Usage: ./forth [filename]\n");
        return 1;
    }
    blkfp = fopen("blkfs", "r+");
    Machine *m = emul_init();
    m->ramstart = RAMSTART;
    m->iord[STDIO_PORT] = iord_stdio;
    m->iowr[STDIO_PORT] = iowr_stdio;
    m->iowr[RET_PORT] = iowr_ret;
    m->iord[BLK_PORT] = iord_blk;
    m->iowr[BLK_PORT] = iowr_blk;
    // initialize memory
    for (int i=0; i<sizeof(KERNEL); i++) {
        m->mem[i] = KERNEL[i];
    }

    // Run!
    running = 1;

    while (running && emul_step());

    if (tty) {
        printf("\nDone!\n");
        termInfo.c_lflag |= ECHO;
        termInfo.c_lflag |= ICANON;
        tcsetattr(0, TCSAFLUSH, &termInfo);
        emul_printdebug();
    }
    if (blkfp != NULL) {
        fclose(blkfp);
    }
    fclose(fp);
    return retcode;
}
