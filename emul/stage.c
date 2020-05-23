#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include "emul.h"
#include "forth-bin.h"

#ifndef BLKFS_PATH
#error BLKFS_PATH needed
#endif

/* Staging binaries

The role of a stage executable is to compile definitions in a dictionary and
then spit the difference between the starting binary and the new binary.

That binary can then be grafted to an exiting Forth binary to augment its
dictionary.

We could, if we wanted, run only with the bootstrap binary and compile core
defs at runtime, but that would mean that those defs live in RAM. In may system,
RAM is much more constrained than ROM, so it's worth it to give ourselves the
trouble of compiling defs to binary.

*/

#define RAMSTART 0
#define STDIO_PORT 0x00
// To know which part of RAM to dump, we listen to port 2, which at the end of
// its compilation process, spits its HERE addr to port 2 (MSB first)
#define HERE_PORT 0x02
// Port for block reads. Write 2 bytes, MSB first, on that port and then
// read 1024 bytes from the DATA port.
#define BLK_PORT 0x03
#define BLKDATA_PORT 0x04

static int running;
// We support double-pokes, that is, a first poke to tell where to start the
// dump and a second one to tell where to stop. If there is only one poke, it's
// then ending HERE and we start at sizeof(KERNEL).
static uint16_t start_here = 0;
static uint16_t end_here = 0;
static uint16_t blkid = 0;
static FILE *blkfp;

static uint8_t iord_stdio()
{
    int c = getc(stdin);
    if (c == EOF) {
        running = 0;
    }
    return (uint8_t)c;
}

static void iowr_stdio(uint8_t val)
{
    // we don't output stdout in stage0
}

static void iowr_here(uint8_t val)
{
    start_here <<=8;
    start_here |= (end_here >> 8);
    end_here <<= 8;
    end_here |= val;
}

static void iowr_blk(uint8_t val)
{
    blkid <<= 8;
    blkid |= val;
    fseek(blkfp, blkid*1024, SEEK_SET);
}

static uint8_t iord_blkdata()
{
    return getc(blkfp);
}

int main(int argc, char *argv[])
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
    m->iowr[HERE_PORT] = iowr_here;
    m->iowr[BLK_PORT] = iowr_blk;
    m->iord[BLKDATA_PORT] = iord_blkdata;
    // initialize memory
    for (int i=0; i<sizeof(KERNEL); i++) {
        m->mem[i] = KERNEL[i];
    }

    // Run!
    running = 1;

    while (running && emul_step());

    // We're done, now let's spit dict data
    for (int i=start_here; i<end_here; i++) {
        putchar(m->mem[i]);
    }
    emul_printdebug();
    return 0;
}

