/* Common code between forth and stage binaries.

They all run on the same kind of virtual machine: A z80 CPU, 64K of RAM/ROM.
*/

#include <string.h>
#include "emul.h"
// Port for block reads. Write 2 bytes, MSB first, on that port and then
// read 1024 bytes from the DATA port.
#define BLK_PORT 0x03
#define BLKDATA_PORT 0x04

#ifndef BLKFS_PATH
#error BLKFS_PATH needed
#endif
#ifndef FBIN_PATH
#error FBIN_PATH needed
#endif

static Machine m;
static ushort traceval = 0;
static uint16_t blkid = 0;
static FILE *blkfp;

static uint8_t io_read(int unused, uint16_t addr)
{
    addr &= 0xff;
    IORD fn = m.iord[addr];
    if (fn != NULL) {
        return fn();
    } else {
        fprintf(stderr, "Out of bounds I/O read: %d\n", addr);
        return 0;
    }
}

static void io_write(int unused, uint16_t addr, uint8_t val)
{
    addr &= 0xff;
    IOWR fn = m.iowr[addr];
    if (fn != NULL) {
        fn(val);
    } else {
        fprintf(stderr, "Out of bounds I/O write: %d / %d (0x%x)\n", addr, val, val);
    }
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

static void iowr_blkdata(uint8_t val)
{
    putc(val, blkfp);
}

static uint8_t mem_read(int unused, uint16_t addr)
{
    return m.mem[addr];
}

static void mem_write(int unused, uint16_t addr, uint8_t val)
{
    if (addr < m.ramstart) {
        fprintf(stderr, "Writing to ROM (%d)!\n", addr);
        emul_memdump();
        fprintf(stderr, "Press any key to continue...\n");
        while (getchar() > 0x100);
    }
    m.mem[addr] = val;
}

Machine* emul_init()
{
    fprintf(stderr, "Using blkfs %s\n", BLKFS_PATH);
    blkfp = fopen(BLKFS_PATH, "r+");
    if (!blkfp) {
        fprintf(stderr, "Can't open\n");
        return NULL;
    }
    // initialize memory
    memset(m.mem, 0, 0x10000);
    FILE *bfp = fopen(FBIN_PATH, "r");
    if (!bfp) {
        fprintf(stderr, "Can't open forth.bin\n");
        return NULL;
    }
    int i = 0;
    int c = getc(bfp);
    while (c != EOF) {
        m.mem[i++] = c;
        c = getc(bfp);
    }
    fclose(bfp);
    m.ramstart = 0;
    m.minsp = 0xffff;
    m.maxix = 0;
    for (int i=0; i<0x100; i++) {
        m.iord[i] = NULL;
        m.iowr[i] = NULL;
    }
    Z80RESET(&m.cpu);
    m.cpu.memRead = mem_read;
    m.cpu.memWrite = mem_write;
    m.cpu.ioRead = io_read;
    m.cpu.ioWrite = io_write;
    m.iowr[BLK_PORT] = iowr_blk;
    m.iord[BLKDATA_PORT] = iord_blkdata;
    m.iowr[BLKDATA_PORT] = iowr_blkdata;
    return &m;
}

void emul_deinit()
{
    fclose(blkfp);
}

bool emul_step()
{
    if (!m.cpu.halted) {
        Z80Execute(&m.cpu);
        ushort newsp = m.cpu.R1.wr.SP;
        if (newsp != 0 && newsp < m.minsp) {
            m.minsp = newsp;
        }
        if (m.cpu.R1.wr.IX > m.maxix) {
            m.maxix = m.cpu.R1.wr.IX;
        }
        return true;
    } else {
        return false;
    }
}

bool emul_steps(unsigned int steps)
{
    while (steps) {
        if (!emul_step()) {
            return false;
        }
        steps--;
    }
    return true;
}

void emul_loop()
{
    while (emul_step());
}

void emul_trace(ushort addr)
{
    ushort newval = m.mem[addr+1] << 8 | m.mem[addr];
    if (newval != traceval) {
        traceval = newval;
        fprintf(stderr, "trace: %04x PC: %04x\n", traceval, m.cpu.PC);
    }
}

void emul_memdump()
{
    fprintf(stderr, "Dumping memory to memdump. PC %04x\n", m.cpu.PC);
    FILE *fp = fopen("memdump", "w");
    fwrite(m.mem, 0x10000, 1, fp);
    fclose(fp);
}

void emul_printdebug()
{
    fprintf(stderr, "Min SP: %04x\n", m.minsp);
    fprintf(stderr, "Max IX: %04x\n", m.maxix);
}
