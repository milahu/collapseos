/* Common code between forth and stage binaries.

They all run on the same kind of virtual machine: A z80 CPU, 64K of RAM/ROM.
*/

#include <string.h>
#include "emul.h"
// Port for block reads. Each read or write has to be done in 5 IO writes:
// 1 - r/w. 1 for read, 2 for write.
// 2 - blkid MSB
// 3 - blkid LSB
// 4 - dest addr MSB
// 5 - dest addr LSB
#define BLK_PORT 0x03

#ifndef BLKFS_PATH
#error BLKFS_PATH needed
#endif
#ifndef FBIN_PATH
#error FBIN_PATH needed
#endif

static Machine m;
static ushort traceval = 0;
static uint64_t blkop = 0; // 5 bytes
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
    blkop <<= 8;
    blkop |= val;
    uint8_t rw = blkop >> 32;
    if (rw) {
        uint16_t blkid = (blkop >> 16);
        uint16_t dest = blkop & 0xffff;
        blkop = 0;
        fseek(blkfp, blkid*1024, SEEK_SET);
        if (rw==2) { // write
            fwrite(&m.mem[dest], 1024, 1, blkfp);
        } else { // read
            fread(&m.mem[dest], 1024, 1, blkfp);
        }
    }
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
    fseek(blkfp, 0, SEEK_END);
    if (ftell(blkfp) < 100 * 1024) {
        fclose(blkfp);
        fprintf(stderr, "emul/blkfs too small, something's wrong, aborting.\n");
        return NULL;
    }
    fseek(blkfp, 0, SEEK_SET);
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

void emul_debugstr(char *s)
{
    sprintf(s, "SP %04x (%04x) IX %04x (%04x)",
        m.cpu.R1.wr.SP, m.minsp, m.cpu.R1.wr.IX, m.maxix);
}

void emul_printdebug()
{
    fprintf(stderr, "Min SP: %04x\n", m.minsp);
    fprintf(stderr, "Max IX: %04x\n", m.maxix);
}

byte iord_noop() { return 0; }
void iowr_noop(byte val) {}
