#include <string.h>
#include "at28.h"

void at28_init(AT28 *at28, Z80Context *cpu, ushort startoffset, ushort size)
{
    at28->cpu = cpu;
    memset(at28->mem, 0, LEN16BIT);
    at28->startoffset = startoffset;
    at28->size = size;
    at28->wrstamp = 0;
}

static void _maybe_end_write(AT28 *at28)
{
    unsigned int ts = at28->cpu->tstates;
    unsigned int stamp = at28->wrstamp;
    // if ts < stamp, it means that the CPU re-initialized its counter
    if (stamp && ((ts < stamp) || (ts > stamp+80000))) {
        at28->mem[at28->wraddr] = at28->wrval;
        at28->wrstamp = 0;
    }
}

byte at28_mem_read(AT28 *at28, ushort addr)
{
    _maybe_end_write(at28);
    if ((addr >= at28->startoffset) && (addr < at28->startoffset+at28->size)) {
        if (at28->wrstamp) {
            if (addr == at28->wraddr) {
                // poll
                at28->pollval ^= 0b01000000; // bit 6 toggle
                return at28->pollval;
            } else {
                // reading another addr interrupts write
                at28->wrstamp = 0;
            }
        }
        return at28->mem[addr];
    } else {
        return emul_mem_read(0, addr);
    }
}

void at28_mem_write(AT28 *at28, ushort addr, byte val)
{
    _maybe_end_write(at28);
    if ((addr >= at28->startoffset) && (addr < at28->startoffset+at28->size)) {
        at28->wrstamp = at28->cpu->tstates;
        at28->wraddr = addr;
        at28->wrval = at28->pollval = val;
    } else {
        emul_mem_write(0, addr, val);
    }
}
