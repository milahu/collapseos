#pragma once
#include "emul.h"

/* Emulates the behavior of an AT28 EEPROM. When reading, behaves like regular
 * RAM. When writing, be in "writing mode" for 10ms. If we assume 8MHz, that
 * means 80k t-states tracked from the CPU.
 *
 * While we're in programming mode, reading the written address will emulate
 * the "polling mode" of the AT28, that is, each read toggles IO/6.
 *
 * If another write happens before we're done writing or if we read from another
 * address, writing fails (both the new write and the old one) and nothing is
 * written to memory.
 */
typedef struct {
    // CPU reference needed to keep track of time
    Z80Context *cpu;
    // only range startoffset:size is used
    byte mem[LEN16BIT];
    // offset at which the EEPROM begins
    ushort startoffset;
    // EEPROM size
    ushort size;
    // t-state stamp of the active writing operation. 0 means none.
    unsigned int wrstamp;
    // address being written to
    ushort wraddr;
    // byte being written
    byte wrval;
    // last polled value. Next polling will yield this value with 6th bit
    // toggled.
    byte pollval;
} AT28;

void at28_init(AT28 *at28, Z80Context *cpu, ushort startoffset, ushort size);
byte at28_mem_read(AT28 *at28, ushort addr);
void at28_mem_write(AT28 *at28, ushort addr, byte val);
