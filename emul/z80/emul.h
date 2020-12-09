#pragma once
#include <stdint.h>
#include <stdbool.h>
#include "z80.h"

#define MAX_PCHOOK_COUNT 8
#define LEN8BIT 0x100
#define LEN16BIT 0x10000

typedef byte (*IORD) ();
typedef void (*IOWR) (byte data);
typedef byte (*EXCH) (byte data);

typedef struct _Machine {
    Z80Context cpu;
    byte mem[LEN16BIT];
    // Set to non-zero to specify where ROM ends. Any memory write attempt
    // below ramstart will trigger a warning.
    ushort ramstart;
    // The minimum value reached by SP at any point during execution.
    ushort minsp;
    // same principle for IX
    ushort maxix;
    // Array of 0x100 function pointers to IO read and write routines. Leave to
    // NULL when IO port is unhandled.
    IORD iord[LEN8BIT];
    IOWR iowr[LEN8BIT];
    // function to call when PC falls in one of the hooks
    void (*pchookfunc) (struct _Machine *m);
    // List of PC values at which we want to call pchookfunc
    ushort pchooks[MAX_PCHOOK_COUNT];
    byte pchooks_cnt;
} Machine;

typedef enum {
    TRI_HIGH,
    TRI_LOW,
    TRI_HIGHZ
} Tristate;

Machine* emul_init(char *binpath, ushort binoffset);
bool emul_step();
bool emul_steps(unsigned int steps);
void emul_loop();
void emul_trace(ushort addr);
void emul_memdump();
void emul_debugstr(char *s);
void emul_printdebug();
uint8_t emul_mem_read(int unused, uint16_t addr);
void emul_mem_write(int unused, uint16_t addr, uint8_t val);
// use when a port is a NOOP, but it's not an error to access it.
byte iord_noop();
void iowr_noop(byte val);
