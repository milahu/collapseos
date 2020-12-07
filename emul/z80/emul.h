#pragma once
#include <stdint.h>
#include <stdbool.h>
#include "z80.h"

#define MAX_PCHOOK_COUNT 8

typedef byte (*IORD) ();
typedef void (*IOWR) (byte data);
typedef byte (*EXCH) (byte data);

typedef struct _Machine {
    Z80Context cpu;
    byte mem[0x10000];
    // Set to non-zero to specify where ROM ends. Any memory write attempt
    // below ramstart will trigger a warning.
    ushort ramstart;
    // The minimum value reached by SP at any point during execution.
    ushort minsp;
    // same principle for IX
    ushort maxix;
    // Array of 0x100 function pointers to IO read and write routines. Leave to
    // NULL when IO port is unhandled.
    IORD iord[0x100];
    IOWR iowr[0x100];
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
// use when a port is a NOOP, but it's not an error to access it.
byte iord_noop();
void iowr_noop(byte val);
