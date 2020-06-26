#include <stdint.h>
#include <stdbool.h>

#define SP_ADDR 0xffff
#define RS_ADDR 0xff00
#define SYSVARS 0xe800

typedef uint8_t byte;
typedef uint16_t word;
// Native words in this C Forth VMs are indexed in an array. The word in memory
// is the typical 0x00 to indicate native, followed by an index byte. The
// Execute routine will then know which native word to execute.
typedef void (*NativeWord) ();
typedef byte (*IORD) ();
typedef void (*IOWR) (byte data);

/* Native word placement
    Being a C VM, all actual native code is outside the VM's memory. However,
    we have a stable ABI to conform to. VM_init() configures the memory by
    placing references to stable words at proper offsets, and then add all other
    native words next to it. This will result in a "boot binary" that is much
    more compact than a real Collapse OS memory layout.
*/
typedef struct {
    byte mem[0x10000];
    word SP;
    word RS;
    word IP;
    NativeWord nativew[0x100];
    byte nativew_count;
    // Array of 0x100 function pointers to IO read and write routines. Leave to
    // NULL when IO port is unhandled.
    IORD iord[0x100];
    IOWR iowr[0x100];
    word xcurrent; // only used during native bootstrap
    word maxRS;
    word minSP;
    bool running;
} VM;

VM* VM_init();
void VM_deinit();
bool VM_steps(int n);
void VM_memdump();
void VM_debugstr(char *s);
void VM_printdbg();
