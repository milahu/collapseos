#include <stdint.h>
#include <stdio.h>
#include "cpu.h"

#ifndef FBIN_PATH
#error FBIN_PATH needed
#endif

extern uint8_t byteregtable[8];
extern union _bytewordregs_ regs;
extern INTHOOK INTHOOKS[0x100];

/* we have a fake INT API:
INT 1: EMIT. AL = char to spit
INT 2: KEY. AL = char read
*/

void int1() {
    putchar(getreg8(regal));
}

void int2() {
    putreg8(regal, getchar());
}

int main(int argc, char *argv[])
{
    INTHOOKS[1] = int1;
    INTHOOKS[2] = int2;
    reset86();
    // initialize memory
    FILE *bfp = fopen(FBIN_PATH, "r");
    if (!bfp) {
        fprintf(stderr, "Can't open forth.bin\n");
        return 1;
    }
    int i = 0;
    int c = getc(bfp);
    while (c != EOF) {
        write86(i++, c);
        c = getc(bfp);
    }
    fclose(bfp);
    while (exec86(100));
    return 0;
}
