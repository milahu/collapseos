#include <stdint.h>
#include <stdio.h>
#include "cpu.h"

extern uint8_t	RAM[0x100000];

int main(int argc, char *argv[])
{
    int i = 0;
    int ch = 0;

    reset86();
    ch = getchar();
    while (ch != EOF) {
        RAM[i++] = ch;
        ch = getchar();
    }
    printregs();
    exec86(1);
    printregs();
    return 0;
}
