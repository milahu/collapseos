#include <string.h>
#include <ctype.h>

#include "kbd.h"

void kbd_init(KBD *kbd)
{
    memset(kbd->pressed, 0xff, 8);
    kbd->selected = 0xff;
}

uint8_t kbd_rd(KBD *kbd)
{
    uint8_t res = 0xff;
    for (int i=0; i<8; i++) {
        if ((kbd->selected & (1<<i)) == 0) {
            res &= kbd->pressed[i];
        }
    }
    return res;
}

void kbd_wr(KBD *kbd, uint8_t val)
{
    kbd->selected = val;
}

// The key is separated in two nibble. High nibble is group, low nibble is key.
void kbd_setkey(KBD *kbd, uint8_t key, bool pressed)
{
    uint8_t group = kbd->pressed[key>>4];
    if (pressed) {
        group &= ~(1<<(key&0x7));
    } else {
        group |= 1<<(key&0x7);
    }
    kbd->pressed[key>>4] = group;
}

// Attempts to returns a key code corresponding to the specified char. 0 if
// nothing matches.
uint8_t kbd_trans(char c)
{
    c = toupper(c);
    switch (c) {
    case 0x0a:
    case 0x0d: return 0x10; // ENTER
    case '+': return 0x11;
    case '-': return 0x12;
    case '*': return 0x13;
    case '/': return 0x14;
    case '^': return 0x15;
    case '3': return 0x21;
    case '6': return 0x22;
    case '9': return 0x23;
    case ')': return 0x24;
    case '.': return 0x30;
    case '2': return 0x31;
    case '5': return 0x32;
    case '8': return 0x33;
    case '(': return 0x34;
    case '0': return 0x40;
    case '1': return 0x41;
    case '4': return 0x42;
    case '7': return 0x43;
    case ',': return 0x44;
    case 0x7f: return 0x67; // DEL
    case '"': return 0x11;
    case 'W': return 0x12;
    case 'R': return 0x13;
    case 'M': return 0x14;
    case 'H': return 0x15;
    case '?': return 0x20;
    case 'V': return 0x22;
    case 'Q': return 0x23;
    case 'L': return 0x24;
    case 'G': return 0x25;
    case ':': return 0x30;
    case 'Z': return 0x31;
    case 'U': return 0x32;
    case 'P': return 0x33;
    case 'K': return 0x34;
    case 'F': return 0x35;
    case 'C': return 0x36;
    case ' ': return 0x40;
    case 'Y': return 0x41;
    case 'T': return 0x42;
    case 'O': return 0x43;
    case 'J': return 0x44;
    case 'E': return 0x45;
    case 'B': return 0x46;
    case 'X': return 0x51;
    case 'S': return 0x52;
    case 'N': return 0x53;
    case 'I': return 0x54;
    case 'D': return 0x55;
    case 'A': return 0x56;
    default: return 0;
    }
}

