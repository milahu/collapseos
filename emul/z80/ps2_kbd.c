#include "ps2_kbd.h"

void kbd_init(Kbd *kbd, Tristate *TH)
{
    kbd->kc = 0;
    kbd->breaking = false;
    kbd->TH = TH;
}

void kbd_pressshift(Kbd *kbd, bool ispressed)
{
    kbd->kc = 0x12; // shift keycode
    kbd->breaking = !ispressed;
}

void kbd_presskey(Kbd *kbd, uint8_t key)
{
    kbd->kc = 0;
    switch (key) {
        case '\t': kbd->kc = 0xd; break;
        case '`': kbd->kc = 0xe; break;
        case 'q': kbd->kc = 0x15; break;
        case '1': kbd->kc = 0x16; break;
        case 'z': kbd->kc = 0x1a; break;
        case 's': kbd->kc = 0x1b; break;
        case 'a': kbd->kc = 0x1c; break;
        case 'w': kbd->kc = 0x1d; break;
        case '2': kbd->kc = 0x1e; break;
        case 'c': kbd->kc = 0x21; break;
        case 'x': kbd->kc = 0x22; break;
        case 'd': kbd->kc = 0x23; break;
        case 'e': kbd->kc = 0x24; break;
        case '4': kbd->kc = 0x25; break;
        case '3': kbd->kc = 0x26; break;
        case ' ': kbd->kc = 0x29; break;
        case 'v': kbd->kc = 0x2a; break;
        case 'f': kbd->kc = 0x2b; break;
        case 't': kbd->kc = 0x2c; break;
        case 'r': kbd->kc = 0x2d; break;
        case '5': kbd->kc = 0x2e; break;
        case 'n': kbd->kc = 0x31; break;
        case 'b': kbd->kc = 0x32; break;
        case 'h': kbd->kc = 0x33; break;
        case 'g': kbd->kc = 0x34; break;
        case 'y': kbd->kc = 0x35; break;
        case '6': kbd->kc = 0x36; break;
        case 'm': kbd->kc = 0x3a; break;
        case 'j': kbd->kc = 0x3b; break;
        case 'u': kbd->kc = 0x3c; break;
        case '7': kbd->kc = 0x3d; break;
        case '8': kbd->kc = 0x3e; break;
        case ',': kbd->kc = 0x41; break;
        case 'k': kbd->kc = 0x42; break;
        case 'i': kbd->kc = 0x43; break;
        case 'o': kbd->kc = 0x44; break;
        case '0': kbd->kc = 0x45; break;
        case '9': kbd->kc = 0x46; break;
        case '.': kbd->kc = 0x49; break;
        case '/': kbd->kc = 0x4a; break;
        case 'l': kbd->kc = 0x4b; break;
        case ';': kbd->kc = 0x4c; break;
        case 'p': kbd->kc = 0x4d; break;
        case '-': kbd->kc = 0x4e; break;
        case '\'': kbd->kc = 0x52; break;
        case '[': kbd->kc = 0x54; break;
        case '=': kbd->kc = 0x55; break;
        case '\r': kbd->kc = 0x5a; break;
        case ']': kbd->kc = 0x5b; break;
        case '\\': kbd->kc = 0x5d; break;
        case '\b': kbd->kc = 0x66; break;
    }
}

uint8_t kbd_rd(Kbd *kbd)
{
    // There are 3 modes for reading the kbd:
    // 1. TH = highz: we're polling the the existence of a KC
    // 2. TH = low: return low nibble
    // 3. TH = high: return high nibble and reset KC
    uint8_t res = 0;
    uint8_t tosend = kbd->kc;
    if (kbd->breaking) {
        tosend = 0xf0;
    }
    if (*kbd->TH == TRI_HIGHZ) { // polling
        if (!kbd->kc) {
            res = 0x10; // indicate KC absence;
        }
    } else if (*kbd->TH == TRI_LOW) { // TH selected
        res = tosend & 0xf;
    } else {
        res = tosend >> 4;
        if (kbd->breaking) {
            kbd->breaking = false;
        } else {
            kbd->kc = 0;
        }
    }
    return res;
}

