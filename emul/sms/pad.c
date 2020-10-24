#include "pad.h"

void pad_init(Pad *pad, Tristate *TH)
{
    pad->pressed = 0xff;
    pad->TH = TH;
}

void pad_setbtn(Pad *pad, PAD_BTN btn, bool pressed)
{
    if (pressed) {
        pad->pressed &= ~(1 << btn);
    } else {
        pad->pressed |= (1 << btn);
    }
}

uint8_t pad_rd(Pad *pad)
{
    uint8_t res;
    if (*pad->TH == TRI_LOW) { // TH selected
        // A and START shifted in from bits 7:6 into 5:4
        res = (pad->pressed & 0xf) | ((pad->pressed & 0xc0) >> 2);
    } else {
        res = pad->pressed & 0x3f;
    }
    // Bits 7:6 are always high
    res |= 0b11000000;
    return res;
}
