#include <stdio.h>
#include <string.h>
#include "tms9918.h"

static uint8_t COLORS[0x10] = { // TODO: put actual color codes
    0, // transparent
    0, // black
    1, // medium green
    1, // light green
    1, // dark blue
    1, // light blue
    1, // dark red
    1, // cyan
    1, // medium red
    1, // light red
    1, // dark yellow
    1, // light yellow
    1, // dark green
    1, // magenta
    1, // gray
    1, // white
};

void tms_init(TMS9918 *tms)
{
    memset(tms->vram, 0, TMS_VRAM_SIZE);
    memset(tms->regs, 0, 0x10);
    tms->has_cmdlsb = false;
    tms->curaddr = 0;
    tms->databuf = 0;
    tms->width = 40*6;
    tms->height = 24*8;
}

uint8_t tms_cmd_rd(TMS9918 *tms)
{
    return 0;
}

void tms_cmd_wr(TMS9918 *tms, uint8_t val)
{
    if (!tms->has_cmdlsb) {
        tms->cmdlsb = val;
        tms->has_cmdlsb = true;
        return;
    }
    tms->has_cmdlsb = false;
    if ((val & 0xc0) == 0x80) {
        // set register
        tms->regs[val&0xf] = tms->cmdlsb;
    } else {
        // VRAM
        tms->curaddr = ((val&0x3f) << 8) + tms->cmdlsb;
        if ((val & 0x40) == 0) { // reading VRAM
            tms->databuf = tms->vram[tms->curaddr];
        }
    }
}

uint8_t tms_data_rd(TMS9918 *tms)
{
    return tms->databuf;
}

void tms_data_wr(TMS9918 *tms, uint8_t val)
{
    tms->databuf = val;
    if (tms->curaddr < TMS_VRAM_SIZE) {
        tms->vram[tms->curaddr++] = val;
    }
}

// Returns a 8-bit RGB value (0b00bbggrr)
uint8_t tms_pixel(TMS9918 *tms, uint16_t x, uint16_t y)
{
    if ((tms->regs[1]&0x18) == 0x10 && (tms->regs[0]&0x40) == 0) {
        // Text mode
        uint16_t nameoff = (tms->regs[2] & 0xf) << 10;
        uint16_t patternoff = (tms->regs[4] & 0x7) << 11;
        uint8_t nameid = tms->vram[nameoff+(((y/8) * 40) + (x/6))];
        uint8_t patternline = tms->vram[patternoff+(nameid*8)+(y%8)];
        uint8_t color = tms->regs[7];
        if ((patternline>>(8-(x%6)))&1) {
            color >>= 4;
        }
        color &= 0xf;
        return color;
    } else { // unsupported mode
        return 0;
    }
}
