#include <string.h>
#include "tms9918.h"

void tms_init(TMS9918 *tms)
{
    memset(tms->vram, 0, TMS_VRAM_SIZE);
    memset(tms->regs, 0, 0x10);
    tms->has_cmdlsb = false;
    tms->curaddr = 0;
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
    }
}

uint8_t tms_data_rd(TMS9918 *tms)
{
    if (tms->curaddr < TMS_VRAM_SIZE) {
        return tms->vram[tms->curaddr++];
    } else {
        return 0;
    }
}

void tms_data_wr(TMS9918 *tms, uint8_t val)
{
    if (tms->curaddr < TMS_VRAM_SIZE) {
        tms->vram[tms->curaddr++] = val;
    }
}

// Returns a 8-bit RGB value (0b00bbggrr)
uint8_t tms_pixel(TMS9918 *tms, uint16_t x, uint16_t y)
{
    return 0; // no TMS9918 mode implemented yet
}
