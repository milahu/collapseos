#include <string.h>
#include "vdp.h"

void vdp_init(VDP *vdp)
{
    memset(vdp->vram, 0, VDP_VRAM_SIZE);
    memset(vdp->regs, 0, 0x10);
    vdp->has_cmdlsb = false;
    vdp->curaddr = 0;
}

uint8_t vdp_cmd_rd(VDP *vdp)
{
    return 0;
}

void vdp_cmd_wr(VDP *vdp, uint8_t val)
{
    if (!vdp->has_cmdlsb) {
        vdp->cmdlsb = val;
        vdp->has_cmdlsb = true;
        return;
    }
    vdp->has_cmdlsb = false;
    if ((val & 0xc0) == 0x80) {
        // set register
        vdp->regs[val&0xf] = vdp->cmdlsb;
    } else if ((val & 0xc0) == 0xc0) {
        // palette RAM
        vdp->curaddr = 0x4000 + (vdp->cmdlsb&0x1f);
    } else {
        // VRAM
        vdp->curaddr = ((val&0x3f) << 8) + vdp->cmdlsb;
    }
}

uint8_t vdp_data_rd(VDP *vdp)
{
    uint8_t res = vdp->vram[vdp->curaddr];
    if (vdp->curaddr < VDP_VRAM_SIZE) {
        vdp->curaddr++;
    }
    return res;
}

void vdp_data_wr(VDP *vdp, uint8_t val)
{
    vdp->vram[vdp->curaddr] = val;
    if (vdp->curaddr < VDP_VRAM_SIZE) {
        vdp->curaddr++;
    }
}

// Returns a 8-bit RGB value (0b00bbggrr)
uint8_t vdp_pixel(VDP *vdp, uint16_t x, uint16_t y)
{
    if (x >= VDP_SCREENW) {
        return 0;
    }
    if (y >= VDP_SCREENH) {
        return 0;
    }
    // name table offset
    uint16_t offset = (vdp->regs[2] & 0xe) << 10;
    offset += ((y/8) << 6) + ((x/8) << 1);
    uint16_t tableval = vdp->vram[offset] + (vdp->vram[offset+1] << 8);
    uint16_t tilenum = tableval & 0x1ff;
    // is palette select bit on? if yes, use sprite palette instead
    uint8_t palettemod = tableval & 0x800 ? 0x10 : 0;
    // tile offset this time. Each tile is 0x20 bytes long.
    offset = tilenum * 0x20;
    // Each 4 byte is a row. Find row first.
    offset += ((y%8) * 4);
    uint8_t bitnum = 7 - (x%8);
    // Now, let's compose the result by pushing the right bit of our 4 bytes
    // into our result.
    uint8_t palette_id = ((vdp->vram[offset] >> bitnum) & 1) + \
           (((vdp->vram[offset+1] >> bitnum) & 1) << 1) + \
           (((vdp->vram[offset+2] >> bitnum) & 1) << 2) + \
           (((vdp->vram[offset+3] >> bitnum) & 1) << 3);
    uint8_t rgb = vdp->vram[0x4000+palettemod+palette_id];
    return rgb;
}
