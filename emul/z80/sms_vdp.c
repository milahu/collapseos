#include <string.h>
#include "sms_vdp.h"

void vdp_init(VDP *vdp)
{
    tms_init(&vdp->tms);
    memset(vdp->cram, 0, VDP_CRAM_SIZE);
}

void vdp_cmd_wr(VDP *vdp, uint8_t val)
{
    if (!vdp->tms.has_cmdlsb) {
        tms_cmd_wr(&vdp->tms, val);
    } else {
        if ((val & 0xc0) == 0xc0) {
            // palette RAM
            // curaddr > VRAM == addr in CRAM
            vdp->tms.curaddr = TMS_VRAM_SIZE + (vdp->tms.cmdlsb&0x1f);
        } else {
            tms_cmd_wr(&vdp->tms, val);
        }
    }
}

uint8_t vdp_data_rd(VDP *vdp)
{
    TMS9918 *tms = &vdp->tms;
    if (tms->curaddr < TMS_VRAM_SIZE) {
        return tms_data_rd(&vdp->tms);
    } else if (tms->curaddr - TMS_VRAM_SIZE < VDP_CRAM_SIZE) {
        return vdp->cram[tms->curaddr++-TMS_VRAM_SIZE];
    } else {
        return 0;
    }
}

void vdp_data_wr(VDP *vdp, uint8_t val)
{
    TMS9918 *tms = &vdp->tms;
    if (tms->curaddr < TMS_VRAM_SIZE) {
        tms_data_wr(&vdp->tms, val);
    } else if (tms->curaddr - TMS_VRAM_SIZE < VDP_CRAM_SIZE) {
        vdp->cram[tms->curaddr++-TMS_VRAM_SIZE] = val;
    }
}

uint8_t vdp_pixel(VDP *vdp, uint16_t x, uint16_t y)
{
    if (x >= VDP_SCREENW) {
        return 0;
    }
    if (y >= VDP_SCREENH) {
        return 0;
    }
    TMS9918 *tms = &vdp->tms;
    // name table offset
    uint16_t offset = (tms->regs[2] & 0xe) << 10;
    offset += ((y/8) << 6) + ((x/8) << 1);
    uint16_t tableval = tms->vram[offset] + (tms->vram[offset+1] << 8);
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
    uint8_t palette_id = ((tms->vram[offset] >> bitnum) & 1) + \
           (((tms->vram[offset+1] >> bitnum) & 1) << 1) + \
           (((tms->vram[offset+2] >> bitnum) & 1) << 2) + \
           (((tms->vram[offset+3] >> bitnum) & 1) << 3);
    uint8_t rgb = vdp->cram[palettemod+palette_id];
    return rgb;
}
