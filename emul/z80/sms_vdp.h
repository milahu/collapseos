#include "tms9918.h"

#define VDP_CRAM_SIZE 0x20
#define VDP_SCREENW (32*8)
#define VDP_SCREENH (24*8)

typedef struct {
    TMS9918 tms;
    uint8_t cram[VDP_CRAM_SIZE];
} VDP;

void vdp_init(VDP *vdp);
void vdp_cmd_wr(VDP *vdp, uint8_t val);
uint8_t vdp_data_rd(VDP *vdp);
void vdp_data_wr(VDP *vdp, uint8_t val);
uint8_t vdp_pixel(VDP *vdp, uint16_t x, uint16_t y);
