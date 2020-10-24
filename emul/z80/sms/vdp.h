#include <stdint.h>
#include <stdbool.h>

#define VDP_VRAM_SIZE 0x4020
#define VDP_SCREENW (32*8)
#define VDP_SCREENH (24*8)
// Offset of the name table
#define VDP_NTABLE_OFFSET 0x3800


typedef struct {
    // the last 0x20 is palette RAM
    uint8_t vram[VDP_VRAM_SIZE];
    uint8_t regs[0x10];
    uint8_t cmdlsb;
    bool has_cmdlsb;
    uint16_t curaddr;
} VDP;

void vdp_init(VDP *vdp);
uint8_t vdp_cmd_rd(VDP *vdp);
void vdp_cmd_wr(VDP *vdp, uint8_t val);
uint8_t vdp_data_rd(VDP *vdp);
void vdp_data_wr(VDP *vdp, uint8_t val);
// result is a RGB value
uint8_t vdp_pixel(VDP *vdp, uint16_t x, uint16_t y);
