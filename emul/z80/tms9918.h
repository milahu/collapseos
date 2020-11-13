#pragma once
#include <stdint.h>
#include <stdbool.h>

#define TMS_VRAM_SIZE 0x4000
// Offset of the name table
#define TMS_NTABLE_OFFSET 0x3800


typedef struct {
    uint8_t vram[TMS_VRAM_SIZE];
    uint8_t regs[0x10];
    uint8_t cmdlsb;
    bool has_cmdlsb;
    uint16_t curaddr;
    uint16_t width; // in pixels
    uint16_t height; // in pixels
} TMS9918;

void tms_init(TMS9918 *tms);
uint8_t tms_cmd_rd(TMS9918 *tms);
void tms_cmd_wr(TMS9918 *tms, uint8_t val);
uint8_t tms_data_rd(TMS9918 *tms);
void tms_data_wr(TMS9918 *tms, uint8_t val);
// result is a RGB value
uint8_t tms_pixel(TMS9918 *tms, uint16_t x, uint16_t y);
