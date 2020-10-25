#include <string.h>
#include "t6a04.h"

void t6a04_init(T6A04 *lcd)
{
    memset(lcd->ram, 0, T6A04_RAMSIZE);
    lcd->enabled = false;
    lcd->incmode = T6A04_XINC;
    lcd->offset = 0;
    lcd->currow = 0;
    lcd->curcol = 0;
    lcd->just_moved = true;
    lcd->has8bitmode = false;
}

uint8_t t6a04_cmd_rd(T6A04 *lcd)
{
    return 0; // we are always ready for a new cmd
}

/*
 * 0x00/0x01: 6/8 bit mode
 * 0x02/0x03: enable/disable
 * 0x04-0x07: incmodes
 * 0x20-0x34: set col
 * 0x40-0x7f: set Z offset
 * 0x80-0xbf: set row
 * 0xc0-0xff: set contrast
 */
void t6a04_cmd_wr(T6A04 *lcd, uint8_t val)
{
    if ((val & 0xc0) == 0xc0) {
        // contrast, ignoring
    } else if (val & 0x80) {
        lcd->currow = val & 0x3f;
        lcd->just_moved = true;
    } else if (val & 0x40) {
        lcd->offset = val & 0x3f;
    } else if (val & 0x20) {
        lcd->curcol = val & 0x1f;
        lcd->just_moved = true;
    } else if (val & 0x18) {
        // stuff we don't emulate
    } else if (val & 0x04) {
        lcd->incmode = val & 0x03;
    } else if (val & 0x02) {
        lcd->enabled = val & 0x01;
    } else {
        lcd->has8bitmode = val;
    }
}

// Advance current position according to current incmode
static void _advance(T6A04 *lcd)
{
	uint8_t maxY = lcd->has8bitmode ? 14 : 19;
    switch (lcd->incmode) {
        case T6A04_XDEC:
            lcd->currow = (lcd->currow-1) & 0x3f;
            break;
        case T6A04_XINC:
            lcd->currow = (lcd->currow+1) & 0x3f;
            break;
        case T6A04_YDEC:
            if (lcd->curcol == 0) {
                lcd->curcol = maxY;
            } else {
                lcd->curcol--;
            }
            break;
        case T6A04_YINC:
            if (lcd->curcol < maxY) {
                lcd->curcol++;
            } else {
                lcd->curcol = 0;
            }
            break;
    }
}

uint8_t t6a04_data_rd(T6A04 *lcd)
{
    uint8_t res;
    if (lcd->just_moved) {
        // After a move command, the first read op is a noop.
        lcd->just_moved = false;
        return 0;
    }
    if (lcd->has8bitmode) {
        int pos = lcd->currow * T6A04_ROWSIZE + lcd->curcol;
        res = lcd->ram[pos];
    } else {
        // 6bit mode is a bit more complicated because the 6-bit number often
        // spans two bytes. We manage this by loading two bytes into a uint16_t
        // and then shift it right properly.
        // bitpos represents the leftmost bit of our 6bit number.
        int bitpos = lcd->curcol * 6;
        // offset represents the shift right we need to perform from the two
        // bytes following bitpos/8 so that we can have our number with a 6-bit
        // mask.
        // Example, col 3 has a bitpos of 18, which means that it loads bytes 2
        // and 3. Its bits would be in bit pos 14:8, which means it has an
        // offset of 8. There is always an offset and its always in the 3-10
        // range
        int offset = 10 - (bitpos % 8); // 10 is for 16bit - 6bit
        int pos = (lcd->currow * T6A04_ROWSIZE) + (bitpos / 8);
        uint16_t word = lcd->ram[pos] << 8;
        word |= lcd->ram[pos+1];
        res = (word >> offset) & 0x3f;
    }
    _advance(lcd);
    return res;
}

void t6a04_data_wr(T6A04 *lcd, uint8_t val)
{
    lcd->just_moved = false;
    if (lcd->has8bitmode) {
        int pos = lcd->currow * T6A04_ROWSIZE + lcd->curcol;
        lcd->ram[pos] = val;
    } else {
        // See comments in t6a04_data_rd().
        int bitpos = lcd->curcol * 6;
        int offset = 10 - (bitpos % 8);
        int pos = (lcd->currow * T6A04_ROWSIZE) + (bitpos / 8);
        uint16_t word = lcd->ram[pos] << 8;
        word |= lcd->ram[pos+1];
        // word contains our current ram value. Let's fit val in this.
        word &= ~(0x003f << offset);
        word |= val << offset;
        lcd->ram[pos] = word >> 8;
        lcd->ram[pos+1] = word & 0xff;
    }
    _advance(lcd);
}

bool t6a04_pixel(T6A04 *lcd, uint8_t y, uint8_t x)
{
    x = (x + lcd->offset) & 0x3f;
    uint8_t val = lcd->ram[x * T6A04_ROWSIZE + (y / 8)];
    return (val >> (7 - (y % 8))) & 1;
}
