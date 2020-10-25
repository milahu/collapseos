#include <stdint.h>
#include <stdbool.h>

#define T6A04_ROWSIZE (120/8)
#define T6A04_RAMSIZE 64*T6A04_ROWSIZE

typedef enum {
    T6A04_XDEC = 0,
    T6A04_XINC = 1,
    T6A04_YDEC = 2,
    T6A04_YINC = 3
} T6A04_INCMODE;

typedef struct {
    // RAM is organized in 64 rows of 120 pixels (there is some offscreen
    // memory). Each byte holds 8 pixels. unset means no pixel, set means pixel.
    uint8_t ram[T6A04_RAMSIZE];
    bool enabled;
    // Whether the 8bit mode is enabled.
    bool has8bitmode;
    // Current "increment mode"
    T6A04_INCMODE incmode;
    // current Z offset
    uint8_t offset;
    // Currently active row
    uint8_t currow;
    // Currently active col (actual meaning depends on whether we're in 8bit
    // mode)
    uint8_t curcol;
    // True when a movement command was just made or if the LCD was just
    // initialized. When this is true, a read operation on the data port will be
    // invalid (returns zero and doesn't autoinc).
    bool just_moved;
} T6A04;

void t6a04_init(T6A04 *lcd);
uint8_t t6a04_cmd_rd(T6A04 *lcd);
void t6a04_cmd_wr(T6A04 *lcd, uint8_t val);
uint8_t t6a04_data_rd(T6A04 *lcd);
void t6a04_data_wr(T6A04 *lcd, uint8_t val);
bool t6a04_pixel(T6A04 *lcd, uint8_t y, uint8_t x);
