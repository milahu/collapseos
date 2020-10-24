#include <stdint.h>
#include <stdbool.h>
#include "port.h"

typedef enum {
    PAD_BTN_UP = 0,
    PAD_BTN_DOWN = 1,
    PAD_BTN_LEFT = 2,
    PAD_BTN_RIGHT = 3,
    PAD_BTN_B = 4,
    PAD_BTN_C = 5,
    PAD_BTN_A = 6,
    PAD_BTN_START = 7
} PAD_BTN;

typedef struct {
    uint8_t pressed;
    Tristate *TH;
} Pad;

void pad_init(Pad *pad, Tristate *TH);
void pad_setbtn(Pad *pad, PAD_BTN btn, bool pressed);
uint8_t pad_rd(Pad *pad);
