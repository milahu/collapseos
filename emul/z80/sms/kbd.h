#include <stdint.h>
#include <stdbool.h>
#include "port.h"

#define KBD_BUFSZ 0x10

typedef struct {
    uint8_t kc; // last keycode to be pressed. 0 means none.
    bool breaking; // whether we should send 0xf0 before kc
    Tristate *TH;
} Kbd;

void kbd_init(Kbd *kbd, Tristate *TH);
void kbd_pressshift(Kbd *kbd, bool ispressed);
void kbd_presskey(Kbd *kbd, uint8_t keycode);
uint8_t kbd_rd(Kbd *kbd);
