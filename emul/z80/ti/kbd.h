#include <stdint.h>
#include <stdbool.h>

// These are keycodes for special keys
#define KBD_ALPHA 0x57
#define KBD_2ND 0x65

// NOTE: We don't manage the ON key here.
typedef struct {
    // Bitmask of pressed keys. Like on real hardware, 0xff means nothing
    // pressed. The group 7 has no key, but for code simplicity, we have a neat
    // array of 8 bytes.
    uint8_t pressed[8];
    // Selected groups. Active low.
    uint8_t selected;
} KBD;

void kbd_init(KBD *kbd);
uint8_t kbd_rd(KBD *kbd);
void kbd_wr(KBD *kbd, uint8_t val);
// The key is separated in two nibble. High nibble is group, low nibble is key.
void kbd_setkey(KBD *kbd, uint8_t key, bool pressed);
// Attempts to returns a key code corresponding to the specified char. 0 if
// nothing matches.
uint8_t kbd_trans(char c);
