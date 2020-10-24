#include <stdint.h>
#include <stdbool.h>

typedef struct {
    // Bit 7: interrupt status, low when interrupt request pending.
    // Bit 6: Parity error
    // Bit 5: Receiver overrun
    // Bit 4: Framing error
    // Bit 3: Clear To Send
    // Bit 2: Data Carrier Detected
    // Bit 1: Transmit Data Register Empty (TDRE)
    // Bit 0: Receive Data Register Full (RDRF)
    // We care about bits 7, 1, 0, maybe 5 later.
    uint8_t status;
    // Bit 7: interrupt enable
    // Bits 6:5: RTS + transmit interrupt enable
    // Bits 4:2: parity + stop bit
    // Bits 1:0: speed divider
    // We don't actually care about any of those except the interrupt enable
    // bits.
    uint8_t control;
    uint8_t rx;
    uint8_t tx;
    // Will be set to true the first time acia_has_irq() is called when IRQ is
    // set. Then, as long as it stays true, acia_has_irq() will return false.
    // When IRQ status is reset, so is in_int.
    bool in_int;
} ACIA;

void acia_init(ACIA *acia);
bool acia_has_irq(ACIA *acia);
bool acia_hasrx(ACIA *acia);
bool acia_hastx(ACIA *acia);
uint8_t acia_read(ACIA *acia);
void acia_write(ACIA *acia, uint8_t val);
uint8_t acia_ctl_rd(ACIA *acia);
void acia_ctl_wr(ACIA *acia, uint8_t val);
uint8_t acia_data_rd(ACIA *acia);
void acia_data_wr(ACIA *acia, uint8_t val);
