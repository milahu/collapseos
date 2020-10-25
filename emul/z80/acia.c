#include "acia.h"

static void _check_irq(ACIA *acia)
{
    // do we have RDRF?
    if ((acia->status & 0x01) && (acia->control & 0x80)) {
        acia->status |= 0x80;
    }
    // do we have TDRE?
    if ((acia->status & 0x02) && ((acia->control & 0xe0) == 0xe0)) {
        acia->status |= 0x80;
    }
}

void acia_init(ACIA *acia)
{
    acia->status = 0x02; // TDRE
    acia->control = 0x00;
    acia->rx = 0;
    acia->tx = 0;
    acia->in_int = false;
}

bool acia_has_irq(ACIA *acia)
{
    if (acia->in_int) {
        return false;
    }
    acia->in_int = acia->status & 0x80;
    return acia->in_int;
}

bool acia_hasrx(ACIA *acia)
{
    return acia->status & 0x01; // RDRF
}

bool acia_hastx(ACIA *acia)
{
    return !(acia->status & 0x02); // TRDE
}

uint8_t acia_read(ACIA *acia)
{
    acia->status |= 0x02; // TRDE high
    _check_irq(acia);
    return acia->tx;
}

void acia_write(ACIA *acia, uint8_t val)
{
    acia->status |= 0x01; // RDRF high
    acia->rx = val;
    _check_irq(acia);
}

uint8_t acia_ctl_rd(ACIA *acia)
{
    return acia->status;
}

void acia_ctl_wr(ACIA *acia, uint8_t val)
{
    acia->control = val;
    _check_irq(acia);
}

uint8_t acia_data_rd(ACIA *acia)
{
    acia->status &= ~0x81; // RDRF and IRQ low
    acia->in_int = false;
    return acia->rx;
}

void acia_data_wr(ACIA *acia, uint8_t val)
{
    acia->tx = val;
    acia->status &= ~0x82; // TRDE and IRQ low
    acia->in_int = false;
    _check_irq(acia);
}

