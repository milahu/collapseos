#include "port.h"

void ports_init(Ports *ports)
{
    ports->ctl = 0xff;
    ports->TRA = TRI_HIGHZ;
    ports->THA = TRI_HIGHZ;
    ports->TRB = TRI_HIGHZ;
    ports->THB = TRI_HIGHZ;
}

uint8_t ports_ctl_rd(Ports *ports)
{
    return ports->ctl;
}

void ports_ctl_wr(Ports *ports, uint8_t val)
{
    ports->ctl = val;
    ports->TRA = TRI_HIGHZ;
    ports->THA = TRI_HIGHZ;
    ports->TRB = TRI_HIGHZ;
    ports->THB = TRI_HIGHZ;
    if ((val & 0x01) == 0) {
        ports->TRA = ((val & 0x10) == 0) ? TRI_LOW : TRI_HIGH;
    }
    if ((val & 0x02) == 0) {
        ports->THA = ((val & 0x20) == 0) ? TRI_LOW : TRI_HIGH;
    }
    if ((val & 0x04) == 0) {
        ports->TRB = ((val & 0x40) == 0) ? TRI_LOW : TRI_HIGH;
    }
    if ((val & 0x08) == 0) {
        ports->THB = ((val & 0x80) == 0) ? TRI_LOW : TRI_HIGH;
    }
}

uint8_t ports_A_rd(Ports *ports)
{
    // Bits 7:6 are port B's Down/Up
    // Bits 5:0 are port A's TR/TL/R/L/D/U
    uint8_t res = 0xff;
    if (ports->portA_rd != NULL) {
        res &= ports->portA_rd() | 0b11000000;
    }
    if (ports->portB_rd != NULL) {
        res &= (ports->portB_rd() << 6) | 0b00111111;
    }
    return res;

}

uint8_t ports_B_rd(Ports *ports)
{
    // Bit 7: Port B's TH
    // Bit 6: Port A's TH
    // Bit 5: unused
    // Bit 4: unused (reset button)
    // Bits 3:0 are port B's TR/TL/R/L
    uint8_t res = 0xff;
    if (ports->portA_rd != NULL) {
        res &= ports->portA_rd() | 0b10111111;
    }
    if (ports->portB_rd != NULL) {
        uint8_t portb = ports->portB_rd();
        res &= (portb << 1) | 0b01111111; // TH
        res &= (portb >> 2) | 0b11110000; // TR/TL/R/L
    }
    return res;
}
