#pragma once
#include "emul.h"

// Each port is a bitmask of each pin's status. 1 means high.
// From Bit 0 to 6: up, down, left, right, TL, TR, TH

typedef struct {
    uint8_t ctl;
    Tristate TRA;
    Tristate THA;
    Tristate TRB;
    Tristate THB;
    IORD portA_rd;
    IORD portB_rd;
} Ports;

void ports_init(Ports *ports);
uint8_t ports_ctl_rd(Ports *ports);
void ports_ctl_wr(Ports *ports, uint8_t val);
uint8_t ports_A_rd(Ports *ports);
uint8_t ports_B_rd(Ports *ports);
