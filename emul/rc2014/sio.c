#include <stdio.h>
#include <string.h>
#include "sio.h"

void sio_init(SIO *sio)
{
    memset(sio->wr, 0, sizeof(sio->wr));
    memset(sio->rr, 0, sizeof(sio->rr));
    sio->rx = 0;
    sio->tx = 0;
    sio->in_int = false;
}

bool sio_has_irq(SIO *sio)
{
    bool res = sio->in_int;
    sio->in_int = false;
    return res;
}

bool sio_hasrx(SIO *sio)
{
    return sio->rr[0] & 0x01; // Receive Character Available
}

bool sio_hastx(SIO *sio)
{
    return !(sio->rr[0] & 0x04); // Transmit Buffer Empty
}

uint8_t sio_read(SIO *sio)
{
    sio->rr[0] |= 0x04; // Transmit Buffer Empty high
    return sio->tx;
}

void sio_write(SIO *sio, uint8_t val)
{
    sio->rr[0] |= 0x01; // Receive Character Available high
    sio->rx = val;
    sio->in_int = true;
}

uint8_t sio_actl_rd(SIO *sio)
{
    uint8_t target = sio->wr[0] & 0x3; // PTR
    return sio->rr[target];
}

void sio_actl_wr(SIO *sio, uint8_t val)
{
    uint8_t target = sio->wr[0] & 0x7; // PTR
    sio->wr[target] = val;
    if (target != 0) {
        sio->wr[0] &= ~0x7;
    }
}

uint8_t sio_adata_rd(SIO *sio)
{
    sio->rr[0] &= ~0x01; // Receive Character Available low
    return sio->rx;
}

void sio_adata_wr(SIO *sio, uint8_t val)
{
    sio->tx = val;
    sio->rr[0] &= ~0x04; // Transmit Buffer Empty low
}

