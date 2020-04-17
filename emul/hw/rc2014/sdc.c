#include <stdio.h>
#include "sdc.h"

void sdc_init(SDC *sdc)
{
    sdc->selected = false;
    sdc->initstat = 0;
    sdc->recvidx = 0;
    sdc->sendidx = -1;
    sdc->resp = 0xff;
}

void sdc_cslow(SDC *sdc)
{
    sdc->selected = true;
}

void sdc_cshigh(SDC *sdc)
{
    sdc->selected = false;
}

void sdc_spi_wr(SDC *sdc, uint8_t val)
{
    if (!sdc->selected) {
        return;
    }
    sdc->resp = 0xff;
    if (sdc->initstat < 8) {
        // not woken yet.
        sdc->initstat++;
        return;
    }
    if (sdc->sendidx >= 0) {
        sdc->resp = sdc->sendbuf[sdc->sendidx++];
        if (sdc->sendidx == 5) {
            sdc->sendidx = -1;
        }
        return;
    }
    if ((sdc->recvidx == 0) && ((val > 0x7f) || (val < 0x40))) {
        // not a command
        return;
    }
    sdc->recvbuf[sdc->recvidx++] = val;
    if (sdc->recvidx < 6) {
        // incomplete command
        return;
    }
    // Command complete
    val &= 0x3f;
    sdc->recvidx = 0;
    uint8_t *b = sdc->recvbuf;
    fprintf(stderr, "cmd %02x %02x %02x %02x %02x\n", b[0], b[1], b[2], b[3], b[4]);
    uint8_t cmd = b[0] & 0x3f;
    if (sdc->initstat == 8) {
        // At this stage, we're expecting CMD0
        if (cmd == 0) {
            sdc->initstat++;
            sdc->sendbuf[4] = 0x01;
            sdc->sendidx = 4;
        }
        return;
    }
    if (sdc->initstat == 9) {
        // At this stage, we're expecting CMD8 with 0x1aa arg2
        if ((cmd == 8) && (b[3] == 0x01) && (b[4] == 0xaa)) {
            sdc->initstat++;
            sdc->sendbuf[0] = 0x01;
            sdc->sendbuf[1] = 0;
            sdc->sendbuf[2] = 0;
            sdc->sendbuf[3] = 0x01;
            sdc->sendbuf[4] = 0xaa;
            sdc->sendidx = 0;
        } else {
            sdc-> initstat = 8;
        }
        return;
    }
    if (sdc->initstat == 10) {
        // At this stage, we're expecting CMD55
        if (cmd == 55) {
            sdc->initstat++;
            sdc->sendbuf[4] = 0x01;
            sdc->sendidx = 4;
        } else {
            sdc->initstat = 8;
        }
        return;
    }
    if (sdc->initstat == 11) {
        // At this stage, we're expecting CMD41
        if ((cmd == 41) && (b[1] == 0x40) && (b[2] == 0x00)) {
            sdc->initstat++;
            sdc->sendbuf[4] = 0x00;
            sdc->sendidx = 4;
        } else {
            sdc->initstat = 8;
        }
        return;
    }
    // We have a fully initialized card.
    // Simulate success for any unknown command.
    sdc->sendbuf[4] = 0x00;
    sdc->sendidx = 4;
}

uint8_t sdc_spi_rd(SDC *sdc)
{
    if (!sdc->selected) {
        return 0xff;
    }
    return sdc->resp;
}
