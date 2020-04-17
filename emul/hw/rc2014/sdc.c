#include <stdio.h>
#include "sdc.h"

// Add data to crc with polynomial 0x1021
// https://stackoverflow.com/a/23726131
static uint16_t crc16(uint16_t crc, uint8_t data)
{
    uint8_t x = crc >> 8 ^ data;
    x ^= x>>4;
    crc = (crc << 8) ^ ((uint16_t)(x << 12)) ^ ((uint16_t)(x <<5)) ^ ((uint16_t)x);
    return crc;
}

void sdc_init(SDC *sdc)
{
    sdc->selected = false;
    sdc->initstat = 0;
    sdc->recvidx = 0;
    sdc->sendidx = -1;
    sdc->resp = 0xff;
    sdc->fp = NULL;
    sdc->cmd17bytes = -1;
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
    if (sdc->cmd17bytes >= 0) {
        if (sdc->fp) {
            sdc->resp = getc(sdc->fp);
        }
        sdc->crc16 = crc16(sdc->crc16, sdc->resp);
        sdc->cmd17bytes++;
        if (sdc->cmd17bytes == 512) {
            sdc->sendbuf[3] = sdc->crc16 >> 8;
            sdc->sendbuf[4] = sdc->crc16 & 0xff;
            sdc->sendidx = 3;
            sdc->cmd17bytes = -1;
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
    uint8_t cmd = b[0] & 0x3f;
    uint16_t arg1 = (b[1] << 8) | b[2];
    uint16_t arg2 = (b[3] << 8) | b[4];
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
        if ((cmd == 8) && (arg2 == 0x01aa)) {
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
        if ((cmd == 41) && (arg1 == 0x4000)) {
            sdc->initstat++;
            sdc->sendbuf[4] = 0x00;
            sdc->sendidx = 4;
        } else {
            sdc->initstat = 8;
        }
        return;
    }
    // We have a fully initialized card.
    if (cmd == 17) {
        if (sdc->fp) {
            fseek(sdc->fp, arg2*512, SEEK_SET);
        }
        sdc->sendbuf[3] = 0x00;
        // data token
        sdc->sendbuf[4] = 0xfe;
        sdc->sendidx = 3;
        sdc->cmd17bytes = 0;
        sdc->crc16 = 0;
        return;
    }
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
