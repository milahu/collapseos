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
    sdc->initstat = 0;
    sdc->recvidx = 0;
    sdc->sendidx = -1;
    sdc->fp = NULL;
    sdc->cmd17bytes = -1;
    sdc->cmd24bytes = -2;
}

byte sdc_spix(SDC *sdc, byte val)
{
    byte resp = 0xff;
    if (sdc->initstat < 8) {
        // not woken yet.
        sdc->initstat++;
        return resp;
    }
    if (sdc->sendidx >= 0) {
        resp = sdc->sendbuf[sdc->sendidx++];
        if (sdc->sendidx == 5) {
            sdc->sendidx = -1;
        }
        return resp;
    }
    if (sdc->cmd17bytes >= 0) {
        if (sdc->fp) {
            resp = getc(sdc->fp);
        }
        sdc->crc16 = crc16(sdc->crc16, resp);
        sdc->cmd17bytes++;
        if (sdc->cmd17bytes == 512) {
            sdc->sendbuf[3] = sdc->crc16 >> 8;
            sdc->sendbuf[4] = sdc->crc16 & 0xff;
            sdc->sendidx = 3;
            sdc->cmd17bytes = -1;
        }
        return resp;
    }
    if (sdc->cmd24bytes == -1) {
        if (val == 0xff) {
            // it's ok to receive idle bytes before the data token.
            return resp;
        }
        if (val == 0xfe) {
            // data token, good
            sdc->cmd24bytes = 0;
        } else {
            // something is wrong, cancel cmd24
            sdc->cmd24bytes = -2;
        }
        return resp;
    }
    if (sdc->cmd24bytes >= 0) {
        if (sdc->cmd24bytes < 512) {
            if (sdc->fp) {
                putc(val, sdc->fp);
            }
            sdc->crc16 = crc16(sdc->crc16, val);
        } else if (sdc->cmd24bytes == 512) {
            // CRC MSB
            if (val == (sdc->crc16>>8)) {
                fprintf(stderr, "Good CRC16 MSB\n");
            } else {
                fprintf(stderr, "Bad CRC16 MSB\n");
            }
        } else {
            if (val == (sdc->crc16&0xff)) {
                fprintf(stderr, "Good CRC16 LSB\n");
            } else {
                fprintf(stderr, "Bad CRC16 LSB\n");
            }
            // valid response for CMD24
            sdc->sendbuf[4] = 0x05;
            sdc->sendidx = 4;
            sdc->cmd24bytes = -3;
        }
        sdc->cmd24bytes++;
        return resp;
    }
    if ((sdc->recvidx == 0) && ((val > 0x7f) || (val < 0x40))) {
        // not a command
        return resp;
    }
    sdc->recvbuf[sdc->recvidx++] = val;
    if (sdc->recvidx < 6) {
        // incomplete command
        return resp;
    }
    // Command complete
    val &= 0x3f;
    sdc->recvidx = 0;
    uint8_t *b = sdc->recvbuf;
    uint8_t cmd = b[0] & 0x3f;
    uint16_t arg1 = (b[1] << 8) | b[2];
    uint16_t arg2 = (b[3] << 8) | b[4];
    // printf("cmd %02x %04x %04x\n", cmd, arg1, arg2);
    if (sdc->initstat == 8) {
        // At this stage, we're expecting CMD0
        if (cmd == 0) {
            sdc->initstat++;
            sdc->sendbuf[4] = 0x01;
            sdc->sendidx = 4;
        }
        return resp;
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
        return resp;
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
        return resp;
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
        return resp;
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
        return resp;
    }
    if (cmd == 24) {
        if (sdc->fp) {
            fseek(sdc->fp, arg2*512, SEEK_SET);
        }
        sdc->sendbuf[4] = 0x00;
        sdc->sendidx = 4;
        sdc->cmd24bytes = -1;
        sdc->crc16 = 0;
        return resp;
    }
    // Simulate success for any unknown command.
    sdc->sendbuf[4] = 0x00;
    sdc->sendidx = 4;
    return resp;
}
