#include "emul.h"

typedef struct {
    // Initialization status. 0 == not woken 8 == woken 9 == CMD0 received
    // 10 == CMD8 received, 11 == CMD55 received, 12 == CMD41 received (fully
    // initialized).
    unsigned int initstat;
    // We receive commands into this buffer.
    byte recvbuf[6];
    // Where the next SPI byte should be stored in recvbuf.
    unsigned int recvidx;
    // Buffer to the arguments for a response
    byte sendbuf[5];
    // Index of the next byte from sendbuf we should return. If -1, buffer is
    // empty.
    int sendidx;
    // File used for contents read/write
    FILE *fp;
    // number of bytes read into the current CMD17. -1 means no CMD17 active.
    int cmd17bytes;
    // number of bytes received for the current CMD24. -2 means no CMD24 active.
    // -1 means we're still waiting for the data token.
    int cmd24bytes;
    // running crc16 during read and write operations.
    uint16_t crc16;
} SDC;

void sdc_init(SDC *sdc);
byte sdc_spix(SDC *sdc, byte val);
