#include <stdint.h>
#include <stdbool.h>

typedef struct {
    bool selected;
    // Initialization status. 0 == not woken 8 == woken 9 == CMD0 received
    // 10 == CMD8 received, 11 == CMD55 received, 12 == CMD41 received (fully
    // initialized).
    unsigned int initstat;
    // We receive commands into this buffer.
    uint8_t recvbuf[6];
    // Where the next SPI byte should be stored in recvbuf.
    unsigned int recvidx;
    // Buffer to the arguments for a response
    uint8_t sendbuf[5];
    // Index of the next byte from sendbuf we should return. If -1, buffer is
    // empty.
    int sendidx;
    // One byte response. When all other response buffers are empty, return
    // this.
    uint8_t resp;
} SDC;

void sdc_init(SDC *sdc);
void sdc_cslow(SDC *sdc);
void sdc_cshigh(SDC *sdc);
void sdc_spi_wr(SDC *sdc, uint8_t val);
uint8_t sdc_spi_rd(SDC *sdc);
