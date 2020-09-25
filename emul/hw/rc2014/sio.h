#include <stdint.h>
#include <stdbool.h>

/* Extremely simplified Zilog SIO */

typedef struct {
    // Write registers WR7:0
    uint8_t wr[7];
    // Read registers RR2:0
    uint8_t rr[2];
    uint8_t rx;
    uint8_t tx;
    // Set to true when writing a byte while SIO is enabled and set back to
    // false when sio_has_irq() is false.
    bool in_int;
} SIO;

void sio_init(SIO *sio);
bool sio_has_irq(SIO *sio);
bool sio_hasrx(SIO *sio);
bool sio_hastx(SIO *sio);
uint8_t sio_read(SIO *sio);
void sio_write(SIO *sio, uint8_t val);
uint8_t sio_actl_rd(SIO *sio);
void sio_actl_wr(SIO *sio, uint8_t val);
uint8_t sio_adata_rd(SIO *sio);
void sio_adata_wr(SIO *sio, uint8_t val);
