#include "emul.h"

/* Emulates a SPI relay designed for the RC2014, enabled by poking on the CTL
   port, then allowing a SPI exchange by writing to, then reading from, the
   data port.
*/

typedef struct {
    bool selected;
    byte resp;
    EXCH spixfn;
} SPI;
    
void spi_init(SPI *spi, EXCH spixfn);
void spi_ctl_wr(SPI *spi, byte val);
void spi_wr(SPI *spi, byte val);
byte spi_rd(SPI *spi);
