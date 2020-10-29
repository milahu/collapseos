#include "rc2014_spi.h"

void spi_init(SPI *spi, EXCH spixfn)
{
    spi->selected = false;
    spi->resp = 0xff;
    spi->spixfn = spixfn;
}

// TODO: for now, any nonzero value enables the SPI. To allow
// emulation of systems with multi-devices SPI relay, change
// this.
void spi_ctl_wr(SPI *spi, byte val)
{
    spi->selected = val;
}

void spi_wr(SPI *spi, byte val)
{
    if (spi->selected) {
        spi->resp = spi->spixfn(val);
    }
}

byte spi_rd(SPI *spi)
{
    return spi->selected ? spi->resp : 0xff;
}
