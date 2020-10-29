#include "sms_spi.h"

void spi_init(SPI *spi, Tristate *TH, Tristate *TR, EXCH spixfn)
{
    spi->TH = TH;
    spi->TR = TR;
    spi->lastTH = *TH;
    spi->bitcnt = 0;
    spi->bit = false;
    spi->rx = 0;
    spi->tx = 0xff;
    spi->spixfn = spixfn;
}

byte spi_rd(SPI *spi)
{

    return (byte)spi->bit; // return in bit 0
}

void spi_pulse(SPI *spi)
{
    if (*spi->TH != spi->lastTH) {
        spi->lastTH = *spi->TH;
        if (spi->lastTH == TRI_HIGH) {
            spi->rx <<= 1;
            if (*spi->TR == TRI_HIGH) {
                spi->rx++;
            }
            spi->bit = spi->tx >> 7;
            spi->tx <<= 1;
            spi->bitcnt++;
            if (spi->bitcnt == 8) {
                spi->tx = spi->spixfn(spi->rx);
                spi->bitcnt = 0;
            }
        }
    }
}
