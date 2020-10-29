#include "emul.h"

/* This emulates a SPI device being connected directly on a controller port.

   TH and TR control CLK and DI, input bit 0 (BTN_UP) is DO.

   This device has to "pulse" frequently so that it checks whether there's
   been a change in CLK.

   We have a bit of a challenge here with regards to an important limitation
   of the SMS: only 2 pins can be set as output. Without the ability to
   programatically select the SPI slave, we have to keep it selected at all
   times. This makes us vulnerable to going out of sync. This is a known
   limitation, the user will have to re-insert the SD card when that happens.
*/

typedef struct {
    Tristate *TH; // CLK
    Tristate *TR; // DI
    Tristate lastTH; // value of TH on last pulse
    byte bitcnt; // current xchange bit count
    bool bit; // bit to return in spi_rd()
    byte rx;
    byte tx;
    EXCH spixfn;
} SPI;

void spi_init(SPI *spi, Tristate *TH, Tristate *TR, EXCH spixfn);
byte spi_rd(SPI *spi); // for sms_ports' portX_rd
void spi_pulse(SPI *spi);
