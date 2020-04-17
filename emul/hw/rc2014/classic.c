/* RC2014 classic
 *
 * - 8K of ROM in range 0x0000-0x2000
 * - 32K of RAM in range 0x8000-0xffff
 * - ACIA in ports 0x80 (ctl) and 0x81 (data)
 *
 * ACIA is hooked to stdin/stdout. CTRL+D exits when in TTY mode.
 */

#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include "../../emul.h"
#include "acia.h"
#include "sdc.h"

#define RAMSTART 0x8000
#define ACIA_CTL_PORT 0x80
#define ACIA_DATA_PORT 0x81
#define SDC_CSHIGH 0x06
#define SDC_CSLOW 0x05
#define SDC_SPI 0x04
#define MAX_ROMSIZE 0x2000

static ACIA acia;
static SDC sdc;

static uint8_t iord_acia_ctl()
{
    return acia_ctl_rd(&acia);
}

static uint8_t iord_acia_data()
{
    return acia_data_rd(&acia);
}

static void iowr_acia_ctl(uint8_t val)
{
    acia_ctl_wr(&acia, val);
}

static void iowr_acia_data(uint8_t val)
{
    acia_data_wr(&acia, val);
}

static uint8_t iord_sdc_spi()
{
    return sdc_spi_rd(&sdc);
}

static void iowr_sdc_spi(uint8_t val)
{
    sdc_spi_wr(&sdc, val);
}

static void iowr_sdc_cshigh(uint8_t val)
{
    sdc_cshigh(&sdc);
}

static void iowr_sdc_cslow(uint8_t val)
{
    sdc_cslow(&sdc);
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: ./classic /path/to/rom\n");
        return 1;
    }
    FILE *fp = fopen(argv[1], "r");
    if (fp == NULL) {
        fprintf(stderr, "Can't open %s\n", argv[1]);
        return 1;
    }
    Machine *m = emul_init();
    m->ramstart = RAMSTART;
    int i = 0;
    int c;
    while ((c = fgetc(fp)) != EOF && i < MAX_ROMSIZE) {
        m->mem[i++] = c & 0xff;
    }
    pclose(fp);
    if (i == MAX_ROMSIZE) {
        fprintf(stderr, "ROM image too large.\n");
        return 1;
    }
    bool tty = isatty(fileno(stdin));
    struct termios term, saved_term;
    if (tty) {
        // Turn echo off: the shell takes care of its own echoing.
        if (tcgetattr(0, &term) == -1) {
            printf("Can't setup terminal.\n");
            return 1;
        }
        saved_term = term;
        term.c_lflag &= ~ECHO;
        term.c_lflag &= ~ICANON;
		term.c_cc[VMIN] = 0;
		term.c_cc[VTIME] = 0;
        tcsetattr(0, TCSADRAIN, &term);
    }

    acia_init(&acia);
    sdc_init(&sdc);
    m->iord[ACIA_CTL_PORT] = iord_acia_ctl;
    m->iord[ACIA_DATA_PORT] = iord_acia_data;
    m->iowr[ACIA_CTL_PORT] = iowr_acia_ctl;
    m->iowr[ACIA_DATA_PORT] = iowr_acia_data;
    m->iord[SDC_SPI] = iord_sdc_spi;
    m->iowr[SDC_SPI] = iowr_sdc_spi;
    m->iowr[SDC_CSHIGH] = iowr_sdc_cshigh;
    m->iowr[SDC_CSLOW] = iowr_sdc_cslow;

    char tosend = 0;
    while (emul_step()) {
        // Do we have an interrupt?
        if (acia_has_irq(&acia)) {
            Z80INT(&m->cpu, 0);
        }
        // Is the RC2014 transmitting?
        if (acia_hastx(&acia)) {
            putchar(acia_read(&acia));
            fflush(stdout);
        }
        // Do we have something to send?
        if (!tosend) {
            char c;
            if (read(fileno(stdin), &c, 1) == 1) {
                if (c == 4) {   // CTRL+D
                    // Stop here
                    break;
                }
                tosend = c;
            } else if (!tty) {
                // This means we reached EOF
                break;
            }
        }
        if (tosend && !acia_hasrx(&acia)) {
            acia_write(&acia, tosend);
            tosend = 0;
        }
    }

    if (tty) {
        printf("Done!\n");
        tcsetattr(0, TCSADRAIN, &saved_term);
        emul_printdebug();
    }
    return 0;
}
