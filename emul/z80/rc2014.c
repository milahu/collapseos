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
#include "emul.h"
#include "acia.h"
#include "sio.h"
#include "sdc.h"

#define RAMSTART 0x8000
#define ACIA_CTL_PORT 0x80
#define ACIA_DATA_PORT 0x81
#define SIO_ACTL_PORT 0x80
#define SIO_ADATA_PORT 0x81
#define SDC_CTL 0x05
#define SDC_SPI 0x04
#define MAX_ROMSIZE 0x2000

bool use_sio = false;
static ACIA acia;
static SIO sio;
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

static uint8_t iord_sio_ctl()
{
    return sio_actl_rd(&sio);
}

static uint8_t iord_sio_data()
{
    return sio_adata_rd(&sio);
}

static void iowr_sio_ctl(uint8_t val)
{
    sio_actl_wr(&sio, val);
}

static void iowr_sio_data(uint8_t val)
{
    sio_adata_wr(&sio, val);
}

static uint8_t iord_sdc_spi()
{
    return sdc_spi_rd(&sdc);
}

static void iowr_sdc_spi(uint8_t val)
{
    sdc_spi_wr(&sdc, val);
}

// in emulation, exchanges are always instantaneous, so we
// always report as ready.
static uint8_t iord_sdc_ctl()
{
    return 0;
}

static void iowr_sdc_ctl(uint8_t val)
{
    sdc_ctl_wr(&sdc, val);
}

static bool has_irq()
{
    return use_sio ? sio_has_irq(&sio) : acia_has_irq(&acia);
}

static bool hastx()
{
    return use_sio ? sio_hastx(&sio) : acia_hastx(&acia);
}

static bool hasrx()
{
    return use_sio ? sio_hasrx(&sio) : acia_hasrx(&acia);
}

static uint8_t _read()
{
    return use_sio ? sio_read(&sio) : acia_read(&acia);
}

static void _write(uint8_t val)
{
    if (use_sio) { sio_write(&sio, val); } else { acia_write(&acia, val); }
}

static void usage()
{
    fprintf(stderr, "Usage: ./classic [-s] [-c sdcard.img] /path/to/rom\n");
}

int main(int argc, char *argv[])
{
    FILE *fp = NULL;
    int ch;

    if (argc < 2) {
        usage();
        return 1;
    }
    acia_init(&acia);
    sio_init(&sio);
    sdc_init(&sdc);

    while ((ch = getopt(argc, argv, "sc:")) != -1) {
        switch (ch) {
            case 's':
                use_sio = true;
                break;
            case 'c':
                fprintf(stderr, "Setting up SD card image with %s\n", optarg);
                sdc.fp = fopen(optarg, "r+");
                break;
        }
    }
    if (optind != argc-1) {
        usage();
        return 1;
    }
    fp = fopen(argv[optind], "r");
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

    if (use_sio) {
        m->iord[SIO_ACTL_PORT] = iord_sio_ctl;
        m->iord[SIO_ADATA_PORT] = iord_sio_data;
        m->iowr[SIO_ACTL_PORT] = iowr_sio_ctl;
        m->iowr[SIO_ADATA_PORT] = iowr_sio_data;
    } else {
        m->iord[ACIA_CTL_PORT] = iord_acia_ctl;
        m->iord[ACIA_DATA_PORT] = iord_acia_data;
        m->iowr[ACIA_CTL_PORT] = iowr_acia_ctl;
        m->iowr[ACIA_DATA_PORT] = iowr_acia_data;
    }
    m->iord[SDC_SPI] = iord_sdc_spi;
    m->iowr[SDC_SPI] = iowr_sdc_spi;
    m->iord[SDC_CTL] = iord_sdc_ctl;
    m->iowr[SDC_CTL] = iowr_sdc_ctl;

    char tosend = 0;
    while (emul_step()) {
        // Do we have an interrupt?
        if (has_irq()) {
            Z80INT(&m->cpu, 0);
        }
        // Is the RC2014 transmitting?
        if (hastx()) {
            putchar(_read());
            fflush(stdout);
        }
        // Do we have something to send?
        if (!tosend) {
            char c;
            if (read(fileno(stdin), &c, 1) == 1) {
                if (c == 5) {
                    emul_memdump();
                    c = 0; // don't send to RC2014
                }
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
        if (tosend && !hasrx()) {
            _write(tosend);
            tosend = 0;
        }
    }

    if (tty) {
        printf("Done!\n");
        tcsetattr(0, TCSADRAIN, &saved_term);
        emul_printdebug();
    }
    if (sdc.fp) {
        fclose(sdc.fp);
    }
    return 0;
}
