/* ./smsrom fname

Transforms binary at fname into an 8K, 16K or 32K Sega Master System ROM with
a header fit for an Export SMS. The resulting ROM is spit to stdout.

Whether the ROM is 8, 16 or 32K depends on the size of binary at fname.
*/
#include <stdio.h>
#include <inttypes.h>

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Usage: ./smsrom fname\n");
        return 1;
    }
    FILE *fp = fopen(argv[1], "r");
    if (!fp) {
        fprintf(stderr, "Can't open %s.\n", argv[1]);
        return 1;
    }
    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    uint8_t hdsz = 0x4a; // size flag in header. either 4a, 4b or 4c.
    int romsize = 0x2000;
    while (romsize-16 < fsize) {
        romsize *= 2;
        hdsz++;
        if (romsize > 0x8000) {
            fprintf(stderr, "binary too big\n");
            return 1;
        }
    }
    uint16_t chksum = 0;
    for (int i=0; i<romsize-16; i++) {
        int c = getc(fp);
        if (c == EOF) c = 0;
        putchar(c);
        chksum += c;
    }
    // and now, the header
    printf("TMR SEGA");
    putchar(0);
    putchar(0);
    putchar(chksum & 0xff);
    putchar(chksum >> 8);
    putchar(0);
    putchar(0);
    putchar(0);
    putchar(hdsz);
    return 0;
}
