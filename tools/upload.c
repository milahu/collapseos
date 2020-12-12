#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>

#include "common.h"

/* Push specified file to specified device running Forth and verify
 * that the sent contents is correct.
 */

int main(int argc, char **argv)
{
    if (argc != 4) {
        fprintf(stderr, "Usage: ./upload device memptr fname\n");
        return 1;
    }
    unsigned int memptr = strtol(argv[2], NULL, 16);
    FILE *fp = fopen(argv[3], "r");
    if (!fp) {
        fprintf(stderr, "Can't open %s.\n", argv[3]);
        return 1;
    }
    fseek(fp, 0, SEEK_END);
    unsigned int bytecount = ftell(fp);
    fprintf(stderr, "memptr: 0x%04x bytecount: 0x%04x.\n", memptr, bytecount);
    if (!bytecount) {
        // Nothing to read
        fclose(fp);
        return 0;
    }
    if (memptr+bytecount > 0xffff) {
        fprintf(stderr, "memptr+bytecount out of range.\n");
        fclose(fp);
        return 1;
    }
    rewind(fp);
    int fd = ttyopen(argv[1]);
    if (fd < 0) {
        fprintf(stderr, "Could not open %s\n", argv[1]);
        return 1;
    }
    uint16_t checksum = 0;
    char s[0x40];
    // Write all received bytes, keeping a checksum, then print that checksum
    sprintf(s,
        ": _ 0 %d %d DO KEY DUP I C! + LOOP .X ; _",
        memptr+bytecount, memptr);
    sendcmd(fd, s);

    int returncode = 0;
    while (fread(s, 1, 1, fp)) {
        putc('.', stderr);
        fflush(stderr);
        unsigned char c = s[0];
        checksum += c;
        write(fd, &c, 1);
        usleep(1000); // let it breathe
    }
    fprintf(stderr, "\nBytes sent, checksum...\n");
    mread(fd, s, 4); // read hex string
    s[4] = 0; // null terminate
    uint16_t checksum2 = strtol(s, NULL, 16);
    if (checksum == checksum2) {
        fprintf(stderr, "OK\n");
    } else {
        fprintf(stderr, "Mismatch! Expected %04x and got %04x.\n", checksum, checksum2);
        returncode = 1;
    }
    readprompt(fd);
    sendcmdp(fd, "FORGET _");
    fclose(fp);
    if (fd > 0) {
        close(fd);
    }
    return returncode;
}

