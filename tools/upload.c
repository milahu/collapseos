#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

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
    int fd = open(argv[1], O_RDWR|O_NOCTTY|O_SYNC);
    if (fd < 0) {
        fprintf(stderr, "Could not open %s\n", argv[1]);
        return 1;
    }
    set_interface_attribs(fd, 0, 0);
    set_blocking(fd, 1);
    char s[0x40];
    sprintf(s,
        ": _ 0x%04x 0x%04x DO KEY DUP .x I A! LOOP ; _",
        memptr+bytecount, memptr);
    sendcmd(fd, s);

    int returncode = 0;
    while (fread(s, 1, 1, fp)) {
        putchar('.');
        fflush(stdout);
        unsigned char c = s[0];
        write(fd, &c, 1);
        usleep(1000); // let it breathe
        mread(fd, s, 2); // read hex pair
        s[2] = 0; // null terminate
        unsigned char c2 = strtol(s, NULL, 16);
        if (c != c2) {
            // mismatch!
            unsigned int pos = ftell(fp);
            fprintf(stderr, "Mismatch at byte %d! %d != %d.\n", pos, c, c2);
            // we don't exit now because we need to "consume" our whole program.
            returncode = 1;
        }
        usleep(1000); // let it breathe
    }
    readprompt(fd);
    sendcmdp(fd, "FORGET _");
    printf("Done!\n");
    fclose(fp);
    return returncode;
}

