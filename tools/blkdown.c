#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#include "common.h"

/* Pull specified block no "srcblk" from "device" into block no "dstblk" in
 * local "blkfs". If "dstblk" is omitted, it is the same as "srcblk"
 */

int main(int argc, char **argv)
{
    if ((argc != 5) && (argc != 4)) {
        fprintf(stderr, "Usage: ./blkdown device srcblk blkfs [dstblk]\n");
        return 1;
    }
    unsigned int srcblk = strtol(argv[2], NULL, 10);
    unsigned int dstblk = srcblk;
    if (argc == 5) {
        dstblk = strtol(argv[4], NULL, 10);
    }
    FILE *fp = fopen(argv[3], "r+");
    if (!fp) {
        fprintf(stderr, "Can't open %s.\n", argv[3]);
        return 1;
    }
    int fd = ttyopen(argv[1]);
    if (fd < 0) {
        fprintf(stderr, "Could not open %s\n", argv[1]);
        return 1;
    }
    fseek(fp, 1024 * dstblk, SEEK_SET);
    char s[0x40];
    sprintf(s, ": _ %d BLK@ 1024 0 DO I BLK( + C@ .x LOOP ; _", srcblk);
    sendcmd(fd, s);

    int returncode = 0;
    for (int i=0; i<1024; i++) {
        usleep(1000); // let it breathe
        mread(fd, s, 2); // read hex pair
        s[2] = 0; // null terminate
        char *endptr = NULL;
        unsigned char c = strtol(s, &endptr, 16);
        if (endptr != &s[2]) {
            fprintf(stderr, "Non-hex value (%s) at index %d.\n", s, i);
            // we don't exit now because we need to "consume" our whole program.
            returncode = 1;
        }
        if (returncode == 0) {
            fwrite(&c, 1, 1, fp);
        }
    }
    readprompt(fd);
    sendcmdp(fd, "FORGET _");
    printf("Done!\n");
    fclose(fp);
    return returncode;
}
