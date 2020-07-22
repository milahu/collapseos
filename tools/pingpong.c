#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#include "common.h"

/* Sends the contents of `fname` to `device`, expecting every sent byte to be
 * echoed back verbatim. Compare every echoed byte with the one sent and bail
 * out if a mismatch is detected. When the whole file is sent, push a null char
 * to indicate EOF to the receiving end.
 *
 * It is recommended that you send contents that has gone through `ttysafe`.
 *
 * If "delayus" is specified, this will be the delay we wait between the moment
 * we write the "ping" and the moment where we fetch the "pong".
 */
int main(int argc, char **argv)
{
    int delayus = 1000;
    if (argc == 4) {
        delayus = atoi(argv[3]);
    } else if (argc != 3) {
        fprintf(stderr, "Usage: ./pingpong device fname [delayus] \n");
        return 1;
    }
    FILE *fp = fopen(argv[2], "r");
    if (!fp) {
        fprintf(stderr, "Can't open %s.\n", argv[2]);
        return 1;
    }
    int fd = ttyopen(argv[1]);
    unsigned char c;
    int returncode = 0;
    while (fread(&c, 1, 1, fp)) {
        putchar('.');
        fflush(stdout);
        write(fd, &c, 1);
        usleep(delayus); // let it breathe
        if (!mexpect(fd, c)) {
            // mismatch!
            unsigned int pos = ftell(fp);
            fprintf(stderr, "Mismatch at byte %d!\n", pos);
            returncode = 1;
            break;
        }
    }
    // To close the receiving loop on the other side, we send a straight null
    c = 0;
    write(fd, &c, 1);
    printf("Done!\n");
    fclose(fp);
    if (fd > 0) {
        close(fd);
    }
    return returncode;
}


