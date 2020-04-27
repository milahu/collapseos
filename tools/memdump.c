#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#include "common.h"

/* Read specified number of bytes at specified memory address through a Forth
 * remote shell and dump it to stdout.
 */

int main(int argc, char **argv)
{
    if (argc != 4) {
        fprintf(stderr, "Usage: ./memdump device memptr bytecount\n");
        return 1;
    }
    unsigned int memptr = strtol(argv[2], NULL, 16);
    unsigned int bytecount = strtol(argv[3], NULL, 16);
    fprintf(stderr, "memptr: 0x%04x bytecount: 0x%04x.\n", memptr, bytecount);
    if (memptr+bytecount > 0xffff) {
        fprintf(stderr, "memptr+bytecount out of range.\n");
        return 1;
    }
    if (!bytecount) {
        // nothing to spit
        return 0;
    }

    int fd = open(argv[1], O_RDWR|O_NOCTTY);
    char s[0x30];
    sprintf(s, ": _ 0x%04x 0x%04x DO I @ .x LOOP ; _", memptr+bytecount, memptr);
    sendcmd(fd, s);

    for (int i=0; i<bytecount; i++) {
        read(fd, s, 2); // read hex pair
        s[2] = 0; // null terminate
        unsigned char c = strtol(s, NULL, 16);
        putchar(c);
    }
    read(fd, s, 2); // read prompt
    sendcmdp(fd, "FORGET _");
    return 0;
}
