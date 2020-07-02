#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#include "common.h"

/* Execute code from stdin on the target machine.
 */

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Usage: ./exec device\n");
        return 1;
    }
    int fd = ttyopen(argv[1]);
    if (fd < 0) {
        fprintf(stderr, "Could not open %s\n", argv[1]);
        return 1;
    }
    set_blocking(fd, 0);
    int c = getchar();
    while (c != EOF) {
        if (c == '\n') c = '\r';
        write(fd, &c, 1);
        while (read(fd, &c, 1) == 1) {
            putchar(c);
            fflush(stdout);
        }
        c = getchar();
    }
    printf("Done!\n");
    return 0;
}


