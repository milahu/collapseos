#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#include "common.h"

/* Execute code from target file on the target machine.
   fname can be "-" for stdin.
 */

int main(int argc, char **argv)
{
    if (argc != 3) {
        fprintf(stderr, "Usage: ./exec device fname\n");
        return 1;
    }
    FILE *fp = stdin;
    if (strcmp(argv[2], "-") != 0) {
        fp = fopen(argv[2], "r");
        if (fp == NULL) {
            fprintf(stderr, "Could not open %s\n", argv[2]);
            return 1;
        }
    }
    int fd = ttyopen(argv[1]);
    if (fd < 0) {
        fprintf(stderr, "Could not open %s\n", argv[1]);
        return 1;
    }
    set_blocking(fd, 0);
    int c = getc(fp);
    while (c != EOF) {
        if (c == '\n') c = '\r';
        write(fd, &c, 1);
        usleep(1000); // let it breathe
        while (read(fd, &c, 1) == 1) {
            putchar(c);
            fflush(stdout);
        }
        c = getc(fp);
    }
    if (fd > 0) {
        close(fd);
    }
    if (strcmp(argv[2], "-") != 0) {
        fclose(fp);
    }
    printf("Done!\n");
    return 0;
}


