#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>

void usage()
{
    fprintf(stderr, "Usage: blkunpack dirname\n");
}

int main(int argc, char *argv[])
{
    char buf[1024];
    int blkid = 0;
    if (argc != 2) {
        usage();
        return 1;
    }
    while (fread(buf, 1024, 1, stdin) == 1) {
        char fullpath[0x200];
        sprintf(fullpath, "%s/%03d", argv[1], blkid);
        char c = 0;
        for (int i=0; i<1024; i++) {
            c |= buf[i];
        }
        if (c) {
            // not an empty block
            printf("%s\n", fullpath);
            FILE *fp = fopen(fullpath, "w");
            for (int i=0; i<16; i++) {
                int len = strlen(&buf[i*64]);
                fwrite(&buf[i*64], len, 1, fp);
                fputc('\n', fp);
            }
            fclose(fp);
        } else {
            // empty block, delete
            unlink(fullpath);
        }
        blkid++;
    }
    return 0;
}
