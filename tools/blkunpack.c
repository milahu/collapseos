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
            FILE *fp = fopen(fullpath, "w");
            for (int i=0; i<16; i++) {
                char *line = &buf[i*64];
                // line length is *not* strlen()! it's the
                // position of the first non-null, starting
                // from the right. Then, we normalize nulls
                // to space.
                int j;
                for (j=63; j>=0; j--) {
                    if (line[j] != '\0') {
                        break;
                    }
                }
                int len = j+1;
                if (len) {
                    for (; j>=0; j--) {
                        if (line[j] == '\0') {
                            line[j] = ' ';
                        }
                    }
                    fwrite(line, len, 1, fp);
                }
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
