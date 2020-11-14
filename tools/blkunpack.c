#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

void usage()
{
    fprintf(stderr, "Usage: blkunpack < blkfs > blk.fs\n");
}

int main(int argc, char *argv[])
{
    char buf[1024];
    int blkid = 0;
    if (argc != 1) {
        usage();
        return 1;
    }
    while (fread(buf, 1024, 1, stdin) == 1) {
        int linecnt = 0 ;
        for (int i=1023; i>=0; i--) {
            if (buf[i]) {
                linecnt = (i / 64) + 1;
                break;
            }
        }
        if (linecnt) {
            // not an empty block
            printf("( ----- %03d )\n", blkid);
            for (int i=0; i<linecnt; i++) {
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
                    fwrite(line, len, 1, stdout);
                }
                fputc('\n', stdout);
            }
        }
        blkid++;
    }
    return 0;
}
