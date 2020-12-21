#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

/* Unpacks blkfs into its source form.
 *
 * If numerical "upto" is specified, we stop unpacking when reaching this
 * blkno.
 */
void usage()
{
    fprintf(stderr, "Usage: blkunpack [upto] < blkfs > blk.fs\n");
}

int main(int argc, char *argv[])
{
    char buf[1024];
    int blkid = 0;
    int upto = 0;
    if (argc > 2) {
        usage();
        return 1;
    }
    if (argc == 2) {
        upto = strtol(argv[1], NULL, 10);
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
        if (blkid == upto) {
            break;
        }
    }
    return 0;
}
