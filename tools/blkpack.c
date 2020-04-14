#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>

void usage()
{
    fprintf(stderr, "Usage: blkpack dirname\n");
}

int main(int argc, char *argv[])
{
    DIR *dp;
    struct dirent *ep;
    char *buf = NULL;
    int blkcnt = 0;
    if (argc != 2) {
        usage();
        return 1;
    }
    dp = opendir(argv[1]);
    if (dp == NULL) {
        fprintf(stderr, "Couldn't open directory.\n");
        return 1;
    }
    while ((ep = readdir(dp))) {
        if ((strcmp(ep->d_name, ".") == 0) || strcmp(ep->d_name, "..") == 0) {
            continue;
        }
        if (ep->d_type != DT_REG) {
            continue;
        }
        int blkid = atoi(ep->d_name);
        if (blkid >= blkcnt) {
            int newcnt = blkid+1;
            buf = realloc(buf, newcnt*1024);
            bzero(buf+(blkcnt*1024), (newcnt-blkcnt)*1024);
            blkcnt = newcnt;
        }
        char fullpath[0x200];
        strcpy(fullpath, argv[1]);
        strcat(fullpath, "/");
        strcat(fullpath, ep->d_name);
        FILE *fp = fopen(fullpath, "r");
        char *line = NULL;
        size_t n = 0;
        for (int i=0; i<16; i++) {
            ssize_t cnt = getline(&line, &n, fp);
            if (cnt < 0) break;
            if (cnt > 65) {
                fprintf(stderr, "Line %d too long in blk %s\n", i+1, ep->d_name);
            }
            strncpy(buf+(blkid*1024)+(i*64), line, cnt-1);
        }
        free(line);
    }
    fwrite(buf, 1024, blkcnt, stdout);
    free(buf);
    closedir(dp);
    return 0;
}

