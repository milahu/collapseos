#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

static char *buf;
static int blkcnt;

static void usage()
{
    fprintf(stderr, "Usage: blkpack dirname [dirname ...]\n");
}

static int spit(char *dirname)
{
    DIR *dp;
    struct dirent *ep;

    dp = opendir(dirname);
    if (dp == NULL) {
        fprintf(stderr, "Couldn't open directory %s.\n", dirname);
        return 1;
    }
    while ((ep = readdir(dp))) {
        if ((strcmp(ep->d_name, ".") == 0) || strcmp(ep->d_name, "..") == 0) {
            continue;
        }
        int blkid = atoi(ep->d_name);
        if (blkid >= blkcnt) {
            int newcnt = blkid+1;
            buf = realloc(buf, newcnt*1024);
            memset(buf+(blkcnt*1024), 0, (newcnt-blkcnt)*1024);
            blkcnt = newcnt;
        }
        char *fullpath = malloc(strlen(dirname) + MAXNAMLEN + 2);
        strcpy(fullpath, dirname);
        strcat(fullpath, "/");
        strcat(fullpath, ep->d_name);
        FILE *fp = fopen(fullpath, "r");
        free(fullpath);
        if (fp == NULL) {
            fprintf(stderr, "Could not open %s: %s\n", ep->d_name, strerror(errno));
            continue;
        }
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
        ssize_t cnt = getline(&line, &n, fp);
        if (cnt > 0) {
            fprintf(stderr, "blk %s has more than 16 lines\n", ep->d_name);
        }
        free(line);
        fclose(fp);
    }
    closedir(dp);
    return 0;
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        usage();
        return 1;
    }
    buf = NULL;
    blkcnt = 0;
    for (int i=1; i<argc; i++) {
        if (spit(argv[i]) != 0) {
            return 1;
        }
    }
    fwrite(buf, 1024, blkcnt, stdout);
    free(buf);
    return 0;
}

