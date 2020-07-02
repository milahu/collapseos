#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#define BREATHE usleep(2000)
//#define DEBUG(...) fprintf(stderr, __VA_ARGS__)
#define DEBUG(...)

void mread(int fd, char *s, int count)
{
    while (count) {
        while (read(fd, s, 1) == 0) {
            BREATHE;
        }
        s++;
        count--;
    }
}

// Make sure that nothing is waiting in the pipeline
static void mempty(int fd)
{
    char c;
    while (read(fd, &c, 1) == 1) {
        DEBUG("Emptying %d\n", c);
        BREATHE;
    }
}

static void mexpect(int fd, char ec)
{
    char c;
    mread(fd, &c, 1);
    if (c != ec) {
        fprintf(stderr, "Expected %d but got %d\n", ec, c);
    }
}

void readprompt(int fd)
{
    mexpect(fd, ' ');
    mexpect(fd, 'o');
    mexpect(fd, 'k');
    mexpect(fd, '\r');
    mexpect(fd, '\n');
}

void sendcmd(int fd, char *cmd)
{
    DEBUG("Sending %s\n", cmd);
    char junk[2];
    while (*cmd) {
        DEBUG("W: %d\n", *cmd);
        write(fd, cmd, 1);
        BREATHE;
        read(fd, &junk, 1);
        DEBUG("R: %d\n", *junk);
        cmd++;
        // The other side is sometimes much slower than us and if we don't let
        // it breathe, it can choke.
        BREATHE;
    }
    write(fd, "\r", 1);
    mexpect(fd, '\r');
    mexpect(fd, '\n');
    BREATHE;
}

// Send a cmd and also read the " ok" prompt
void sendcmdp(int fd, char *cmd)
{
    sendcmd(fd, cmd);
    readprompt(fd);
}

// from https://stackoverflow.com/a/6947758
// discussion from https://stackoverflow.com/a/26006680 is interesting,
// but we don't want POSIX compliance.

int set_interface_attribs(int fd, int speed, int parity)
{
    struct termios tty;
    if (tcgetattr (fd, &tty) != 0) {
        fprintf(stderr, "error %d from tcgetattr", errno);
        return -1;
    }

    if (speed) {
        cfsetospeed (&tty, speed);
        cfsetispeed (&tty, speed);
    }

    tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;     // 8-bit chars
    // disable IGNBRK for mismatched speed tests; otherwise receive break
    // as \000 chars
    tty.c_iflag &= ~IGNBRK;         // disable break processing
    tty.c_iflag &= ~ICRNL;          // disable CR->NL mapping
    tty.c_lflag = 0;                // no signaling chars, no echo,
                                    // no canonical processing
    tty.c_oflag = 0;                // no remapping, no delays
    tty.c_cc[VMIN]  = 0;            // read doesn't block
    tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

    tty.c_iflag &= ~(IXON | IXOFF | IXANY); // shut off xon/xoff ctrl

    tty.c_cflag |= (CLOCAL | CREAD);// ignore modem controls,
                                    // enable reading
    tty.c_cflag &= ~(PARENB | PARODD);      // shut off parity
    tty.c_cflag |= parity;
    tty.c_cflag &= ~CSTOPB;
    tty.c_cflag &= ~CRTSCTS;

    if (tcsetattr (fd, TCSANOW, &tty) != 0) {
        fprintf(stderr, "error %d from tcsetattr", errno);
        return -1;
    }
    return 0;
}

void set_blocking(int fd, int should_block)
{
    struct termios tty;
    memset(&tty, 0, sizeof tty);
    if (tcgetattr (fd, &tty) != 0) {
        fprintf(stderr, "error %d from tggetattr", errno);
        return;
    }

    tty.c_cc[VMIN]  = should_block ? 1 : 0;
    tty.c_cc[VTIME] = 1;            // 0.1 seconds read timeout

    if (tcsetattr (fd, TCSANOW, &tty) != 0) {
        fprintf(stderr, "error %d setting term attributes", errno);
    }
}

int ttyopen(char *devname)
{
    int fd = 0;
    if (strcmp(devname, "-") != 0) {
        fd = open(devname, O_RDWR|O_NOCTTY|O_SYNC);
    }
    set_interface_attribs(fd, 0, 0);
    set_blocking(fd, 0);
    mempty(fd);
    set_blocking(fd, 1);
    return fd;
}
