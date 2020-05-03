#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

void mread(int fd, char *s, int count)
{
    while (count) {
        while (read(fd, s, 1) == 0) {
            usleep(1000);
        }
        s++;
        count--;
    }
}

void mexpect(int fd, char ec)
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
    char junk[2];
    while (*cmd) {
        write(fd, cmd, 1);
        read(fd, &junk, 1);
        cmd++;
        // The other side is sometimes much slower than us and if we don't let
        // it breathe, it can choke.
        usleep(1000);
    }
    write(fd, "\r", 1);
    mexpect(fd, '\r');
    mexpect(fd, '\n');
    usleep(1000);
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

