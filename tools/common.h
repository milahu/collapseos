void sendcmd(int fd, char *cmd);
void sendcmdp(int fd, char *cmd);
void mread(int fd, char *s, int count);
void readprompt(int fd);
int set_interface_attribs(int fd, int speed, int parity);
void set_blocking(int fd, int should_block);
int ttyopen(char *devname);
