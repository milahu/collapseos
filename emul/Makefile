TARGETS = forth 
OBJS = emul.o libz80/libz80.o
CDIR = ../cvm
STAGE = $(CDIR)/stage
BLKFS = $(CDIR)/blkfs

.PHONY: all
all: $(TARGETS)

forth: forth.c $(OBJS) $(BLKFS)
	$(CC) forth.c $(OBJS) -lncurses -o $@

libz80/libz80.o: libz80/z80.c
	$(MAKE) -C libz80/codegen opcodes
	$(CC) -Wall -ansi -g -c -o libz80/libz80.o libz80/z80.c

emul.o: emul.c forth.bin $(BLKFS)
	$(CC) -DFBIN_PATH=\"`pwd`/forth.bin\" -DBLKFS_PATH=\"`pwd`/$(BLKFS)\" -c -o emul.o emul.c

forth.bin: xcomp.fs $(STAGE) $(BLKFS)
	$(CDIR)/stage < xcomp.fs > $@

$(BLKFS): $(STAGE)

$(STAGE):
	$(MAKE) -C $(CDIR) all

.PHONY: clean
clean:
	rm -f $(TARGETS) emul.o *.bin libz80/libz80.o
