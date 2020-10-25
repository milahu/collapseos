TARGETS = forth 
OBJS = cpu.o
CDIR = ../../cvm
STAGE = $(CDIR)/stage
BLKFS = $(CDIR)/blkfs

.PHONY: all
all: $(TARGETS)

forth: forth.c forth.bin $(OBJS)
	$(CC) -DFBIN_PATH=\"`pwd`/forth.bin\" forth.c $(OBJS) -lncurses -o $@

emul.o: emul.c $(BLKFS)
	$(CC) -DFBIN_PATH=\"`pwd`/forth.bin\" -DBLKFS_PATH=\"`pwd`/$(BLKFS)\" -c -o emul.o emul.c

forth.bin: foo.asm
	nasm -o $@ foo.asm
#forth.bin: xcomp.fs $(STAGE) $(BLKFS)
#	$(CDIR)/stage < xcomp.fs > $@

$(BLKFS): $(STAGE)

$(STAGE):
	$(MAKE) -C $(CDIR) all

.PHONY: clean
clean:
	rm -f $(TARGETS) $(OBJS) forth.bin

