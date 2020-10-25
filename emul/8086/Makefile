TARGETS = forth pcat
OBJS = cpu.o
CDIR = ../../cvm
STAGE = $(CDIR)/stage
BLKFS = $(CDIR)/blkfs

.PHONY: all
all: $(TARGETS)

forth: forth.c forth.bin $(OBJS) $(BLKFS)
	$(CC) -DFBIN_PATH=\"`pwd`/forth.bin\" -DBLKFS_PATH=\"`pwd`/$(BLKFS)\" forth.c $(OBJS) -lncurses -o $@

forth.bin: xcomp.fs $(STAGE)
	$(CDIR)/stage < xcomp.fs > $@

pcat: pcat.c $(OBJS) $(BLKFS)
	$(CC) -DBLKFS_PATH=\"`pwd`/$(BLKFS)\" pcat.c $(OBJS) -lncurses -o $@

$(BLKFS): $(STAGE)

$(STAGE):
	$(MAKE) -C $(CDIR) all

.PHONY: clean
clean:
	rm -f $(TARGETS) $(OBJS) forth.bin

