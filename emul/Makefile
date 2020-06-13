TARGETS = forth stage 
OBJS = emul.o libz80/libz80.o
BLKPACK = ../tools/blkpack
BLKUNPACK = ../tools/blkunpack

.PHONY: all
all: $(TARGETS)

$(BLKPACK):
	$(MAKE) -C ../tools

.PHONY: $(BLKUNPACK)
$(BLKUNPACK): $(BLKPACK)

stage: stage.c $(OBJS) blkfs
	$(CC) stage.c $(OBJS) -o $@

blkfs: $(BLKPACK)
	$(BLKPACK) ../blk > $@

forth: forth.c $(OBJS) blkfs
	$(CC) forth.c $(OBJS) -lncurses -o $@

libz80/libz80.o: libz80/z80.c
	$(MAKE) -C libz80/codegen opcodes
	$(CC) -Wall -ansi -g -c -o libz80/libz80.o libz80/z80.c

emul.o: emul.c
	$(CC) -DFBIN_PATH=\"`pwd`/forth.bin\" -DBLKFS_PATH=\"`pwd`/blkfs\" -c -o emul.o emul.c


.PHONY: updatebootstrap
updatebootstrap: stage xcomp.fs pack
	./stage < xcomp.fs > new.bin
	mv new.bin forth.bin

.PHONY: pack
pack:
	rm blkfs && $(MAKE) blkfs

.PHONY: unpack
unpack:
	$(BLKUNPACK) ../blk < blkfs

.PHONY: clean
clean:
	rm -f $(TARGETS) emul.o *-bin.h blkfs libz80/libz80.o
