TARGETS = forth stage blkfs
OBJS = emul.o libz80/libz80.o
BIN2C = ../tools/bin2c
BLKPACK = ../tools/blkpack
BLKUNPACK = ../tools/blkunpack

.PHONY: all
all: $(TARGETS)

$(BLKPACK):
	$(MAKE) -C ../tools

.PHONY: $(BIN2C) $(BLKUNPACK)
$(BIN2C): $(BLKPACK)
$(BLKUNPACK): $(BLKPACK)

# not dependent on forth.bin to avoid circular deps.
forth-bin.h: $(BIN2C)
	$(BIN2C) KERNEL < forth.bin > $@

stage: stage.c $(OBJS) forth-bin.h
	$(CC) stage.c -DBLKFS_PATH=\"`pwd`/blkfs\" $(OBJS) -o $@

blkfs: $(BLKPACK)
	$(BLKPACK) ../blk > $@

forth: forth.c $(OBJS) forth-bin.h
	$(CC) forth.c -DBLKFS_PATH=\"`pwd`/blkfs\" $(OBJS) -lncurses -o $@

libz80/libz80.o: libz80/z80.c
	$(MAKE) -C libz80/codegen opcodes
	$(CC) -Wall -ansi -g -c -o libz80/libz80.o libz80/z80.c

emul.o: emul.c
	$(CC) -c -o emul.o emul.c


.PHONY: updatebootstrap
updatebootstrap: stage xcomp.fs pack
	./stage < xcomp.fs > forth.bin

.PHONY: pack
pack:
	rm blkfs && $(MAKE) blkfs

.PHONY: unpack
unpack:
	$(BLKUNPACK) ../blk < blkfs

.PHONY: clean
clean:
	rm -f $(TARGETS) emul.o *-bin.h blkfs
