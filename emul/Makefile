TARGETS = forth stage2
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

stage0.bin: stage2 xcomp.fs
	cat xcomp.fs | ./stage2 > stage0.bin

stage0-bin.h: stage0.bin $(BIN2C)
	$(BIN2C) KERNEL < stage0.bin > $@

stage1: stage.c $(OBJS) stage0-bin.h 
	$(CC) stage.c $(OBJS) -o $@

stage1dbg: stage.c $(OBJS) stage0-bin.h 
	$(CC) -DDEBUG stage.c $(OBJS) -o $@

# not dependent on forth.bin to avoid circular deps.
forth-bin.h: $(BIN2C)
	$(BIN2C) KERNEL < forth.bin > $@

stage2: stage.c $(OBJS) forth-bin.h blkfs-bin.h
	$(CC) -DSTAGE2 stage.c $(OBJS) -o $@

blkfs: $(BLKPACK)
	$(BLKPACK) ../blk > $@

blkfs-bin.h: blkfs $(BIN2C)
	$(BIN2C) BLKFS < blkfs > $@

forth: forth.c $(OBJS) forth-bin.h blkfs-bin.h
	$(CC) forth.c $(OBJS) -o $@

libz80/libz80.o: libz80/z80.c
	$(MAKE) -C libz80/codegen opcodes
	$(CC) -Wall -ansi -g -c -o libz80/libz80.o libz80/z80.c

emul.o: emul.c
	$(CC) -c -o emul.o emul.c


.PHONY: updatebootstrap
updatebootstrap: stage1 stage1.fs
	./stage1 < stage1.fs > forth.bin

.PHONY: pack
pack:
	rm blkfs && $(MAKE) blkfs

.PHONY: unpack
unpack:
	$(BLKUNPACK) ../blk < blkfs

.PHONY: clean
clean:
	rm -f $(TARGETS) emul.o *-bin.h stage{1,2}.bin blkfs
	$(MAKE) -C ../tools clean
