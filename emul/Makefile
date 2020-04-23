TARGETS = forth/forth forth/stage2
# Those Forth source files are in a particular order
FORTHSRCS = core.fs cmp.fs print.fs parse.fs readln.fs fmt.fs blk.fs
FORTHSRC_PATHS = ${FORTHSRCS:%=../forth/%} forth/run.fs
OBJS = emul.o libz80/libz80.o
SLATEST = ../tools/slatest
STRIPFC = ../tools/stripfc
BIN2C = ../tools/bin2c
BLKPACK = ../tools/blkpack
BLKUNPACK = ../tools/blkunpack

.PHONY: all
all: $(TARGETS)

$(BLKPACK):
	$(MAKE) -C ../tools

.PHONY: $(STRIPFC) $(SLATEST) $(BIN2C)
$(STRIPFC): $(BLKPACK)
$(SLATEST): $(BLKPACK)
$(BIN2C): $(BLKPACK)
$(BLKUNPACK): $(BLKPACK)

# z80c.bin is not in the prerequisites because it's a bootstrap
# binary that should be updated manually through make updatebootstrap.
forth/forth0.bin: $(SLATEST)
	cp forth/z80c.bin $@
	$(SLATEST) $@
	cat forth/pre.fs forth/emul.fs >> $@

forth/forth0-bin.h: forth/forth0.bin $(BIN2C)
	$(BIN2C) KERNEL < forth/forth0.bin | tee $@ > /dev/null

forth/stage1: forth/stage.c $(OBJS) forth/forth0-bin.h 
	$(CC) forth/stage.c $(OBJS) -o $@

forth/stage1dbg: forth/stage.c $(OBJS) forth/forth0-bin.h 
	$(CC) -DDEBUG forth/stage.c $(OBJS) -o $@

# We don't really need to use stripfc, but we do it anyway to test that we
# don't mistakenly break our code with that tool. It's easier to debug here.
forth/core.bin: $(FORTHSRC_PATHS) forth/stage1
	cat $(FORTHSRC_PATHS) ./forth/stop.fs | $(STRIPFC) | ./forth/stage1 > $@

forth/forth1.bin: forth/core.bin $(SLATEST)
	cat forth/z80c.bin forth/core.bin > $@
	$(SLATEST) $@

forth/forth1-bin.h: forth/forth1.bin $(BIN2C)
	$(BIN2C) KERNEL < forth/forth1.bin > $@

forth/stage2: forth/stage.c $(OBJS) forth/forth1-bin.h forth/blkfs-bin.h
	$(CC) -DSTAGE2 forth/stage.c $(OBJS) -o $@

blkfs: $(BLKPACK)
	$(BLKPACK) ../blk > $@

forth/blkfs-bin.h: blkfs $(BIN2C)
	$(BIN2C) BLKFS < blkfs > $@

forth/forth: forth/forth.c $(OBJS) forth/forth1-bin.h blkfs
	$(CC) forth/forth.c $(OBJS) -o $@

libz80/libz80.o: libz80/z80.c
	$(MAKE) -C libz80/codegen opcodes
	$(CC) -Wall -ansi -g -c -o libz80/libz80.o libz80/z80.c

emul.o: emul.c
	$(CC) -c -o emul.o emul.c


.PHONY: updatebootstrap
updatebootstrap: forth/stage2
	cat ./forth/xcomp.fs | ./forth/stage2 > ./forth/z80c.bin

.PHONY: pack
pack:
	rm blkfs && $(MAKE) blkfs

.PHONY: unpack
unpack:
	$(BLKUNPACK) ../blk < blkfs

.PHONY: clean
clean:
	rm -f $(TARGETS) emul.o forth/*-bin.h forth/forth?.bin blkfs
	$(MAKE) -C ../tools clean
