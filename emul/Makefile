TARGETS = forth stage2
# Those Forth source files are in a particular order
FORTHSRCS = core.fs cmp.fs print.fs parse.fs readln.fs fmt.fs blk.fs
FORTHSRC_PATHS = ${FORTHSRCS:%=../forth/%} run.fs
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
forth0.bin:
	cp z80c.bin $@
	cat pre.fs emul.fs >> $@

forth0-bin.h: forth0.bin $(BIN2C)
	$(BIN2C) KERNEL < forth0.bin | tee $@ > /dev/null

stage1: stage.c $(OBJS) forth0-bin.h 
	$(CC) stage.c $(OBJS) -o $@

stage1dbg: stage.c $(OBJS) forth0-bin.h 
	$(CC) -DDEBUG stage.c $(OBJS) -o $@

# We don't really need to use stripfc, but we do it anyway to test that we
# don't mistakenly break our code with that tool. It's easier to debug here.
core.bin: $(FORTHSRC_PATHS) stage1
	cat $(FORTHSRC_PATHS) stop.fs | $(STRIPFC) | ./stage1 > $@

forth1.bin: core.bin $(SLATEST)
	cat z80c.bin core.bin > $@
	$(SLATEST) $@

forth1-bin.h: forth1.bin $(BIN2C)
	$(BIN2C) KERNEL < forth1.bin > $@

stage2: stage.c $(OBJS) forth1-bin.h blkfs-bin.h
	$(CC) -DSTAGE2 stage.c $(OBJS) -o $@

blkfs: $(BLKPACK)
	$(BLKPACK) ../blk > $@

blkfs-bin.h: blkfs $(BIN2C)
	$(BIN2C) BLKFS < blkfs > $@

forth: forth.c $(OBJS) forth1-bin.h blkfs-bin.h
	$(CC) forth.c $(OBJS) -o $@

libz80/libz80.o: libz80/z80.c
	$(MAKE) -C libz80/codegen opcodes
	$(CC) -Wall -ansi -g -c -o libz80/libz80.o libz80/z80.c

emul.o: emul.c
	$(CC) -c -o emul.o emul.c


.PHONY: updatebootstrap
updatebootstrap: stage2
	cat xcomp.fs | ./stage2 > z80c.bin

.PHONY: pack
pack:
	rm blkfs && $(MAKE) blkfs

.PHONY: unpack
unpack:
	$(BLKUNPACK) ../blk < blkfs

.PHONY: clean
clean:
	rm -f $(TARGETS) emul.o *-bin.h forth?.bin blkfs
	$(MAKE) -C ../tools clean
