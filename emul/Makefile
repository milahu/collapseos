TARGETS = runbin/runbin forth/forth
BIN2C = ../tools/bin2c
# Those Forth source files are in a particular order
BOOTSRCS = ./forth/conf.fs \
	../forth/xcomp.fs \
	./forth/xcomp.fs \
	../forth/boot.fs \
	../forth/z80c.fs \
	../forth/icore.fs \
	./forth/xstop.fs

FORTHSRCS = core.fs cmp.fs print.fs str.fs parse.fs readln.fs fmt.fs z80a.fs \
	link.fs
FORTHSRC_PATHS = ${FORTHSRCS:%=../forth/%} forth/run.fs
OBJS = emul.o libz80/libz80.o
SLATEST = ../tools/slatest
STRIPFC = ../tools/stripfc

.PHONY: all
all: $(TARGETS)

$(STRIPFC):
$(SLATEST):
$(BIN2C):
	$(MAKE) -C ../tools

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

forth/stage2: forth/stage.c $(OBJS) forth/forth1-bin.h 
	$(CC) -DSTAGE2 forth/stage.c $(OBJS) -o $@

forth/forth: forth/forth.c $(OBJS) forth/forth1-bin.h 
	$(CC) forth/forth.c $(OBJS) -o $@

runbin/runbin: runbin/runbin.c $(OBJS)
	$(CC) runbin/runbin.c $(OBJS) -o $@

libz80/libz80.o: libz80/z80.c
	$(MAKE) -C libz80/codegen opcodes
	$(CC) -Wall -ansi -g -c -o libz80/libz80.o libz80/z80.c

emul.o: emul.c
	$(CC) -c -o emul.o emul.c


.PHONY: updatebootstrap
updatebootstrap: forth/stage2
	cat $(BOOTSRCS) | ./forth/stage2 > ./forth/z80c.bin

.PHONY: clean
clean:
	rm -f $(TARGETS) emul.o forth/*-bin.h forth/forth?.bin
	$(MAKE) -C ../tools clean
