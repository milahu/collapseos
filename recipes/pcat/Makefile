TARGET = disk.bin
BASEDIR = ../..
EDIR = $(BASEDIR)/emul
STAGE = $(EDIR)/stage

.PHONY: all
all: $(TARGET) 
mbr.bin: mbr.fs $(STAGE)
	cat mbr.fs | $(STAGE) > $@

os.bin: xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

disk.bin: mbr.bin os.bin
	cat mbr.bin os.bin > $@

$(STAGE):
	$(MAKE) -C $(EDIR) stage

.PHONY: emul
emul: $(TARGET)
	qemu-system-i386 -fda $(TARGET)

.PHONY: clean
clean:
	rm *.bin
