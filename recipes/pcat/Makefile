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
	dd if=$(EDIR)/blkfs of=$@ bs=512 seek=16

$(STAGE):
	$(MAKE) -C $(EDIR) stage

.PHONY: emul
emul: $(TARGET)
	qemu-system-i386 -drive file=$(TARGET),if=floppy,format=raw

.PHONY: clean
clean:
	rm *.bin
