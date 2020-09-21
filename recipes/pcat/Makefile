TARGET = disk.bin
BASE = ../..
CDIR = $(BASE)/cvm
BLKPACK = $(BASE)/tools/blkpack
STAGE = $(CDIR)/stage

.PHONY: all
all: $(TARGET) 
os.bin: xcomp.fs $(STAGE) blkfs
	$(STAGE) blkfs < xcomp.fs > $@

$(BLKPACK):
	$(MAKE) -C ../tools

blkfs: $(BLKPACK)
	$(BLKPACK) $(BASE)/blk blk > $@

$(STAGE):
	$(MAKE) -C $(CDIR) stage

mbr.bin: mbr.fs $(STAGE) blkfs
	cat mbr.fs | $(STAGE) blkfs > $@

$(TARGET): mbr.bin os.bin
	cat mbr.bin os.bin > $@
	dd if=blkfs of=$@ bs=512 seek=16

.PHONY: emul
emul: $(TARGET)
	qemu-system-i386 -drive file=$(TARGET),if=floppy,format=raw

.PHONY: clean
clean:
	rm *.bin
