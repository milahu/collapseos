# See /doc/hw/z80/trs80.txt
TARGET = os.bin
BASE = ../../..
BLKPACK = $(BASE)/tools/blkpack
STAGE = $(BASE)/cvm/stage

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE) blkfs
	cat xcomp.fs | $(STAGE) blkfs > $@

$(BLKPACK):
	$(MAKE) -C ../tools

blkfs: $(BLKPACK)
	$(BLKPACK) $(BASE)/blk blk > $@

$(STAGE):
	$(MAKE) -C $(BASE)/cvm stage
