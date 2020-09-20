TARGET = os.bin
BASE = ../../
STAGE = $(BASE)/cvm/stage
BLKPACK = $(BASE)/tools/blkpack
EMUL = $(BASE)/emul/hw/sms/sms

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

os.sms: $(TARGET) $(STAGE)
	dd if=$(TARGET) bs=32752 conv=sync | cat - sega.bin > $@

$(EMUL):
	$(MAKE) -C ${@:%/sms=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
