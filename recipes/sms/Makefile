TARGET = os.bin
BASE = ../..
STAGE = $(BASE)/cvm/stage
BLKPACK = $(BASE)/tools/blkpack
SMSROM = $(BASE)/tools/smsrom
EMUL = $(BASE)/emul/z80/sms

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE) blkfs
	cat xcomp.fs | $(STAGE) blkfs > $@

$(SMSROM):
$(BLKPACK):
	$(MAKE) -C ../tools

blkfs: $(BLKPACK)
	$(BLKPACK) $(BASE)/blk blk > $@

$(STAGE):
	$(MAKE) -C $(BASE)/cvm stage

os.sms: $(TARGET) $(STAGE) $(SMSROM)
	$(SMSROM) $(TARGET) > $@

$(EMUL):
	$(MAKE) -C ${@:%/sms=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
