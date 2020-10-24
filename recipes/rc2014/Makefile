TARGET = os.bin
BASE = ../..
CDIR = $(BASE)/cvm
EDIR = $(BASE)/emul
STAGE = $(CDIR)/stage
BLKPACK = $(BASE)/tools/blkpack
EMUL = $(EDIR)/rc2014/classic

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE) blkfs
	$(STAGE) blkfs < xcomp.fs > $@

$(BLKPACK):
	$(MAKE) -C ../tools

blkfs: $(BLKPACK)
	$(BLKPACK) $(BASE)/blk blk > $@

$(STAGE):
	$(MAKE) -C $(CDIR) stage

$(EMUL):
	$(MAKE) -C ${@:%/classic=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
