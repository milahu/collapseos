TARGET = stage1.bin
BASEDIR = ../..
FDIR = $(BASEDIR)/forth
EDIR = $(BASEDIR)/emul
STAGE2 = $(EDIR)/stage2
EMUL = $(BASEDIR)/emul/hw/rc2014/classic

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE2)
	cat xcomp.fs | $(STAGE2) > $@

$(SLATEST):
	$(MAKE) -C $(BASEDIR)/tools

$(EMUL):
	$(MAKE) -C ${@:%/classic=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
