TARGET = stage1.bin
BASEDIR = ../..
FDIR = $(BASEDIR)/forth
EDIR = $(BASEDIR)/emul
STAGE = $(EDIR)/stage
EMUL = $(BASEDIR)/emul/hw/rc2014/classic

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(SLATEST):
	$(MAKE) -C $(BASEDIR)/tools

$(EMUL):
	$(MAKE) -C ${@:%/classic=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
