TARGET = os.bin
BASEDIR = ../..
CDIR = $(BASEDIR)/cvm
EDIR = $(BASEDIR)/emul
STAGE = $(CDIR)/stage
EMUL = $(EDIR)/hw/rc2014/classic

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(STAGE):
	$(MAKE) -C $(CDIR) stage

$(EMUL):
	$(MAKE) -C ${@:%/classic=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
