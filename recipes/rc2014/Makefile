TARGET = os.bin
BASEDIR = ../..
EDIR = $(BASEDIR)/emul
STAGE = $(BASEDIR)/cvm/stage
EMUL = $(EDIR)/hw/rc2014/classic

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(STAGE):
	$(MAKE) -C $(EDIR) stage

$(EMUL):
	$(MAKE) -C ${@:%/classic=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
