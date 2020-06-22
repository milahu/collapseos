TARGET = os.bin
BASEDIR = ../..
EDIR = $(BASEDIR)/emul
STAGE = $(EDIR)/stage

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(STAGE):
	$(MAKE) -C $(EDIR) stage

