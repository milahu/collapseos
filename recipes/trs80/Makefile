TARGET = stage1.bin
EDIR = ../../emul
STAGE = $(EDIR)/stage

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@
