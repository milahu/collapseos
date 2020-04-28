TARGET = stage1.bin
EDIR = ../../emul
STAGE2 = $(EDIR)/stage2

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE2)
	cat xcomp.fs | $(STAGE2) > $@
