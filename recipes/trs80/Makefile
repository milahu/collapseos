TARGET = os.bin
EDIR = ../../emul
STAGE = $(EDIR)/stage

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(STAGE):
	$(MAKE) -C $(EDIR) stage
