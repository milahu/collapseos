TARGET = os.bin
CDIR = ../../cvm
STAGE = $(CDIR)/stage

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(STAGE):
	$(MAKE) -C $(CDIR) stage
