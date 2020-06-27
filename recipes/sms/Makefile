TARGET = os.bin
BASE = ../../
STAGE = $(BASE)/cvm/stage
EMUL = $(BASE)/emul/hw/sms/sms

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(STAGE):
	$(MAKE) -C $(BASE)/cvm stage

os.sms: $(TARGET) $(STAGE)
	dd if=$(TARGET) bs=32752 conv=sync | cat - sega.bin > $@

$(EMUL):
	$(MAKE) -C ${@:%/sms=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
