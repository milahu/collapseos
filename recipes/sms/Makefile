TARGET = os.bin
STAGE = ../../emul/stage
EMUL = ../../emul/hw/sms/sms

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

os.sms: $(TARGET)
	dd if=$(TARGET) bs=32752 conv=sync | cat - sega.bin > $@

$(EMUL):
	$(MAKE) -C ${@:%/sms=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
