TARGET = os.bin
STAGE = ../../emul/stage
EMUL = ../../emul/hw/sms/sms

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE)
	cat xcomp.fs | $(STAGE) > $@

$(EMUL):
	$(MAKE) -C ${@:%/sms=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
