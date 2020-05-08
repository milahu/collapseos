TARGET = stage1.bin
BASEDIR = ../..
FDIR = $(BASEDIR)/forth
EDIR = $(BASEDIR)/emul
STAGE2 = $(EDIR)/stage2
EMUL = $(BASEDIR)/emul/hw/ti/ti84
MKTIUPGRADE = mktiupgrade

.PHONY: all
all: $(TARGET) 
$(TARGET): xcomp.fs $(STAGE2)
	cat xcomp.fs | $(STAGE2) > $@

$(EMUL):
	$(MAKE) -C ${@:%/ti84=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
os.rom: $(TARGET)
	cp $(TARGET) $@
	truncate -s 1M $@

os.8xu: os.rom
	$(MKTIUPGRADE) -p -k keys/0A.key -d TI-84+ os.rom $@ 00

.PHONY: send
send: os.8xu
	tilp -n --calc ti84+ --cable DirectLink os.8xu
