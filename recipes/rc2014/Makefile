TARGET = stage1.bin
BASEDIR = ../..
FDIR = $(BASEDIR)/forth
EDIR = $(BASEDIR)/emul/forth
STAGE2 = $(EDIR)/stage2
EMUL = $(BASEDIR)/emul/hw/rc2014/classic

PATHS = \
	$(FDIR)/core.fs \
	$(FDIR)/cmp.fs \
	$(FDIR)/parse.fs \
	$(BASEDIR)/drv/acia.fs \
	$(FDIR)/print.fs \
	$(FDIR)/fmt.fs \
	$(FDIR)/link.fs \
	run.fs
SLATEST = $(BASEDIR)/tools/slatest
STRIPFC = $(BASEDIR)/tools/stripfc

.PHONY: all
all: $(TARGET) 
$(TARGET): z80c.bin $(SLATEST) $(PATHS)
	cp z80c.bin $@
	$(SLATEST) $@
	cat $(PATHS) | $(STRIPFC) >> $@

z80c.bin: xcomp.fs
	cat xcomp.fs | $(STAGE2) > $@

$(SLATEST):
	$(MAKE) -C $(BASEDIR)/tools

$(EMUL):
	$(MAKE) -C ${@:%/classic=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
