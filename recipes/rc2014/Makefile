TARGET = stage1.bin
BASEDIR = ../..
FDIR = $(BASEDIR)/forth
EDIR = $(BASEDIR)/emul/forth
STAGE2 = $(EDIR)/stage2
EMUL = $(BASEDIR)/emul/hw/rc2014/classic
BOOTSRCS = conf.fs \
	$(EDIR)/xcomp.fs \
	$(FDIR)/boot.z80 \
	$(BASEDIR)/drv/acia.z80 \
	$(BASEDIR)/drv/sdc.z80 \
	$(FDIR)/icore.fs \
	$(EDIR)/xstop.fs

PATHS = \
	$(FDIR)/core.fs \
	$(FDIR)/cmp.fs \
	$(FDIR)/str.fs \
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

z80c.bin: conf.fs
	cat $(BOOTSRCS) | $(STAGE2) > $@

$(SLATEST):
	$(MAKE) -C $(BASEDIR)/tools

$(EMUL):
	$(MAKE) -C ${@:%/classic=%}

.PHONY: emul
emul: $(EMUL) $(TARGET)
	$(EMUL) $(TARGET)
	
