PROGNAME = ps2ctl
AVRDUDEMCU ?= t45
AVRDUDEARGS ?= -c usbtiny -P usb 
TARGETS = $(PROGNAME).bin
BASEDIR = ../../..
EDIR = $(BASEDIR)/emul

# Rules

.PHONY: send all clean

all: $(TARGETS)
	@echo Done!

send: $(PROGNAME).bin
	avrdude $(AVRDUDEARGS) -p $(AVRDUDEMCU) -U flash:w:$(PROGNAME).bin

$(PROGNAME).bin: $(PROGNAME).fs
	cd $(EDIR) && ./avra.sh < ../recipes/sms/kbd/$(PROGNAME).fs > ../recipes/sms/kbd/$@

clean:
	rm -f $(TARGETS)
