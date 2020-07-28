PROGNAME = at28wr
USBDEV = /dev/cuaU0
BAUDS = 115200
AVRDUDEARGS = -F -V -c arduino -P $(USBDEV) -b $(BAUDS)
AVRDUDEMCU = atmega328p
TARGET = $(PROGNAME).hex
AVRA = avra

# Rules

.PHONY: send all clean

all: $(TARGET)
	@echo Done!

send: $(TARGET)
	avrdude $(AVRDUDEARGS) -p $(AVRDUDEMCU) -U flash:w:$(TARGET)

$(TARGET): $(PROGNAME).asm
	$(AVRA) -o $@ $(PROGNAME).asm

clean:
	rm -f $(TARGET) *.S.eep.hex *.S.obj
