EXTOBJS = ../../emul.o ../../libz80/libz80.o
OBJS = acia.o sdc.o classic.o 
TARGET = classic

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJS) $(EXTOBJS)
	$(CC) $(OBJS) $(EXTOBJS) -o $@

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJS)
