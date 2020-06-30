EXTOBJS = ../../emul.o ../../libz80/libz80.o
OBJS = ti84.o t6a04.o kbd.o 
TARGET = ti84
CFLAGS += `pkg-config --cflags xcb`
LDFLAGS += `pkg-config --libs xcb`

.PHONY: all
all: $(TARGET)

../../emul.o:
	make -C ../..

$(TARGET): $(OBJS) $(EXTOBJS)
	$(CC) $(OBJS) $(EXTOBJS) -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJS)
	
