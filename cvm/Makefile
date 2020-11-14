TARGETS = forth stage
OBJS = vm.o
BLKPACK = ../tools/blkpack
BLKUNPACK = ../tools/blkunpack

.PHONY: all
all: $(TARGETS)

$(BLKPACK):
	$(MAKE) -C ../tools

.PHONY: $(BLKUNPACK)
$(BLKUNPACK): $(BLKPACK)

stage: stage.c $(OBJS) blkfs
	$(CC) -DFBIN_PATH=\"`pwd`/stage.bin\" -DBLKFS_PATH=\"`pwd`/blkfs\" stage.c $(OBJS) -o $@

blkfs: $(BLKPACK)
	$(BLKPACK) ../blk > $@

forth.bin: stage common.fs forth.fs blkfs
	cat common.fs forth.fs | ./stage > $@

forth: forth.c $(OBJS) forth.bin
	$(CC) -DFBIN_PATH=\"`pwd`/forth.bin\" -DBLKFS_PATH=\"`pwd`/blkfs\" forth.c $(OBJS) -lcurses -o $@

vm.o: vm.c blkfs
	$(CC) -c -o vm.o vm.c


.PHONY: updatebootstrap
updatebootstrap: stage common.fs stage.fs pack
	cat common.fs stage.fs | ./stage > new.bin
	mv new.bin stage.bin

.PHONY: pack
pack:
	rm blkfs && $(MAKE) blkfs

.PHONY: unpack
unpack:
	$(BLKUNPACK) ../blk < blkfs

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o forth.bin blkfs
