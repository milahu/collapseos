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
	$(CC) -DBLKFS_PATH=\"`pwd`/blkfs\" stage.c $(OBJS) -o $@

blkfs: $(BLKPACK)
	$(BLKPACK) ../blk > $@

forth: forth.c $(OBJS)
	$(CC) -DBLKFS_PATH=\"`pwd`/blkfs\" forth.c $(OBJS) -lcurses -o $@

vm.o: vm.c blkfs
	$(CC) -DFBIN_PATH=\"`pwd`/forth.bin\" -c -o vm.o vm.c


.PHONY: updatebootstrap
updatebootstrap: stage xcomp.fs pack
	./stage < xcomp.fs > new.bin
	mv new.bin forth.bin

.PHONY: pack
pack:
	rm blkfs && $(MAKE) blkfs

.PHONY: unpack
unpack:
	$(BLKUNPACK) ../blk < blkfs

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o blkfs
