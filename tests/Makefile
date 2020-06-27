CDIR = ../cvm
EDIR = ../emul

.PHONY: run
run:
	$(MAKE) -C $(CDIR) all
	$(MAKE) -C $(EDIR) all
	cd cvm && ./runtests.sh
	cd z80 && ./runtests.sh
