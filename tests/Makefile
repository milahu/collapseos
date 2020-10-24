.PHONY: run
run:
	$(MAKE) -C ../cvm all
	$(MAKE) -C ../emul/z80 all
	cd all && ./runtests.sh
	cd z80 && ./runtests.sh
