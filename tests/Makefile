.PHONY: run
run:
	$(MAKE) -C ../cvm all
	$(MAKE) -C ../emul all
	cd all && ./runtests.sh
	cd z80 && ./runtests.sh
