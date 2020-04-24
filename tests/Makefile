EMULDIR = ../emul

.PHONY: run
run:
	$(MAKE) -C $(EMULDIR) all
	cd forth && ./runtests.sh
