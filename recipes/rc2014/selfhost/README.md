# Assembling Collapse OS from within it

This is where we tie lose ends, complete the circle, loop the loop: we assemble
a new Collapse OS *entirely* from within Collapse OS and write it to EEPROM,
either for another RC2014 or for an OS upgrade.

## Gathering parts

* stage4 from `sdcard` recipe. If you want to write to EEPROM as the final step,
  you'll need a hybrid stage4 that also includes stuff from the `eeprom` recipe.

## Building stage 1

### Part 1

Building the first part of stage 1 (the binary part, before the inlined-source
part) from within Collapse OS is actually very similar from building it from a
modern environment. If you take the time to look at the base recipe `Makefile`,
you'll see `cat xcomp.fs | $(STAGE2)`. That command builds part 1. Open
`xcomp.fs` in a text editor and take a look at it.

To assemble stage 1 from RC2014, all you need to do is to type those commands
in the same order, and replace the `H@ 256 /MOD 2 PC! 2 PC!` lines with `H@ .X`.
Those commands will inform you of the begin/end offsets of the assembled binary.

The meaning of these commands is not explained here. You are encouraged to read
the in-system documentation for more information.

However, one thing you should know is that because the SD card driver is a bit
slow, some of these commands take a long time. Multiple minutes. Be patient.

Once all your commands are run and that you have your begin/end offset (write
them down somewhere), you're ready to assemble part 2.

### What to do on SDerr?

If you get `SDerr` in the middle of a LOAD operation, something went wrong with
the SD card. The bad news is that it left your xcomp operation in an
inconsistent state. If your at the beginning of it, it's easier to restart it
entirely.

If you're towards the end, you might want to repair it. To do so, you'll have to
bring your `XCURRENT` and `HERE` values to where they were before the LOAD
operation. You could have thought ahead and printed them before the LOAD, but if
you didn't, you'll just have to dig in your memory with `DUMP`.

You're looking at the offset of the last wordref of the *previous* LOAD
operation. That offset is going in `XCURRENT`. Then, you're looking at the end
of that word. That offset goes in `HERE`. Once you've done that, relaunch your
LOAD.

### Part 2

TODO
