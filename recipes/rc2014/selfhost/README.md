# Assembling Collapse OS from within it

This is where we tie lose ends, complete the circle, loop the loop: we assemble
a new Collapse OS *entirely* from within Collapse OS and write it to EEPROM,
either for another RC2014 or for an OS upgrade.

## Gathering parts

* stage3 from `sdcard` recipe. If you want to write to EEPROM as the final step,
  you'll need a hybrid stage3 that also includes stuff from the `eeprom` recipe.

## Building stage 1

Build Collapse OS' stage 1 from within Collapse OS is very similar to how we do
it from the makefile. If you take the time to look at the base recipe
`Makefile`, you'll see `cat xcomp.fs | $(STAGE2)`. That's the thing.  Open
`xcomp.fs` in a text editor and take a look at it.

To assemble stage 1 from RC2014, all you need to do is to type those commands
in the same order, and replace the `/MOD 2 PC! 2 PC!` words with `.X`.
Those commands will inform you of the begin/end offsets of the assembled binary.

I'm not going to explain in detail what each command do, but only give you an
overview of what is happening.  You are encouraged to read the in-system
documentation for more information.

The first part is configuration of your new system. When RAM starts, where RSP
starts, what ports to use for what device, etc. These configuration declarations
are expected in the boot code and driver code.

Then, we load the Z80 assembler and the cross compiler (xcomp for short), which
we'll of course need for the task ahead.

Then come xcomp overrides, which are needed for xcomp to be effective.

At this point, we're about to begin spitting binary content, this will be our
starting offset. `ORG` will soon be set to your current `H@`.

Then, we assemble the boot binary, drivers' native words, then inner core,
close the binary with a hook word. We're finished with cross-compiling.

We're at the offset that will be `CURRENT` on boot, so we update `LATEST`.

Then, we spit the source code that will be interpreted by stage 1 on boot so
that it bootstraps itself to a full interpreter. Not all units are there
because they don't fit in 8K, but they're sufficient for our needs. We also
need the linker so that we can relink ourselves to stage 2.

Finally, we have initialization code, then a spit of the ending offset.

Go ahead, run that. However, one thing you should know is that because the SD
card driver is a bit slow, some of these commands take a long time. Multiple
minutes. Be patient.

Once all your commands are run and that you have your begin/end offset (write
them down somewhere), you're at the same point as you were after the `make`
part of the base recipe. The contents between your start and end offset is the
exact same as the contents of `stage1.bin` when you run `make`. Continue your
deployment from there.

Good luck!

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

### Verifying

You can use `/tools/memdump` to dump the memory between your begin/end offsets
so that you can compare against your reference stage 1. Before you do, you have
to take yourself out of xcomp mode. First, run `XCOFF` to go back to your
regular dict. Then, run `FORGET CODE` to undo the xcomp overrides you've added
before. That will rewind `HERE`. You don't want that. Put `HERE` back to after
your ending offset so that you don't overwrite your binary.

Then, you can run `/tools/memdump`.
