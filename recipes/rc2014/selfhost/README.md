# Assembling Collapse OS from within it

This is where we tie lose ends, complete the circle, loop the loop: we assemble
a new Collapse OS *entirely* from within Collapse OS and write it to EEPROM,
either for another RC2014 or for an OS upgrade.

## Gathering parts

* stage3 from `sdcard` recipe. If you want to write to EEPROM as the final step,
  you'll need a hybrid stage3 that also includes stuff from the `eeprom` recipe.

## Building the binary

Build Collapse OS' from within Collapse OS is very similar to how we do
it from the makefile. If you take the time to look at the base recipe
`Makefile`, you'll see `cat xcomp.fs | $(STAGE)`. That's the thing.  Open
`xcomp.fs` in a text editor and take a look at it. You'll see that it loads
B599, which contains the meat, and then spits stuff to port 2, which is a
special signal for the `stage` binary.

To assemble from RC2014, all you need to do is load B599. This will
yield a binary in memory. To know the start/end offset of the binary, you'll
type the same two commands and in `xcomp.fs`, but replace the `/MOD 2 PC! 2 PC!`
words with `.X`. Then, write that binary between those offsets on your target
media. That binary should be the exact same as what you get in `os.bin`
when you run `make`.

Go ahead, run that. However, one thing you should know is that because the SD
card driver is a bit slow, some of these commands take a long time. Multiple
minutes. Be patient.

Is that it? Yes. But for your own enlightenment, open B618 and look at it, I'll
give you an overview of its contents.  I'm not going to explain in detail what
each command do, however.  You are encouraged to read the in-system
documentation for more information.

The first part is configuration of your new system. When RAM starts, where RSP
and PSP start, what ports to use for what device, etc. These configuration
declarations are expected in the boot code and driver code.

Then, we load the Z80 assembler and the cross compiler (xcomp for short), which
we'll of course need for the task ahead.

Then come xcomp overrides, which are needed for xcomp to be effective.

At this point, we're about to begin spitting binary content, this will be our
starting offset. `ORG` will soon be set to your current `H@`.

Then, we assemble the boot binary, drivers' native words, then inner core,
close the binary with a hook word. We're finished with cross-compiling.

We're at the offset that will be `CURRENT` on boot, so we update `LATEST`.

Then, we spit the init source code that will be interpreted on boot.
And... that's it!

### What to do on SDerr?

If you get `SDerr` in the middle of a LOAD operation, something went wrong with
the SD card. The bad news is that it left your xcomp operation in an
inconsistent state. The easiest thing to do it to restart the operation from
scratch. Those error are not frequent unless hardware is faulty.

### Verifying

You can use `/tools/memdump` to dump the memory between your begin/end offsets
so that you can compare against your reference stage 1. Before you do, you have
to take yourself out of xcomp mode. First, run `XCOFF` to go back to your
regular dict. Then, run `FORGET CODE` to undo the xcomp overrides you've added
before. That will rewind `HERE`. You don't want that. Put `HERE` back to after
your ending offset so that you don't overwrite your binary.

Then, you can run `/tools/memdump`.
