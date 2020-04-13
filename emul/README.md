# emul

This folder contains a couple of tools running under the [libz80][libz80]
emulator.

## Not real hardware

In the few emulated apps described below, we don't try to emulate real hardware
because the goal here is to facilitate userspace development.

These apps run on imaginary hardware and use many cheats to simplify I/Os.

For real hardware emulation (which helps developing drivers), see the `hw`
folder.

## Build

First, make sure that the `libz80` git submodule is checked out. If not, run
`git submodule init && git submodule update`.

After that, you can run `make` and it builds all applications.

## forth

Collapse OS' Forth interpreter, which is in the process of replacing the
zasm-based project.

The Forth interpreter is entirely self-hosting, that is, it assembles its
binary with itself.

There are 3 build stages.

**Stage 0**: At this stage, all we have are our bootstrap binaries, `boot.bin`
and `z80c.bin`. We concatenate them into `forth0.bin` ans then wrap the
emulator around it which is named `stage1` (because it builds the stage 1) to
have a barebone forth interpreter.

**Stage 1**: The `stage1` binary allows us to augment `forth0.bin` with
the compiled dictionary of a full Forth interpreter. We feed it with
`$(FORTHSRCS)` and then dump the resulting compiled dict. 

From there, we can create `forth1.bin`, which is wrapped by both the `forth`
and `stage2` executables. `forth` is the interpreter you'll use.

**Stage 2**: `stage2` is used to resolve the chicken-and-egg problem and use
the power of a full Forth intepreter, including an assembler, to assemble
`z80c.bin`. This is a manual step executed through `make updatebootstrap`.

Normally, running this step should yield the exact same `boot.bin` and
`z80c.bin` as before, unless of course you've changed the source.

## Problems?

If the libz80-wrapped zasm executable works badly (hangs, spew garbage, etc.),
it's probably because you've broken your bootstrap binaries. They're easy to
mistakenly break. To verify if you've done that, look at your git status. If
`boot.bin` or `z80c.bin` are modified, try resetting them and then run
`make clean all`. Things should go better afterwards.

If that doesn't work, there's also the nuclear option of `git reset --hard`
and `git clean -fxd`.

If that still doesn't work, it might be because the current commit you're on
is broken, but that is rather rare: the repo on Github is plugged on Travis
and it checks that everything is smooth.
