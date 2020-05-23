# emul

This folder contains a couple of tools running under the [libz80][libz80]
emulator.

## Requirements

You need `ncurses` to build the `forth` executable. In debian-based distros,
it's `libncurses5-dev`.

## Not real hardware

In the few emulated apps described below, we don't try to emulate real hardware
because the goal here is to facilitate "high level" development.

These apps run on imaginary hardware and use many cheats to simplify I/Os.

For real hardware emulation (which helps developing drivers), see the `hw`
folder.

## Build

First, make sure that the `libz80` git submodule is checked out. If not, run
`git submodule init && git submodule update`.

After that, you can run `make` and it builds the `forth` interpreter.

Run `./forth` to get the Collapse OS prompt. Type `0 LIST` for help.

## Problems?

If the libz80-wrapped zasm executable works badly (hangs, spew garbage, etc.),
it's probably because you've broken your bootstrap binaries. They're easy to
mistakenly break. To verify if you've done that, look at your git status. If
`forth.bin` is modified, try resetting it and then run `make clean all`. Things
should go better afterwards.

If that doesn't work, there's also the nuclear option of `git reset --hard`
and `git clean -fxd`.

If that still doesn't work, it might be because the current commit you're on
is broken, but that is rather rare: the repo on Github is plugged on Travis
and it checks that everything is smooth.

[libz80]: https://github.com/ggambetta/libz80
