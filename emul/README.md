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

## Usage

Run `./forth` to get the Collapse OS prompt. Type `0 LIST` for help.

The program is a curses interface with a limited, fixed size so that it can
provide a AT-XY interface (which is yet to implement).

You can get a REPL by launching the program with [`rlwrap(1)`][rlwrap] like
this:

    rlwrap -e '' -m -S '> ' ./forth /dev/stdin

## Problems?

If the `forth` executable works badly (hangs, spew garbage, etc.),
it's probably because you've broken your bootstrap binaries. They're easy to
mistakenly break. To verify if you've done that, look at your git status. If
`forth.bin` is modified, try resetting it and then run `make clean all`. Things
should go better afterwards.

A modified `blkfs` can also break things (although even with a completely broken
blkfs, you should still get to prompt), you might want to run `make pack` to
ensure that the `blkfs` file is in sync with the contents of the `blk/` folder.

If that doesn't work, there's also the nuclear option of `git reset --hard`
and `git clean -fxd`.

If that still doesn't work, it might be because the current commit you're on
is broken, but that is rather rare: the repo on Github is plugged on Travis
and it checks that everything is smooth.

[libz80]: https://github.com/ggambetta/libz80
[rlwrap]: https://linux.die.net/man/1/rlwrap
