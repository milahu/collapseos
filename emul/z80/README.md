# emul

This folder contains a couple of tools running under the [libz80][libz80]
emulator.

## Requirements

You need `ncurses` to build the `forth` executable. In debian-based distros,
it's `libncurses5-dev`.

## Build

Run `make` and it builds the `forth` interpreter.

## Usage

The `./forth` executable here works like the one in `/cvm`, except that it runs
under an emulated z80 machine instead of running natively. Refer to
`/cvm/README.md` for details.

## Not real hardware

`./forth` doesn't try to emulate real hardware
because the goal here is to facilitate "high level" development.

These apps run on imaginary hardware and use many cheats to simplify I/Os.

For real hardware emulation (which helps developing drivers), see subfolders.

[libz80]: https://github.com/ggambetta/libz80
