# Sega Master System emulator

This emulates a Sega Master system with a monochrome screen and a Genesis pad
hooked to port A.

## Build

You need `xcb` and `pkg-config` to build this. If you have them, run `make`.
You'll get a `sms` executable.

## Usage

Launch the emulator with `./sms /path/to/rom` (you can use the binary from the
`sms` recipe.

This will show a window with the screen's content on it. The mappings to the
pad are:

* W --> Up
* A --> Left
* S --> Down
* D --> Right
* H --> A
* J --> B
* K --> C
* L --> Start

If your ROM is configured with PS/2 keyboard input, run this emulator with the
`-k` flag to replace SMS pad emulation with keyboard emulation.

In both cases (pad or keyboard), only port A emulation is supported.

Press ESC to quit.
