# TI-84+ emulator

This emulates a TI-84+ with its screen and keyboard. This is suitable for
running the `ti84` recipe.

## Build

You need `xcb` and `pkg-config` to build this. If you have them, run `make`.
You'll get a `ti84` executable.

## Usage

Launch the emulator with `./ti84 /path/to/rom` (you can use the binary from the
`ti84` recipe. Use the small one, not the one having been filled to 1MB).

This will show a window with the LCD screen's content on it. Most applications,
upon boot, halt after initialization and stay halted until the ON key is
pressed. The ON key is mapped to the tilde (~) key.

Press ESC to quit.

As for the rest of the mappings, they map at the key level. For example, the 'Y'
key maps to '1' (which yields 'y' when in alpha mode). Therefore, '1' and 'Y'
map to the same calculator key. Backspace maps to DEL.

Left Shift maps to 2nd. Left Ctrl maps to Alpha.
