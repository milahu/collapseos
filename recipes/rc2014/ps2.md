# Interfacing a PS/2 keyboard

Serial connection through ACIA is nice, but you are probably plugging a modern
computer on the other side of that ACIA, right? Let's go a step further away
from those machines and drive a PS/2 keyboard directly!

## Goal

Have a PS/2 keyboard drive the stdio input of the Collapse OS shell instead of
the ACIA.

## Gathering parts

* A RC2014 Classic that could install the base recipe
* A PS/2 keyboard. A USB keyboard + PS/2 adapter should work, but I haven't
  tried it yet.
* A PS/2 female connector. Not so readily available, at least not on digikey. I
  de-soldered mine from an old motherboard I had laying around.
* ATtiny85/45/25 (main MCU for the device)
* 74xx595 (shift register)
* 40106 inverter gates
* Diodes for `A*`, `IORQ`, `RO`.
* Proto board, RC2014 header pins, wires, IC sockets, etc.
* [AVRA][avra]

## Building the PS/2 interface

Let's start with the PS/2 connector, which has two pins:

![PS/2 connector](ps2-conn.png)

Both are connected to the ATtiny45, `CLK` being on `PB2` to have `INT0` on it.

The `DATA` line is multi-use. That is, `PB1` is connected both to the PS/2 data
line and to the 595's `SER`. This saves us a precious pin.

![ATtiny45](ps2-t45.png)

The ATtiny 45 hooks everything together. `CE` comes from the z80 bus, see below.

![74xx595](ps2-595.png)

This allows us to supply the z80 bus with data within its 375ns limits. `SRCLR`
is hooked to the `CE` line so that whenever a byte is read, the 595 is zeroed
out as fast as possible so that the z80 doesn't read "false doubles".

The 595, to have its `SRCLR` becoming effective, needs a `RCLK` trigger, which
doesn't happen immediately. It's the ATtiny45, in its `PCINT` interrupt, that
takes care of doing that trigger (as fast as possible).

![z80](ps2-z80.png)

Our device is read only, on one port. That makes the "Chip Enable" (`CE`)
selection rather simple. In my design, I chose the IO port 8, so I inverted
`A3`. I chose a 40106 inverter to do that, do as you please for your own design.

I wanted to hook `CE` to a flip flop so that the MCU could relax a bit more
w.r.t. reacting to its `PB4` pin changes, but I didn't have NAND gates that are
fast enough in stock, so I went with this design. But otherwise, I would
probably have gone the flip-flop way. Seems more solid.

## Using the PS/2 interface

To use this interface, you have to build a new Collapse OS binary. We'll use
the xcomp unit from the base recipe and modify it.

First, we need a `(ps2kc)` routine. In this case, it's easy, it's
`: (ps2kc) 8 PC@ ;`. Add this after ACIA loading. Then, we can load PS/2
subsystem. You add `411 414 LOADR`. Then, at initialization, you add `PS2$`
after `ACIA$`. You also need to define `PS2_MEM` at the top. You can probably
use `SYSVARS + 0x7a`.

Rebuild, reflash, should work. For debugging purposes, you might not want to
go straight to plugging PS/2 `(key)` into the system. What I did myself was
to load the PS/2 subsystem *before* ACIA (which overrides with its own `(key)`)
and added a dummy word in between to access PS/2's key.

Also (and this is a TODO: investigate), I had a problem where the break key I
got from `(ps2kc)` was 0x70 instead of 0xf0 which had the effect of duplicating
all my keystrokes. I added a 0x70 -> 0xf0 replacement in my version of
`(ps2kc)`. Does the trick (at the cost of a non-functional numpad 0).

[avra]: https://github.com/hsoft/avra
