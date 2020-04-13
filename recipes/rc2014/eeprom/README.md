# Writing to a AT28 from Collapse OS

## Goal

Write in an AT28 EEPROM from within Collapse OS so that you can have it update
itself.

## Gathering parts

* A RC2014 Classic
* `stage3.bin` from the base recipe
* An extra AT28C64B
* 1x 40106 inverter gates
* Proto board, RC2014 header pins, wires, IC sockets, etc.

## Building the EEPROM holder

The AT28 is SRAM compatible so you could use a RAM module for it. However,
there is only one RAM module with the Classic version of the RC2014 and we
need it to run Collapse OS.

You could probably use the 64K RAM module for this purpose, but I don't have one
and I haven't tried it. For this recipe, I built my own module which is the same
as the regular ROM module but with `WR` wired and geared for address range
`0x2000-0x3fff`.

If you're tempted by the idea of hacking your existing RC2014 ROM module by
wiring `WR` and write directly to the range `0x0000-0x1fff` while running it,
be aware that it's not that easy. I was also tempted by this idea, tried it,
but on bootup, it seems that some random `WR` triggers happen and it corrupts
the EEPROM contents. Theoretically, we could go around that by putting the AT28
in write protection mode, but I preferred building my own module.

I don't think you need a schematic. It's really simple.

## Using the at28 driver

The AT28 driver is at `drv/at28.fs` and is a pure forth source file so it's
rather easy to set up from the base Stage 3 binary:

    cat ../stage3.bin ../pre.fs ../../../drv/at28.fs ../run.fs > os.bin
    ../../../emul/hw/rc2014/classic os.bin

## Writing contents to the AT28

The driver provides `AT28!` which can be plugged in adev's `A!*`.

It's not in the Stage 3 binary, but because it's a small piece of Forth code,
let's just run its definition code:

    cat ../../../drv/at28.fs | ./stripfc | ./exec <tty device>

Then, upload your binary to some place in memory, for example `a000`. To do so,
run this from your modern computer:

    ./upload <tty device> a000 <filename>

Then, activate `AT28!` with `' AT28! A!* !` and then run
`0xa000 0x2000 <size-of-bin> AMOVE`. `AT28!` checks every myte for integrity,
so it there's no error, you should be fine. Your content is now on the EEPROM!

Why not upload content directly to `0x2000` after having activated `AT28!`?
Technically, you could. It was my first idea too. However, at the time of this
writing, I always get weird mismatch errors about halfway through. Maybe that
the ACIA interrupt does something wrong...
