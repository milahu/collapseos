# Writing to a AT28 from Collapse OS

## Goal

Write in an AT28 EEPROM from within Collapse OS so that you can have it update
itself.

## Gathering parts

* A RC2014 Classic
* `stage2.bin` from the base recipe
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

### Assembling stage 3

Stage 2 gives you a full interpreter, but it's missing the "Addressed devices"
module and the AT28 driver. We'll need to assemble a stage 3.

When you'll have a system with function disk block system, you'll be able to
directly `LOAD` them, but for this recipe, we can't assume you have, so what
you'll have to do is to manually paste the code from the appropriate blocks.

Addressed devices are at B140. To know what you have to paste, open the loader
block (B142) and see what blocks it loads. For each of the blocks, copy/paste
the code in your interpreter.

Do the same thing with the AT28 driver (B150)

If you're doing the real thing and not using the emulator, pasting so much code
at once might freeze up the RC2014, so it is recommended that you use
`/tools/exec` that let the other side enough time to breathe.

After your pasting, you'll have a compiled dict of that code in memory. You'll
need to relocate it in the same way you did for stage 2, but instead of using
`RLCORE`, which is a convenience word hardcoded for stage 1, we'll parametrize
`RLDICT`, the word doing the real work.

`RLDICT` takes 2 arguments, `target` and `offset`. `target` is the first word
of your relocated dict. In our case, it's going to be `' ADEVMEM+`. `offset` is
the offset we'll apply to every eligible word references in our dict. In our
case, that offset is the offset of the *beginning* of the `ADEVMEM+` entry (that
is, `' ADEVMEM+ WORD(` minus the offset of the last word (which should be a hook
word) in the ROM binary.

That offset can be conveniently fetched from code because it is the value of
the `LATEST` constant in stable ABI, which is at offset `0x08`. Therefore, our
offset value is:

    ' ADEVMEM+ WORD( 0x08 @ -

You can now run `RLDICT` and proceed with concatenation (and manual adjustments
of course) as you did with stage 2. Don't forget to adjust `run.fs` so that it
runs `ADEV$`.

## Writing contents to the AT28

The driver provides `AT28!` which can be plugged in adev's `A!*`.

First, upload your binary to some place in memory, for example `a000`. To do so,
run this from your modern computer:

    ./upload <tty device> a000 <filename>

Then, activate `AT28!` with `' AT28! A!* !` and then run
`0xa000 0x2000 <size-of-bin> AMOVE`. `AT28!` checks every myte for integrity,
so it there's no error, you should be fine. Your content is now on the EEPROM!

Why not upload content directly to `0x2000` after having activated `AT28!`?
Technically, you could. It was my first idea too. However, at the time of this
writing, I always get weird mismatch errors about halfway through. Maybe that
the ACIA interrupt does something wrong...
