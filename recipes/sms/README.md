# Sega Master System

The Sega Master System was a popular gaming console running on z80. It has a
simple, solid design and, most interestingly of all, its even more popular
successor, the Megadrive (Genesis) had a z80 system for compatibility!

This makes this platform *very* scavenge-friendly and worth working on.

[SMS Power][smspower] is an awesome technical resource to develop for this
platform and this is where most of my information comes from.

This platform is tight on RAM. It has 8k of it. However, if you have extra RAM,
you can put it on your cartridge.

## Related recipes

This recipe is for installing a minimal Collapse OS system on the SMS. There
are other recipes related to the SMS:

* [Interfacing a PS/2 keyboard](kbd.md)

## Gathering parts

* A Sega Master System or a MegaDrive (Genesis).
* A Megadrive D-pad controller.
* A way to get an arbitrary ROM to run on the SMS. Either through a writable
  ROM cartridge or an [Everdrive][everdrive].

## Hacking up a ROM cart

SMS Power has instructions to transform a ROM cartrige into a battery-backed
SRAM one, which allows you to write to it through another device you'll have
to build. This is all well and good, but if you happen to have an AT28 EEPROM,
things are much simpler!

Because AT28 EEPROM are SRAM compatible, they are an almost-drop-in replacement
to the ROM you'll pop off your cartridge. AT28 are a bit expensive, but they're
so handy! For SMS-related stuff, I recommend the 32K version instead of the 8K
one because fitting Collapse OS with fonts in 8K is really tight.


1. De-solder the ROM
2. Take a 28 pins IC socket
3. Cut off its WE pin (the one just under VCC), leaving a tiny bit of metal.
4. Hard-wire it to VCC so that WE is never enabled.
5. Solder your socket where the ROM was.
6. Insert Collapse OS-filled EEPROM in socket.

As simple as this! (Note that this has only been tested on a SMS so far. I
haven't explored whether this can run on a megadrive).

## Build the ROM

Running `make os.sms` will produce a `os.sms` ROM that can be put as is on a SD
card to the everdrive or flashed as is on a writable ROM cart. Then, just run
the thing!

To run Collapse OS in a SMS emulator, run `make emul`.

## Usage

On boot, you will get a regular Collapse OS BASIC shell. See the rest of the
documentation for shell usage instructions.

The particularity here is that, unlike with the RC2014, we don't access Collapse
OS through a serial link. Our input is a D-Pad and our output is a TV. The
screen is 32x28 characters. A bit tight, but usable.

D-Pad is used as follow:

* There's always an active cursor. On boot, it shows "a".
* Up/Down increase/decrease the value of the cursor.
* Left/Right does the same, by increments of 5.
* A button is backspace.
* B button skips cursor to next "class" (number, lowcase, upcase, symbols).
* C button "enters" cursor character and advance the cursor by one.
* Start button is like pressing Return.

Of course, that's not a fun way to enter text, but using the D-Pad is the
easiest way to get started which doesn't require soldering. Your next step after
that would be to [build a PS/2 keyboard adapter!](kbd/README.md)

## Slow initialization in emulation

When running under the emulator, video initialization is slow, it takes several
seconds. It's the emulator's fault. On real hardware, it's not as slow.

[smspower]: http://www.smspower.org
[everdrive]: https://krikzz.com
