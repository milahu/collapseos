# RC2014

The [RC2014][rc2014] is a nice and minimal z80 system that has the advantage
of being available in an assembly kit. Assembling it yourself involves quite a
bit of soldering due to the bus system. However, one very nice upside of that
bus system is that each component is isolated and simple.

The machine used in this recipe is the "Classic" RC2014 with an 8k ROM module
, 32k of RAM, a 7.3728Mhz clock and a serial I/O.

The ROM module being supplied in the assembly kit is an EPROM, not EEPROM, so
you can't install Collapse OS on it. You'll have to supply your own.

There are many options around to boot arbitrary sources. What was used in this
recipe was a AT28C64B EEPROM module. I chose it because it's compatible with
the 8k ROM module which is very convenient. If you do the same, however, don't
forget to set the A14 jumper to high because what is the A14 pin on the AT27
ROM module is the WE pin on the AT28! Setting the jumper high will keep is
disabled.

## Related recipes

This recipe is for installing a minimal Collapse OS system on the RC2014. There
are other recipes related to the RC2014:

* [Writing to a AT28 from Collapse OS](eeprom.md)
* [Accessing a MicroSD card](sdcard.md)
* [Self-hosting](selfhost.md)
* [Interfacing a PS/2 keyboard](ps2.md)
* [Using Zilog's SIO as a console](sio.md)
* [Making an ATmega328P blink](avr.md)

## Recipe

The goal is to have the shell running and accessible through the Serial I/O.

You'll need specialized tools to write data to the AT28 EEPROM. There seems to
be many devices around made to write in flash and EEPROM modules, but being in
a "understand everything" mindset, I [built my own][romwrite]. This is the
device I use in this recipe.

### Gathering parts

* A "classic" RC2014 with Serial I/O
* [Forth's stage binary][stage]
* [romwrite][romwrite] and its specified dependencies
* [GNU screen][screen]
* A FTDI-to-TTL cable to connect to the Serial I/O module

### Build the binary

Building the binary is as simple as running `make`. This will yield `os.bin`
which can then be written to EEPROM.

This build is controlled by the `xcomp.fs` unit, which loads `blk/618`. That's
what you need to modify if you want to customize your build (if you do, you'll
need to rebuild `/emul/stage` because the blkfs is embedded in it).

### Emulate

The Collapse OS project includes a RC2014 emulator suitable for this image.
You can invoke it with `make emul`. See `emul/hw/rc2014/README.md` for details.

### Write to the ROM

Plug your romwrite atmega328 to your computer and identify the tty bound to it.
In my case (arduino uno), it's `/dev/ttyACM0`. Then:

    screen /dev/ttyACM0 9600
    CTRL-A + ":quit"
    cat rom.bin | pv -L 10 > /dev/ttyACM0

See romwrite's README for details about these commands.

Note that this method is slow and clunky, but before long, you won't be using
it anymore. Writing to an EEPROM is much easier and faster from a RC2014
running Collapse OS, so once you have that first Collapse OS ROM, you'll be
much better equipped for further toying around (unless, of course, you already
had tools to write to EEPROM. In which case, you'll be ignoring this section
altogether).

### Running

Put the AT28 in the ROM module, don't forget to set the A14 jumper high, then
power the thing up. Connect the FTDI-to-TTL cable to the Serial I/O module and
identify the tty bound to it (in my case, `/dev/ttyUSB0`). Then:

    screen /dev/ttyUSB0 115200

Press the reset button on the RC2014 and the "ok" prompt should appear.

[rc2014]: https://rc2014.co.uk
[romwrite]: https://github.com/hsoft/romwrite
[stage]: ../../emul
[screen]: https://www.gnu.org/software/screen/
