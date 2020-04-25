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

* [Writing to a AT28 from Collapse OS](eeprom/README.md)
* [Accessing a MicroSD card](sdcard/README.md)
* [Self-hosting](selfhost/README.md)
* [Interfacing a PS/2 keyboard](ps2/README.md)

## Recipe

The goal is to have the shell running and accessible through the Serial I/O.

You'll need specialized tools to write data to the AT28 EEPROM. There seems to
be many devices around made to write in flash and EEPROM modules, but being in
a "understand everything" mindset, I [built my own][romwrite]. This is the
device I use in this recipe.

### Gathering parts

* A "classic" RC2014 with Serial I/O
* [Forth's stage 2 binary][stage2]
* [romwrite][romwrite] and its specified dependencies
* [GNU screen][screen]
* A FTDI-to-TTL cable to connect to the Serial I/O module

### Configure your build

Modules used in this build are configured through the `conf.fs` file in this
folder. There isn't much to configure, but it's there.

### Build stage 1

Self-bootstrapping is in Forth's DNA, which is really nice, but it makes
cross-compiling a bit tricky. It's usually much easier to bootstrap a Forth
from itself than trying to compile it from a foreign host.

This makes us adopt a 2 stages strategy. A tiny core is built from a foreign
host, and then we run that tiny core on the target machine and let it bootstrap
itself, then write our full interpreter binary.

We could have this recipe automate that 2 stage build process all automatically,
but that would rob you of all your fun, right? Instead, we'll run that 2nd
stage on the RC2014 itself!

To build your stage 1, run `make` in this folder, this will yield `os.bin`.
This will contain that tiny core and, appended to it, the Forth source code it
needs to run to bootstrap itself. When it's finished bootstrapping, you will
get a prompt to an almost-full Forth interpreter (there's not enough space in
8K to fit both link.fs and readln.fs, so we ditch readln. Our prompt is raw. No
backspace no buffer. Hardcore mode.)

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

Press the reset button on the RC2014 to have Forth begin its bootstrap process.
Note that it has to build more than half of itself from source. It takes about
30 seconds to complete.

Once bootstrapping is done you should see the Collapse OS prompt. That's a full
Forth interpreter. You can have fun right now.

However, that long boot time is kinda annoying. Moreover, that bootstrap code
being in source form takes precious space from our 8K ROM. That brings us to
building stage 2.

### Building stage 2

You're about to learn a lot about this platform and its self-bootstrapping
nature, but its a bumpy ride. Grab something. Why not a beer?

Our stage 1 prompt is the result of Forth's inner core interpreting the source
code of the Full Forth, which was appended to the binary inner core in ROM.
This results in a compiled dictionary, in RAM, at address 0x8000+system RAM.

Unfortunately, this compiled dictionary isn't usable as-is. Offsets compiled in
there are compiled based on a 0x8000-or-so base offset. What we need is a
0xa00-or-so base offset, that is, something suitable to be appended to the boot
binary, in ROM, in binary form.

Fortunately, inside the compiled source is the contents of link.fs which will
allow us to relink our compiled dictionary so that in can be relocated in ROM,
next to our boot binary. I won't go into relinking details. Look at the source.
For now, let's just use it:

    RLCORE

That command will take the dict from `' H@` up to `CURRENT`, copy it in free
memory and then relocate it. It will print 3 addresses during its processing.

The first address is the top copied address. The process didn't touch memory
above this point. The second address is the wordref of the last copied entry.
The 3rd is the bottom address of the copied dict. When that last address is
printed, the processing is over (because we don't have a `>` prompt, we don't
have any other indicator that the process is over).

### Assembling the stage 2 binary

At that point, we have a fully relocated binary in memory. Depending on our
situations, the next steps differ.

* If we're on a RC2014 that has writing capabilities to permanent storage,
  we'll want to assemble that binary directly on the RC2014 and write it to
  permanent storage.
* If we're on a RC2014 that doesn't have those capabilities, we'll want to dump
  memory on our modern environment using `/tools/memdump` and then assemble that
  binary there.
* If we're in the emulator, we'll want to dump our memory using `CTRL+E` and
  then assemble our stage 2 binary from that dump.

In these instructions, we assume an emulated environment. I'll use actual
offsets of an actual assembling session, but these of course are only examples.
It is very likely that these will not be the same offsets for you.

So you've pressed `CTRL+E` and you have a `memdump` file. Open it with a hex
editor (I like `hexedit`) to have a look around and to decide what we'll extract
from that memdump. `RLCORE` already gave you important offsets (in my case,
`9a3c`, `99f6` and `8d60`), but although the beginning of will always be the
same (`8d60`), the end offset depends on the situation.

If you look at data between `99f6` and `9a3c`, you'll see that this data is not
100% dictionary entry material. Some of it is buffer data allocated at
initialization. To locate the end of a word, look for `0042`, the address for
`EXIT`. In my case, it's at `9a1a` and it's the end of the `INIT` word.

Moreover, the `INIT` routine that is in there is not quite what we want,
because it doesn't contain the `HERE` adjustment that we find in `pre.fs`.
We'll want to exclude it from our binary, so let's go a bit further, at `99cf`,
ending at `99de`.

So, the end of our compiled dict is actually `99de`. Alright, let's extract it:

    dd if=memdump bs=1 skip=36192 count=3198 > dict.bin

`36192` is `8d60` and `3198` is `99de-8d60`. This needs to be prepended by the
boot binary. But that one, we already have. It's `z80c.bin`

    cat z80c.bin dict.bin > stage2.bin

Is it ready to run yet? no. There are 3 adjustments we need to manually make
using our hex editor.

1. We need to link `H@` to the hook word of the boot binary. In my case, it's
   a matter of writing `02` at `08ec` and `00` at `08ed`, `H@`'s prev field.
2. We need to end our binary with a hook word. It can have a zero-length name
   and the prev field needs to properly point to the previous wordref. In my
   case, that was `RLCORE` at offset `1559` for a `stage2.bin` size of `1568`,
   which means that I appended `0F 00 00` at the end of the file.
3. Finally, we need to adjust `LATEST` which is at offset `08`. This needs to
   point to the last wordref of the file, which is equal to the length of
   `stage2.bin` because we've just added a hook word. This means that we write
   `6B` at offset `08` and `15` at offset `09`.

Now are we ready yet? ALMOST! There's one last thing we need to do: add runtime
source. In our case, because we have a compiled dict, the only source we need
to include is `run.fs`:

    cat stage2.bin run.fs > stage2r.bin

That's it! our binary is ready to run!

    ../../emul/hw/rc2014/classic stage2r.bin

And there you have it, a stage2 binary that you've assembled yourself.

### Assembling stage 3

Stage 2 gives you a useable prompt, but bare. Because 8K isn't a lot of space
to cram source code, we're limited in what we can include for this stage.

However, now that we have a usable prompt, we can do a lot (be cautious though:
there is no `readln` yet, so you have no backspace), for example, build a
stage 3 with `readln`.

Copy the unit's source

    cat ../../forth/readln.fs | ../../tools/stripfc | xclip

and just paste it in your terminal. If you're doing the real thing and not
using the emulator, pasting so much code at once might freeze up the RC2014, so
it is recommended that you use `/tools/exec` that let the other side enough
time to breathe.

After your pasting, you'll have a compiled dict of that code in memory. You'll
need to relocate it in the same way you did for stage 2, but instead of using
`RLCORE`, which is a convenience word hardcoded for stage 1, we'll parametrize
`RLDICT`, the word doing the real work.

`RLDICT` takes 2 arguments, `target` and `offset`. `target` is the first word
of your relocated dict. In our case, it's going to be `' INBUFSZ`. `offset` is
the offset we'll apply to every eligible word references in our dict. In our
case, that offset is the offset of the *beginning* of the `INBUFSZ` entry (that
is, `' INBUFSZ WORD(` minus the offset of the last word (which should be a hook
word) in the ROM binary.

That offset can be conveniently fetched from code because it is the value of
the `LATEST` constant in stable ABI, which is at offset `0x08`. Therefore, our
offset value is:

    ' INBUFSZ WORD( 0x08 @ -

You can now run `RLDICT` and proceed with concatenation (and manual adjustments
of course) as you did with stage 2. Don't forget to adjust `run.fs` so that it
initializes `RDLN$` instead of creating a minimal `(c<)`.

Keep that `stage3.bin` around, you will need it for further recipes.

[rc2014]: https://rc2014.co.uk
[romwrite]: https://github.com/hsoft/romwrite
[stage2]: ../../emul
[screen]: https://www.gnu.org/software/screen/
