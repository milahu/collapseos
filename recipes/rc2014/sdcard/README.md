# Accessing a MicroSD card

SD cards are great because they are accessible directly. No supporting IC is
necessary. The easiest way to access them is through the SPI protocol.

Due to the way IO works in z80, implementing SPI through it as a bit awkward:
You can't really keep pins high and low on an IO line. You need some kind of
intermediary between z80 IOs and SPI.

There are many ways to achieve this. This recipe explains how to build your own
hacked off SPI relay for the RC2014. It can then be used with `sdc.fs` to
drive a SD card.

## Goal

Read and write to a SD card from Collapse OS using a SPI relay of our own
design.

## Gathering parts

* A RC2014 Classic
* `stage3.bin` from the base recipe
* A MicroSD breakout board. I use Adafruit's.
* A proto board + header pins with 39 positions so we can make a RC2014 card.
* Diodes, resistors and stuff
* 40106 (Inverter gates)
* 4011 (NAND gates)
* 74xx139 (Decoder)
* 74xx161 (Binary counter)
* 74xx165 (Parallel input shift register)
* 74xx595 (Shift register)

## Building the SPI relay

The [schematic][schematic] supplied with this recipe works well with `sdc.fs`.
Of course, it's not the only possible design that works, but I think it's one
of the most straighforwards.

The basic idea with this relay is to have one shift register used as input,
loaded in parallel mode from the z80 bus and a shift register that takes the
serial input from `MISO` and has its output wired to the z80 bus.

These two shift registers are clocked by a binary counter that clocks exactly
8 times whenever a write operation on port `4` occurs. Those 8 clocks send
data we've just received in the `74xx165` into `MOSI` and get `MISO` into the
`74xx595`.

The `74xx139` then takes care of activating the right ICs on the right
combinations of `IORQ/WR/RD/Axx`.

The rest of the ICs is fluff around this all.

My first idea was to implement the relay with an AVR microcontroller to
minimize the number of ICs, but it's too slow. We have to be able to respond
within 300ns! Following that, it became necessary to add a 595 and a 165, but
if we're going to add that, why not go the extra mile and get rid of the
microcontroller?

To that end, I was heavily inspired by [this design][inspiration].

This board uses port `4` for SPI data, port `5` to pull `CS` low and port `6`
to pull it high. Port `7` is unused but monopolized by the card.

Little advice: If you make your own design, double check propagation delays!
Some NAND gates, such as the 4093, are too slow to properly respond within
a 300ns limit. For example, in my own prototype, I use a 4093 because that's
what I have in inventory. For the `CS` flip-flop, the propagation delay doesn't
matter. However, it *does* matter for the `SELECT` line, so I don't follow my
own schematic with regards to the `M1` and `A2` lines and use two inverters
instead.

## Building your stage 4

Using the same technique as you used for building your stage 3, you can append
required words to your boot binary. Required units are `forth/blk.fs` and
`drv/sdc.fs`. You also need `drv/sdc.z80` but to save you the troubles of
rebuilding from stage 1 for this recipe, we took the liberty of already having
included it in the base recipe.

## Testing in the emulator

The RC2014 emulator includes SDC emulation. You can attach a SD card image to
it by invoking it with a second argument:

    ../../../emul/hw/rc2014/classic stage4.bin ../../../emul/blkfs

You will then run with a SD card having the contents from `/blk`.

## Usage

First, the SD card needs to be initialized

    SDC$

If there is no error message, we're fine. Then, we need to hook `BLK@*` and
`BLK!*` into the SDC driver:

    ' SDC@ BLK@* !
    ' SDC! BLK!* !

And thats it! You have full access to disk block mechanism:

    102 LOAD
    BROWSE

(at this moment, the driver is a bit slow though...)

## How do I fill my SD card with Collapse OS' FS?

Very easy. You see that `/emul/blkfs` file? You dump it to your raw device.
For example, if the device you get when you insert your SD card is `/dev/sdb`,
then you type `cat emul/blkfs | sudo tee /dev/sdb > /dev/null`.

[schematic]: spirelay/spirelay.pdf
[inspiration]: https://www.ecstaticlyrics.com/electronics/SPI/fast_z80_interface.html
