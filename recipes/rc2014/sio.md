# Using Zilog's SIO as a console

The RC2014 has an optional module called the Dual Serial Module SIO/2 which is
built around Zilog's SIO chip. This module is nice because when paired with the
Dual Clock Module and when using port B, it's possible to run a UART with a baud
rate lower than 115200.

Collapse OS has a driver for it (although for now, only port A is supported by
it). Let's use it.

## Gathering parts

* A "classic" RC2014
* A Dual Serial Module SIO/2

## Build the binary

You'll have to edit the base recipe's xcomp unit like we do in the sdcard
recipe.

* Locate RC2014 recipe in blkfs
* Locate SIO driver
* The driver main page gives you references for declarations and for code.
* In the base xcomp unit, replace ACIA declataions with SIO's
* Replace ACIA code with SIO's
* At the bottom, replace "ACIA$" with "SIO$".

You can build with `make`.

## Setup

After you've placed the binary on your RC2014's EEPROM, simply replace your
regular Serial Module with the Dual Serial Module and plug yourself into port A.

You should have a functional console.
