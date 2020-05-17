( Receives keystrokes from PS/2 keyboard and send them to the
'164. On the PS/2 side, it works the same way as the controller
in the rc2014/ps2 recipe.  However, in this case, what we have
on the other side isn't a z80 bus, it's the one of the two
controller ports of the SMS through a DB9 connector.

The PS/2 related code is copied from rc2014/ps2 without much
change. The only differences are that it pushes its data to a
'164 instead of a '595 and that it synchronizes with the SMS
with a SR latch, so we don't need PCINT. We can also afford to
run at 1MHz instead of 8.

*** Register Usage ***

GPIOR0 flags:
0 - when set, indicates that the DATA pin was high when we
    received a bit through INT0. When we receive a bit, we set
    flag T to indicate it.

R16: tmp stuff
R17: recv buffer. Whenever we receive a bit, we push it in
     there.
R18: recv step:
     - 0: idle
     - 1: receiving data
     - 2: awaiting parity bit
     - 3: awaiting stop bit
R19: Register used for parity computations and tmp value in
     some other places
R20: data being sent to the '164
Y: pointer to the memory location where the next scan code from
   ps/2 will be written.
Z: pointer to the next scan code to push to the 595 )

0x015f CONSTANT RAMEND
0x11 CONSTANT GPIOR0
0x16 CONSTANT PINB
1 CONSTANT DATA

H@ ORG !
L1 FLBL, ( main )
L2 FLBL, ( hdlINT0 )

( Read DATA and set GPIOR0/0 if high. Then, set flag T.
  no SREG fiddling because no SREG-modifying instruction )
RJMPOP L2 FLBL! ( hdlINT0 )
PINB DATA SBIC,
GPIOR0 0 SBI,
SET,
RETI,

RJMPOP L1 FLBL! ( main )
16 RAMEND 0xff AND LDI,
