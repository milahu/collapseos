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

0x0060 CONSTANT SRAM_START
0x015f CONSTANT RAMEND
0x3d CONSTANT SPL
0x3e CONSTANT SPH
0x11 CONSTANT GPIOR0
0x35 CONSTANT MCUCR
0x33 CONSTANT TCCR0B
0x3b CONSTANT GIMSK
0x38 CONSTANT TIFR
0x16 CONSTANT PINB
0x17 CONSTANT DDRB
0x18 CONSTANT PORTB
2 CONSTANT CLK
1 CONSTANT DATA
3 CONSTANT CP
0 CONSTANT LQ
4 CONSTANT LR
0x100-100 CONSTANT TIMER_INITVAL

H@ ORG !
L1 FLBL, ( main )
L2 FLBL, ( hdlINT0 )

( Read DATA and set GPIOR0/0 if high. Then, set flag T.
  no SREG fiddling because no SREG-modifying instruction )
L2 ' RJMP FLBL! ( hdlINT0 )
PINB DATA SBIC,
GPIOR0 0 SBI,
SET,
RETI,

L1 ' RJMP FLBL! ( main )
16 RAMEND 0xff AND LDI,
SPL 16 OUT,
16 RAMEND 8 RSHIFT LDI,
SPH 16 OUT,
( init variables )
18 CLR,
GPIOR0 18 OUT,
( Setup int0
  INT0, falling edge )
16 0x02 ( ISC01 ) LDI,
MCUCR 16 OUT,
( Enable INT0 )
16 0x40 ( INT0 ) LDI,
GIMSK 16 OUT,
( Setup buffer )
29 ( YH ) CLR,
28 ( YL ) SRAM_START 0xff AND LDI,
31 ( ZH ) CLR,
30 ( ZL ) SRAM_START 0xff AND LDI,
( Setup timer. We use the timer to clear up "processbit"
registers after 100us without a clock. This allows us to start
the next frame in a fresh state. at 1MHZ, no prescaling is
necessary. Each TCNT0 tick is already 1us long. )
16 0x01 ( CS00 ) LDI, ( no prescaler )
TCCR0B 16 OUT,
( init DDRB )
DDRB CP SBI,
PORTB LR CBI,
DDRB LR SBI,
SEI,

L1 LBL! ( loop )
L2 FLBL, ( BRTS processbit. flag T set? we have a bit to
           process )
28 ( YL ) 30 ( ZL ) CP, ( if YL == ZL, buf is empty )
L3 FLBL, ( BRNE sendTo164. YL != ZL? buf has data )
( nothing to do. Before looping, let's check if our
  communication timer overflowed. )
16 TIFR IN,
16 1 ( TOV0 ) SBRC,
L4 FLBL, ( RJMP processbitReset, timer0 overflow? reset )
( Nothing to do for real. )
L1 ' RJMP LBL, ( loop )

( Process the data bit received in INT0 handler. )
L2 ' BRTS FLBL! ( processbit )
19 GPIOR0 IN, ( backup GPIOR0 before we reset T )
19 0x1 ANDI, ( only keep the first flag )
GPIOR0 0 CBI,
CLT, ( ready to receive another bit )
