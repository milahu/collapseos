0xbf00 CONSTANT RS_ADDR
0xbffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
0x8000 CONSTANT HERESTART
SYSVARS 0x70 + CONSTANT LCD_MEM
SYSVARS 0x72 + CONSTANT KBD_MEM
0x01 CONSTANT KBD_PORT
212 LOAD  ( z80 assembler )
: ZFILL, ( u ) 0 DO 0 A, LOOP ;
262 LOAD  ( xcomp )
522 LOAD  ( font compiler )
282 LOAD  ( boot.z80.decl )
270 LOAD  ( xcomp overrides )

( TI-84+ requires specific code at specific offsets which
  come in conflict with Collapse OS' stable ABI. We thus
  offset the binary by 0x100, which is our minimum possible
  increment and fill the TI stuff with the code below. )

0x5a JPnn, 0x15 ZFILL, ( 0x18 )
0x5a JPnn, ( reboot ) 0x1d ZFILL, ( 0x38 )
( handleInterrupt )
DI,
AF PUSHqq,
    ( did we push the ON button? )
    0x04 ( PORT_INT_TRIG ) INAn,
    0 ( INT_TRIG_ON ) A BITbr,
    IFNZ,
        ( yes? acknowledge and boot )
        0x03 ( PORT_INT_MASK ) INAn,
        0x00 ( INT_MASK_ON ) A RESbr, ( ack interrupt )
        0x03 ( PORT_INT_MASK ) OUTnA,
        AF POPqq,
        EI,
        0x100 JPnn,
    THEN,
AF POPqq,
EI,
RETI,

0x03 ZFILL, ( 0x53 )
0x5a JPnn, ( 0x56 ) 0xff A, 0xa5 A, 0xff A, ( 0x5a )
( boot )
DI,
    (im1)
    ( enable the ON key interrupt )
    0x03 ( PORT_INT_MASK ) INAn,
    0x00 ( INT_MASK_ON ) A SETbr,
    0x03 ( PORT_INT_MASK ) OUTnA,
    A 0x80 LDrn,
    0x07 ( PORT_BANKB ) OUTnA,
EI,
( LCD off )
A 0x02 ( LCD_CMD_DISABLE ) LDrn,
0x10 ( LCD_PORT_CMD ) OUTnA,
HALT,

0x95 ZFILL, ( 0x100 )
( All set, carry on! )

CURRENT @ XCURRENT !

0x100 BIN( !
283 335 LOADR ( boot.z80 )
353 LOAD  ( xcomp core low )
CREATE ~FNT CPFNT3x5
555 560 LOADR ( LCD low )
566 570 LOADR ( KBD low )
380 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," LCD$ KBD$ " EOT,
ORG @ 0x100 - 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
