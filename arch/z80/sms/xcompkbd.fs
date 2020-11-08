( xcomp for a SMS with PS/2 keyboard on port A )
( 8K of onboard RAM )
0xdd00 CONSTANT RS_ADDR
( Memory register at the end of RAM. Must not overwrite )
0xddca CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
0xc000 CONSTANT HERESTART
SYSVARS 0x70 + CONSTANT VDP_MEM
0xbf   CONSTANT VDP_CTLPORT
0xbe   CONSTANT VDP_DATAPORT
32     CONSTANT VDP_COLS
24     CONSTANT VDP_ROWS
SYSVARS 0x72 + CONSTANT CPORT_MEM
0x3f   CONSTANT CPORT_CTL
0xdc   CONSTANT CPORT_D1
0xdd   CONSTANT CPORT_D2
SYSVARS 0x73 + CONSTANT PS2_MEM
5 LOAD  ( z80 assembler )
: ZFILL, ( u ) 0 DO 0 A, LOOP ;
262 LOAD  ( xcomp )
524 LOAD  ( font compiler )
282 LOAD  ( boot.z80.decl )
270 LOAD  ( xcomp overrides )

DI, 0x100 JP, 0x62 ZFILL, ( 0x66 )
RETN, 0x98 ZFILL, ( 0x100 )
( All set, carry on! )
CURRENT @ XCURRENT !
0x100 BIN( !
283 335 LOADR ( boot.z80 )
353 LOAD  ( xcomp core low )
CREATE ~FNT CPFNT7x7
603 608 LOADR ( VDP )
625 626 LOADR ( SMS ports )
620 LOAD ( PAD ) : (ps2kc) (ps2kcA) ; 411 414 LOADR
390 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," VDP$ PS2$ (im1) " EOT,
ORG @ 0x100 - 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
