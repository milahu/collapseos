0xf000 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT RAMSTART
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
270 LOAD  ( xcomp overrides )

0x3000 BIN( !
282 LOAD  ( boot.z80 )
492 LOAD  ( trs80.z80 )
393 LOAD  ( xcomp core low )
420 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," CURRENT @ HERE ! "
440 445 XPACKR ( core )
499 500 XPACKR ( trs80.fs )
( 0x0a == NLPTR. TRS-80 wants CR-only newlines )
," : _ ['] CR 0x0a RAM+ ! BLK$ FD$ (ok) RDLN$ ; _ "
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
