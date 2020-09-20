0xff00 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
0 CONSTANT HERESTART
212 LOAD ( z80 assembler )
262 LOAD ( xcomp )
282 LOAD ( boot.z80.decl )
270 LOAD ( xcomp overrides )
0x3000 BIN( !
283 335 LOADR ( boot.z80 )
353 LOAD ( xcomp core low )
602 LOAD ( trs80 )
380 LOAD ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
( 0x0a == NLPTR. TRS-80 wants CR-only newlines )
," ' CR 0x0a RAM+ ! BLK$ FD$ " EOT,
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
