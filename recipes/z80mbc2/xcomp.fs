0xff00 CONSTANT RS_ADDR        0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
282 LOAD  ( boot.z80.decl )
270 LOAD  ( xcomp overrides )
283 335 LOADR ( boot.z80 )
353 LOAD  ( xcomp core low )
CODE (emit)
    A 1 LDrn, 1 OUTnA, HL POPqq, A L LDrr, 0 OUTnA,
;CODE
CODE (key)
    BEGIN, 1 INAn, A INCr, JRZ, AGAIN,
    A DECr, PUSHA,
;CODE
: _sel ( sec )
( 32 sectors per track, 512 tracks per disk )
    32 /MOD ( addr sec trk )
    0x0a ( seltrk ) 1 PC! 0 PC! 0 0 PC! ( addr sec )
    0x0b ( selsec ) 1 PC! 0 PC! ( addr ) ;
: _ ( addr )
    ( get 512 bytes )
    0x86 ( readsec ) 1 PC!
    512 0 DO 0 PC@ SWAP C!+ LOOP DROP
;
: FD@ 2 * DUP _sel BLK( _ 1+ _sel BLK( 512 + _ ;
: _ ( addr )
    ( write 512 bytes )
    0x0c ( writesec ) 1 PC!
    512 0 DO C@+ 0 PC! LOOP DROP
;
: FD! 2 * DUP _sel BLK( _ 1+ _sel BLK( 512 + _ ;
: FD$ ( select disk 0 )
    0x09 ( seldisk ) 1 PC! 0 0 PC! ( sel disk 0 )
;
380 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," CURRENT @ HERE ! BLK$ FD$ ' FD@ BLK@* ! ' FD! BLK!* ! " EOT,
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
