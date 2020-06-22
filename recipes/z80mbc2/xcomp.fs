0xff00 CONSTANT RS_ADDR        0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT RAMSTART
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )            270 LOAD  ( xcomp overrides )
282 LOAD  ( boot.z80 )         353 LOAD  ( xcomp core low )
CODE (emit)
    A 1 LDrn, 1 OUTnA, HL POPqq, A L LDrr, 0 OUTnA,
;CODE
CODE (key)
    BEGIN, 1 INAn, A INCr, JRZ, AGAIN,
    A DECr, PUSHA,
;CODE
380 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," CURRENT @ HERE ! " EOT,
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
