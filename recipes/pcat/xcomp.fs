0xff00 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT RAMSTART
750 LOAD  ( 8086 asm )
262 LOAD  ( xcomp )
270 LOAD  ( xcomp overrides )
812 827 LOADR
353 LOAD  ( xcomp core low )
CODE (emit)
    AX POPx, AH 0x0e MOVri, ( print char ) 0x10 INT,
;CODE
CODE (key) AH AH XORrr, 0x16 INT, AX PUSHx, ;CODE
: FOO (key) (emit) ;
: BOOT 0x08 @ LIT< FOO _find DROP EXECUTE BYE ;
(entry) _
( Update LATEST )
PC ORG @ 8 + !
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
