0xff00 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
30 LOAD   ( 8086 asm )
262 LOAD  ( xcomp ) 270 LOAD  ( xcomp overrides )
445 461 LOADR ( 8086 boot code )
353 LOAD  ( xcomp core low )
CODE (emit) AX POPx, 1 INT, ;CODE
CODE (key) 2 INT, AH 0 MOVri, AX PUSHx, ;CODE
380 LOAD  ( xcomp core high )
(entry) _ ( Update LATEST ) PC ORG @ 8 + !
EOT,
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!