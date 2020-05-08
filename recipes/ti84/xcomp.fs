0x8000 CONSTANT RAMSTART
0xb000 CONSTANT RS_ADDR
RAMSTART 0x70 + CONSTANT TI_MEM
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
: CODE XCODE ;
: IMMEDIATE XIMM ;
: (entry) (xentry) ;
: : [ ' X: , ] ;

CURRENT @ XCURRENT !

282 LOAD  ( boot.z80 )
393 LOAD  ( icore )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," 42 42 PC! BYE "
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
