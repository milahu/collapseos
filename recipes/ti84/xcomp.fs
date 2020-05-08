0x8000 CONSTANT RAMSTART
0xb000 CONSTANT RS_ADDR
RAMSTART 0x70 + CONSTANT TI_MEM
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
522 LOAD  ( font compiler )
: CODE XCODE ;
: IMMEDIATE XIMM ;
: (entry) (xentry) ;
: : [ ' X: , ] ;

CURRENT @ XCURRENT !

282 LOAD  ( boot.z80 )
555 LOAD  ( ti.z80 )
393 LOAD  ( icore )
(entry) ~FNT CPFNT3x5
(entry) _
( Update LATEST )
PC ORG @ 8 + !
422 437 XPACKR ( core )
556 560 XPACKR ( ti )
438 446 XPACKR ( print fmt )
," : _ LCD$ LIT< Hello (print) LIT< World! (print) BYE ; _ "
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
