0xe800 CONSTANT RAMSTART
0xf000 CONSTANT RS_ADDR
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
: CODE XCODE ;
: IMMEDIATE XIMM ;
: (entry) (xentry) ;
: : [ ' X: , ] ;

CURRENT @ XCURRENT !

H@ 256 /MOD 2 PC! 2 PC!
282 LOAD  ( boot.z80 )
393 LOAD  ( icore )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," CURRENT @ HERE ! "
," : (emit) 0 PC! ; "
," : KEY 0 PC@ ; "
422 470 XPACKR
," ' KEY 12 RAM+ ! "
H@ 256 /MOD 2 PC! 2 PC!
