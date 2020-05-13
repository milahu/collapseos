0xe800 CONSTANT RAMSTART
0xf000 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
: CODE XCODE ;
: COMPILE XCOMPILE ; IMMEDIATE
: [COMPILE] X[COMPILE] ; IMMEDIATE
: IMMEDIATE XIMM ;
: (entry) (xentry) ;
: : [ ' X: , ] ;

CURRENT @ XCURRENT !

282 LOAD  ( boot.z80 )
393 LOAD  ( icore low )
: (emit) 0 PC! ;
: (key) 0 PC@ ;
415 LOAD  ( icore high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," CURRENT @ HERE ! "
422 459 XPACKR
," ' (key) 12 RAM+ ! "
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
