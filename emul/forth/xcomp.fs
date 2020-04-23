262 LOAD  ( xcomp )
: CODE XCODE ;
: IMMEDIATE XIMM ;
: (entry) (xentry) ;
: : [ ' X: , ] ;

CURRENT @ XCURRENT !

H@ 256 /MOD 2 PC! 2 PC!
H@ XOFF !
282 LOAD  ( boot.z80 )
