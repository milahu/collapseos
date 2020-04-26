0x8000 CONSTANT RAMSTART
0xf000 CONSTANT RS_ADDR
0x80   CONSTANT ACIA_CTL
0x81   CONSTANT ACIA_IO
4      CONSTANT SDC_SPI
5      CONSTANT SDC_CSLOW
6      CONSTANT SDC_CSHIGH
RAMSTART 0x70 + CONSTANT ACIA_MEM
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
: CODE XCODE ;
: IMMEDIATE XIMM ;
: (entry) (xentry) ;
: : [ ' X: , ] ;

CURRENT @ XCURRENT !

H@ 256 /MOD 2 PC! 2 PC!
H@ XOFF !
282 LOAD  ( boot.z80 )
352 LOAD  ( acia.z80 )
372 LOAD  ( sdc.z80 )
393 LOAD  ( icore )
(entry) _
( Update LATEST )
H@ XOFF @ - XOFF @ 8 + !
422 441 XPACKR ( core cmp )
446 452 XPACKR ( parse )
358 360 XPACKR ( acia.fs )
442 445 XPACKR ( print )
459 463 XPACKR ( fmt )
123 132 XPACKR ( linker )
," : x KEY DUP EMIT ; "
," : _ ACIA$ (ok) ['] x 0x0c RAM+ ! ; _ "
H@ 256 /MOD 2 PC! 2 PC!
