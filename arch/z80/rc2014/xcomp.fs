\ RC2014 classic with MC6850
8 VALUES RS_ADDR $ff00 PS_ADDR $fffa HERESTART $8000
         6850_CTL $80 6850_IO $81
         SPI_DATA 4 SPI_CTL 5 SDC_DEVID 1
RS_ADDR $90 - VALUE SYSVARS
SYSVARS $80 + VALUE SDC_MEM
ARCHM Z80A XCOMPL Z80H
XCOMPH Z80C COREL Z80H ASMH
340 342 LOADR \ MC6850
332 LOAD \ SPI relay
250 258 LOADR \ SD Card
330 LOAD \ AT28
X' SDC@ ALIAS (blk@)
X' SDC! ALIAS (blk!)
BLKSUB
: INIT 6850$ BLK$ ;
XWRAP
