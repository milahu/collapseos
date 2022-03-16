\ RC2014 classic with SIO
10 CONSTS $ff00 RS_ADDR $fffa PS_ADDR $8000 HERESTART
         $80 SIOA_CTL $81 SIOA_DATA
         $82 SIOB_CTL $83 SIOB_DATA
         4 SPI_DATA 5 SPI_CTL 1 SDC_DEVID
RS_ADDR $90 - VALUE SYSVARS
SYSVARS $409 - VALUE BLK_MEM
SYSVARS $80 + VALUE SDC_MEM
ARCHM XCOMP Z80A 
XCOMPC Z80C COREL
345 347 LOADR \ SIO
332 LOAD  \ SPI relay
250 258 LOADR \ SD Card
339 LOAD  \ AT28
ALIAS SDC@ (blk@)
ALIAS SDC! (blk!)
BLKSUB
: INIT SIOA$ BLK$ ;
ALIAS SIOA<? (key?) ALIAS SIOA> (emit)
ALIAS SIOA<? RX<? ALIAS SIOA> TX>
XWRAP
