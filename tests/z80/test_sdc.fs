212 LOAD ( z80a )
: SPI_DATA 4 ;
: SPI_CTL 5 ;
: SDC_DEVID 1 ;
596 LOAD ( spi )
423 436 LOADR ( sdc )

0x0000 0x00 _crc16 0x0000 #eq
0x0000 0x01 _crc16 0x1021 #eq
0x5678 0x34 _crc16 0x34e4 #eq
