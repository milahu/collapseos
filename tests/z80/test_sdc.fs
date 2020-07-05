212 LOAD ( z80a )
: SPI_DATA 4 ;
: SPI_CSLOW 5 ;
: SPI_CSHIGH 6 ;
596 LOAD ( spi )
603 616 LOADR ( sdc )

0x0000 0x00 _crc16 0x0000 #eq
0x0000 0x01 _crc16 0x1021 #eq
0x5678 0x34 _crc16 0x34e4 #eq
