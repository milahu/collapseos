212 LOAD ( z80a )
: SDC_SPI 4 ;
: SDC_CSLOW 5 ;
: SDC_CSHIGH 6 ;
372 LOAD ( sdc.z80 )
374 LOAD ( sdc.fs )

0x0000 0x00 _crc16 0x0000 #eq
0x0000 0x01 _crc16 0x1021 #eq
0x5678 0x34 _crc16 0x34e4 #eq
