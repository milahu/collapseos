( ----- 600 )
601 ACIA                       606 Zilog SIO driver
615 SPI relay                  619 Xcomp unit
( ----- 601 )
ACIA driver

Manage I/O from an asynchronous communication interface adapter
(ACIA). provides "(emit)" to put c char on the ACIA as well as
an input buffer from which a provided "(key)" reads. This driver
installs an interrupt handler at RST38 to handle RX.

To use, begin by loading declarations (B582) before xcomp is
loaded. These declarations provide default values for ports and
memory offsets that you can override. See B582.

Then, in the driver part, load range 583-588.
( ----- 602 )
0x80 CONSTANT ACIA_CTL ( IO port for ACIA's control register )
0x81 CONSTANT ACIA_IO ( IO port for ACIA's data registers )
0x20 CONSTANT ACIA_BUFSZ ( SZ-1 must be a mask )
( Address in memory that can be used variables shared
  with ACIA's native words. 4 bytes used. )
CREATE ACIA_MEM SYSVARS 0x70 + ,
( Points to ACIA buf )
: ACIA( ACIA_MEM @ 2+ ;
( Read buf idx Pre-inc )
: ACIAR> ACIA_MEM @ ;
( Write buf idx Post-inc )
: ACIAW> ACIA_MEM @ 1+ ;
( This means that if W> == R>, buffer is full.
  If R>+1 == W>, buffer is empty. )
( ----- 603 )
( ACIA INT handler, read into ACIAW> )
( Set RST 38 jump ) PC ORG @ 0x39 + !
    AF PUSH,
    ACIA_CTL INAi, 0x01 ANDi, ( is ACIA rcv buf full? )
    IFZ, ( no, abort ) AF POP, EI, RETI, THEN,
    HL PUSH,
    HL ACIAW> LDdi, A (HL) LDrr,
    HL DECd, (HL) CPr, ( W> == R> ? )
    IFNZ, ( buffer not full )
        ( get wr ptr ) HL ACIA( LDd(i),
        L ADDr, IFC, H INCr, THEN, L A LDrr,
        ( fetch/write ) ACIA_IO INAi, (HL) A LDrr,
        ( advance W> ) ACIAW> LDA(i), A INCr,
        ACIA_BUFSZ 1- ANDi, ACIAW> LD(i)A,
    THEN,
    HL POP, AF POP, EI, RETI,
( ----- 604 )
: (key)
    ( inc then fetch )
    [ ACIAR> LITN ] C@ 1+ [ ACIA_BUFSZ 1- LITN ] AND
    ( As long as R> == W>-1, it means that buffer is empty )
    BEGIN DUP [ ACIAW> LITN ] C@ = NOT UNTIL
    DUP [ ACIA( LITN ] @ + C@ ( ridx c )
    SWAP [ ACIAR> LITN ] C! ( c )
;
: (emit)
    ( As long at CTL bit 1 is low, we are transmitting. wait )
    BEGIN [ ACIA_CTL LITN ] PC@ 0x02 AND UNTIL
    ( The way is clear, go! )
    [ ACIA_IO LITN ] PC!
;
( ----- 605 )
: ACIA$
    H@ [ ACIA( LITN ] ! 0 [ ACIAR> LITN ] C!
    1 [ ACIAW> LITN ] C! ( write index starts one pos later )
    [ ACIA_BUFSZ LITN ] ALLOT
( setup ACIA
  CR7 (1) - Receive Interrupt enabled
  CR6:5 (00) - RTS low, transmit interrupt disabled.
  CR4:2 (101) - 8 bits + 1 stop bit
  CR1:0 (10) - Counter divide: 64 )
    0b10010110 [ ACIA_CTL LITN ] PC!
    (im1) ;
( ----- 606 )
Zilog SIO driver

Declarations at B607

Driver load range at B608-B610
( ----- 607 )
0x80 CONSTANT SIO_ACTL   0x81 CONSTANT SIO_ADATA
0x82 CONSTANT SIO_BCTL   0x83 CONSTANT SIO_BDATA
0x20 CONSTANT SIO_BUFSZ ( SZ-1 must be a mask )
( Address in memory that can be used variables shared
  with SIO native words. 4 bytes used. )
CREATE SIO_MEM SYSVARS 0x70 + ,
( Points to SIO buf )
: SIO( SIO_MEM @ 2+ ;
( Read buf idx Pre-inc )
: SIOR> SIO_MEM @ ;
( Write buf idx Post-inc )
: SIOW> SIO_MEM @ 1+ ;
( This means that if W> == R>, buffer is full.
  If R>+1 == W>, buffer is empty. )
( ----- 608 )
( INT handler. Set RST 38 jump ) PC ORG @ 0x39 + !
AF PUSH, BEGIN,
SIO_ACTL INAi, ( RR0 ) 0x01 ANDi, ( is recv buf full? )
IFZ, ( nope, exit ) A 0x20 ( CMD 4 ) LDri, SIO_ACTL OUTiA,
    AF POP, EI, RETI, THEN,
HL PUSH,
HL SIOW> LDdi, A (HL) LDrr,
HL DECd, (HL) CPr, ( W> == R> ? )
IFNZ, ( buffer not full )
    ( get wr ptr ) HL SIO( LDd(i),
    L ADDr, IFC, H INCr, THEN, L A LDrr,
    ( fetch/write ) SIO_ADATA INAi, (HL) A LDrr,
    ( advance W> ) SIOW> LDA(i), A INCr,
    SIO_BUFSZ 1- ANDi, SIOW> LD(i)A,
THEN, HL POP, JR, AGAIN,
( ----- 609 )
: (key)
    ( inc then fetch )
    [ SIOR> LITN ] C@ 1+ [ SIO_BUFSZ 1- LITN ] AND
    ( As long as R> == W>-1, it means that buffer is empty )
    BEGIN DUP [ SIOW> LITN ] C@ = NOT UNTIL
    DUP [ SIO( LITN ] @ + C@ ( ridx c )
    SWAP [ SIOR> LITN ] C! ( c )
;
: (emit)
    ( As long at CTL bit 2 is low, we are transmitting. wait )
    BEGIN [ SIO_ACTL LITN ] PC@ 0x04 AND UNTIL
    ( The way is clear, go! )
    [ SIO_ADATA LITN ] PC!
;
( ----- 610 )
: _ [ SIO_ACTL LITN ] PC! ;
: SIO$
    H@ [ SIO( LITN ] ! 0 [ SIOR> LITN ] C!
    1 [ SIOW> LITN ] C! ( write index starts one pos later )
    [ SIO_BUFSZ LITN ] ALLOT
    0x18 _ ( CMD3 )
    0x24 _ ( CMD2/PTR4 ) 0b11000100 _ ( WR4/64x/1stop/nopar )
    0x03 _ ( PTR3 ) 0b11000001 _ ( WR3/RXen/8char )
    0x05 _ ( PTR5 ) 0b01101000 _ ( WR5/TXen/8char )
    0x21 _ ( CMD2/PTR1 ) 0b00011000 _ ( WR1/Rx INT all chars )
    (im1)
;
( ----- 619 )
( RC2014 classic with ACIA )
0xff00 CONSTANT RS_ADDR        0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
0x8000 CONSTANT HERESTART
4 CONSTANT SPI_DATA 5 CONSTANT SPI_CTL 1 CONSTANT SDC_DEVID
602 LOAD  ( acia decl )
5 LOAD    ( z80 assembler )
262 LOAD  ( xcomp )            282 LOAD  ( boot.z80.decl )
270 LOAD  ( xcomp overrides )  283 335 LOADR ( boot.z80 )
353 LOAD  ( xcomp core low )   603 605 LOADR ( acia )
419 LOAD  ( SPI relay )        423 436 LOADR ( SD Card )
400 LOAD  ( AT28 )
390 LOAD  ( xcomp core high )
(entry) _
PC ORG @ 8 + ! ( Update LATEST )
," ACIA$ BLK$ " EOT,
( ----- 620 )
( RC2014 classic with SIO )
0xff00 CONSTANT RS_ADDR        0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
0x8000 CONSTANT HERESTART
4 CONSTANT SPI_DATA 5 CONSTANT SPI_CTL 1 CONSTANT SDC_DEVID
607 LOAD  ( SIO decl )
5 LOAD    ( z80 assembler )
262 LOAD  ( xcomp )            282 LOAD  ( boot.z80.decl )
270 LOAD  ( xcomp overrides )  283 335 LOADR ( boot.z80 )
353 LOAD  ( xcomp core low )   608 610 LOADR ( SIO )
419 LOAD  ( SPI relay )        423 436 LOADR ( SD Card )
400 LOAD  ( AT28 )
390 LOAD  ( xcomp core high )
(entry) _
PC ORG @ 8 + ! ( Update LATEST )
," SIO$ BLK$ " EOT,
