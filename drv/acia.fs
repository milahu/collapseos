( ACIA

Manage I/O from an asynchronous communication interface adapter
(ACIA). provides "EMIT" to put c char on the ACIA as well as
an input buffer. You have to call "~ACIA" on interrupt for
this module to work well.

CONFIGURATION

ACIA_CTL: IO port for the ACIA's control registers
ACIA_IO: IO port for the ACIA's data registers
ACIA_MEM: Address in memory that can be used variables shared
          with ACIA's native words. 8 bytes used.
)

0x20 CONSTANT ACIABUFSZ

( Points to ACIA buf )
: ACIA( [ ACIA_MEM 4 + LITN ] ;
( Points to ACIA buf end )
: ACIA) [ ACIA_MEM 6 + LITN ] ;
( Read buf pointer. Pre-inc )
: ACIAR> [ ACIA_MEM LITN ] ;
( Write buf pointer. Post-inc )
: ACIAW> [ ACIA_MEM 2 + LITN ] ;
( This means that if W> == R>, buffer is full.
  If R>+1 == W>, buffer is empty. )


: ACIA$
    H@ DUP DUP ACIA( ! ACIAR> !
    1+ ACIAW> ! ( write index starts one position later )
    ACIABUFSZ ALLOT
    H@ ACIA) !
( setup ACIA
  CR7 (1) - Receive Interrupt enabled
  CR6:5 (00) - RTS low, transmit interrupt disabled.
  CR4:2 (101) - 8 bits + 1 stop bit
  CR1:0 (10) - Counter divide: 64
)
    0b10010110 ACIA_CTL PC!

( setup interrupt )
    ( 4e == INTJUMP )
    0xc3 0x4e RAM+ C! ( JP upcode )
    ['] ~ACIA 0x4f RAM+ !
    (im1)
;

: KEY
    ( inc then fetch )
    ACIAR> @ 1+ DUP ACIA) @ = IF
        DROP ACIA( @
    THEN

    ( As long as R> == W>-1, it means that buffer is empty )
    BEGIN DUP ACIAW> @ = NOT UNTIL

    ACIAR> !
    ACIAR> @ C@
;

: EMIT
    ( As long at CTL bit 1 is low, we are transmitting. wait )
    BEGIN ACIA_CTL PC@ 0x02 AND UNTIL
    ( The way is clear, go! )
    ACIA_IO PC!
;

