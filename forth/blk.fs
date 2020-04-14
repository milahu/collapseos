( I/O blocks )

: BLKMEM+ 0x57 RAM+ @ + ;
( n -- Fetches block n and write it to BLK( )
: BLK@* 0 BLKMEM+ ;
( n -- Write back BLK( to storage at block n )
: BLK!* 2 BLKMEM+ ;
( Current blk pointer in ( )
: BLK> 4 BLKMEM+ ;
: BLK( 6 BLKMEM+ ;

: BLK$
    H@ 0x57 RAM+ !
    1030 ALLOT
    -1 BLK> !
;

: BLK@
    DUP BLK> = IF DROP EXIT THEN
    DUP BLK> ! BLK@* @ EXECUTE
;

: .2 DUP 10 < IF SPC THEN . ;

: LIST
    BLK@
    16 0 DO
        I 1 + .2 SPC
        64 I * BLK( + (print)
        CRLF
    LOOP
;
