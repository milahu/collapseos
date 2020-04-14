( I/O blocks )

: BLKMEM+ 0x57 RAM+ @ + ;
( n -- Fetches block n and write it to BLK( )
: BLK@* 0 BLKMEM+ ;
( n -- Write back BLK( to storage at block n )
: BLK!* 2 BLKMEM+ ;
( Current blk pointer in ( )
: BLK> 4 BLKMEM+ ;
( backup for CINPTR when LOADing )
: BLKC<* 6 BLKMEM+ ;
: BLK( 8 BLKMEM+ ;

: BLK$
    H@ 0x57 RAM+ !
    ( 1024 for the block, 8 for variables )
    1032 ALLOT
    ( LOAD detects end of block with ASCII EOT. This is why
      we write it there. EOT == 0x04 )
    4 C,
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

: _
    (boot<)
    DUP 4 = IF
        DROP
        ( We're finished interpreting )
        BLKC<* @ 0x0c RAM+ !
        C<
    THEN
;

: LOAD
    BLK@
    ( 2e == BOOT C< PTR )
    BLK( 0x2e RAM+ !
    ( Save current C< ptr )
    0x0c RAM+ @ BLKC<* !
    ( 0c == CINPTR )
    ['] _ 0x0c RAM+ !
;
