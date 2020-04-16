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
    ( 1024 for the block, 6 for variables )
    1030 ALLOT
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
        I 1+ .2 SPC
        64 I * BLK( + (print)
        CRLF
    LOOP
;

: _
    (boot<)
    DUP 4 = IF
        DROP
        ( We're finished interpreting )
        EXIT!
    THEN
;

: LOAD
    ( save BLK>, CINPTR and boot< ptr to RSP )
    BLK> @ >R
    0x0c RAM+ @ >R
    0x2e RAM+ @ >R
    BLK@
    ( Point to beginning of BLK )
    BLK( 0x2e RAM+ !
    ( 0c == CINPTR )
    ['] _ 0x0c RAM+ !
    INTERPRET
    R> 0x2e RAM+ !
    ( Before we restore CINPTR, are we restoring it to "_"?
      if yes, it means we're in a nested LOAD which means we
      should also load back the saved BLK>. Otherwise, we can
      ignore the BLK> from RSP. )
    I 0x0c RAM+ @ = IF
        ( nested load )
        R> DROP ( CINPTR )
        R> BLK@
    ELSE
        ( not nested )
        R> 0x0c RAM+ !
        R> DROP ( BLK> )
    THEN
;
