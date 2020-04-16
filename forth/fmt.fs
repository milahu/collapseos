( requires core, parse, cmp )

: _
    999 SWAP        ( stop indicator )
    DUP 0 = IF '0' EXIT THEN    ( 0 is a special case )
    BEGIN
    DUP 0 = IF DROP EXIT THEN
    10 /MOD         ( r q )
    SWAP '0' + SWAP ( d q )
    AGAIN
;

: .               ( n -- )
    ( handle negative )
    DUP <0 IF '-' EMIT -1 * THEN
    _
    BEGIN
    DUP '9' > IF DROP EXIT THEN ( stop indicator, we're done )
    EMIT
    AGAIN
;

: ? @ . ;

: _
    DUP 9 > IF 10 - 'a' +
    ELSE '0' + THEN
;

( For hex display, there are no negatives )

: .x
    256 MOD     ( ensure < 0x100 )
    16 /MOD     ( l h )
    _ EMIT ( l )
    _ EMIT
;

: .X
    256 /MOD    ( l h )
    .x .x
;

( a -- a+8 )
: _
    DUP         ( save for 2nd loop )
    ':' EMIT DUP .x SPC
    4 0 DO
        DUP @
        256 /MOD SWAP
        .x .x
        SPC
        2+
    LOOP
    DROP
    8 0 DO
        C@+
        DUP <>{ 0x20 &< 0x7e |> <>}
        IF DROP '.' THEN
        EMIT
    LOOP
    CRLF
;

( n a -- )
: DUMP
    LF
    BEGIN
        OVER 1 < IF 2DROP EXIT THEN
        _
        SWAP 8 - SWAP
    AGAIN
;
