( requires core, str )
( string being sent to parse routines are always null
  terminated )

: (parsec)          ( a -- n f )
    ( apostrophe is ASCII 39 )
    DUP C@ 39 = NOT IF 0 EXIT THEN      ( a 0 )
    DUP 2+ C@ 39 = NOT IF 0 EXIT THEN   ( a 0 )
    ( surrounded by apos, good, return )
    1+ C@ 1                             ( n 1 )
;

( returns negative value on error )
: _            ( c -- n )
    ( '0' is ASCII 48 )
    48 -
    DUP 0< IF EXIT THEN                 ( bad )
    DUP 10 < IF EXIT THEN               ( good )
    ( 'a' is ASCII 97. 59 = 97 - 48 )
    49 -
    DUP 0< IF EXIT THEN                 ( bad )
    DUP 6 < IF 10 + EXIT THEN           ( good )
    ( bad )
    255 -
;

: (parseh)          ( a -- n f )
    ( '0': ASCII 0x30 'x': 0x78 0x7830: 30768 )
    DUP @ 30768 = NOT IF 0 EXIT THEN    ( a 0 )
    ( We have "0x" prefix )
    2+
    0  ( a r )
    BEGIN
    SWAP C@+                            ( r a+1 c )
    DUP NOT IF 2DROP 1 EXIT THEN        ( r, 1 )
    _                                   ( r a n )
    DUP 0< IF ROT 2DROP 0 EXIT THEN     ( a 0 )
    ROT 16 * +                          ( a r*16+n )
    AGAIN
;

( returns negative value on error )
: _            ( c -- n )
    ( '0' is ASCII 48 )
    48 -
    DUP 0< IF EXIT THEN                 ( bad )
    DUP 2 < IF EXIT THEN                ( good )
    ( bad )
    255 -
;

: (parseb)          ( a -- n f )
    ( '0': ASCII 0x30 'b': 0x62 0x6230: 25136 )
    DUP @ 25136 = NOT IF 0 EXIT THEN    ( a 0 )
    ( We have "0b" prefix )
    2+
    0  ( a r )
    BEGIN
    SWAP C@+                               ( r a+1 c )
    DUP NOT IF 2DROP 1 EXIT THEN           ( r 1 )
    _                                      ( r a n )
    DUP 0< IF ROT 2DROP 0 EXIT THEN        ( a 0 )
    ROT 2 * +                              ( a r*2+n )
    AGAIN
;

: (parse)           ( a -- n )
    (parsec) IF EXIT THEN
    (parseh) IF EXIT THEN
    (parseb) IF EXIT THEN
    (parsed) IF EXIT THEN
    ( nothing works )
    LIT< (wnf) (find) IF EXECUTE ELSE ABORT THEN
;

' (parse) (parse*) !
