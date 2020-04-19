: (c<) KEY DUP EMIT ;
: INIT
    ACIA$
    ." Collapse OS" CRLF
    ( 0c == CINPTR )
    ['] (c<) 0x0c RAM+ !
;
INIT

