: (c<) KEY DUP EMIT ;
: _
    ACIA$
    ." Collapse OS" CRLF
    ( 0c == CINPTR )
    ['] (c<) 0x0c RAM+ !
; _
