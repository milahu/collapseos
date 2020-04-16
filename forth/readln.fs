( requires core, parse, print )

( Managing variables in a core module is tricky. Sure, we
  have (sysv), but here we need to allocate a big buffer, and
  that cannot be done through (sysv). What we do is that we
  allocate that buffer at runtime and use (sysv) to point to
  it, a pointer that is set during the initialization
  routine. )

64 CONSTANT INBUFSZ
: RDLNMEM+ 0x53 RAM+ @ + ;
( current position in INBUF )
: IN> 0 RDLNMEM+ ;
( points to INBUF )
: IN( 2 RDLNMEM+ ;
( points to INBUF's end )
: IN) INBUFSZ 2+ RDLNMEM+ ;

( flush input buffer )
( set IN> to IN( and set IN> @ to null )
: (infl) 0 IN( DUP IN> ! ! ;

( handle backspace: go back one char in IN>, if possible, then
  emit SPC + BS )
: (inbs)
    ( already at IN( ? )
    IN> @ IN( = IF EXIT THEN
    IN> @ 1- IN> !
    SPC BS
;

( read one char into input buffer and returns whether we
  should continue, that is, whether CR was not met. )
: (rdlnc)                   ( -- f )
    ( buffer overflow? same as if we typed a newline )
    IN> @ IN) = IF 0x0a ELSE KEY THEN     ( c )
    ( del? same as backspace )
    DUP 0x7f = IF DROP 0x8 THEN
    ( lf? same as cr )
    DUP 0x0a = IF DROP 0xd THEN
    ( echo back )
    DUP EMIT                    ( c )
    ( bacspace? handle and exit )
    DUP 0x8 = IF (inbs) EXIT THEN
    ( write and advance )
    DUP     ( keep as result )  ( c c )
    ( Here, we take advantage of the fact that c's MSB is
      always zero and thus ! automatically null-terminates
      our string )
    IN> @ ! 1 IN> +!            ( c )
    ( if newline, replace with zero to indicate EOL )
    DUP 0xd = IF DROP 0 THEN
;

( Read one line in input buffer and make IN> point to it )
: (rdln)
    ( Should we prompt? if we're executing a word, FLAGS bit
      0, then we shouldn't. )
    FLAGS @ 0x1 AND NOT IF '>' EMIT SPC THEN
    (infl)
    BEGIN (rdlnc) NOT UNTIL
    LF IN( IN> !
;

( And finally, implement a replacement for the (c<) routine )
: (rdln<)
    IN> @ C@                    ( c )
    ( not EOL? good, inc and return )
    DUP IF 1 IN> +! EXIT THEN   ( c )
    ( EOL ? readline. we still return typed char though )
    (rdln)                      ( c )
;

( Initializes the readln subsystem )
: RDLN$
    ( 53 == rdln's memory )
    H@ 0x53 RAM+ !
    ( 2 for IN>, plus 2 for extra bytes after buffer: 1 for
      the last typed 0x0a and one for the following NULL. )
    INBUFSZ 4 + ALLOT
    (infl)
    ['] (rdln<) 0x0c RAM+ !
;

