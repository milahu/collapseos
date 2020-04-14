( Words allowing printing strings. Require core )
( This used to be in core, but some drivers providing EMIT
  are much much easier to write with access to core words,
  and these words below need EMIT... )

: (print)
    BEGIN
    DUP C@   ( a c )
    ( exit if null )
    DUP NOT IF 2DROP EXIT THEN
    EMIT     ( a )
    1 +      ( a+1 )
    AGAIN
;

: ."
    34 ,        ( 34 == litWord )
    BEGIN
        C< DUP          ( c c )
        ( 34 is ASCII for " )
        DUP 34 = IF DROP DROP 0 0 THEN
        C,
    0 = UNTIL
    COMPILE (print)
; IMMEDIATE

: ABORT" [COMPILE] ." COMPILE ABORT ; IMMEDIATE

: (uflw) ABORT" stack underflow" ;
: (wnf) ABORT" word not found" ;

: BS 8 EMIT ;
: LF 10 EMIT ;
: CR 13 EMIT ;
: CRLF CR LF ;
: SPC 32 EMIT ;
