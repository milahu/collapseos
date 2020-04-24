( Words allowing printing strings. Require core )
( This used to be in core, but some drivers providing EMIT
  are much much easier to write with access to core words,
  and these words below need EMIT... )

: (print)
    BEGIN
    C@+      ( a+1 c )
    ( exit if null )
    DUP NOT IF 2DROP EXIT THEN
    EMIT     ( a )
    AGAIN
;

: ."
    34 ,        ( 34 == litWord )
    BEGIN
        C<
        ( 34 is ASCII for " )
        DUP 34 = IF DROP 0 THEN
        DUP C,
    NOT UNTIL
    COMPILE (print)
; IMMEDIATE

: ABORT" [COMPILE] ." COMPILE ABORT ; IMMEDIATE

: (uflw) ABORT" stack underflow" ;

: BS 8 EMIT ;
: LF 10 EMIT ;
: CR 13 EMIT ;
: CRLF CR LF ;
: SPC 32 EMIT ;

: (wnf) (print) SPC ABORT" word not found" ;
: (ok) SPC ." ok" CRLF ;
