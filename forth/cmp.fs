( Words useful for complex comparison operations )

: >= < NOT ;
: <= > NOT ;
: 0>= 0< NOT ;

( n1 -- n1 true )
: <>{ 1 ;

( n1 f -- f )
: <>} SWAP DROP ;


: _|&
    ( n1 n2 cell )
    >R >R DUP R> R>          ( n1 n1 n2 cell )
    @ EXECUTE                ( n1 f )
;

( n1 f n2 -- n1 f )
: _|
    CREATE , DOES>
    ( n1 f n2 cell )
    ROT IF 2DROP 1 EXIT THEN ( n1 true )
    _|&
;

: _&
    CREATE , DOES>
    ( n1 f n2 cell )
    ROT NOT IF 2DROP 0 EXIT THEN ( n1 true )
    _|&
;

( All words below have this signature:
  n1 f n2 -- n1 f )
' = _| |=
' = _& &=
' > _| |>
' > _& &>
' < _| |<
' < _& &<
