: SLEN      ( a -- n )
    DUP     ( astart aend )
    BEGIN C@+ NOT UNTIL
    1- -^
;
