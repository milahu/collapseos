: SLEN      ( a -- n )
    DUP     ( astart aend )
    BEGIN
    DUP C@ 0 = IF -^ EXIT THEN
    1+
    AGAIN
;
