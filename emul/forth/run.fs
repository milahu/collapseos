: EFS@
    256 /MOD 3 PC! 3 PC!
    1024 0 DO
        4 PC@
        BLK( I + C!
    LOOP
;
: EFS!
    256 /MOD 3 PC! 3 PC!
    1024 0 DO
        BLK( I + C@ 4 PC!
    LOOP
;

: INIT
    CURRENT @ HERE !
    BLK$
    ['] EFS@ BLK@* !
    ['] EFS! BLK!* !
    RDLN$
    LIT< _sys [entry]
    INTERPRET
;
