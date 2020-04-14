: EFS@
    256 /MOD 3 PC! 3 PC!
    1024 0 DO
        3 PC@
        BLK( I + C!
    LOOP
;
: INIT
    CURRENT @ HERE !
    BLK$
    ['] EFS@ BLK@* !
    RDLN$
    Z80A$
    LIT< _sys [entry]
    INTERPRET
;
