0xe800 CONSTANT RAMSTART
0xff00 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
212 LOAD  ( z80 assembler )
262 LOAD  ( xcomp )
270 LOAD  ( xcomp overrides )

282 LOAD  ( boot.z80 )
353 LOAD  ( xcomp core low )
: (emit) 0 PC! ;
: (key) 0 PC@ ;
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

380 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," CURRENT @ HERE ! "
," BLK$ "
," ' EFS@ BLK@* ! "
," ' EFS! BLK!* ! "
EOT,
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
