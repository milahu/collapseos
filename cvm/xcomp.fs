0xff00 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT SYSVARS
0 CONSTANT HERESTART
: CODE ( natidx -- ) (entry) 0 C, C, ;
VARIABLE ORG
CREATE BIN( 0 ,
: PC H@ ORG @ - ;
155 LOAD  ( ALLOT0 )
262 LOAD  ( xcomp )
270 LOAD  ( xcomp overrides )

H@ ORG !
0x0b ALLOT0
0 C, 0 C, ( EXIT )
0x08 ALLOT0
( END OF STABLE ABI )
H@ 4 + XCURRENT ! ( make next CODE have 0 prev field )
0x00 CODE EXIT
0x01 CODE (br)
0x02 CODE (?br)
0x03 CODE (loop)
0x05 CODE (n)
0x06 CODE (s)
0x04 CODE 2>R
0x07 CODE >R
0x08 CODE R>
0x09 CODE 2R>
0x0a CODE EXECUTE
0x0b CODE ROT
0x0c CODE DUP
0x0d CODE ?DUP
0x0e CODE DROP
0x0f CODE SWAP
0x10 CODE OVER
0x11 CODE PICK
0x12 CODE (roll)
0x13 CODE 2DROP
0x14 CODE 2DUP
0x15 CODE S0
0x16 CODE 'S
0x17 CODE AND
0x18 CODE OR
0x19 CODE XOR
0x1a CODE NOT
0x1b CODE +
0x1c CODE -
0x1d CODE *
0x1e CODE /MOD
0x1f CODE !
0x20 CODE @
0x21 CODE C!
0x22 CODE C@
0x23 CODE PC!
0x24 CODE PC@
0x25 CODE I
0x26 CODE I'
0x27 CODE J
0x28 CODE BYE
0x29 CODE (resSP)
0x2a CODE (resRS)
0x2b CODE S=
0x2c CODE CMP
0x2d CODE _find
0x2e CODE 0
0x2f CODE 1
0x30 CODE -1
0x31 CODE 1+
0x32 CODE 1-
0x33 CODE 2+
0x34 CODE 2-
0x35 CODE RSHIFT
0x36 CODE LSHIFT
353 LOAD  ( xcomp core low )
: (emit) 0 PC! ;
: (key) 0 PC@ ;
: EFS@
    1 3 PC! ( read )
    256 /MOD 3 PC! 3 PC! ( blkid )
    BLK( 256 /MOD 3 PC! 3 PC! ( dest )
;
: EFS!
    2 3 PC! ( write )
    256 /MOD 3 PC! 3 PC! ( blkid )
    BLK( 256 /MOD 3 PC! 3 PC! ( dest )
;
: COLS 80 ; : LINES 32 ;
: AT-XY 6 PC! ( y ) 5 PC! ( x ) ;

380 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," BLK$ "
," ' EFS@ BLK@* ! "
," ' EFS! BLK!* ! "
EOT,
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
