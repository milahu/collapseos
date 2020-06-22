0xff00 CONSTANT RS_ADDR
0xfffa CONSTANT PS_ADDR
RS_ADDR 0x80 - CONSTANT RAMSTART
750 LOAD  ( 8086 asm )
262 LOAD  ( xcomp )
270 LOAD  ( xcomp overrides )
812 829 LOADR
353 LOAD  ( xcomp core low )
CODE (emit) 1 chkPS,
    AX POPx, AH 0x0e MOVri, ( print char ) 0x10 INT,
;CODE
CODE (key)
    AH AH XORrr, 0x16 INT, AH AH XORrr, AX PUSHx,
;CODE
CODE 13H08H ( driveno -- cx dx )
    DI POPx, DX PUSHx, ( protect ) DX DI MOVxx, AX 0x800 MOVxI,
    DI DI XORxx, ES DI MOVsx,
    0x13 INT, DI DX MOVxx, DX POPx, ( unprotect )
    CX PUSHx, DI PUSHx,
    DI 0x800 MOVxI, ES DI MOVsx,
;CODE
CODE 13H ( ax bx cx dx -- ax bx cx dx )
    SI POPx, ( DX ) CX POPx, BX POPx, AX POPx,
    DX PUSHx, ( protect ) DX SI MOVxx, DI DI XORxx,
    0x13 INT, SI DX MOVxx, DX POPx, ( unprotect )
    AX PUSHx, BX PUSHx, CX PUSHx, SI PUSHx,
;CODE
: FDSPT 0x70 RAM+ ;
: FDHEADS 0x71 RAM+ ;
: _ ( AX BX sec )
    ( AH=read sectors, AL=1 sector, BX=dest,
      CH=trackno CL=secno DH=head DL=drive )
    FDSPT C@ /MOD ( AX BX sec trk )
    FDHEADS C@ /MOD ( AX BX sec head trk )
    8 LSHIFT ROT OR 1+ ( AX BX head CX )
    SWAP 8 LSHIFT 0x03 C@ ( boot drive ) OR ( AX BX CX DX )
    13H 2DROP 2DROP
;
: FD@
    2 * 16 + ( blkfs starts at sector 16 )
    0x0201 BLK( 2 PICK _
    0x0201 BLK( 0x200 + ROT 1+ _ ;
: FD!
    2 * 16 + ( blkfs starts at sector 16 )
    0x0301 BLK( 2 PICK _
    0x0301 BLK( 0x200 + ROT 1+ _ ;
: FD$
    ( get number of sectors per track with command 08H. )
    0x03 ( boot drive ) C@ 13H08H
    8 RSHIFT 1+ FDHEADS C!
    0x3f AND FDSPT C!
;
: COLS 80 ; : LINES 25 ;
CODE AT-XY ( x y )
    ( DH=row DL=col BH=page )
    AX POPx, BX POPx, DX PUSHx, ( protect )
    DH AL MOVrr, DL BL MOVrr, BX BX XORxx, AH 2 MOVri,
    0x10 INT, DX POPx, ( unprotect )
;CODE
380 LOAD  ( xcomp core high )
(entry) _
( Update LATEST )
PC ORG @ 8 + !
," BLK$ FD$ "
," ' FD@ BLK@* ! "
," ' FD! BLK!* ! "
EOT,
ORG @ 256 /MOD 2 PC! 2 PC!
H@ 256 /MOD 2 PC! 2 PC!
