\ This is xcomp code that is common to both serial and grid
\ binaries.
3 VALUES PS_ADDR 0xfffa RS_ADDR 0xff00 HERESTART 0
RS_ADDR 0x90 - VALUE SYSVARS
SYSVARS 0x80 + VALUE GRID_MEM
2 LOAD ( assembler common words )
CREATE nativeidx 0 ,
: NATIVE nativeidx @ DUP C, 1+ nativeidx ! ;
200 205 LOADR ( xcomp low )

HERE TO ORG
0x15 ALLOT0
( END OF STABLE ABI )
HERE 4 + TO XCURRENT ( make next CODE have 0 prev field )
CODE EXIT NATIVE
CODE (br) NATIVE
CODE (?br) NATIVE
CODE (loop) NATIVE
CODE (b) NATIVE
CODE (n) NATIVE
CODE (s) NATIVE
CODE >R NATIVE
CODE R> NATIVE
CODE 2>R NATIVE
CODE 2R> NATIVE
CODE EXECUTE NATIVE
CODE ROT NATIVE
CODE DUP NATIVE
CODE ?DUP NATIVE
CODE DROP NATIVE
CODE SWAP NATIVE
CODE OVER NATIVE
CODE AND NATIVE
CODE OR NATIVE
CODE XOR NATIVE
CODE NOT NATIVE
CODE + NATIVE
CODE - NATIVE
CODE * NATIVE
CODE /MOD NATIVE
CODE ! NATIVE
CODE @ NATIVE
CODE C! NATIVE
CODE C@ NATIVE
CODE PC! NATIVE
CODE PC@ NATIVE
CODE I NATIVE
CODE I' NATIVE
CODE J NATIVE
CODE BYE NATIVE
CODE ABORT NATIVE
CODE QUIT NATIVE
CODE []= NATIVE
CODE = NATIVE
CODE < NATIVE
CODE FIND NATIVE
CODE 1+ NATIVE
CODE 1- NATIVE
CODE TICKS NATIVE
CODE ROT> NATIVE
CODE CRC16 NATIVE
CODE CARRY? NATIVE
CODE >> NATIVE
CODE << NATIVE
CODE >>8 NATIVE
CODE <<8 NATIVE
CODE 'S NATIVE
CODE 'R NATIVE
210 231 LOADR ( forth low )
: (key?) 0 PC@ 1 ;
: EFS@
    1 3 PC! ( read )
    L|M 3 PC! 3 PC! ( blkid )
    BLK( L|M 3 PC! 3 PC! ( dest )
;
: EFS!
    2 3 PC! ( write )
    L|M 3 PC! 3 PC! ( blkid )
    BLK( L|M 3 PC! 3 PC! ( dest )
;
: INIT BLK$ ['] EFS@ [*TO] BLK@* ['] EFS! [*TO] BLK!* BLK$ ;
( fork between grid and serial begins here )
