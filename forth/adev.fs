( Addressed devices.

Abstractions to read and write to devices that allow addressed
access. At all times, we have one active "fetch" device and
one active "store" device, A@ and A!.

Those words have the same signature as C@ and C!, and in fact,
initially default to proxy of those words.
)

: ADEVMEM+ 0x55 RAM+ @ + ;
: A@* 0 ADEVMEM+ ;
: A!* 2 ADEVMEM+ ;

: ADEV$
    H@ 0x55 RAM+ !
    4 ALLOT
    ['] C@ A@* !
    ['] C! A!* !
;

: A@ A@* @ EXECUTE ;
: A! A!* @ EXECUTE ;

( Same as MOVE, but with A@ and A! )
( src dst u -- )
: AMOVE
    ( u ) 0 DO
        SWAP DUP I + A@   ( dst src x )
        ROT SWAP OVER I + ( src dst x dst )
        A!                ( src dst )
    LOOP
    2DROP
;
