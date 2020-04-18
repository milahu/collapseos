: H@ HERE @ ;
: IMMEDIATE
    CURRENT @ 1-
    DUP C@ 128 OR SWAP C!
;
: [ INTERPRET 1 FLAGS ! ; IMMEDIATE
: ] R> DROP ;
: LITS 34 , SCPY ;
: LIT< WORD LITS ; IMMEDIATE
: LITA 36 , , ;
: '
    WORD (find) (?br) [ 4 , ] EXIT
    LIT< (wnf) (find) DROP EXECUTE
;
: ['] ' LITA ; IMMEDIATE
: COMPILE ' LITA ['] , , ; IMMEDIATE
: [COMPILE] ' , ; IMMEDIATE
: BEGIN H@ ; IMMEDIATE
: AGAIN COMPILE (br) H@ - , ; IMMEDIATE
: UNTIL COMPILE (?br) H@ - , ; IMMEDIATE
: ( BEGIN LIT< ) WORD S= UNTIL ; IMMEDIATE
( Hello, hello, krkrkrkr... do you hear me?
  Ah, voice at last! Some lines above need comments
  BTW: Forth lines limited to 64 cols because of default
  input buffer size in Collapse OS

  "_": words starting with "_" are meant to be "private",
  that is, only used by their immediate surrondings.

  LITS: 34 == litWord
  LITA: 36 == addrWord
  COMPILE: Tough one. Get addr of caller word (example above
  (br)) and then call LITA on it. )

: +! SWAP OVER @ + SWAP ! ;
: -^ SWAP - ;
: ALLOT HERE +! ;

: IF                ( -- a | a: br cell addr )
    COMPILE (?br)
    H@              ( push a )
    2 ALLOT         ( br cell allot )
; IMMEDIATE

: THEN              ( a -- | a: br cell addr )
    DUP H@ -^ SWAP   ( a-H a )
    !
; IMMEDIATE

: ELSE              ( a1 -- a2 | a1: IF cell a2: ELSE cell )
    COMPILE (br)
    2 ALLOT
    DUP H@ -^ SWAP  ( a-H a )
    !
    H@ 2-          ( push a. -2 for allot offset )
; IMMEDIATE

( During a CASE, the stack grows by 1 at each ENDOF so that
  we can fill all those ENDOF branching addrs. So that we
  know when to stop, we put a 0 on PSP. That's our stopgap. )
: CASE 0 ; IMMEDIATE
: OF
    COMPILE OVER COMPILE =
    [COMPILE] IF
; IMMEDIATE
: ENDOF [COMPILE] ELSE ; IMMEDIATE

( At this point, we have something like "0 e1 e2 e3 val". We
  want top drop val, and then call THEN as long as we don't
  hit 0. )
: ENDCASE
    BEGIN
        DUP NOT IF DROP EXIT THEN
        [COMPILE] THEN
    AGAIN
    COMPILE DROP
; IMMEDIATE

: CREATE
    (entry)            ( empty header with name )
    11                 ( 11 == cellWord )
    C,                 ( write it )
;

( We run this when we're in an entry creation context. Many
  things we need to do.
  1. Change the code link to doesWord
  2. Leave 2 bytes for regular cell variable.
  3. Write down RS' RTOS to entry.
  4. exit parent definition
)
: DOES>
    ( Overwrite cellWord in CURRENT )
    ( 43 == doesWord )
    43 CURRENT @ C!
    ( When we have a DOES>, we forcefully place HERE to 4
      bytes after CURRENT. This allows a DOES word to use ","
      and "C," without messing everything up. )
    CURRENT @ 3 + HERE !
    ( HERE points to where we should write R> )
    R> ,
    ( We're done. Because we've popped RS, we'll exit parent
      definition )
;

: VARIABLE CREATE 2 ALLOT ;
: CONSTANT CREATE , DOES> @ ;
: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

( In addition to pushing H@ this compiles 2 >R so that loop
  variables are sent to PS at runtime )
: DO
    COMPILE SWAP COMPILE >R COMPILE >R
    H@
; IMMEDIATE

( Increase loop counter and returns whether we should loop. )
: _
    R>          ( IP, keep for later )
    R> 1+       ( ip i+1 )
    DUP >R      ( ip i )
    I' =        ( ip f )
    SWAP >R     ( f )
;

( One could think that we should have a sub word to avoid all
  these COMPILE, but we can't because otherwise it messes with
  the RS )
: LOOP
    COMPILE _ COMPILE (?br)
    H@ - ,
    COMPILE R> COMPILE DROP COMPILE R> COMPILE DROP
; IMMEDIATE

: LEAVE R> R> DROP I 1- >R >R ;

( a1 a2 u -- )
: MOVE
    ( u ) 0 DO
        SWAP DUP I + C@   ( a2 a1 x )
        ROT SWAP OVER I + ( a1 a2 x a2 )
        C!                ( a1 a2 )
    LOOP
    2DROP
;

: DELW
    1- 0 SWAP C!
;

: PREV
    3 - DUP @                   ( a o )
    -                           ( a-o )
;

: WORD(
    DUP 1- C@   ( name len field )
    127 AND     ( 0x7f. remove IMMEDIATE flag )
    3 +         ( fixed header len )
    -
;

: FORGET
    ' DUP               ( w w )
    ( HERE must be at the end of prev's word, that is, at the
      beginning of w. )
    WORD( HERE !  ( w )
    PREV CURRENT !
;

: EMPTY
    LIT< _sys (find) NOT IF ABORT THEN
    DUP HERE ! CURRENT !
;

( Drop RSP until I-2 == INTERPRET. )
: EXIT!
    ['] INTERPRET       ( I )
    BEGIN               ( I )
    DUP                 ( I I )
    R> DROP I 2- @     ( I I a )
    = UNTIL
    DROP
;

( a -- a+1 c )
: C@+ DUP C@ SWAP 1+ SWAP ;
