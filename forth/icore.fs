( Inner core. This unit represents core definitions that
  happen right after native definitions. Before core.fs.

  Unlike core.fs and its followers, this unit isn't self-
  sustained. Like native defs it uses the machinery of a
  full Forth interpreter, notably for flow structures.

  Because of that, it has to obey specific rules:

  1. It cannot compile a word from higher layers. Using
     immediates is fine though.
  2. If it references a word from this unit or from native
     definitions, these need to be properly offsetted
     because their offset at compile time are not the same
     as their runtime offsets.
  3. Anything they refer to in the boot binary has to be
     properly stabilized.
  4. Make sure that the words you compile are not overridden
     by the full interpreter.
  5. When using words as immediates, make sure that they're
     not defined in icore or, if they are, make sure that
     they are *not* offsetted

  Those rules are mostly met by the "xcomp" unit, which is
  expected to have been loaded prior to icore and redefines
  ":" and other defining words. So, in other words, when
  compiling icore, ":" doesn't means what you think it means,
  go look in xcomp.
)

: RAM+
    [ RAMSTART LITN ] +
;

: FLAGS 0x08 RAM+ ;
: (parse*) 0x0a RAM+ ;
: HERE 0x04 RAM+ ;
: CURRENT* 0x51 RAM+ ;
: CURRENT CURRENT* @ ;

( w -- a f )
: (find) CURRENT @ SWAP _find ;

: QUIT
    0 FLAGS ! (resRS)
    LIT< INTERPRET (find) DROP EXECUTE
;

: ABORT (resSP) QUIT ;

: = CMP NOT ;
: < CMP -1 = ;
: > CMP 1 = ;

: (parsed)      ( a -- n f )
    ( read first char outside of the loop. it *has* to be
      nonzero. )
    DUP C@                    ( a c )
    DUP NOT IF EXIT THEN      ( a 0 )
    ( special case: do we have a negative? )
    DUP '-' = IF
        ( Oh, a negative, let's recurse and reverse )
        DROP 1 +                  ( a+1 )
        (parsed)                     ( n f )
        SWAP 0 SWAP               ( f 0 n )
        - SWAP EXIT               ( 0-n f )
    THEN
    ( running result, staring at zero )
    0 SWAP                               ( a r c )
    ( Loop over chars )
    BEGIN
    ( parse char )
    '0' -
    ( if bad, return "a 0" )
    DUP 0 < IF 2DROP 0 EXIT THEN   ( bad )
    DUP 9 > IF 2DROP 0 EXIT THEN   ( bad )
    ( good, add to running result )
    SWAP 10 * +                    ( a r*10+n )
    SWAP 1 + SWAP                  ( a+1 r )
    ( read next char )
    OVER C@
    DUP NOT UNTIL
    ( we're done and it's a success. We have "a r c", we want
      "r 1". )
    DROP SWAP DROP 1
;

( This is only the "early parser" in earlier stages. No need
  for an abort message )
: (parse)
    (parsed) NOT IF ABORT THEN
;

: C<
    ( 0c == CINPTR )
    0x0c RAM+ @ EXECUTE
;

: ,
    HERE @ !
    HERE @ 2 + HERE !
;

: C,
    HERE @ C!
    HERE @ 1 + HERE !
;

( The NOT is to normalize the negative/positive numbers to 1
  or 0. Hadn't we wanted to normalize, we'd have written:
  32 CMP 1 - )
: WS? 33 CMP 1 + NOT ;

: TOWORD
    BEGIN
        C< DUP WS? NOT IF EXIT THEN DROP
    AGAIN
;

( Read word from C<, copy to WORDBUF, null-terminate, and
  return, make HL point to WORDBUF. )
: WORD
    ( 0e == WORDBUF )
    0x0e RAM+        ( a )
    TOWORD                   ( a c )
    BEGIN
        ( We take advantage of the fact that char MSB is
          always zero to pre-write our null-termination )
        OVER !            ( a )
        1 +                  ( a+1 )
        C<                   ( a c )
        DUP WS?
    UNTIL
    ( a this point, PS is: a WS )
    ( null-termination is already written )
    2DROP
    0x0e RAM+
;

: SCPY
    BEGIN               ( a )
        DUP C@    ( a c )
        DUP C,    ( a c )
        NOT IF DROP EXIT THEN
        1 +          ( a+1 )
    AGAIN
;

: [entry]
    HERE @       ( w h )
    SWAP SCPY    ( h )
    ( Adjust HERE -1 because SCPY copies the null )
    HERE @ 1 - ( h h' )
    DUP HERE ! ( h h' )
    SWAP -       ( sz )
    ( write prev value )
    HERE @ CURRENT @ - ,
    ( write size )
    C,
    HERE @ CURRENT !
;

: (entry) WORD [entry] ;

: INTERPRET
    BEGIN
    WORD
    (find)
    IF
        1 FLAGS !
        EXECUTE
        0 FLAGS !
    ELSE
        (parse*) @ EXECUTE
    THEN
    AGAIN
;

( system c< simply reads source from binary, starting at
  LATEST. Convenient way to bootstrap a new system. )
: (boot<)
    ( 2e == BOOT C< PTR )
    0x2e RAM+ @   ( a )
    DUP C@        ( a c )
    SWAP 1 +      ( c a+1 )
    0x2e RAM+ !   ( c )
;

: BOOT
    0x02 RAM+ CURRENT* !
    LIT< (parse) (find) DROP (parse*) !
    ( 2e == SYSTEM SCRATCHPAD )
    CURRENT @ 0x2e RAM+ !
    ( 0c == CINPTR )
    LIT< (boot<) (find) DROP 0x0c RAM+ !
    LIT< INIT (find)
    IF EXECUTE
    ELSE DROP INTERPRET THEN
;

( LITN has to be defined after the last immediate usage of
  it to avoid bootstrapping issues )
: LITN
    ( 32 == NUMBER )
    32 , ,
;

: IMMED? 1 - C@ 0x80 AND ;

( ';' can't have its name right away because, when created, it
  is not an IMMEDIATE yet and will not be treated properly by
  xcomp. )
: _
    ['] EXIT ,
    R> DROP     ( exit : )
; IMMEDIATE

XCURRENT @ ( to PSP )

: :
    (entry)
    ( We cannot use LITN as IMMEDIATE because of bootstrapping
      issues. Same thing for ",".
      32 == NUMBER 14 == compiledWord )
    [ 32 H@ ! 2 ALLOT 14 H@ ! 2 ALLOT ] ,
    BEGIN
    WORD
    (find)
    ( is word )
    IF DUP IMMED? IF EXECUTE ELSE , THEN
    ( maybe number )
    ELSE (parse*) @ EXECUTE LITN THEN
    AGAIN
;

( from PSP ) ';' SWAP 4 - C!
