( Allow cross-compilation of z80c and icore.
  Include this file right before your cross compilation, then
  set XCURRENT to CURRENT and XOFF to H@.

  This redefines defining words to achieve cross compilation.
  The goal is two-fold:

  1. Add an offset to all word references in definitions.
  2. Don't shadow important words we need right now.

  Words overrides like ":", "IMMEDIATE" and "CODE" are not
  automatically shadowed to allow the harmless inclusion of
  this unit. This shadowing has to take place in your xcomp
  configuration.

  See example in /emul/forth/xcomp.fs

  Why limit ourselves to icore? Oh, I've tried cross-compiling
  the whole shebang. I tried. And failed. Too dynamic.
)

VARIABLE XCURRENT
VARIABLE XOFF

: XCON XCURRENT CURRENT* ! ;
: XCOFF 0x02 RAM+ CURRENT* ! ;

: (xentry) XCON (entry) XCOFF ;

: XCODE XCON CODE XCOFF ;

: XIMM XCON IMMEDIATE XCOFF ;

: X:
    (xentry)
    ( 0e == compiledWord )
    [ 0x0e LITN ] C,
    BEGIN
    WORD
    ( cross compile CURRENT )
    XCURRENT @ SWAP     ( w xcur w )
    _find               ( w a f )
    IF
        ( is word )
        ( never supposed to encounter an IMMEDIATE in xdict )
        DUP IMMED? IF ABORT THEN
        ( not an immed. drop backup w and write, with
          offset. )
        DUP XOFF @ > IF XOFF @ - THEN
        ,
    ELSE ( w )
        ( not found? it might be an immediate that isn't yet defined in our
          cross-compiled dict. It's alright, we can find-and-execute it. )
        ( system CURRENT )
        0x02 RAM+ @ SWAP        ( cur w )
        _find                   ( a f )
        IF
            ( found. It *must* be an IMMED )
            DUP IMMED? NOT IF ABORT THEN
            EXECUTE
        ELSE
            ( not found. maybe number )
            (parse*) @ EXECUTE LITN
        THEN
    THEN
    AGAIN
;
