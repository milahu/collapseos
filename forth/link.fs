( depends: cmp, parse
  Relink a dictionary by applying offsets to all word
  references in words of the "compiled" type.

  A typical usage of this unit would be to, right after a
  bootstrap-from-icore-from-source operation, identify the
  root word of the source part, probably "H@", and run
  " ' thatword RLDICT ". Then, take the resulting relinked
  binary, concatenate it to the boot binary, and write to
  boot media.

  LIMITATIONS

  This unit can't automatically detect all offsets needing
  relinking. This is a list of situations that aren't handled:

  Cells: It's not possible to know for sure whether a cellWord
  contains an address or a number. They are therefore not
  automatically relinked. You have to manually relink each of
  them with RLCELL. In the case of a DOES> word, PFA+2, which
  is always an offset, is automatically relinked, but not
  PFA+0.
)

( Skip atom, considering special atom types. )
( a -- a+n )
: ASKIP
    DUP @       ( a n )
    ( ?br or br or NUMBER )
    DUP <>{ 0x67 &= 0x53 |= 0x20 |= 0x24 |= <>}
    IF DROP 4 + EXIT THEN
    ( regular word )
    0x22 = NOT IF 2+ EXIT THEN
    ( it's a lit, skip to null char )
    ( a )
    1+ ( we skip by 2, but the loop below is pre-inc... )
    BEGIN 1+ DUP C@ NOT UNTIL
    ( skip null char )
    1+
;

( Get word addr, starting at name's address )
: '< ' DUP WHLEN - ;

( Relink atom at a, applying offset o with limit ol.
  Returns a, appropriately skipped.
)
( a o ol -- a+n )
: RLATOM
    ROT             ( o ol a )
    DUP @           ( o ol a n )
    DUP 0x24 = IF
        ( 0x24 is an addrWord, which should be offsetted in
          the same way that a regular word would. To achieve
          this, we skip ASKIP and instead of skipping 4 bytes
          like a numberWord, we skip only 2, which means that
          our number will be treated like a regular wordref.
        )
        DROP
        2+          ( o ol a+2 )
        ROT ROT 2DROP ( a )
        EXIT
    THEN
    ROT             ( o a n ol )
    < IF ( under limit, do nothing )
        SWAP DROP    ( a )
    ELSE
        ( o a )
        SWAP OVER @ ( a o n )
        -^          ( a n-o )
        OVER !      ( a )
    THEN
    ASKIP
;

( Relink a word with specified offset. If it's not of the type
  "compiled word", ignore. If it is, advance in word until a2
  is met, and for each word that is above ol, reduce that
  reference by o.
  Arguments: a1: wordref a2: word end addr o: offset to apply
             ol: offset limit. don't apply on refs under it.
)
( ol o a1 a2 -- )
: RLWORD
    SWAP DUP C@             ( ol o a2 a1 n )
    ( 0e == compiledWord, 2b == doesWord )
    DUP <>{ 0x0e &= 0x2b |= <>} NOT IF
        ( unwind all args )
        2DROP 2DROP
        EXIT
    THEN
    ( we have a compiled word or doesWord, proceed )
    ( doesWord is processed exactly like a compiledWord, but
      starts 2 bytes further. )
    ( ol o a2 a1 n )
    0x2b = IF 2+ THEN
    ( ol o a2 a1 )
    1+                          ( ol o a2 a1+1 )
    BEGIN                       ( ol o a2 a1 )
        2OVER                   ( ol o a2 a1 ol o )
        SWAP                    ( ol o a2 a1 o ol )
        RLATOM                  ( ol o a2 a+n )
        2DUP < IF ABORT THEN    ( Something is very wrong )
        2DUP =                  ( ol o a2 a+n f )
        IF
            ( unwind )
            2DROP 2DROP
            EXIT
        THEN
    AGAIN
;

( TODO implement RLCELL )

( Copy dict from target wordref, including header, up to HERE.
  We're going to compact the space between that word and its
  prev word. To do this, we're copying this whole memory area
  in HERE and then iterate through that copied area and call
  RLWORD on each word. That results in a dict that can be
  concatenated to target's prev entry in a more compact way.

  This copy of data doesn't allocate anything, so H@ doesn't
  move. Moreover, we reserve 4 bytes at H@ to write our target
  and offset because otherwise, things get too complicated
  with the PSP.

  The output of this word is 3 numbers: top copied address,
  top copied CURRENT, and then the beginning of the copied dict
  at the end to indicate that we're finished processing.
)
( target -- )
: RLDICT
    ( First of all, let's get our offset. It's easy, it's
      target's prev field, which is already an offset, minus
      its name length. We expect, in RLDICT that a target's
      prev word is a "hook word", that is, an empty word. )
    ( H@ == target )
    DUP H@ !
    DUP 1- C@ 0x7f AND          ( t namelen )
    SWAP 3 - @                  ( namelen po )
    -^                          ( o )
    ( H@+2 == offset )
    H@ 2+ !                     ( )
    ( We have our offset, now let's copy our memory chunk )
    H@ @ DUP WHLEN -            ( src )
    DUP H@ -^                   ( src u )
    DUP ROT SWAP                ( u src u )
    H@ 4 +                      ( u src u dst )
    SWAP                        ( u src dst u )
    MOVE                        ( u )
    ( Now, let's iterate that dict down )
    ( wr == wordref we == word end )
    ( To get our wr and we, we use H@ and CURRENT, which we
      offset by u+4. +4 before, remember, we're using 4 bytes
      as variable space. )
    4 +                         ( u+4 )
    DUP H@ +                    ( u we )
    DUP .X CRLF
    SWAP CURRENT @ +            ( we wr )
    DUP .X CRLF
    BEGIN                       ( we wr )
        DUP ROT                 ( wr wr we )
        ( call RLWORD. we need a sig: ol o wr we )
        H@ @                    ( wr wr we ol )
        H@ 2+ @                 ( wr wr we ol o )
        2SWAP                   ( wr ol o wr we )
        RLWORD                  ( wr )
        ( wr becomes wr's prev and we is wr-header )
        DUP                     ( wr wr )
        PREV                    ( oldwr newwr )
        SWAP                    ( wr oldwr )
        DUP WHLEN -             ( wr we )
        SWAP                    ( we wr )
        ( Are we finished? We're finished if wr-4 <= H@ )
        DUP 4 - H@ <=
    UNTIL
    H@ 4 + .X CRLF
;

( Relink a regular Forth full interpreter. )
: RLCORE
    LIT< H@ (find) DROP RLDICT
;
