( Configuration words: RAMSTART, RS_ADDR )
H@ 256 /MOD 2 PC! 2 PC!

( STABLE ABI
  Those jumps below are supposed to stay at these offsets,
  always. If they change bootstrap binaries have to be
  adjusted because they rely on them. Those entries are
  referenced directly by their offset in Forth code with a
  comment indicating what that number refers to.
)

H@ ORG !

0 JPnn,           ( 00, main )
0 JPnn,           ( 03, find )
NOP, NOP,         ( 06, unused )
NOP, NOP,         ( 08, LATEST )
NOP,              ( 0a, unused )
0 JPnn,           ( 0b, cellWord )
0 JPnn,           ( 0e, compiledWord )
0 JPnn,           ( 11, pushRS )
0 JPnn,           ( 14, popRS )
JP(IY), NOP,      ( 17, nativeWord )
0 JPnn,           ( 1a, next )
0 JPnn,           ( 1d, chkPS )
NOP, NOP,         ( 20, numberWord )
NOP, NOP,         ( 22, litWord )
NOP, NOP,         ( 24, addrWord )
NOP, NOP,         ( 26, unused )
0 JPnn,           ( 28, flagsToBC )
0 JPnn,           ( 2b, doesWord )
NOP, NOP,         ( 2e, unused )
RAMSTART 0x4e + JPnn, ( RST 30 )
NOP, NOP, NOP,    ( unused )
NOP, NOP,         ( unused )
RAMSTART 0x4e + JPnn, ( RST 38 )
NOP,              ( unused )

( BOOT DICT
  There are only 5 words in the boot dict, but these words'
  offset need to be stable, so they're part of the "stable
  ABI"
)
'E' A, 'X' A, 'I' A, 'T' A,
0 A,,   ( prev )
4 A,
L1 BSET ( EXIT )
    0x17 A,,         ( nativeWord )
    0x14 CALLnn,     ( popRS )
    RAMSTART 0x06 + LD(nn)HL, ( RAMSTART+0x06 == IP )
    JPNEXT,

NOP, NOP, NOP,   ( unused )

'(' A, 'b' A, 'r' A, ')' A,
PC L1 @ - A,, ( prev )
4 A,
L1 BSET ( BR )
    0x17 A,,         ( nativeWord )
L2 BSET ( used in CBR )
    RAMSTART 0x06 + LDHL(nn), ( RAMSTART+0x06 == IP )
    E (HL) LDrr,
    HL INCss,
    D (HL) LDrr,
    HL DECss,
    DE ADDHLss,
    RAMSTART 0x06 + LD(nn)HL, ( RAMSTART+0x06 == IP )
    JPNEXT,

'(' A, '?' A, 'b' A, 'r' A, ')' A,
PC L1 @ - A,, ( prev )
5 A,
L1 BSET ( CBR )
    0x17 A,,         ( nativeWord )
    HL POPqq,
    chkPS,
    A H LDrr,
    L ORr,
    JRZ, L2 BWR ( BR + 2. False, branch )
    ( True, skip next 2 bytes and don't branch )
    RAMSTART 0x06 + LDHL(nn), ( RAMSTART+0x06 == IP )
    HL INCss,
    HL INCss,
    RAMSTART 0x06 + LD(nn)HL, ( RAMSTART+0x06 == IP )
    JPNEXT,

'E' A, 'X' A, 'E' A, 'C' A, 'U' A, 'T' A, 'E' A,
PC L1 @ - A,, ( prev )
7 A,
H@ XCURRENT !        ( set current tip of dict )
L2 BSET ( used frequently below )
    0x17 A,,         ( nativeWord )
    IY POPqq,        ( is a wordref )
    chkPS,
    L 0 IY+ LDrIXY,
    H 1 IY+ LDrIXY,
    ( HL points to code pointer )
    IY INCss,
    IY INCss,
    ( IY points to PFA )
    JP(HL),

( END OF STABLE ABI )

( Name of BOOT word )
L1 BSET
'B' A, 'O' A, 'O' A, 'T' A, 0 A,

PC ORG @ 1 + ! ( main )
( STACK OVERFLOW PROTECTION:
  To avoid having to check for stack underflow after each pop
  operation (which can end up being prohibitive in terms of
  costs), we give ourselves a nice 6 bytes buffer. 6 bytes
  because we seldom have words requiring more than 3 items
  from the stack. Then, at each "exit" call we check for
  stack underflow.
)
    SP 0xfffa LDddnn,
    RAMSTART SP LD(nn)dd, ( RAM+00 == INITIAL_SP )
    IX RS_ADDR LDddnn,
( HERE begins at RAMEND )
    HL RAMSTART 0x80 + LDddnn,
    RAMSTART 0x04 + LD(nn)HL, ( RAM+04 == HERE )
( LATEST is a label to the latest entry of the dict. It is
  written at offset 0x08 by the process or person building
  Forth. )
    0x08 LDHL(nn),
    RAMSTART 0x02 + LD(nn)HL, ( RAM+02 == CURRENT )
    EXDEHL,
    HL L1 @ LDddnn,
    0x03 CALLnn,        ( 03 == find )
    DE PUSHqq,
    L2 @ 2 + JPnn,

PC ORG @ 4 + ! ( find )
( Find the entry corresponding to word name where (HL) points
  to in dictionary having its tip at DE and sets DE to point
  to that entry. Z if found, NZ if not.
)

    BC PUSHqq,
    HL PUSHqq,
	( First, figure out string len )
    BC 0 LDddnn,
    A XORr,
    CPIR,
	( C has our length, negative, -1 )
    A C LDrr,
    NEG,
    A DECr,
	( special case. zero len? we never find anything. )
    JRZ, L1 FWR ( fail )

    C A LDrr, ( C holds our length )
( Let's do something weird: We'll hold HL by the *tail*.
  Because of our dict structure and because we know our
  lengths, it's easier to compare starting from the end.
  Currently, after CPIR, HL points to char after null. Let's
  adjust. Because the compare loop pre-decrements, instead
  of DECing HL twice, we DEC it once. )
    HL DECss,
BEGIN, ( inner )
    ( DE is a wordref, first step, do our len correspond? )
    HL PUSHqq,          ( --> lvl 1 )
    DE PUSHqq,          ( --> lvl 2 )
    DE DECss,
    LDA(DE),
    0x7f ANDn,          ( remove IMMEDIATE flag )
    C CPr,
    JRNZ, L4 FWR ( loopend )
    ( match, let's compare the string then )
    DE DECss, ( Skip prev field. One less because we )
    DE DECss, ( pre-decrement )
    B C LDrr, ( loop C times )
BEGIN, ( loop )
    ( pre-decrement for easier Z matching )
    DE DECss,
    HL DECss,
    LDA(DE),
    (HL) CPr,
    JRNZ, L3 FWR ( loopend )
    DJNZ, AGAIN, ( loop )
L4 FSET L3 FSET ( loopend )
( At this point, Z is set if we have a match. In all cases,
  we want to pop HL and DE )
    DE POPqq,           ( <-- lvl 2 )
    HL POPqq,           ( <-- lvl 1 )
    JRZ, L4 FWR ( end, match? we're done! )
    ( no match, go to prev and continue )
    HL PUSHqq,          ( --> lvl 1 )
    DE DECss,
    DE DECss,
    DE DECss,           ( prev field )
    DE PUSHqq,          ( --> lvl 2 )
    EXDEHL,
    E (HL) LDrr,
    HL INCss,
    D (HL) LDrr,
    ( DE conains prev offset )
    HL POPqq,           ( <-- lvl 2 )
    ( HL is prev field's addr. Is offset zero? )
    A D LDrr,
    E ORr,
    IFZ, ( noprev )
        ( get absolute addr from offset )
        ( carry cleared from "or e" )
        DE SBCHLss,
        EXDEHL,             ( result in DE )
    THEN, ( noprev )
    HL POPqq,           ( <-- lvl 1 )
    JRNZ, AGAIN, ( inner, try to match again )
    ( Z set? end of dict, unset Z )
L1 FSET ( fail )
    A XORr,
    A INCr,
L4 FSET ( end )
    HL POPqq,
    BC POPqq,
    RET,

PC ORG @ 0x29 + ! ( flagsToBC )
    BC 0 LDddnn,
    CZ RETcc, ( equal )
    BC INCss,
    CC RETcc, ( > )
    ( < )
    BC DECss,
    BC DECss,
    RET,

PC ORG @ 0x12 + ! ( pushRS )
    IX INCss,
    IX INCss,
    0 IX+ L LDIXYr,
    1 IX+ H LDIXYr,
    RET,

PC ORG @ 0x15 + ! ( popRS )
    L 0 IX+ LDrIXY,
    H 1 IX+ LDrIXY,
    IX DECss,
    IX DECss,
    RET,

'(' A, 'u' A, 'f' A, 'l' A, 'w' A, ')' A, 0 A,
L1 BSET ( abortUnderflow )
    HL PC 7 - LDddnn,
    DE RAMSTART 0x02 + LDdd(nn),   ( RAM+02 == CURRENT )
    0x03 CALLnn, ( find )
    DE PUSHqq,
    L2 @ 2 + JPnn, ( EXECUTE, skip nativeWord )


PC ORG @ 0x1e + ! ( chkPS )
    HL PUSHqq,
    RAMSTART LDHL(nn), ( RAM+00 == INITIAL_SP )
( We have the return address for this very call on the stack
  and protected registers. Let's compensate )
    HL DECss,
    HL DECss,
    HL DECss,
    HL DECss,
    SP SUBHLss,
    HL POPqq,
    CNC RETcc,      ( INITIAL_SP >= SP? good )
    JR, L1 BWR ( abortUnderflow )

L3 BSET ( chkRS )
    IX PUSHqq, HL POPqq,
    DE RS_ADDR LDddnn,
    DE SUBHLss,
    CNC RETcc,      ( IX >= RS_ADDR? good )
    JR, L1 BWR ( abortUnderflow )


PC ORG @ 0x1b + ! ( next )
( This routine is jumped to at the end of every word. In it,
  we jump to current IP, but we also take care of increasing
  it by 2 before jumping. )
	( Before we continue: are stacks within bounds? )
    0x1d CALLnn, ( chkPS )
    L3 @ CALLnn, ( chkRS )
    DE RAMSTART 0x06 + LDdd(nn), ( RAMSTART+0x06 == IP )
    H D LDrr,
    L E LDrr,
    DE INCss,
    DE INCss,
    RAMSTART 0x06 + DE LD(nn)dd, ( RAMSTART+0x06 == IP )
	( HL is an atom list pointer. We need to go into it to
      have a wordref )
    E (HL) LDrr,
    HL INCss,
    D (HL) LDrr,
    DE PUSHqq,
    L2 @ 2 + JPnn, ( EXECUTE, skip nativeWord )

( WORD ROUTINES )

PC ORG @ 0x0f + ! ( compiledWord )
( Execute a list of atoms, which always end with EXIT.
  IY points to that list. What do we do:
  1. Push current IP to RS
  2. Set new IP to the second atom of the list
  3. Execute the first atom of the list. )
    RAMSTART 0x06 + LDHL(nn), ( RAMSTART+0x06 == IP )
    0x11 CALLnn,     ( 11 == pushRS )
    IY PUSHqq, HL POPqq,
    HL INCss,
    HL INCss,
    RAMSTART 0x06 + LD(nn)HL, ( RAMSTART+0x06 == IP )
	( IY still is our atom reference )
    L 0 IY+ LDrIXY,
    H 1 IY+ LDrIXY,
    HL PUSHqq,      ( arg for EXECUTE )
    L2 @ 2 + JPnn, ( EXECUTE, skip nativeWord )

PC ORG @ 0x0c + ! ( cellWord )
( Pushes the PFA directly )
    IY PUSHqq,
    JPNEXT,

PC ORG @ 0x2c + ! ( doesWord )
( The word was spawned from a definition word that has a
  DOES>. PFA+2 (right after the actual cell) is a link to the
  slot right after that DOES>. Therefore, what we need to do
  push the cell addr like a regular cell, then follow the
  linkfrom the PFA, and then continue as a regular
  compiledWord.
)
    IY PUSHqq, ( like a regular cell )
    L 2 IY+ LDrIXY,
    H 3 IY+ LDrIXY,
    HL PUSHqq, IY POPqq,
    0x0e JPnn, ( 0e == compiledWord )


( addrWord is the exact same thing as a numberWord except that
  it is treated differently by meta-tools. See notes.txt )
PC ORG @ 0x20 + ! ( numberWord )
PC ORG @ 0x24 + ! ( addrWord )
( This is not a word, but a number literal. This works a bit
  differently than others: PF means nothing and the actual
  number is placed next to the numberWord reference in the
  compiled word list. What we need to do to fetch that number
  is to play with the IP.
)
    RAMSTART 0x06 + LDHL(nn), ( RAMSTART+0x06 == IP )
    E (HL) LDrr,
    HL INCss,
    D (HL) LDrr,
    HL INCss,
    RAMSTART 0x06 + LD(nn)HL, ( RAMSTART+0x06 == IP )
    DE PUSHqq,
    JPNEXT,

PC ORG @ 0x22 + ! ( litWord )
( Similarly to numberWord, this is not a real word, but a
  string literal. Instead of being followed by a 2 bytes
  number, it's followed by a null-terminated string. When
  called, puts the string's address on PS )
    RAMSTART 0x06 + LDHL(nn), ( RAMSTART+0x06 == IP )
    HL PUSHqq,
    ( skip to null char )
    A XORr, ( look for null )
    B A LDrr,
    C A LDrr,
    CPIR,
	( CPIR advances HL regardless of comparison, so goes one
      char after NULL. This is good, because that's what we
      want... )
    RAMSTART 0x06 + LD(nn)HL, ( RAMSTART+0x06 == IP )
    JPNEXT,
