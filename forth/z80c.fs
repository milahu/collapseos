( Core words in z80. This requires a full Forth interpreter
  to run, but is also necessary for core.fs. This means that
  it needs to be compiled from a prior bootstrapped binary.

  This stage is tricky due to the fact that references in
  Forth are all absolute, except for prev word refs. This
  means that there are severe limitations to the kind of code
  you can put here. Those limitations are the same as those
  described in icore.fs.

  Oh, also: KEY and EMIT are not defined here. There're
  expected to be defined in platform-specific code.

  This unit expects the same conf as boot.fs.
)

CODE EXECUTE
    DE POPqq,
    chkPS,
    0x33 JPnn,      ( 33 == execute )

( a b c -- b c a )
CODE ROT
    HL POPqq,       ( C )
    DE POPqq,       ( B )
    BC POPqq,       ( A )
    chkPS,
    DE PUSHqq,      ( B )
    HL PUSHqq,      ( C )
    BC PUSHqq,      ( A )
;CODE

( a -- a a )
CODE DUP
    HL POPqq,       ( A )
    chkPS,
    HL PUSHqq,      ( A )
    HL PUSHqq,      ( A )
;CODE

( a -- )
CODE DROP
    HL POPqq,
;CODE

( a b -- b a )
CODE SWAP
    HL POPqq,       ( B )
    DE POPqq,       ( A )
    chkPS,
    HL PUSHqq,      ( B )
    DE PUSHqq,      ( A )
;CODE

( a b -- a b a )
CODE OVER
    HL POPqq,       ( B )
    DE POPqq,       ( A )
    chkPS,
    DE PUSHqq,      ( A )
    HL PUSHqq,      ( B )
    DE PUSHqq,      ( A )
;CODE

( a b -- a b a b )
CODE 2DUP
    HL POPqq,       ( B )
    DE POPqq,       ( A )
    chkPS,
    DE PUSHqq,      ( A )
    HL PUSHqq,      ( B )
    DE PUSHqq,      ( A )
    HL PUSHqq,      ( B )
;CODE

( a b -- )
CODE 2DROP
    HL POPqq,
    HL POPqq,
;CODE

( a b c d -- a b c d a b )

CODE 2OVER
    HL POPqq,       ( D )
    DE POPqq,       ( C )
    BC POPqq,       ( B )
    IY POPqq,       ( A )
    chkPS,
    IY PUSHqq,      ( A )
    BC PUSHqq,      ( B )
    DE PUSHqq,      ( C )
    HL PUSHqq,      ( D )
    IY PUSHqq,      ( A )
    BC PUSHqq,      ( B )
;CODE

( a b c d -- c d a b )

CODE 2SWAP
    HL POPqq,       ( D )
    DE POPqq,       ( C )
    BC POPqq,       ( B )
    IY POPqq,       ( A )
    chkPS,
    DE PUSHqq,      ( C )
    HL PUSHqq,      ( D )
    IY PUSHqq,      ( A )
    BC PUSHqq,      ( B )
;CODE

CODE AND
    HL POPqq,
    DE POPqq,
    chkPS,
    A E LDrr,
    L ANDr,
    L A LDrr,
    A D LDrr,
    H ANDr,
    H A LDrr,
    HL PUSHqq,
;CODE

CODE OR
    HL POPqq,
    DE POPqq,
    chkPS,
    A E LDrr,
    L ORr,
    L A LDrr,
    A D LDrr,
    H ORr,
    H A LDrr,
    HL PUSHqq,
;CODE

CODE XOR
    HL POPqq,
    DE POPqq,
    chkPS,
    A E LDrr,
    L XORr,
    L A LDrr,
    A D LDrr,
    H XORr,
    H A LDrr,
    HL PUSHqq,
;CODE

CODE NOT
    HL POPqq,
    chkPS,
    A L LDrr,
    H ORr,
    HL 0 LDddnn,
    IFZ,
        ( false, make 1 )
        HL INCss,
    THEN,
    HL PUSHqq,
;CODE

CODE +
    HL POPqq,
    DE POPqq,
    chkPS,
    DE ADDHLss,
    HL PUSHqq,
;CODE

CODE -
    DE POPqq,
    HL POPqq,
    chkPS,
    DE SUBHLss,
    HL PUSHqq,
;CODE

CODE *
    DE POPqq,
    BC POPqq,
    chkPS,
	( DE * BC -> DE (high) and HL (low) )
    HL 0 LDddnn,
    A 0x10 LDrn,
( loop )
    HL ADDHLss,
    E RLr,
    D RLr,
    JRNC, 4 A, ( noinc )
    BC ADDHLss,
    JRNC, 1 A, ( noinc )
    DE INCss,
( noinc )
    A DECr,
    JRNZ, -14 A, ( loop )
    HL PUSHqq,
;CODE

( Borrowed from http://wikiti.brandonw.net/ )
( Divides AC by DE and places the quotient in AC and the
  remainder in HL )
CODE /MOD
    DE POPqq,
    BC POPqq,
    chkPS,
    A B LDrr,
    B 16 LDrn,
    HL 0 LDddnn,
    BEGIN, ( loop )
        SCF,
        C RLr,
        RLA,
        HL ADCHLss,
        DE SBCHLss,
        IFC,
            DE ADDHLss,
            C DECr,
        THEN,
    DJNZ, AGAIN, ( loop )
    B A LDrr,
    HL PUSHqq,
    BC PUSHqq,
;CODE

CODE !
    HL POPqq,
    DE POPqq,
    chkPS,
    (HL) E LDrr,
    HL INCss,
    (HL) D LDrr,
;CODE

CODE @
    HL POPqq,
    chkPS,
    E (HL) LDrr,
    HL INCss,
    D (HL) LDrr,
    DE PUSHqq,
;CODE

CODE C!
    HL POPqq,
    DE POPqq,
    chkPS,
    (HL) E LDrr,
;CODE

CODE C@
    HL POPqq,
    chkPS,
    L (HL) LDrr,
    H 0 LDrn,
    HL PUSHqq,
;CODE

CODE PC!
    BC POPqq,
    HL POPqq,
    chkPS,
    L OUT(C)r,
;CODE

CODE PC@
    BC POPqq,
    chkPS,
    H 0 LDrn,
    L INr(C),
    HL PUSHqq,
;CODE

CODE I
    L 0 IX+ LDrIXY,
    H 1 IX+ LDrIXY,
    HL PUSHqq,
;CODE

CODE I'
    L 2 IX- LDrIXY,
    H 1 IX- LDrIXY,
    HL PUSHqq,
;CODE

CODE J
    L 4 IX- LDrIXY,
    H 3 IX- LDrIXY,
    HL PUSHqq,
;CODE

CODE >R
    HL POPqq,
    chkPS,
    ( 17 == pushRS )
    17 CALLnn,
;CODE

CODE R>
    ( 20 == popRS )
    20 CALLnn,
    HL PUSHqq,
;CODE

CODE BYE
    HALT,
;CODE

CODE (resSP)
    ( INITIAL_SP == RAM+0 )
    SP RAMSTART LDdd(nn),
;CODE

CODE (resRS)
    IX RS_ADDR LDddnn,
;CODE

CODE S=
    DE  POPqq,
    HL  POPqq,
    chkPS,
    ( pre-push false )
    BC 0 LDddnn,
    BC PUSHqq,
    BEGIN, ( loop )
        LDA(DE),
        (HL) CPr,
        JRNZ, L1 FWR ( not equal? break early to "end".
                       NZ is set. )
        A ORr,   ( if our char is null, stop )
        HL INCss,
        DE INCss,
    JRNZ, AGAIN, ( loop )
    ( success, change false to true )
    HL POPqq,
    HL INCss,
    HL PUSHqq,
L1 FSET ( end )
;CODE

CODE CMP
    HL  POPqq,
    DE  POPqq,
    chkPS,
    DE SUBHLss,
    BC 0 LDddnn,
    IFNZ,
        ( not equal )
        BC INCss,
        IFNC,
            ( < )
            BC DECss,
            BC DECss,
        THEN,
    THEN,
    BC PUSHqq,
;CODE

( cur w -- a f )
CODE _find
    HL POPqq,       ( w )
    DE POPqq,       ( cur )
    chkPS,
    ( 3 == find )
    3 CALLnn,
    IFNZ,
        ( not found )
        HL PUSHqq,
        DE 0 LDddnn,
        DE PUSHqq,
        JPNEXT,
    THEN,
    ( found )
    DE PUSHqq,
    DE 1 LDddnn,
    DE PUSHqq,
;CODE

CODE (im1)
    IM1,
    EI,
;CODE
