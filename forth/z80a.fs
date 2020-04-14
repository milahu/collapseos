( Z80 assembler )

: Z80AMEM+ 0x59 RAM+ @ + ;

( H@ offset at which we consider our PC 0. Used to compute
  PC. To have a proper PC, call  "H@ ORG !" at the beginning
  of your assembly process. )
: ORG 0 Z80AMEM+ ;

( Labels are a convenient way of managing relative jump
  calculations. Backward labels are easy. It is only a matter
  or recording "HERE" and do subtractions. Forward labels
  record the place where we should write the offset, and then
  when we get to that point later on, the label records the
  offset there.

  To avoid using dict memory in compilation targets, we
  pre-declare label variables here, which means we have a
  limited number of it. For now, 4 ought to be enough. )
: L1 2 Z80AMEM+ ;
: L2 4 Z80AMEM+ ;
: L3 6 Z80AMEM+ ;
: L4 8 Z80AMEM+ ;

: Z80A$
    ( 59 == z80a's memory )
    H@ 0x59 RAM+ !
    10 ALLOT
;

( Splits word into msb/lsb, lsb being on TOS )
: SPLITB
    256 /MOD SWAP
;


: PC H@ ORG @ - ;

( A, spits an assembled byte, A,, spits an assembled word
  Both increase PC. To debug, change C, to .X )
: A, C, ;
: A,, SPLITB A, A, ;

( "r" register constants )
7 CONSTANT A
0 CONSTANT B
1 CONSTANT C
2 CONSTANT D
3 CONSTANT E
4 CONSTANT H
5 CONSTANT L
6 CONSTANT (HL)

( "ss" register constants )
0 CONSTANT BC
1 CONSTANT DE
2 CONSTANT HL
3 CONSTANT AF
3 CONSTANT SP

( "cc" condition constants )
0 CONSTANT CNZ
1 CONSTANT CZ
2 CONSTANT CNC
3 CONSTANT CC
4 CONSTANT CPO
5 CONSTANT CPE
6 CONSTANT CP
7 CONSTANT CM

( As a general rule, IX and IY are equivalent to spitting an
  extra 0xdd / 0xfd and then spit the equivalent of HL )
: IX 0xdd A, HL ;
: IY 0xfd A, HL ;
: _ix+- 0xff AND 0xdd A, (HL) ;
: _iy+- 0xff AND 0xfd A, (HL) ;
: IX+ _ix+- ;
: IX- 0 -^ _ix+- ;
: IY+ _iy+- ;
: IY- 0 -^ _iy+- ;

: <<3 8 * ;
: <<4 16 * ;

( -- )
: OP1 CREATE C, DOES> C@ A, ;
0xf3 OP1 DI,
0xfb OP1 EI,
0xeb OP1 EXDEHL,
0xd9 OP1 EXX,
0x76 OP1 HALT,
0xe9 OP1 JP(HL),
0x12 OP1 LD(DE)A,
0x1a OP1 LDA(DE),
0x00 OP1 NOP,
0xc9 OP1 RET,
0x17 OP1 RLA,
0x07 OP1 RLCA,
0x1f OP1 RRA,
0x0f OP1 RRCA,
0x37 OP1 SCF,

( Relative jumps are a bit special. They're supposed to take
  an argument, but they don't take it so they can work with
  the label system. Therefore, relative jumps are an OP1 but
  when you use them, you're expected to write the offset
  afterwards yourself. )

0x18 OP1 JR,
0x38 OP1 JRC,
0x30 OP1 JRNC,
0x28 OP1 JRZ,
0x20 OP1 JRNZ,
0x10 OP1 DJNZ,

( r -- )
: OP1r
    CREATE C,
    DOES>
    C@              ( r op )
    SWAP            ( op r )
    <<3             ( op r<<3 )
    OR A,
;
0x04 OP1r INCr,
0x05 OP1r DECr,
( also works for cc )
0xc0 OP1r RETcc,

( r -- )
: OP1r0
    CREATE C,
    DOES>
    C@              ( r op )
    OR A,
;
0x80 OP1r0 ADDr,
0x88 OP1r0 ADCr,
0xa0 OP1r0 ANDr,
0xb8 OP1r0 CPr,
0xb0 OP1r0 ORr,
0x90 OP1r0 SUBr,
0x98 OP1r0 SBCr,
0xa8 OP1r0 XORr,

( qq -- also works for ss )
: OP1qq
    CREATE C,
    DOES>
    C@              ( qq op )
    SWAP            ( op qq )
    <<4             ( op qq<<4 )
    OR A,
;
0xc5 OP1qq PUSHqq,
0xc1 OP1qq POPqq,
0x03 OP1qq INCss,
0x0b OP1qq DECss,
0x09 OP1qq ADDHLss,

: _1rr
    C@              ( rd rr op )
    ROT             ( rr op rd )
    <<3             ( rr op rd<<3 )
    OR OR A,
;

( rd rr )
: OP1rr
    CREATE C,
    DOES>
    _1rr
;
0x40 OP1rr LDrr,

( ixy+- HL rd )
: LDIXYr,
    ( dd/fd has already been spit )
    LDrr,           ( ixy+- )
    A,
;

( rd ixy+- HL )
: LDrIXY,
    ROT             ( ixy+- HL rd )
    SWAP            ( ixy+- rd HL )
    LDIXYr,
;

: OP2 CREATE , DOES> @ 256 /MOD A, A, ;
0xedb1 OP2 CPIR,
0xed46 OP2 IM0,
0xed56 OP2 IM1,
0xed5e OP2 IM2,
0xed44 OP2 NEG,
0xed4d OP2 RETI,

( n -- )
: OP2n
    CREATE C,
    DOES>
    C@ A, A,
;
0xd3 OP2n OUTnA,
0xdb OP2n INAn,
0xc6 OP2n ADDn,
0xe6 OP2n ANDn,
0xf6 OP2n Orn,
0xd6 OP2n SUBn,

( r n -- )
: OP2rn
    CREATE C,
    DOES>
    C@              ( r n op )
    ROT             ( n op r )
    <<3             ( n op r<<3 )
    OR A, A,
;
0x06 OP2rn LDrn,

( b r -- )
: OP2br
    CREATE C,
    DOES>
    0xcb A,
    C@              ( b r op )
    ROT             ( r op b )
    <<3             ( r op b<<3 )
    OR OR A,
;
0xc0 OP2br SETbr,
0x80 OP2br RESbr,
0x40 OP2br BITbr,

( bitwise rotation ops have a similar sig )
( r -- )
: OProt
    CREATE C,
    DOES>
    0xcb A,
    C@              ( r op )
    OR A,
;
0x10 OProt RLr,
0x00 OProt RLCr,
0x18 OProt RRr,
0x08 OProt RRCr,
0x20 OProt SLAr,
0x38 OProt SRLr,

( cell contains both bytes. MSB is spit as-is, LSB is ORed with r )
( r -- )
: OP2r
    CREATE ,
    DOES>
    @ SPLITB SWAP   ( r lsb msb )
    A,              ( r lsb )
    SWAP <<3        ( lsb r<<3 )
    OR A,
;
0xed41 OP2r OUT(C)r,
0xed40 OP2r INr(C),

( ss -- )
: OP2ss
    CREATE C,
    DOES>
    0xed A,
    C@ SWAP         ( op ss )
    <<4             ( op ss<< 4 )
    OR A,
;
0x4a OP2ss ADCHLss,
0x42 OP2ss SBCHLss,

( dd nn -- )
: OP3ddnn
    CREATE C,
    DOES>
    C@              ( dd nn op )
    ROT             ( nn op dd )
    <<4             ( nn op dd<<4 )
    OR A,
    A,,
;
0x01 OP3ddnn LDddnn,

( nn -- )
: OP3nn
    CREATE C,
    DOES>
    C@ A,
    A,,
;
0xcd OP3nn CALLnn,
0xc3 OP3nn JPnn,
0x22 OP3nn LD(nn)HL,
0x2a OP3nn LDHL(nn),

( Specials )

( dd nn -- )
: LDdd(nn),
    0xed A,
    SWAP <<4 0x4b OR A,
    A,,
;

( nn dd -- )
: LD(nn)dd,
    0xed A,
    <<4 0x43 OR A,
    A,,
;

: JP(IX), IX DROP JP(HL), ;
: JP(IY), IY DROP JP(HL), ;

( 26 == next )
: JPNEXT, 26 JPnn, ;

: CODE
    ( same as CREATE, but with native word )
    (entry)
    ( 23 == nativeWord )
    23 ,
;

: ;CODE JPNEXT, ;


( Macros )
( clear carry + SBC )
: SUBHLss, A ORr, SBCHLss, ;

( Routines )
( 29 == chkPS )
: chkPS, 29 CALLnn, ;

( Flow

  There are 2 label types: backward and forward. For each
  type, there are two actions: set and write. Setting a label
  is declaring where it is. It has to be performed at the
  label's destination. Writing a label is writing its offset
  difference to the binary result. It has to be done right
  after a relative jump operation. Yes, labels are only for
  relative jumps.

  For backward labels, set happens before write. For forward
  labels, write happen before set. The write operation writes
  a dummy placeholder, and then the set operation writes the
  offset at that placeholder's address.

  Variable actions are expected to be called with labels in
  front of them. Example, "L2 FSET"

  About that "1 -": z80 relative jumps record "e-2", that is,
  the offset that *counts the 2 bytes of the jump itself*.
  Because we set the label *after* the jump OP1 itself, that's
  1 byte that is taken care of. We still need to adjust by
  another byte before writing the offset.
)

( Place BEGIN, where you want to jump back and AGAIN after
  a relative jump operator. Just like BSET and BWR. )
: BEGIN, PC ;
: AGAIN, PC - 1 - A, ;

: BSET PC SWAP ! ;
: BWR @ AGAIN, ;
( same as BSET, but we need to write a placeholder )
: FJR, PC 0 A, ;
: IFZ, JRZ, FJR, ;
: IFNZ, JRNZ, FJR, ;
: IFC, JRC, FJR, ;
: IFNC, JRNC, FJR, ;
: THEN,
    DUP PC          ( l l pc )
    -^ 1 -          ( l off )
    ( warning: l is a PC offset, not a mem addr! )
    SWAP ORG @ +    ( off addr )
    C!
;
: FWR BSET 0 A, ;
: FSET @ THEN, ;

