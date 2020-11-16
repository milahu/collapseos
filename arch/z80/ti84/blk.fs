( ----- 600 )
TI-84+ Recipe

Support code for the TI-84+ recipe. Contains drivers for the
keyboard and LCD.

551 LCD                        564 Keyboard
( ----- 601 )
TI-84+ LCD driver

Implement (emit) on TI-84+ (for now)'s LCD screen.
Load range: 555-560

The screen is 96x64 pixels. The 64 rows are addressed directly
with CMD_ROW but columns are addressed in chunks of 6 or 8 bits
(there are two modes).

In 6-bit mode, there are 16 visible columns. In 8-bit mode,
there are 12.

Note that "X-increment" and "Y-increment" work in the opposite
way than what most people expect. Y moves left and right, X
moves up and down.
                                                        (cont.)
( ----- 602 )
# Z-Offset

This LCD has a "Z-Offset" parameter, allowing to offset rows on
the screen however we wish. This is handy because it allows us
to scroll more efficiently. Instead of having to copy the LCD
ram around at each linefeed (or instead of having to maintain
an in-memory buffer), we can use this feature.

The Z-Offset goes upwards, with wrapping. For example, if we
have an 8 pixels high line at row 0 and if our offset is 8,
that line will go up 8 pixels, wrapping itself to the bottom of
the screen.

The principle is this: The active line is always the bottom
one. Therefore, when active row is 0, Z is FNTH+1, when row is
1, Z is (FNTH+1)*2, When row is 8, Z is 0.              (cont.)
( ----- 603 )
# 6/8 bit columns and smaller fonts

If your glyphs, including padding, are 6 or 8 pixels wide,
you're in luck because pushing them to the LCD can be done in a
very efficient manner.  Unfortunately, this makes the LCD
unsuitable for a Collapse OS shell: 6 pixels per glyph gives us
only 16 characters per line, which is hardly usable.

This is why we have this buffering system. How it works is that
we're always in 8-bit mode and we hold the whole area (8 pixels
wide by FNTH high) in memory. When we want to put a glyph to
screen, we first read the contents of that area, then add our
new glyph, offsetted and masked, to that buffer, then push the
buffer back to the LCD. If the glyph is split, move to the next
area and finish the job.
                                                        (cont.)
( ----- 604 )
That being said, it's important to define clearly what CURX and
CURY variable mean. Those variable keep track of the current
position *in pixels*, in both axes.
( ----- 605 )
( Required config: LCD_MEM )
: _mem+ [ LCD_MEM LITN ] @ + ;
: FNTW 3 ; : FNTH 5 ;
: COLS 96 FNTW 1+ / ; : LINES 64 FNTH 1+ / ;
( Wait until the lcd is ready to receive a command. It's a bit
  weird to implement a waiting routine in asm, but the forth
  version is a bit heavy and we don't want to wait longer than
  we have to. )
CODE _wait
    BEGIN,
        0x10 ( CMD ) INAi,
        RLA, ( When 7th bit is clr, we can send a new cmd )
    JRC, AGAIN,
;CODE
( ----- 606 )
( two pixel buffers that are 8 pixels wide (1b) by FNTH
  pixels high. This is where we compose our resulting pixels
  blocks when spitting a glyph. )
: LCD_BUF 0 _mem+ ;
: _cmd 0x10 ( CMD ) PC! _wait ;
: _data! 0x11 ( DATA ) PC! _wait ;
: _data@ 0x11 ( DATA ) PC@ _wait ;
: LCDOFF 0x02 ( CMD_DISABLE ) _cmd ;
: LCDON 0x03 ( CMD_ENABLE ) _cmd ;
( ----- 607 )
: _yinc 0x07 _cmd ; : _xinc 0x05 _cmd ;
: _zoff! ( off -- ) 0x40 + _cmd ;
: _col! ( col -- ) 0x20 + _cmd ;
: _row! ( row -- ) 0x80 + _cmd ;
: LCD$
    H@ [ LCD_MEM LITN ] ! FNTH 2 * ALLOT
    LCDON 0x01 ( 8-bit mode ) _cmd
    FNTH 1+ _zoff!
;
( ----- 608 )
: _clrrows ( n u -- Clears u rows starting at n )
    SWAP _row!
    ( u ) 0 DO
        _yinc 0 _col!
        11 0 DO 0 _data! LOOP
        _xinc 0 _data!
    LOOP ;
: NEWLN ( ln -- )
    DUP 1+ FNTH 1+ * _zoff!
    FNTH 1+ * FNTH 1+ _clrrows ;
: LCDCLR 0 64 _clrrows ;
( ----- 609 )
: _atrow! ( pos -- ) COLS / FNTH 1+ * _row! ;
: _tocol ( pos -- col off ) COLS MOD FNTW 1+ * 8 /MOD ;
: CELL! ( c pos -- )
    DUP _atrow! DUP _tocol _col! ROT ( pos coff c )
    0x20 - FNTH * ~FNT + ( pos coff a )
    _xinc _data@ DROP
    FNTH 0 DO ( pos coff a )
        C@+ 2 PICK 8 -^ LSHIFT
        _data@ 8 LSHIFT OR
        LCD_BUF I + 2DUP FNTH + C!
        SWAP 8 RSHIFT SWAP C!
    LOOP 2DROP
    DUP _atrow!
    FNTH 0 DO LCD_BUF I + C@ _data! LOOP
    DUP _atrow! _tocol NIP 1+ _col!
    FNTH 0 DO LCD_BUF FNTH + I + C@ _data! LOOP ;
( ----- 614 )
Keyboard driver

Load range: 566-570

Implement a (key) word that interpret keystrokes from the
builtin keyboard. The word waits for a digit to be pressed and
returns the corresponding ASCII value.

This routine waits for a key to be pressed, but before that, it
waits for all keys to be de-pressed. It does that to ensure
that two calls to _wait only go through after two actual key
presses (otherwise, the user doesn't have enough time to
de-press the button before the next _wait routine registers the
same key press as a second one).

                                                        (cont.)
( ----- 615 )
Sending 0xff to the port resets the keyboard, and then we have
to send groups we want to "listen" to, with a 0 in the group
bit. Thus, to know if *any* key is pressed, we send 0xff to
reset the keypad, then 0x00 to select all groups, if the result
isn't 0xff, at least one key is pressed.
( ----- 616 )
( Requires KBD_MEM, KBD_PORT )
( gm -- pm, get pressed keys mask for group mask gm )
CODE _get
    HL POP,
    chkPS,
    DI,
        A 0xff LDri,
        KBD_PORT OUTiA,
        A L LDrr,
        KBD_PORT OUTiA,
        KBD_PORT INAi,
    EI,
    L A LDrr, HL PUSH,
;CODE
( ----- 617 )
( wait until all keys are de-pressed. To avoid repeat keys, we
  require 64 subsequent polls to indicate all depressed keys.
  all keys are considered depressed when the 0 group returns
  0xff. )
: _wait 64 BEGIN 0 _get 0xff = NOT IF DROP 64 THEN
    1- DUP NOT UNTIL DROP ;
( digits table. each row represents a group. 0 means
  unsupported. no group 7 because it has no key. )
CREATE _dtbl
    0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,
    0xd C, '+' C, '-' C, '*' C, '/' C, '^' C, 0 C, 0 C,
    0 C, '3' C, '6' C, '9' C, ')' C, 0 C, 0 C, 0 C,
    '.' C, '2' C, '5' C, '8' C, '(' C, 0 C, 0 C, 0 C,
    '0' C, '1' C, '4' C, '7' C, ',' C, 0 C, 0 C, 0 C,
    0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0x80 ( alpha ) C,
    0 C, 0 C, 0 C, 0 C, 0 C, 0x81 ( 2nd ) C, 0 C, 0x7f C,
( ----- 618 )
( alpha table. same as _dtbl, for when we're in alpha mode. )
CREATE _atbl
    0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,
    0xd C, '"' C, 'W' C, 'R' C, 'M' C, 'H' C, 0 C, 0 C,
    '?' C, 0 C, 'V' C, 'Q' C, 'L' C, 'G' C, 0 C, 0 C,
    ':' C, 'Z' C, 'U' C, 'P' C, 'K' C, 'F' C, 'C' C, 0 C,
    0x20 C, 'Y' C, 'T' C, 'O' C, 'J' C, 'E' C, 'B' C, 0 C,
    0 C, 'X' C, 'S' C, 'N' C, 'I' C, 'D' C, 'A' C, 0x80 C,
    0 C, 0 C, 0 C, 0 C, 0 C, 0x81 ( 2nd ) C, 0 C, 0x7f C,
: _2nd@ [ KBD_MEM LITN ] C@ 1 AND ;
: _2nd! [ KBD_MEM LITN ] C@ 0xfe AND + [ KBD_MEM LITN ] C! ;
: _alock@ [ KBD_MEM LITN ] C@ 2 AND ;
: _alock^ [ KBD_MEM LITN ] C@ 2 XOR [ KBD_MEM LITN ] C! ;
( ----- 619 )
: _gti ( -- tindex, that it, index in _dtbl or _atbl )
    0 ( gid ) 0 ( dummy )
    BEGIN ( loop until a digit is pressed )
        DROP
        1+ DUP 7 = IF DROP 0 THEN ( inc gid )
        1 OVER LSHIFT 0xff -^ ( group dmask ) _get
    DUP 0xff = NOT UNTIL _wait
    ( gid dmask )
    0xff XOR ( dpos ) 0 ( dindex )
    BEGIN 1+ 2DUP RSHIFT NOT UNTIL 1-
    ( gid dpos dindex ) NIP
    ( gid dindex ) SWAP 8 * + ;
( ----- 620 )
: _tbl^ ( swap input tbl )
    _atbl = IF _dtbl ELSE _atbl THEN ;
: (key)
    0 _2nd! 0 ( lastchr ) BEGIN
        _alock@ IF _atbl ELSE _dtbl THEN
        OVER 0x80 ( alpha ) =
            IF _tbl^ _2nd@ IF _alock^ THEN THEN
        SWAP 0x81 = _2nd!
        _gti + C@
    DUP 0 0x80 >< UNTIL ( loop if not in range )
    ( lowercase? )
    _2nd@ IF DUP 'A' 'Z' =><= IF 0x20 OR THEN THEN
;
: KBD$ 0 [ KBD_MEM LITN ] C! ;
