( -- n )
: _idle 0xff _sdcSR ;

( -- n )
( _sdcSR 0xff until the response is something else than 0xff
  for a maximum of 20 times. Returns 0xff if no response. )
: _wait
    0  ( cnt )
    BEGIN
        _idle
        DUP 0xff = IF DROP ELSE SWAP DROP EXIT THEN
        1+
    DUP 20 = UNTIL
    DROP 0xff
;

( -- )
( The opposite of sdcWaitResp: we wait until response is 0xff.
  After a successful read or write operation, the card will be
  busy for a while. We need to give it time before interacting
  with it again. Technically, we could continue processing on
  our side while the card it busy, and maybe we will one day,
  but at the moment, I'm having random write errors if I don't
  do this right after a write, so I prefer to stay cautious
  for now. )
: _ready BEGIN _idle 0xff = UNTIL ;

( c n -- c )
( Computes n into crc c with polynomial 0x09
  Note that the result is "left aligned", that is, that 8th
  bit to the "right" is insignificant (will be stop bit). )
: _crc7
    XOR           ( c )
    8 0 DO
        2 *       ( <<1 )
        DUP 255 > IF
            ( MSB was set, apply polynomial )
            0xff AND
            0x12 XOR  ( 0x09 << 1, we apply CRC on high bits )
        THEN
    LOOP
;

( c n -- c )
( Computes n into crc c with polynomial 0x1021 )
: _crc16
    SWAP DUP 256 /    ( n c c>>8 )
    ROT XOR           ( c x )
    DUP 16 / XOR      ( c x^x>>4 )
    SWAP 256 *        ( x c<<8 )
    OVER 4096 * XOR   ( x c^x<<12 )
    OVER 32 * XOR     ( x c^x<<5 )
    XOR               ( c )
;

( send-and-crc7 )
( n c -- c )
: _s+crc SWAP DUP _sdcSR DROP _crc7 ;

( cmd arg1 arg2 -- resp )
( Sends a command to the SD card, along with arguments and
  specified CRC fields. (CRC is only needed in initial commands
  though).
  This does *not* handle CS. You have to select/deselect the
  card outside this routine. )
: _cmd
    _wait DROP
    ROT            ( a1 a2 cmd )
    0 _s+crc       ( a1 a2 crc )
    ROT 256 /MOD   ( a2 crc h l )
    ROT            ( a2 h l crc )
    _s+crc         ( a2 h crc )
    _s+crc         ( a2 crc )
    SWAP 256 /MOD  ( crc h l )
    ROT            ( h l crc )
    _s+crc         ( h crc )
    _s+crc         ( crc )
    ( send CRC )
    0x01 OR        ( ensure stop bit )
    _sdcSR DROP
    ( And now we just have to wait for a valid response... )
    _wait
;

( cmd arg1 arg2 -- r )
( Send a command that expects a R1 response, handling CS. )
: SDCMDR1 _sdcSel _cmd _sdcDesel ;

( cmd arg1 arg2 -- r arg1 arg2 )
( Send a command that expects a R7 response, handling CS. A R7
  is a R1 followed by 4 bytes. arg1 contains bytes 0:1, arg2
  has 2:3 )
: SDCMDR7
    _sdcSel
    _cmd        ( r )
    _idle 256 * ( r h )
    _idle +     ( r arg1 )
    _idle 256 * ( r arg1 h )
    _idle +     ( r arg1 arg2 )
    _sdcDesel
;

: _err _sdcDesel ABORT" SDerr" ;

( Initialize a SD card. This should be called at least 1ms
  after the powering up of the card. )
: SDC$
    ( Wake the SD card up. After power up, a SD card has to
      receive at least 74 dummy clocks with CS and DI high. We
      send 80. )
    10 0 DO _idle DROP LOOP

    ( call cmd0 and expect a 0x01 response (card idle)
	  this should be called multiple times. we're actually
      expected to. let's call this for a maximum of 10 times. )
    0  ( dummy )
    10 0 DO  ( r )
        DROP
        0b01000000 0 0  ( CMD0 )
        SDCMDR1
        DUP 0x01 = IF LEAVE THEN
    LOOP
    0x01 = NOT IF _err THEN

    ( Then comes the CMD8. We send it with a 0x01aa argument
      and expect a 0x01aa argument back, along with a 0x01 R1
      response. )
    0b01001000 0 0x1aa  ( CMD8 )
    SDCMDR7                     ( r arg1 arg2 )
    0x1aa = NOT IF _err THEN    ( arg2 check )
    0 = NOT IF _err THEN        ( arg1 check )
    0x01 = NOT IF _err THEN     ( r check )

    ( Now we need to repeatedly run CMD55+CMD41 (0x40000000)
      until the card goes out of idle mode, that is, when
      it stops sending us 0x01 response and send us 0x00
      instead. Any other response means that initialization
      failed. )
    BEGIN
        0b01110111 0 0  ( CMD55 )
        SDCMDR1
        0x01 = NOT IF _err THEN
        0b01101001 0x4000 0x0000  ( CMD41 )
        SDCMDR1
        DUP 0x01 > IF _err THEN
    NOT UNTIL
    ( Out of idle mode! Success! )
;

( dstaddr blkno -- )
: _sdc@
    _sdcSel
    0x51    ( CMD17 )
    0 ROT   ( a cmd 0 blkno )
    _cmd
    IF _err THEN
    _wait
    0xfe = NOT IF _err THEN
    0 SWAP           ( crc a )
    512 0 DO         ( crc a )
        DUP          ( crc a a )
        _idle        ( crc a a n )
        DUP ROT      ( crc a n n a )
        C!           ( crc a n )
        ROT SWAP     ( a crc n )
        _crc16       ( a crc )
        SWAP 1+      ( crc a+1 )
    LOOP
    DROP             ( crc1 )
    _idle 256 *
    _idle +          ( crc2 )
    _wait DROP
    _sdcDesel
    = NOT IF _err THEN
;

: SDC@
    2 * DUP BLK( SWAP    ( b a b )
    _sdc@
    1+ BLK( 512 + SWAP
    _sdc@
;

( srcaddr blkno -- )
: _sdc!
    _sdcSel
    0x58    ( CMD24 )
    0 ROT   ( a cmd 0 blkno )
    _cmd
    IF _err THEN
    _idle DROP
    0xfe _sdcSR DROP
    0 SWAP           ( crc a )
    512 0 DO         ( crc a )
        C@+          ( crc a+1 n )
        ROT OVER     ( a n crc n )
        _crc16       ( a n crc )
        SWAP         ( a crc n )
        _sdcSR DROP  ( a crc )
        SWAP         ( crc a )
    LOOP
    DROP             ( crc )
    256 /MOD         ( lsb msb )
    _sdcSR DROP      ( lsb )
    _sdcSR DROP
    _wait DROP
    _sdcDesel
;

: SDC!
    2 * DUP BLK( SWAP    ( b a b )
    _sdc!
    1+ BLK( 512 + SWAP
    _sdc!
;
