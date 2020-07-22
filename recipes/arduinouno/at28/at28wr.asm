; *** EEPROM write ***
; Listen to UART expecting tty-escaped "pingpong" (from tools/) communication.
;
; Each of those received bytes is written to the EEPROM, starting at addr 0.
; that byte is then re-read and sent back to the UART, tty-escaped.
;
; Addr selection is done through 2 chained '164, data in/out is done directly
; with PD7:2 for bits 7:2 and PB1:0 for bits 1:0 (PD1 and PD0 are used for
; UART).
;
; *** Register Usage ***
;
; r0:  holds whether last received char was tty-escaped (0 = no, 1=yes)
; r16: generic tmp
; r17: generic tmp
; r20: Byte to send to the "data" SR. Wired to D0-D7.
; r21: "high" byte, to send to the "addr" SR. Wired to A8-15
; r22: "low" byte, to send to the "addr" SR. Wired to A0-7
; r23: tmp value to use for sending to the "addr" SR

.include "m328Pdef.inc"

; *** Pins ***
.equ SRCP = PORTB2
.equ SRDS = PORTB1
.equ FLWE = PORTB3
.equ FLOE = PORTB4
.equ FLCE = PORTB5 ; WARNING: same as LED

; *** Consts ***
.equ BAUD_PRESCALE = 103	; 9600 bauds at 16mhz

rjmp    main

; *** Code ***
; Waits until a char is read, then put it in R20
; Perform TTY-escape transparently.
uartrd:
	lds	r16, UCSR0A
	sbrs	r16, RXC0	; RXC0 is set? skip rjmp and fetch char.
	rjmp	uartrd
	lds	r20, UDR0
	; is this the escape char?
	cpi	r20, 0x20
	brne	uartrd_0
	; escape char
	; We "pong" the escape right away.
	rcall	uartwr
	inc	r0
	rjmp	uartrd
uartrd_0:
	; should we escape?
	tst	r0
	breq	uartrd_1
	; yes
	andi	r20, 0x7f
uartrd_1:
	ret

; Sends char in r20 to UART
; Perform TTY-escape transparently.
uartwr:
	lds	r16, UCSR0A
	sbrs    r16, UDRE0	; wait until send buffer is empty
	rjmp	uartwr
	; should we escape?
	tst	r0
	breq	uartwr_0
	; we need to escape
	ori	r20, 0x80
	clr	r0
uartwr_0:
	sts	UDR0, r20
	ret

; send r23 to addr shift register.
; We send highest bits first so that Q7 is the MSB and Q0 is the LSB
sendaddr:
	ldi	r16, 8		; we will loop 8 times
	cbi	PORTB, SRDS
	sbrc	r23, 7		; if latest bit isn't cleared, set SER_DP high
	sbi	PORTB, SRDS
	; toggle SRCP, not waiting between pulses. The CD74AC164 at 5V has a
        ; 5.9ns CP min pulse width. We can't match that at 16mhz. No need to
	; wait.
	cbi	PORTB, SRCP
	sbi	PORTB, SRCP
	lsl	r23		; shift our data left
	dec	r16
	brne	sendaddr+1	; not zero yet? loop! (+1 to avoid reset)
	ret

; send r20 to EEPROM's I/O7:0 through PD7:2 and PB1:0
writedata:
	; send bits 7:2
	mov	r16, r20
	andi	r16, 0xfc
	in	r17, PORTD
	andi	r17, 0x03
	or	r16, r17
	out	PORTD, r16
	; send bits 1:0
	mov	r16, r20
	andi	r16, 0x03
	in	r17, PORTB
	andi	r17, 0xfc
	or	r16, r17
	out	PORTB, r16
	ret

; push r20 to the rom and increase the memory counter
pushdata:
	; first, set up addr
	mov	r23, r21
	rcall	sendaddr
	mov	r23, r22
	rcall	sendaddr
	inc	r22
	brne	pushdata_0	; no overflow? skip
	inc	r21

pushdata_0:
	; addr is latched on WE falling edge
	cbi	PORTB, FLWE

	; now, lets set up data. Plenty enough instructions to ensure a 100ns
	; minimum delay.
	rcall	writedata

	; data is latched on rising edge
	sbi	PORTB, FLWE

	ret

; wait until I/O7 stops toggling
waitio7:
	cbi	PORTB, FLOE
	in	r16, PIND
	sbi	PORTB, FLOE
	andi	r16, 0xfc
	cbi	PORTB, FLOE
	in	r17, PIND
	sbi	PORTB, FLOE
	andi	r17, 0xfc
	cp	r16, r17
	brne	waitio7
	ret

; read EEPROM's I/O7:0 through PD7:2 and PB1:0 and put result in r20.
readdata:
	cbi	PORTB, FLOE
	; read bits 7:2
	in	r20, PIND
	andi	r20, 0xfc
	; read bits 1:0
	in	r16, PINB
	andi	r16, 0x03
	or	r20, r16
	sbi	PORTB, FLOE
	ret

; Set PD7:2 and PB1:0 to output
ioout:
	ldi	r16, 0xfc	; PD7:2
	out	DDRD, r16
	ldi	r16, 0x3f	; PB5:0 (WE, OE and CE too)
	out	DDRB, r16
	ret

; Set PD7:2 and PB1:0 to input
ioin:
	ldi	r16, 0x03	; PD7:2
	out	DDRD, r16
	ldi	r16, 0x3c	; PB1:0
	out	DDRB, r16
	ret

main:
	ldi	r16, low(RAMEND)
	out	SPL, r16
	ldi	r16, high(RAMEND)
	out	SPH, r16

	; We begin with WE and OE disabled (high), but CE stays enabled (low)
	; the whole time.
	sbi	PORTB, FLWE
	sbi	PORTB, FLOE
	cbi	PORTB, FLCE

	; Clear counters and flags
	clr	r0
	clr	r21
	clr	r22

	; Setup UART
	ldi	R16, low(BAUD_PRESCALE)
	sts	UBRR0L, r16
	ldi	r16, high(BAUD_PRESCALE)
	sts	UBRR0H, r16

	ldi	r16, (1<<RXEN0) | (1<<TXEN0)
	sts	UCSR0B, r16

loop:
	rcall	uartrd
	rcall	ioout
	rcall	pushdata
	rcall	ioin
	rcall	waitio7
	rcall	readdata
	rcall	uartwr
	rjmp	loop

