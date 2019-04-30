; run RLA the number of times specified in B
rlaX:
	; first, see if B == 0 to see if we need to bail out
	inc	b
	dec	b
	ret	z	; Z flag means we had B = 0
.loop:	rla
	djnz	.loop
	ret

; Copy BC bytes from (HL) to (DE).
copy:
	; first, let's see if BC is zero. if it is, we have nothing to do.
	; remember: 16-bit inc/dec don't modify flags. that's why we check B
	; and C separately.
	inc	b
	dec	b
	jr	nz, .proceed
	inc	c
	dec	c
	ret	z	; zero? nothing to do
.proceed:
	push	bc
	push	de
	push	hl
	ldir
	pop	hl
	pop	de
	pop	bc
	ret

callHL:
	jp	(hl)
	ret

; If string at (HL) starts with ( and ends with ), "enter" into the parens
; (advance HL and put a null char at the end of the string) and set Z.
; Otherwise, do nothing and reset Z.
enterParens:
	ld	a, (hl)
	cp	'('
	ret	nz		; nothing to do
	push	hl
	ld	a, 0	; look for null char
	; advance until we get null
.loop:
	cpi
	jp	z, .found
	jr	.loop
.found:
	dec	hl	; cpi over-advances. go back to null-char
	dec	hl	; looking at the last char before null
	ld	a, (hl)
	cp	')'
	jr	nz, .doNotEnter
	; We have parens. While we're here, let's put a null
	xor	a
	ld	(hl), a
	pop	hl	; back at the beginning. Let's advance.
	inc	hl
	cp	a	; ensure Z
	ret		; we're good!
.doNotEnter:
	pop	hl
	call	JUMP_UNSETZ
	ret

