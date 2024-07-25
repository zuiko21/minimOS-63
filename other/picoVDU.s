	* = $FFC0
reset:
	SEI		; FFC0: 0F
	LDS #$00FF	; FFC1: 8E 00 FF
	LDX #$6000	; FFC4: CE 60 00
loop:
	CLR 0, X	; FFC7: 6F 00
	INX		; FFC9: 08
	CPX #$8000	; FFCA: 8C 80 00
	BNE loop	; FFCD: 26 F8
lock:
	BRA lock	; FFCF: 20 FE ; EEEEEEEEEEEEEEEEEEK

nmi:
	INCA		; FFD1: 4C
	LDX #$6000	; FFD2: CE 60 00
fill:
	STAA 0, X	; FFD5: A7 00
	INX		; FFD7: 08
	CPX #$8000	; FFD8: 8C 80 00
	BNE fill	; FFDB: 26 F8
null:
	RTI		; FFDD: 3B

	.dsb $FFF8-*, $FF
			; FFDE: FF FF
			; FFE0: FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
			; FFF0: FF FF FF FF FF FF FF FF
vectors:
	.word null	; FFF8: FF DD (IRQ)
	.word null	; FFFA: FF DD (SWI)
	.word nmi	; FFFC: FF D1
	.word reset	; FFFE: FF C0
