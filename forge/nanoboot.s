; nanoBoot for Rosario 6301/6303 SBC
; (c) 2024 Carlos J. Santisteban
; last modified 20240727-1558

	* = $FE00				; make that $1000 for downloadable version

; hardware definitions
	DDR1	= 0
	DDR2	= 1
	IOR1	= 2
	IOR2	= 3

; memory definitions
	last	= $80			; last PA pattern
	count	= last+1		; cycle value for X

reset:
; basic 6301/6303 init
	SEI						; FE00. 0F
	LDS #$00*				; FE01. 8E 00 *	; stack is ready, just in case
	CLR DDR1				; FE04. 7F 00 00	; PA as input (just in case)
	CLR IOR2				; FE07. 7F 00 03	; will keep IOSEL, SC_TX and SC_CL low
	LDAA #%00010100			; FE0A. 86 14		; P22 (SC_CL) and P24 (SC_TX) will be output for PB control and audio
	STAA DDR2				; FE0C. 97 01		; update DDR2
; assume correct mode
	CLR $08					; FE0E. 7F 00 08	; disable all interrupt sources, just in case
	CLR $11					; FE11. 7F 00 11	; disable serial as well
	CLR $10					; FE14. 7F 00 10
; don't care about picoVDU screen
	CLR last				; FE17. 7F 00 80	; initial value (no key)
	CLRA					; FE1A. 4F			; for good measure

; *** main loop ***

; *** data ***

; *** interrupt handlers ***
irq:
	LDX fw_isr				;	; load vector at $FE-FF
	JMP 0, X				;	; jump to it
nmi:
	LDX fw_nmi				;	; load vector at $FC-FD
	JMP 0, X				;	; jump to it
panic:
		INX
		BNE panic			;	; delay loop
	OIM #%00010000, IOR2	; FE33. 72 10 03	; set SC_TX hi to enable sound
	LDAB #213				; FF6A. C6 FF		; cycle counter (213 ms)
b_it:
		LDX #144			; FF6F. CE 00 90	; value for ~500 µs (614t-37 ~144) (3)
bloop:
			DEX				; FF72. 09
			BNE bloop		; FF73. 26 FD		; delay loop
		TBA					; FF75. 17			; copy cycle (1)
		RORA				; FF76. 46
		RORA				; FF77. 46			; rotate right twice, now D0 is at D7 (2)
		ORAA #%01111111		; FF78. 8A 7F		; all LEDs turned on during beep (2)
		STAA IOR1				; FFD0. 97 02		; store pattern into PA (3)
		LDAA #$FF				; FFD2. 86 FF		; all bits output... EEEEEK (2)
		STAA DDR1				; FFD4. 97 00		; ...for a while (3)
		EIM #%00000100, IOR2	; FFD6. 75 04 03	; pulse SC_TX in order to latch PA into PB (6+6)
		EIM #%00000100, IOR2	; FFD9. 75 04 03
		CLR DDR1				; FFDC. 7F 00 00	; PA back to input for lower power (5)
		DECB				; FF7F. 5A			; next cycle (1+3)
		BNE b_it			; FF80. 26 ED
	AIM #%11101111, IOR2	; FF82. 71 EF 03	; clear SC_TX to disable sound
	CLR IOR1				;	; will turn all LEDs off
	LDAA #$FF				; FFD2. 86 FF		; all bits output... EEEEEK (2)
	STAA DDR1				; FFD4. 97 00		; ...for a while (3)
	EIM #%00000100, IOR2	; FFD6. 75 04 03	; pulse SC_TX in order to latch PA into PB (6+6)
	EIM #%00000100, IOR2	; FFD9. 75 04 03
	CLR DDR1				; FFDC. 7F 00 00	; PA back to input for lower power (5)
	BRA panic

null:
	RTI						; FFF0. 3B			; ...never used, just for safety

	.dsb	vectors-*, $FF	; FFF1. FF FF FF FF FF FF FF

; *** end of ROM ***
vectors:
	.word irq	; FFF8. FF F0 (IRQ)
	.word null	; FFFA. FF F0 (SWI)
	.word nmi	; FFF0. FF F0 (NMI)
	.word reset	; FFFE. FE 00 (RESET)
