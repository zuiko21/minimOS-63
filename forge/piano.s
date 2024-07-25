; simple piano for Rosario 6301/6303 SBC
; PA0-PA7 = C4-C5
; PA7 + PA0-PA6 = C5-B5
; hold next button for Sharp, previous for Flat (e.g. PA0+PA1 = C#4) 
; (c) 2024 Carlos J. Santisteban
; last modified 20240725-1045

	* = $F000				; make that $1000 for downloadable version

; hardware definitions
	DDR1	= 0
	DDR2	= 1
	IOR1	= 2
	IOR2	= 3

; memory definitions
	last	= $80			; last PA pattern
	buffer	= last+1		; cycle value for X

reset:
; basic 6301/6303 init
	SEI						; FF00. 0F
	LDS #$00FF				; FF01. 8E 00 FF	; stack is ready, just in case
	CLR DDR1				; FF04. 7F 00 00	; PA as input (just in case)
	CLR IOR2				; FF07. 7F 00 03	; will keep IOSEL, SC_TX and SC_CL low
	LDAA #$14				; FF0A. 86 14		; P22 (SC_CL) and P24 (SC_TX) will be output for PB control and audio
	STAA DDR2				; FF0C. 97 01		; update DDR2
; assume correct mode
	CLR $08					; FF1F. 7F 00 08	; disable all interrupt sources, just in case
	CLR $11					; FF22. 7F 00 11	; disable serial as well
	CLR $10					; FF25. 7F 00 10
; don't care about picoVDU screen
	CLR last				;	; initial value (no key)
	CLRA					;	; for good measure

; *** main loop ***
key:
; check wether a key is pressed
			LDAB IOR1			;	; read PA
			CMPB last			;	; compare to previous
			BEQ key				;	; wait for any key change
		STAB last				;	; register current key
		LDX #index				;	; get offset base
		ABX						;	; get pointer to selected key
; *** *** *** *** *** CHECK *** ***
		LDX 0, X				;	; this is the address of the loop value
		LDX 0, X				;	; and this is the 16-bit loop value
		STX buffer				;	; store for later
		BEQ key					;	; only if it's a valid note
	OIM #$10, IOR2			; FF6C. 72 10 03	; set SC_TX hi to enable sound
cycle:
; reload X from temp
		LDX buffer			; FF6F. ***	; get cached loop value (minus 40t overhead) (4)
bloop:
			DEX				; FF72. 09
			BNE bloop		; FF73. 26 FD		; delay loop (4X)
; toggle speaker output
		EORA #%10000000			;	; toggle PB7 (2)
		STAA IOR1				; FFD0. 97 02		; store pattern into PA (3)
		LDAB #$FF				; FFD2. C6 FF		; all bits output... EEEEEK (2)
		STAB DDR1				; FFD4. D7 00		; ...for a while (3)
		EIM #%00000100, IOR2	; FFD6. 75 04 03	; pulse SC_TX in order to latch PA into PB (6+6)
		EIM #%00000100, IOR2	; FFD9. 75 04 03
		CLR DDR1				; FFDC. 7F 00 00	; PA back to input for lower power (5)
; check whether key has changed
		LDAB IOR1			;	; check PA (3)
		CMPB last			;	; any change? (3)
		BEQ cycle			; FF80. 27 **	; otherwise keep playing (3)
	AIM #%11101111, IOR2	; FF82. 71 EF 03	; clear SC_TX to disable sound
; back to key read
	JMP key

	.dsb	panic-*, $FF	; FF8A. FF FF FF FF...


; *** *** rest is NOT for downloadable version *** ***
nmi:
	RTI						; FFEC. 3B			; ...never used, just for safety

	.dsb	$FFF8-*, $FF	; FFED. FF FF FF
							; FFF0. FF FF FF FF FF FF FF FF

; *** end of ROM ***
vectors:
	.word null	; FFF8. FF EC (IRQ)
	.word null	; FFFA. FF EC (SWI)
	.word null	; FFF0. FF E0 (NMI)
	.word reset	; FFFE. FF 00 (RESET)
