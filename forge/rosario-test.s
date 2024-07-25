; simple test for Rosario 6301/6303 SBC
; (c) 2024 Carlos J. Santisteban
; last modified 20240724-1814

	* = $FF00				; make that $7F00 for downloadable version
reset:
; basic 6301/6303 init
	SEI						; FF00. 0F
	LDS #$00FF				; FF01. 8E 00 FF	; stack is ready, just in case
	CLR $00					; FF04. 7F 00 00	; PA as input (just in case)
	CLR $03					; FF07. 7F 00 03	; will keep IOSEL, SC_TX and SC_CL low
	LDAA #$14				; FF0A. 86 14		; P22 (SC_CL) and P24 (SC_TX) will be output for PB control and audio
	STAA $01				; FF0C. 97 01		; update DDR2
; check mode
	LDAA $03				; FF0E. 96 03		; check P2 curent value
	ANDA #%11100000			; FF10. 84 E0		; only mode bits
	CMPA #%01000000			; FF12. 81 40		; are we in mode 2?
	BEQ mode_ok				; FF14. 27 09		; is so, continue
panic:
			INX				; FF16. 08			; otherwise, try to report!
			BNE panic		; FF17. 26 FD		; loops for ~0.25s
		COMA				; FF19. 43			; reverse bit pattern
		JSR pb_out			; FF1A. BD FF D0	; show it into PB
		BRA panic			; FF1D. 20 F7		; stay flashing LEDs forever!
mode_ok:
; continue with setup
	CLR $08					; FF1F. 7F 00 08	; disable all interrupt sources, just in case
	CLR $11					; FF22. 7F 00 11	; disable serial as well
	CLR $10					; FF25. 7F 00 10
; just in case, clear picoVDU screen
	LDX #$6000				; FF28. CE 60 00	; screen start address
cloop:
		CLR 0, X			; FF2B. 6F 00		; clear pointed byte
		INX					; FF2D. 08			; next byte
		CPX #$8000			; FF2E. 8C 80 00	; until end of screen
		BNE cloop			; FF31. 26 F8
	LDAA #%00000001			; FF33. 86 01		; will turn on PB0
	JSR pb_out				; FF35. BD FF D0	; update PB display
; check available zeropage
	LDX #$80				; FF38. CE 00 80	; lowest built-in RAM address
zloop:
		STX 0, X			; FF3B. EF 00		; store the address as data
		CPX 0, X			; FF3D. AC 00		; correctly stored?
			BNE zbad		; FF3F. 26 24		; if not, error!
		CLR 1, X			; FF41. 6F 01		; otherwise, clear the LSB
		TIM #0, 1, X		; FF43. 6B 01 00	; is it really clear?
			BNE zbad		; FF46. 26 1D		; if not, error!
		SEC					; FF48. 0D			; otherwise, set carry for the rotation
		LDAB #10			; FF49. C6 0A		; rotation limit
zbit:
			DECB			; FF4B. 5A			; avoid infinite loop
				BEQ zbad	; FF4C. 27 17
			ROR 1, X		; FF4E. 66 01		; rotate LSB
			BNE zbit		; FF50. 26 F9		; only zero at the end...
			BCC zbad		; FF52. 24 11		; ...and C should be set by then
		CMPB #1				; FF54. C1 01		; this is the expected value at the end
			BNE zbad		; FF56. 26 0D		; if no error happened...
		INX					; FF58. 08			; ...go for next address
		CPX #$FF			; FF59. 8C 00 FF	; until address limit!
		BNE zbit			; FF5C. 26 ED
	BEQ z_ok				; FF5E. 27 05		; otherwise zeropage is OK
zbad:
		LDAA #%11000000		; FF60. 86 C0		; two LEDs means ZP error
		JMP panic			; FF62. 7E FF C0	; jump to rotation code and lock
z_ok:
	LDAA #%00000010			; FF65. 86 02		; now turn on PB1
	JSR pb_out				; FF67. BD FF D0	; update PB display

; anything else?

; * Rosario seems OK, do a simple 1 kHz beep *
	LDAB #$FF				; FF6A. C6 FF		; cycle counter (255 ms)
	OIM #$10, IOR2			; FF6C. 72 10 03	; set SC_TX hi to enable sound
b_it:
		LDX #140			; FF6F. CE 00 8C	; value for ~500 µs (614t-52 ~140) (3)
bloop:
			DEX				; FF72. 09
			BNE bloop		; FF73. 26 FD		; delay loop
		TBA					; FF75. 17			; copy cycle (1)
		RORA				; FF76. 46
		RORA				; FF77. 46			; rotate right twice, now D0 is at D7 (2)
		ANDA #%10000000		; FF78. 84 80		; keep D7 only (2)
		PSHB				; FF7A. 37			; EEEEEEK (4)
		JSR pb_out			; FF7B. BD FF D0	; update PB for buzzer output (36)
		PULB				; FF7E. 33			; EEEEEEK (3)
		DECB				; FF7F. 5A			; next cycle (1+3)
		BNE b_it			; FF80. 26 ED
	AIM #%11101111, IOR2	; FF82. 71 EF 03	; clear SC_TX to disable sound
	LDAA #%00000101			; FF85. 86 05		; rotating '101' means all OK
	JMP panic				; FF87. 7E FF C0

	.dsb	panic-*, $FF	; FF8A. FF FF FF FF...

; *** useful routines ***
panic:
; * for testing, shift LED pattern in A *
		CLC					; FFC0. 0C			; clear carry for rotations
tloop:
			INX				; FFC1. 08
			BNE tloop		; FFC2. 26 FD		; delay loop
		ROLA				; FFC4. 49			; rotate pattern
		JSR pb_out			; FFC5. BD FF D0	; update PB display
		BRA tloop			; FFC8. 20 F7		; keep forever

	.dsb	pb_out-*, $FF	; FFCA. FF FF FF FF FF FF

pb_out:
; put data in accumulator A thru PB output (affects B and PA which gets back into input mode)
	STAA IOR1				; FFD0. 97 02		; store pattern into PA (3)
	LDAB #$FF				; FFD2. C6 FF		; all bits output... EEEEEK (2)
	STAB DDR1				; FFD4. D7 00		; ...for a while (3)
	EIM #%00000100, IOR2	; FFD6. 75 04 03	; pulse SC_TX in order to latch PA into PB (6+6)
	EIM #%00000100, IOR2	; FFD9. 75 04 03
	CLR DDR1				; FFDC. 7F 00 00	; PA back to input for lower power (5)
	RTS						; FFDF. 39			; * total 25t + 11t overhead

; *** *** rest is NOT for downloadable version *** ***
nmi:
; will turn PA0-3 as output LOW, while PB0-3 will go high and lock
	LDAA #$0F				; FFE0. 86 0F		; this is for both PB0-3 and DDR1
	JSR pb_out				; FFE2. BD FF D0	; update PB display
	CLR IOR1				; FFE5. 7F 00 02	; PA0-3 will go down as "open collector"
	STAA DDR1				; FFE8. 97 00		; update DDR1 for PA0 as output (will sink)
lock:
	BRA lock				; FFEA: 20 FE		; just lock...
null:
	RTI						; FFEC. 3B			; ...never used, just for safety

	.dsb	$FFF8-*, $FF	; FFED. FF FF FF
							; FFF0. FF FF FF FF FF FF FF FF

; *** end of ROM ***
vectors:
	.word null	; FFF8. FF EC (IRQ)
	.word null	; FFFA. FF EC (SWI)
	.word nmi	; FFF0. FF E0 (NMI)
	.word reset	; FFFE. FF 00 (RESET)
