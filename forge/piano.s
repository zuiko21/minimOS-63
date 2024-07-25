; simple piano for Rosario 6301/6303 SBC
; PA0-PA7 = C4-C5
; PA7 + PA0-PA6 = C5-B5
; hold next button for Sharp, previous for Flat (e.g. PA0+PA1 = C#4) 
; (c) 2024 Carlos J. Santisteban
; last modified 20240726-0035

	* = $FE00				; make that $1000 for downloadable version

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
	SEI						; FE00. 0F
	LDS #$00FF				; FE01. 8E 00 FF	; stack is ready, just in case
	CLR DDR1				; FE04. 7F 00 00	; PA as input (just in case)
	CLR IOR2				; FE07. 7F 00 03	; will keep IOSEL, SC_TX and SC_CL low
	LDAA #$14				; FE0A. 86 14		; P22 (SC_CL) and P24 (SC_TX) will be output for PB control and audio
	STAA DDR2				; FE0C. 97 01		; update DDR2
; assume correct mode
	CLR $08					; FE1F. 7F 00 08	; disable all interrupt sources, just in case
	CLR $11					; FE22. 7F 00 11	; disable serial as well
	CLR $10					; FE25. 7F 00 10
; don't care about picoVDU screen
	CLR last				; FE28. 7F 00 80	; initial value (no key)
	CLRA					; FE2B. 4F			; for good measure

; *** main loop ***
key:
; check whether a key is pressed
			LDAB IOR1		; FE2C. D6 02		; read PA
			CMPB last		; FE2E. D1 80		; compare to previous
			BEQ key			; FE30. 26 FA		; wait for any key change
new:
; compute frequency
		STAB last			; FE32. D7 80		; register current key
		LDX #index			; FE34. CE FE 69	; get offset table base
		ABX					; FE37. 3A			; get pointer to selected key
		LDAB 0, X			; FE38. E6 00		; this is the required offset
		LDX #tx				; FE3A. CE FF 69	; count values table base
		ABX					; FE3D. 3A			; point to selected note
		LDX 0, X			; FE3E. EE 00		; and this is the 16-bit loop value
		STX buffer			; FE40. DF 81		; store for later
		BEQ key				; FE42. 27 E8		; only if it's a valid note
; play the note!
	OIM #$10, IOR2			; FE44. 72 10 03	; set SC_TX hi to enable sound
cycle:
; reload X from temp
	LDX buffer				; FE47. DE 81		; get cached loop value (minus 40t overhead) (4)
bloop:
			DEX				; FE49. 09
			BNE bloop		; FE4A. 26 FD		; delay loop (4X)
; toggle speaker output
		EORA #%10000000			; FE4C. 88 80		; toggle PB7 (2)
		STAA IOR1				; FE4E. 97 02		; store pattern into PA (3)
		LDAB #$FF				; FE50. C6 FF		; all bits output... EEEEEK (2)
		STAB DDR1				; FE52. D7 00		; ...for a while (3)
		EIM #%00000100, IOR2	; FE54. 75 04 03	; pulse SC_TX in order to latch PA into PB (6+6)
		EIM #%00000100, IOR2	; FE57. 75 04 03
		CLR DDR1				; FE5A. 7F 00 00	; PA back to input for lower power (5)
; check whether key has changed
		LDAB IOR1			; FE5D. D6 02		; check PA (3)
		CMPB last			; FE5F. D1 80		; any change? (3)
		BEQ cycle			; FE61. 27 E4		; otherwise keep playing (3)
; if so, back to key process
	AIM #%11101111, IOR2	; FE63. 71 EF 03	; clear SC_TX to disable sound
	JMP new					; FE66. 7E FE 32

; *** data ***
; offset for every valid note into 16-bit tables ($FE69)
index:
	.byt	0				; 00		; [no key] = REST
	.byt	c4-tx			; 02		; [1] = C4
	.byt	d4-tx			; 06		; [2] = D4
	.byt	cs4-tx			; 04		; [1+2] = C#4
	.byt	e4-tx			; 0A		; [4] = E4
	.byt	0				; 00		; [1+4] not valid
	.byt	ds4-tx			; 08		; [2+4] = D#4
	.byt	0				; 00		; [1+2+4] not valid
	.byt	f4-tx			; 0C		; [8] = F4
	.dsb	7, 0			; 00...		; [9...15] not valid
	.byt	g4-tx			; 10		; [16] = G4
	.dsb	7, 0			; 00...		; [17...23] not valid
	.byt	fs4-tx			; 0E		; [8+16] = F#4
	.dsb	7, 0			; 00...		; [25...31] not valid
	.byt	a4-tx			; 14		; [32] = A4
	.dsb	15, 0			; 00...		; [33...47] not valid
	.byt	gs4-tx			; 12		; [16+32] = G#4
	.dsb	15, 0			; 00...		; [49...63] not valid
	.byt	b4-tx			; 18		; [64] = B4
	.dsb	31, 0			; 00...		; [65...95] not valid
	.byt	as4-tx			; 16		; [32+64] = A#4
	.dsb	31, 0			; 00...		; [97...127] not valid
	.byt	c5-tx			; 1A		; [128] = C5
	.byt	c5-tx			; 1A		; [1+128] = C5 as well
	.byt	d5-tx			; 1E		; [2+128] = D5
	.byt	cs5-tx			; 1C		; [1+2+128] = C#5
	.byt	e5-tx			; 22		; [4+128] = E5
	.byt	0				; 00		; [133] not valid
	.byt	ds5-tx			; 20		; [2+4+128] = D#5
	.byt	0				; 00		; [135] not valid
	.byt	f5-tx			; 24		; [8+128] = F5
	.dsb	7, 0			; 00...		; [137...143] not valid
	.byt	g5-tx			; 28		; [16+128] = G5
	.dsb	7, 0			; 00...		; [145...151] not valid
	.byt	fs5-tx			; 26		; [8+16+128] = F#5
	.dsb	7, 0			; 00...		; [153...159] not valid
	.byt	a5-tx			; 2C		; [32+128] = A5
	.dsb	15, 0			; 00...		; [161...175] not valid
	.byt	gs5-tx			; 2A		; [16+32+128] = G#5
	.dsb	15, 0			; 00...		; [177...191] not valid
	.byt	b5-tx			; 30		; [64+128] = B5
	.dsb	31, 0			; 00...		; [193...223] not valid
	.byt	as5-tx			; 2E		; [32+64+128] = A#5
	.dsb	31, 0			; 00...		; [225...255] not valid

; selected X values for notes, MUST be BIG-endian! ($FF69)
tx:
	.word	0				; 00 00		; * null note *
c4:
	.word	577				; 02 41
cs4:
	.word	544				; 02 20
d4:
	.word	513				; 02 01
ds4:
	.word	484				; 01 E4
e4:
	.word	456				; 01 C8
f4:
	.word	430				; 01 AE
fs4:
	.word	405				; 01 95
g4:
	.word	382				; 01 7E
gs4:
	.word	360				; 01 68
a4:
	.word	339				; 01 53
as4:
	.word	319				; 01 3F
b4:
	.word	301				; 01 2D
c5:
	.word	284				; 01 1C
cs5:
	.word	267				; 01 0B
d5:
	.word	252				; 00 FC
ds5:
	.word	237				; 00 ED
e5:
	.word	223				; 00 DF
f5:
	.word	210				; 00 D2
fs5:
	.word	198				; 00 C6
g5:
	.word	186				; 00 BA
gs5:
	.word	175				; 00 AF
a5:
	.word	165				; 00 A5
as5:
	.word	155				; 00 9B
b5:
	.word	145				; 00 91
c6:
	.word	137				; 00 89		; just in case

; *** *** rest is NOT for downloadable version *** ***
	.dsb	null-*, $FF		; FF FF FF FF...

null:
	RTI						; FFF0. 3B			; ...never used, just for safety

	.dsb	vectors-*, $FF	; FFF1. FF FF FF FF FF FF FF

; *** end of ROM ***
vectors:
	.word null	; FFF8. FF F0 (IRQ)
	.word null	; FFFA. FF F0 (SWI)
	.word null	; FFF0. FF F0 (NMI)
	.word reset	; FFFE. FE 00 (RESET)
