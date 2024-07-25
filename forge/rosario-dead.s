; minimal test for Rosario 6301/6303 SBC
; (c) 2024 Carlos J. Santisteban
; last modified 20240723-1634

DDR1	= $0				; Write only!
DDR2	= $1				; Write only!
IOR1	= $2
IOR2	= $3				; write affects P20-P24 only, read mode at P25-P27


	* = $FF00				; make that $7F00 for downloadable version
reset:
; basic 6301/6303 init
	SEI						; FF00. 0F
	LDS #$00FF				; FF01. 8E 00 FF	; stack is ready, just in case
	LDAA #$FF				; FF04. 86 FF		; PA...
	STAA DDR1				; FF06. 97 00		; ...as output

	LDAA #%01000000			; FF08. 86 01		; will turn on PA0 EEEEK

; for testing, shift LED pattern
tloop:
			INX				; FF0A. 08
			BNE tloop		; FF0B. 26 FD		; delay loop
		ROLA				; FF0D. 49			; rotate pattern
		STAA IOR1			; FF0E. 97 02		; store pattern into PA
		BRA tloop			; FF10. 20 F8		; keep forever

; *** *** rest is NOT for downloadable version *** ***
	.dsb	nmi-*, $FF		; FF12. FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF20. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF30. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF40. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF50. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF60. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF70. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF80. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FF90. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FFA0. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FFB0. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FFC0. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
							; FFD0. FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF
nmi:
; will turn PA0-3 as output HIGH, and keep rotating
	LDAA #$0F				; FFE0. 86 40		; EEEK this is for PA0-3
	JMP tloop				; FFE2. 7E FF 0A	; update PA display
null:
	RTI						; FFE5. 3B			; ...never used, just for safety

	.dsb	$FFF8-*, $FF	; FFE6. FF FF FF FF FF FF FF FF FF FF
							; FFF0. FF FF FF FF FF FF FF FF

; *** end of ROM ***
vectors:
	.word null	; FFF8. FF E5 (IRQ)
	.word null	; FFFA. FF E5 (SWI)
	.word nmi	; FFF0. FF E0 (NMI)
	.word reset	; FFFE. FF 00 (RESET)
