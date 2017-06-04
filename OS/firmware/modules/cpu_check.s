; 6800 code snippet in order to reject a 6809 CPU!
; v0.6b1
; (C) 2017 Carlos J. Santisteban
; last modified 20170604-1905

.(
	LDA A #2		; 86 02
	TAP				; 06
	TSX				; 30		; safer than NOP
	BVS ok_6800		; 29 03
		JMP lock		; 7E FF E0
ok_6800:
; continue telling improvements over the original 6800?
	LSRD			; 04		; only on 6801/6301 will halve A
	CMP A #2		; 81 02		; regular 6800?
		BEQ set_cpu		; 27 04		; detected 6800/6802/6808 (A=2)
; microcontrollers have A halved at 1
	LDX #0			; CE 00 00	; on Hitachi will become 1
	XGDX			; 18		; A exchanged with X on Hitachi only ***beware of 68HCxx***
; should include here inocuous code for 00/01 & Hitachi, but telling HCs apart
	DEX			; 09		; DEY on 68HC11 will keep X=1 on Hitachi, 0 on Motorola
	CPX #0			; 8C 00 00	; classic uCs had X down to 0, do not know about Y on HC11,
	BNE set_cpu		; 26 02		; not HC, A stays at 0
		LDA A #3		; 86 03		; otherwise is HC11
set_cpu:
; *** here A=0 for 6301/6303, A=1 for 6801/6803 and A=2 for 6800/6802/6808 ***

.)
