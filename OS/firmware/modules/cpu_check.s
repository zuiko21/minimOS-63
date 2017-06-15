; minimOS·63 firmware module
; 6800 code snippet in order to reject a 6809 CPU
; updated to tell Hitachi and other microcontrrollers!
; masm compliant style 20170613
; v0.6b3
; (C) 2017 Carlos J. Santisteban
; last modified 20170614-0932

	LDAA #2			; 86 02
	TAP				; 06		; is ROR $30 on 6809...
	TSX				; 30		; ...safer than NOP
	BVS ok_6800		; 29 03		; ROR on 6809 lets V clear
		JMP lock		; 7E FF E0	; incompatible CPU
ok_6800:
; *** continue telling improvements over the original 6800, if desired ***
	FCB $04			; 04		; LSRD *** only microcontrollers will halve A ***
	CMPA #2			; 81 02		; regular 6800?
		BEQ set_cpu		; 27 0D		; detected 6800/6802/6808 (A=2)
; microcontrollers have A halved at 1
	LDX #0			; CE 00 00	; on Hitachi will become 1
	FCB $18			; 18		; XGDX *** A exchanged with X on Hitachi only... ***
	INX				; 08		; ...but INY on 68HC11!!!
; X=0 on HC11, X=1 on 6801/6803, X=2 & A=0 on Hitachi, A=1 otherwise
	TSTA			; 4D		; zero only on Hitachi, already set
		BEQ set_cpu		; 27 05		; Hitachi detected (A=0)
	DEX				; 09		; becomes 0 on 6801/6803
		BEQ set_cpu		; 27 02		; not HC, A stays at 1
	LDAA #3			; 86 03		; otherwise is HC11
set_cpu:
; *** here A=0 for 6301/6303, A=1 for 6801/6803, A=2 for 6800/6802/6808, A=3 for 68HC11 ***
	STAA fw_cpu		; B7 xx xx	; update firmware variable
