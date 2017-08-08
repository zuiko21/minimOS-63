; minimOS·63 basic I/O driver for visual6800 simulator
; v0.9a2
; (c) 2017 Carlos J. Santisteban
; last modified 20170808-2228

#include	"../usual.h"
; *** begins with sub-function addresses table ***
	FCB		DEV_CNIO	; D_ID, new values 20150323
	FCB		A_BLIN|A_BOUT	; I/O only, non relocatable
	FDB		kow_cin		; blin, input from keyboard
	FDB		kow_cout	; bout, output to display
	FDB		kow_rts		; initialize device, called by POST only
	FDB		kow_rts		; poll, NOT USED
	FDB		1			; irrelevant frequency value as no polled interrupts
	FDB		kow_err		; req, this one can't generate IRQs, thus SEC+RTS eeeeek
	FDB		kow_err		; no config
	FDB		kow_err		; no status
	FDB		kow_rts		; bye, no shutdown procedure
	FDB		debug_info	; info string
	FDB		0			; reserved for D_MEM, this is non-relocatable

; *** info string ***
debug_info:
	FCC		"Console I/O driver for visual6800 simulator 0.9a2"
	FCB		0

; *** output ***
	tr_cnt	EQU	local1		; counter of transferred byted
kow_cout:
	LDX bl_siz			; original size...
	STX tr_cnt			; ...copied locally
	LDAA tr_cnt+1			; check LSB
	BEQ kow_onw			; full page, no trick to do
		INC tr_cnt			; trick for faster block count
kow_onw:
	LDX bl_ptr			; base address
kow_ol:
		LDAA 0,X			; get char in case is control
		CMPA #13			; carriage return?
		BNE kow_ncr			; if so, should generate CR+LF
			LDAA #10			; LF first (and only)
kow_ncr:
		STAA $F				; print it
		INX				; point to next eeeeeeeeeeeek
		DEC tr_cnt+1			; one less to go
		BNE kow_ol			; continue...
			DEC tr_cnt+1			; ...or update MSB
		BNE kow_ol
kow_act:
; *** common routine for actual transfer size computation ***
; not really needed as no errors possible...
		LDAA bl_siz+1			; original LSB...
		SUBA tr_cnt+1			; ...minus remaining bytes
		STAA bl_siz+1			; update size
		LDAA bl_siz			; same for MSB
		SBCA tr_cnt
		STAA bl_siz
kow_rts:
	_DR_OK

; *** input ***
kow_cin:
	LDX bl_siz			; original size...
	STX tr_cnt			; ...copied locally
	LDAA tr_cnt+1			; check LSB
	BEQ kow_inw			; full page, no trick to do
		INC tr_cnt			; trick for faster block count
kow_inw:
	LDX bl_ptr			; base address
kow_il:
		LDAA $D010			; check status
			BEQ kow_empty			; nothing available
		LDAA $D011			; otherwise read char
		CMPA #LF			; linux-like LF?
		BNE kow_emit			; do not process
			LDAA #CR			; or convert to CR
kow_emit:
		STAA 0,X			; store result otherwise
		INX				; point to next eeeeeeeeeeeek
		DEC tr_cnt+1			; one less to go
		BNE kow_il			; continue...
			DEC tr_cnt+1			; ...or update MSB
		BNE kow_il
	BRA kow_act			; report actual size
kow_empty:
	LDX #0				; 0 bytes...
	STX bl_siz			; ...actually transferred
kow_err:
	_DR_ERR(EMPTY)			; nothing yet

