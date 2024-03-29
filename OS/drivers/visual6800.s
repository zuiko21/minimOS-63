; minimOS·63 basic I/O driver for visual6800 simulator
; v0.9a4
; (c) 2017-2022 Carlos J. Santisteban
; last modified 20170831-1638

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
	FCC		"Console I/O driver for visual6800 simulator 0.9a4"
	FCB		0

; *** output ***
kow_cout:
#ifdef	SAFE
	LDX bl_siz			; original size
		BEQ kow_rts			; nothing to do!
#endif
	LDX bl_ptr			; base address
kow_ol:
		LDAA 0,X			; get char in case is control
		CMPA #13			; carriage return?
		BNE kow_ncr			; if so, should generate CR+LF
			LDAA #10			; LF first (and only)
kow_ncr:
		STAA $F				; print it
		INX				; point to next eeeeeeeeeeeek
		DEC bl_siz+1			; one less to go
			BNE kow_ol			; continue...
		LDAA bl_siz			; check MSB value
			BEQ kow_rts			; no more...!
		DEC bl_siz			; ...or update MSB
		BRA kow_ol
kow_rts:
	_DR_OK

; *** input *** single-byte stub
kow_cin:
#ifdef	SAFE
	LDX bl_siz			; original size
		BEQ kow_rts			; nothing to do!
#endif
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
;		INX				; point to next eeeeeeeeeeeek
		DEC bl_siz+1			; one less to go
		LDAA bl_siz+1			; check value
		CMPA #$FF			; wraps?
		BNE kow_rts			; ...and we are done
			DEC bl_siz			; ...or update MSB
		BRA kow_rts
kow_empty:
	_DR_ERR(EMPTY)			; nothing yet
kow_err:
	_DR_ERR(UNAVAIL)		; generic error?
