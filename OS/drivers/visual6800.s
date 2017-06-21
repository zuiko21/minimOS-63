; minimOS·63 basic I/O driver for visual6800 simulator
; v0.9a1
; (c) 2017 Carlos J. Santisteban
; last modified 20170621-1303

#include	"../usual.h"
; *** begins with sub-function addresses table ***
	FCB		DEV_CONIO	; D_ID, new values 20150323
	FCB		A_CIN|A_COUT	; character I/O only, non relocatable
	FDB		kow_cin		; cin, input from keyboard
	FDB		kow_cout	; cout, output to display
	FDB		kow_rts		; initialize device, called by POST only
	FDB		kow_rts		; poll, NOT USED
	FDB		1			; irrelevant frequency value as no polled interrupts
	FDB		kow_empty	; req, this one can't generate IRQs, thus SEC+RTS eeeeek
	FDB		kow_rts		; sin, no block input
	FDB		kow_rts		; sout, no block output
	FDB		kow_rts		; bye, no shutdown procedure
	FDB		debug_info	; info string
	FDB		0			; reserved for D_MEM, this is non-relocatable

; *** info string ***
debug_info:
	FCC		"Console I/O driver for visual6800 simulator 0.9a1"
	FCB		0

; *** output ***
kow_cout:
	LDAA io_c			; get char in case is control
	CMPA #13			; carriage return?
	BNE kow_ncr			; if so, should generate CR+LF
		LDAA #10			; LF first (and only)
kow_ncr:
	STAA $F				; print it
kow_rts:
	_DR_OK

; *** input ***
kow_cin:
	LDAA $D010			; check status
	BEQ kow_empty		; nothing available
		LDAA $D011			; otherwise read char
			CMPA #LF		; linux-like LF?
			BNE kow_emit	; do not process
				LDAA #CR		; or convert to CR
kow_emit:
			STAA io_c		; store result otherwise
			_DR_OK
kow_empty:
	_DR_ERR(EMPTY)		; nothing yet

