; minimOS•63 SWI handler
; v0.6a2
; (c) 2017-2022 Carlos J. Santisteban
; last modified 20170808-2331

#include "../usual.h"
; this is currently a panic/crash routine!
; expected to end in RTI!!!

; first of all, send a CR to default device
	BSR brk_cr		; worth it
; let us get the original return address
; *** think about a padding byte on any BRK call, would make life much simpler!
	TSX				; current stack pointer, last used byte
	LDX 5,X		; get buried return address
brk_ploop:
		STX systmp		; save cursor***is this a safe place??
		LDAA 0,X	; get current char
			BEQ brk_term		; finish printed line
		BSR brk_out		; send out character!
		LDX systmp			; restore counter***
		INX				; next in string
		BRA brk_ploop	; until done
brk_term:
	BSR brk_cr		; another newline
; we are done, should call debugger if desired, otherwise we will just lock
	JMP lock		; let the system DIE
;	RTI				; *** otherwise let it finish the ISR

; send a newline to default device
brk_cr:
	LDAA #13		; CR
brk_out:
	LDAB #0			; default
	STAA io_c		; kernel parameter
	_KERNEL(COUT)	; system call
	RTS
