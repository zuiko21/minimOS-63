; firmware module for minimOS·63
; (C) 2018-2022 Carlos J. Santisteban
; last modified 20180219-0935

; **********************************************
; *** vectored NMI handler with magic number ***
; **********************************************
-nmi:
; registers are saved! but check system pointers
; make NMI reentrant *** could use some 6801 optimisation
#ifdef	MC6801
	LDX systmp			; get word...
	PSHX				; ...and keep it
#else
	LDAA systmp			; get original word
	LDAB systmp+1
	PSHB				; store them in similar order
	PSHA
#endif
; prepare for next routine
	LDX fw_nmi			; get vector to supplied routine
#ifdef	SAFE
; check whether user NMI pointer is valid
; as the magic string will NOT be "executed", can remain the same as 6502
	LDAA 0,X			; get first char
	CMPA #'U'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
	LDAA 1,X			; get second char
	CMPA #'N'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
	LDAA 2,X			; get third char
	CMPA #'j'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
	LDAA 3,X			; get fourth char
	CMPA #'*'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
#endif
	JSR 4,X				; routine OK, skip magic string!
; *** here goes the former nmi_end routine ***
nmi_end:
#ifdef	MC6801
	PULX				; retrieve...
	STX systmp			; ...and restore
#else
	PULA				; retrieve saved vars
	PULB
	STAB systmp+1		; restore values
	STAA systmp
#endif
	RTI					; resume normal execution, hopefully

; *** execute standard NMI handler, if magic string failed ***
rst_nmi:
	BSR std_nmi			; call standard routine
	BRA nmi_end			; and finish, much simpler

; *** default code for NMI handler, if not installed or invalid, should end in RTS ***
std_nmi:
#include "modules/std_nmi.s"

