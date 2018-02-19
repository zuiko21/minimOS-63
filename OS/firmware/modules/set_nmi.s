; minimOS·63 firmware module
; (C) 2018 Carlos J. Santisteban
; last modified 20180219-0946

; SET_NMI, set NMI vector
;		INPUT
; kerntab	= address of NMI code (including magic string, ends in RTS)
;		0 means read current!

; might check whether the pointed code starts with the magic string
; as the magic string will NOT get "executed", can safely be the same as 6502
; no CS as STX is atomic!

-set_nmi:
	LDX kerntab			; get pointer to supplied code + magic string
		BEQ fw_r_nmi		; in case of read...
#ifdef	SAFE
	LDAA 0,X			; first char
	CMPA #'U'			; is it correct?
		BNE fsn_err			; not!
	LDAA 1,X			; second char
	CMPA #'N'			; correct?
		BNE fsn_err			; not!
	LDAA 2,X			; third char
	CMPA #'j'			; correct?
		BNE fsn_err			; not!
	LDAA 3,X			; last char
	CMPA #'*'			; correct?
	BEQ fsn_valid		; yeah, proceed!
fsn_err:
		_DR_ERR(CORRUPT)	; ...or invalid NMI handler
fsn_valid:
#endif
; transfer supplied pointer to firmware vector
	STX fw_nmi			; store for firmware
	_DR_OK				; done
fw_r_nmi:
	LDX fw_nmi			; get current
	STX kerntab			; and return it
	_DR_OK				; done

