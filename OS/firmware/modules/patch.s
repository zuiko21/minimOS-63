; minimOS·63 firmware module
; (C) 2018 Carlos J. Santisteban
; last modified 20180219-0954

; PATCH, patch single function
; kerntab <- address of code
; acc B <- function to be patched

-patch:
	_CRITIC				; disable interrupts, respects A
#ifdef	MC6801
	PSHA				; eeeek
	LDX #fw_table		; take base address
	ABX					; destination entry computed!
	LDD kerntab			; get targeted function pointer
	STD 0,X				; updated for firmware
	PULA				; eeeek
#else
	ADDB #<fw_table		; compute final LSB
	STAB systmp+1		; set temporary pointer
	LDAB #>fw_table		; now for MSB
	ADCB #0				; propagate carry
	STAB systmp			; pointer is ready
	LDX systmp			; set as index
	LDAB kerntab		; get function MSB
	STAB 0,X			; store into firmware
	LDAB kerntab+1		; and LSB
	STAB 1,X
#endif
	_NO_CRIT			; restore interrupts
	_DR_OK				; done
