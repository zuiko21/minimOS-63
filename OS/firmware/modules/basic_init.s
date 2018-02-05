; minimOS·63 firmware module
; (C) 2017-2018 Carlos J. Santisteban
; last modified 20180205-1036

; *** basic initialisation ***
; assumed to start at 'reset' label (in template file)
; no interface needed

-reset:
	SEI					; cold boot
	LDS #SPTR			; initialise stack pointer, machine-dependent
