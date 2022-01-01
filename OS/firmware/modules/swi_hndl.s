; minimOS·63 firmware module
; (C) 2018-2022 Carlos J. Santisteban
; last modified 20180219-0938

; *** vectored SWI handler ***
; might go elsewhere
-swi_hndl:				; label from vector list
	LDX fw_dbg			; get vectored pointer
	JMP 0,X				; MUST end in RTI
