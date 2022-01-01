; minimOS·63 firmware module
; (C) 2018-2022 Carlos J. Santisteban
; last modified 20180219-0946

; SET_DBG, set SWI handler
;		INPUT
; kerntab	= address of SWI routine (ending in RTS)
;		0 means read current!
; no CS as STX is atomic!

-set_dbg:
	LDX kerntab			; get pointer
	BEQ fw_r_brk		; in case of read...
		STX fw_brk			; store for firmware
		_DR_OK				; done
fw_r_brk:
	LDX fw_brk			; get current
	STX kerntab			; and return it
	_DR_OK				; done
