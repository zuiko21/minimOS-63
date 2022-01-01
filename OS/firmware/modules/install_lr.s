; minimOS·63 firmware module
; (C) 2018-2022 Carlos J. Santisteban
; last modified 20180219-0957

-install:
; *** limited version ***
; no CS as STX is atomic!
	LDX kerntab			; the supplied table will be pointed...
	STX kern_ptr		; ...from the standard address
	_DR_OK				; done
