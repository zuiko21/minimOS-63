; minimOS·63 firmware module
; (C) 2017-2018 Carlos J. Santisteban
; last modified 20180205-1051

; *** preset kernel start address (standard label from ROM file) ***
; no interface

	LDX #kernel			; get full address
	STX fw_warm			; store in fwvars, really needed?
