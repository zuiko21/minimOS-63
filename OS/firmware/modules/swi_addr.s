; minimOS·63 firmware module
; (C) 2017-2022 Carlos J. Santisteban
; last modified 20180205-1052

; *** preset SWI handler ***
; no interface

	LDX #std_nmi		; default routine just like firmware-supplied NMI
	STX fw_dbg			; new fwvar
