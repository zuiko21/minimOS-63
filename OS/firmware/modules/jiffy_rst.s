; minimOS·63 firmware module
; (C) 2017-2022 Carlos J. Santisteban
; last modified 20180205-1054

; *** reset jiffy count ***
; no interface

	LDX #0				; not worth a loop
	STX ticks			; reset word
	STX ticks+2			; next
