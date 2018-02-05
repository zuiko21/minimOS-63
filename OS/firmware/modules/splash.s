; minimOS·63 firmware module
; (C) 2017-2018 Carlos J. Santisteban
; last modified 20180205-1059

; *** direct print splash string ***
; no interface, Visual6800 intended

	LDX #fw_splash		; reset index
fws_loop:
		LDAA 0,X			; get char
			BEQ fws_cr			; no more to print
		STAA $0F			; visual 6800 output
		INX					; next char
		BRA fws_loop
fws_cr:
	LDAA #CR			; trailing CR, needed by console!
	STAA $0F			; visual 6800 output
