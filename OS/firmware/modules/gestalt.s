; minimOS·63 firmware module
; (C) 2018-2020 Carlos J. Santisteban
; last modified 20180219-0943

; GESTALT, get system info, API TBD
; cpu_ll	= CPU type
; c_speed	= speed code (16b)
; str_pt	= points to a string with machine name
; ex_pt		= points to a map of default memory conf ???
; k_ram		= available pages of (kernel) SRAM
; *** MAY change ABI/API ***REVISE

-gestalt:

	LDAB fw_cpu			; get kind of CPU (previoulsy stored or determined) (3)
	LDX #SPEED_CODE		; speed code as determined in options.h ()
	STAB cpu_ll			; set outputs (4+)
	STX c_speed
	LDAA himem			; get pages of SRAM??? (3)
	STAA k_ram			; store output (4)
; no "high" RAM on this architecture
	LDX #fw_mname		; get string pointer (3)
	STX str_pt			; put it outside (5)
	LDX #fw_map			; pointer to standard map TBD ???? (3)
	STX ex_pt			; set output (5)
	_DR_OK				; done (7)
