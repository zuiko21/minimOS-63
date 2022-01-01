; minimOS·63 firmware module
; (C) 2017-2022 Carlos J. Santisteban
; last modified 20180205-1048

; *** set default CPU type ***
; no interface needed

; just set expected default type as defined in options.h...
	LDAA #'M'			; default 6800 installed
	STAA fw_cpu			; store variable
