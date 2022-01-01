; minimOS·63 firmware module
; (C) 2018-2022 Carlos J. Santisteban
; last modified 20180219-0954

; INSTALL, copy jump table
;		INPUT
; kerntab	= address of supplied JUMP table

; CS is 28+3 bytes, (6+)18+ 256*43 = (6+) 11026 cycles
-install:
	_CRITIC				; disable interrupts!
	PSHA				; EEEEEEEEEEEK
	CLRB				; reset counter (2)
	LDX #fw_table		; get destination pointer (3) eeeeeeeek
	STX systmp			; store temporarily (5)
fwi_loop:
		LDX kerntab			; set origin pointer (4)
		LDAA 0,X			; get byte from table as supplied (5)
		INX					; increment (4)
		STX kerntab			; ...and update (5)
		LDX systmp			; switch to destination (4)
		STAA 0,X			; copy the byte (6)
		INX					; increment (4)
		STX systmp			; ...and update (5)
		INCB				; advance counter (2)
		BNE fwi_loop		; until whole page is done (4)
;	DEC kerntab			; restore original page, if needed (6)
	LDX #fw_table		; the firmware table will be pointed... (3)
	STX kern_ptr		; ...from the standard address (5)
	PULA				; EEEEEEEEEEEK
	_NO_CRIT			; restore interrupts if needed
	_DR_OK				; all done

