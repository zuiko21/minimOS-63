; minimOS·63 firmware module
; (C) 2018-2020 Carlos J. Santisteban
; last modified 20180219-0943

; SET_ISR, set IRQ vector
;		INPUT
; kerntab	= address of ISR (will take care of all necessary registers)
;		0 means read current!

-set_isr:
; no CS as STX is atomic!
	LDX kerntab			; get pointer
	BEQ fw_r_isr		; in case of read...
		STX fw_isr			; ...or store for firmware
		_DR_OK				; done
fw_r_isr:
	LDX fw_isr			; get current
	STX kerntab			; and return it
	_DR_OK				; done

