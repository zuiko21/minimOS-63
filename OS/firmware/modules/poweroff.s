; minimOS·63 firmware module
; (C) 2018 Carlos J. Santisteban
; last modified 20180219-0952

; POWEROFF, poweroff etc
; acc B <- mode (0 = suspend, 2 = warmboot, 4 = coldboot, 6 = poweroff)
; *** special codes for SWI/NMI triggering (10 = NMI, 12 = SWI) ***
; C -> not implemented

poweroff:
	TSTB				; is it zero? could be CMPB #PW_STAT
	BNE fwp_ns			; not suspend
; this would be suspend code...
fwp_exit:
		_DR_OK				; just continue execution
fwp_ns:
	CMPB #PW_WARM		; warm?
	BNE fwp_nw			; not
		JMP start_kernel	; warm boot!
fwp_nw:
	CMPB #PW_COLD		; cold?
	BNE fwp_nc			; not
		JMP reset			; or go to internal reset!
fwp_nc:
	CMPB #PW_HARD		; NMI?
	BNE fwp_nhi			; not
		JMP nmi				; NMI!
fwp_nhi:
	CMPB #PW_SOFT		; SWI? Not sure if really needed...
	BNE fwp_nsi			; not
		JMP brk_hndl		; SWI!
fwp_nsi:
	CMPB #PW_OFF		; off?
	BNE fwp_exit		; not recognised
; should add here shutdown code
		_PANIC("{OFF}")		; placeholder if not implemented
