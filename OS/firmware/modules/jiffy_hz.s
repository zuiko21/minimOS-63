; minimOS·63 firmware module
; (C) 2017-2022 Carlos J. Santisteban
; last modified 20180205-1054

; *** preset jiffy irq frequency *** REVISE
; should get accurate values from options.h
	LDX #IRQ_PER		; this is period!
; here comes the standard procedure, where JIFFY is able to change frequency!
	STX irq_hz		; set parameter
	_ADMIN(JIFFY)		; proceed
; machines with FIXED IRQ frequency (eg. KERAton) do this instead
;	STX irq_freq		; only for reading value!
