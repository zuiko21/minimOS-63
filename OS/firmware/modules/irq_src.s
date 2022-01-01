; minimOS·63 firmware module
; (C) 2018-2022 Carlos J. Santisteban
; last modified 20180219-0950

; IRQ_SRC, investigate source of interrupt ***** REVISE ****
;		OUTPUT
; *** X	= 0 (periodic), 2 (async IRQ @ 65xx) *********************** 6502 ************
; *** notice NON-standard output register for faster indexed jump! ***
; other even values hardware dependent
irq_src:
; ****** TO BE DONE ******
	_DR_ERR(UNAVAIL)	; not yet implemented
