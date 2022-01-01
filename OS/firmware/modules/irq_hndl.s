; minimOS·63 firmware module
; (C) 2018-2022 Carlos J. Santisteban
; last modified 20180219-0938

; *** vectored IRQ handler ***
-irq:
	LDX fw_isr			; vectored ISR
	JMP 0,X				; MUST end in RTI
