; minimOS·63 firmware module
; (C) 2017-2022 Carlos J. Santisteban
; last modified 20180205-1100

; *** start the kernel ***
; no interface

-start_kernel:
	LDX fw_warm			; get pointer
	JMP 0,X				; jump there!
