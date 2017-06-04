; minimOS-63 zeropage
; v0.6a2
; last modified 20170604-2033

; *** setting these 2 bytes before will allow systmp ***
uz_top	.byt ;$DE
z_used	.byt		;$DF

systmp .word ; $E0
std_in	.byt		;$E2
stdout	.byt		;$E3
local1	.dsb 4	;$E4
local2	.dsb 4	;$E8
local3	.dsb 4	;$EC

; standard minimOS-63 ABI
zpar3	.dsb 4	;$F0
zpar2	.dsb 4	;$F4
zpar		.dsb 4	;$F8
kern_ptr	.word	;$FC
sys_sp	.word	;$FE

; #define	KERNEL(a)	LDX kernptr: JSR a,X
; Kernel function numbers by 3! Each entry is JMP abs
; besides zpar*, auxiliary parameter/error code in B
