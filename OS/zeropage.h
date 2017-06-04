; minimOS-63 zeropage
; v0.6a1
; last modified 20170604-2025

; *** setting these 2 bytes before will allow systmp and help HC05 stack
uz_top	.byt		;...$E0
z_used	.byt		;$E1

; as 68HC05 has stack hardwired to $C0-$FF, needs special kernel with relocated locals
#ifndef HC05
std_in	.byt		;$E2
stdout	.byt		;$E3
local1	.dsb 4	;$E4
local2	.dsb 4	;$E8
local3	.dsb 4	;$EC
#endif

; standard minimOS-63 ABI, hardly mutable
zpar3	.dsb 4	;$F0
zpar2	.dsb 4	;$F4
zpar		.dsb 4	;$F8
kern_ptr	.word	;$FC
sys_sp	.word	;$FE

; #define	KERNEL(a)	LDX kernptr: JSR a,X
; Kernel function numbers by 3! Each entry is JMP abs
; besides zpar*, auxiliary parameter/error code in B
