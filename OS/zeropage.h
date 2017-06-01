; minimOS-63 zeropage & ABI stub

uz_top	.byt		;...$E0
z_used	.byt		;$E1
std_in	.byt		;$E2
stdout	.byt		;$E3
local1	.dsb 4	;$E4
local2	.dsb 4	;$E8
local3	.dsb 4	;$EC
zpar3	.dsb 4	;$F0
zpar2	.dsb 4	;$F4
zpar		.dsb 4	;$F8
kernptr	.word	;$FC
sys_sp	.word	;$FE

#define	_KERNEL(a)	LDX kernptr: JSR a,X
; Kernel function numbers by 3! Each entry is JMP abs
; besides zpar*, auxiliary parameter/error code in B
