; minimOS-63 zeropage
; v0.6a5
; last modified 20170607-1200

.zero
* = 0

; **********************************************************************************************
; ** for Motorola & Hitachi microcontrollers, convenient port addresses (free for other CPUs) **
; **********************************************************************************************
; these are NOT for the 68HC11!

MCU_DDR1	.byt 0			; Port 1 Data Direction Register
MCU_DDR2	.byt 0
MCU_IOR1	.byt 0			; Port 1 Data Register
MCU_IOR2	.byt 0

MCU_DDR3	.byt 0			; Port 3 Data Direction Register
MCU_DDR4	.byt 0
MCU_IOR3	.byt 0			; Port 3 Data Register
MCU_IOR4	.byt 0

MCU_TCSR	.byt 0			; Timer Control and Status Register
MCU_CNTH	.byt 0			; Counter High Byte
MCU_CNTL	.byt 0
MCU_OCRH	.byt 0			; Output Capture Register High Byte

MCU_OCRL	.byt 0
MCU_ICRH	.byt 0			; Input Capture Register High Byte
MCU_ICRL	.byt 0
MCU_P3CS	.byt 0			; Port 3 Control and Status Register

MCU_RMCR	.byt 0			; Rate and Mode Control Register
MCU_TRCS	.byt 0			; Transmit/Receive Control and Status Register
MCU_RXDR	.byt 0			; Receive Data Register
MCU_TXDR	.byt 0			; Transmit Data Register

MCU_RAMC	.byt 0			; RAM Control Register
MCU_RSVD	.dsb 11			; *** reserved ***

; ** LOWRAM microcontrollers system RAM begins here with sysvars, otherwise user zeropage can be down to zero **
MCU_SYSRAM:

; **************************************************************
; ** user zeropage space grows BACKWARDS from uz_top and down **
; **************************************************************

* = $DE
uz_top		.byt			; $DE, highest user zeropage byte
z_used		.byt			; $DF, available or actually used zeropage space

; ************************************
; ** some system reserved variables **
; ************************************

systmp		.word 0			; $E0 for interrupt use, free when interrupts shut off

; ** default I/O devices **
; these will serve as GLOBAL default devices on LOWRAM systems

std_in		.byt 0			; $E2
stdout		.byt 0			; $E3

; ******************************************
; ** local variables for kernel functions **
; ******************************************
* = $E4						; local variables standard address
locals:						; old label for compatibility

; *** include aliases here for local1/locpt1 ***
dr_aut: ma_ix: mm_sig: iol_dev:
local1: locpt1	.dsb	4	; variables for kernel functions @ $E4

dq_off	= dr_aut+1
dq_ptr	= dr_aut+2

; *** include aliases here for local2/locpt2 ***
da_ptr: exec_p: rl_dev: ma_lim:
local2: locpt2	.dsb	4	; variables for kernel functions @ $E8

dr_id	= da_ptr+2
dr_feat	= da_ptr+3

; *** include aliases here for local3/locpt3 ***
dte_ptr: exe_sp: rh_scan:
rl_cur: ex_wr:
local3: locpt3	.dsb	4	; variables for kernel functions @ $EC

loc_str	= rh_scan+2			; temporary string pointer

; ***********************
; ** kernel parameters **
; ***********************

; *** include aliases here for zpar3/zaddr3 ***
b_sig: kerntab: ln_siz:
ex_pt: ma_rs:				; mandatory 24-bit size

zpar3: zaddr3	.dsb	4	; ** up to 4 bytes, including older names @ $F0 **

k_ram	= ma_rs+2			; Kernel RAM pages (0 = 128 byte system)

; *** include aliases here for zpar2/zaddr2 ***
up_ticks: def_io: irq_hz
ma_pt: str_pt:

zpar2: zaddr2	.dsb	4	; ** up to 4 bytes, including older names @ $F4 **

; *** include aliases here for zpar/zaddr ***
io_c: ma_align: cpu_ll:
up_sec: w_rect:				; 32-bit

zpar: zaddr		.dsb	4	; ** up to 4 bytes, including older names @ $F8 **

c_speed	= cpu_ll+1

; ***************************
; ** kernel call interface **
; ***************************

kern_ptr	.word 0			; $FC, will point to supplied JUMP table

; ** temporary SP storage, it is FREE for singletask systems, or valid within disabled interrupts **

sys_sp		.word 0			; $FE
