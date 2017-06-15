; minimOS·63 zeropage
; v0.6a5
; last modified 20170614-1234
; MASM compliant 20170614

	ORG		0

; **********************************************************************************************
; ** for Motorola & Hitachi microcontrollers, convenient port addresses (free for other CPUs) **
; **********************************************************************************************
; these are NOT for the 68HC11!

MCU_DDR1	RMB 1			; Port 1 Data Direction Register
MCU_DDR2	RMB 1
MCU_IOR1	RMB 1			; Port 1 Data Register
MCU_IOR2	RMB 1

MCU_DDR3	RMB 1			; Port 3 Data Direction Register
MCU_DDR4	RMB 1
MCU_IOR3	RMB 1			; Port 3 Data Register
MCU_IOR4	RMB 1

MCU_TCSR	RMB 1			; Timer Control and Status Register
MCU_CNTH	RMB 1			; Counter High Byte
MCU_CNTL	RMB 1
MCU_OCRH	RMB 1			; Output Capture Register High Byte

MCU_OCRL	RMB 1
MCU_ICRH	RMB 1			; Input Capture Register High Byte
MCU_ICRL	RMB 1
MCU_P3CS	RMB 1			; Port 3 Control and Status Register

MCU_RMCR	RMB 1			; Rate and Mode Control Register
MCU_TRCS	RMB 1			; Transmit/Receive Control and Status Register
MCU_RXDR	RMB 1			; Receive Data Register
MCU_TXDR	RMB 1			; Transmit Data Register

MCU_RAMC	RMB 1			; RAM Control Register
MCU_RSVD	RMB 11			; *** reserved ***

; ** LOWRAM microcontrollers system RAM begins here with sysvars, otherwise user zeropage can be down to zero **
MCU_SYSRAM:

; **************************************************************
; ** user zeropage space grows BACKWARDS from uz_top and down **
; **************************************************************

	ORG		$DE
uz_top		RMB 1			; $DE, highest user zeropage byte
z_used		RMB 1			; $DF, available or actually used zeropage space

; ************************************
; ** some system reserved variables **
; ************************************

systmp		RMB 2			; $E0 for interrupt use, free when interrupts shut off

; ** default I/O devices **
; these will serve as GLOBAL default devices on LOWRAM systems

std_in		RMB 1			; $E2
stdout		RMB 1			; $E3

; ******************************************
; ** local variables for kernel functions **
; ******************************************
	ORG		$E4				; local variables standard address
locals:						; old label for compatibility

; *** include aliases here for local1/locpt1 ***
dr_aut:
ma_ix:
mm_sig:
iol_dev:
local1:
locpt1		RMB 4	; variables for kernel functions @ $E4

dq_off		EQU dr_aut+1
dq_ptr		EQU dr_aut+2

; *** include aliases here for local2/locpt2 ***
da_ptr:
exec_p:
ma_lim:
local2:
locpt2		RMB 4	; variables for kernel functions @ $E8

dr_id		EQU da_ptr+2
dr_feat		EQU da_ptr+3
rl_dev		EQU dr_id

; *** include aliases here for local3/locpt3 ***
dte_ptr:
exe_sp:
rh_scan:
rl_cur:
ex_wr:
local3:
locpt3		RMB 4	; variables for kernel functions @ $EC

loc_str		EQU rh_scan+2	; temporary string pointer

; ***********************
; ** kernel parameters **
; ***********************

; *** include aliases here for zpar3/zaddr3 ***
b_sig:
kerntab:
ln_siz:
ex_pt:
ma_rs:
zpar3:
zaddr3		RMB 4	; ** up to 4 bytes, including older names @ $F0 **

k_ram		EQU ma_rs+2		; Kernel RAM pages (0 for 128-byte system)

; *** include aliases here for zpar2/zaddr2 ***
up_ticks:
def_io:
irq_hz
ma_pt:
str_pt:

zpar2:
zaddr2		RMB 4	; ** up to 4 bytes, including older names @ $F4 **

; *** include aliases here for zpar/zaddr ***
io_c:
ma_align:
cpu_ll:
up_sec:
w_rect:				; 32-bit

zpar:
zaddr			RMB 4	; ** up to 4 bytes, including older names @ $F8 **

c_speed		EQU cpu_ll+1

; ***************************
; ** kernel call interface **
; ***************************

	ORG		kern_ptr		; should be there already

	RMB 2			; $FC, will point to supplied JUMP table

; ** temporary SP storage, it is FREE for singletask systems, or valid within disabled interrupts **

sys_sp		RMB 2			; $FE
