; default options for minimOS-63 and other modules
; sort-of-template, possibly KERAton inspired
; copy or link as options.h in root dir
; (c) 2015-2017 Carlos J. Santisteban
; last modified 20170612-1407

; *** set conditional assembly ***

; comment for optimized code without optional checks
#define		SAFE	_SAFE
#define		LOWRAM	_LOWRAM

; *** machine specific info ***
; select type as on executable headers
#define		CPU_TYPE	'M'

; *** machine hardware definitions ***
; Machine-specific ID strings, new 20150122, renamed 20150128, 20160120, 20160308

#define		MACHINE_NAME	"KERAton"
#define		MACHINE_ID	"keraton"

; Firmware selection, new 20160310, will pick up suitable template from firmware/
#define		ARCH			keraton

; Suitable driver package (add .h or .s as needed) in drivers/config/ folder, new 20160308
; may suit different configurations on a machine
#define		DRIVER_PACK		keraton_std

; *** Default files ***
; default shell from folder
#define		SHELL		monitor.s

; default NMI, BRK etc TBD ***********

; ** start of ROM **
ROM_BASE	=	$8000	; KERAton

; ** position of firmware, usually skipping I/O area **
FW_BASE		=	$E000	; standard value


; ** I/O definitions **

; I/O base address, usually one page, new 20160308
IO_BASE	=	$4000	; possible 16K area

; generic address declaration
PIA	=	IO_BASE	; TBD*****

; * optional ACIA/UART address (in external board!) *
ACIA1	=	IO_BASE + $D0	; ACIA address on most (no longer $DFE0 for easier decoding 688+138)
ACIA	=	ACIA1			; for increased compatibility

; *** set standard device *** new 20160331 
DEVICE	=	DEV_LED		; standard I/O device

; *** memory size ***
; * some pointers and addresses * renamed 20150220

; SRAM pages, just in case of mirroring/bus error * NOT YET USED
; 128 pages (32 kiB) is the new generic case, no longer the highest page number!
SRAM =	0

SPTR		=	$FF		; 
**************general case stack pointer, new 
name 20160308
SYSRAM		=	$0200	; generic case system RAM after zeropage and stack, most systems with at least 1 kiB RAM
ZP_AVAIL	=	$E1		; as long as locals start at $E4, not counting used_zp


; *** speed definitions ***

; ** master Phi-2 clock speed, used to compute remaining values! **
PHI2	=	921000		; clock speed in Hz

; ** jiffy interrupt frequency **
IRQ_FREQ =	150			; general case
; T1_DIV no longer specified, should be computed elsewhere
; could be PHI2/IRQ_FREQ-2

; speed code in fixed-point format, new 20150129
SPEED_CODE =	$0F		;.921 MHz system
; could be computed as PHI2*16/1000000
