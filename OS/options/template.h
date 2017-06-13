; default options for minimOS-63 and other modules
; sort-of-template, possibly KERAton inspired
; copy or link as options.h in root dir
; (c) 2015-2017 Carlos J. Santisteban
; last modified 20170613-1059

; *** set conditional assembly ***

; comment for optimized code without optional checks
#define		SAFE			_SAFE
#define		LOWRAM			_LOWRAM

; *** machine specific info ***
; select type as on executable headers
#define		CPU_TYPE		'M'

; *** machine hardware definitions ***
; Machine-specific ID strings, new 20150122, renamed 20150128, 20160120, 20160308

#define		MACHINE_NAME	"KERAton"
#define		MACHINE_ID		"keraton"

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

; I/O base address, usually one page
IO_BASE	=	$7F00	; possible 16K area is $4000-$7FFF

; generic address declaration
PIA		=	IO_BASE + $C0	; Use A4-5 on CS0-1, ¬IO.¬Y3 as ¬CS2

; * KERAton 2651 ACIA address *
ACIA	=	IO_BASE + $B0	; ACIA address ($80-$BF) from ¬IO.¬Y2

; with above devices, $00-$7F and $D0-$FF are free IO

; *** set standard device *** new 20160331 
DEVICE	=	DEV_LED		; standard I/O device ***TBD


; *** memory size ***
; * some pointers and addresses *

; SRAM pages, just in case of mirroring/bus error * NOT YET USED
SRAM 	=	0			; means 128-byte system!

; initial stack pointer
SPTR	=	$BB		; KERAton and other 128-byte RAM systems!

; system RAM location, where firmware and system variables start
SYSRAM	=	$80		; for 128-byte systems

; user-zp available space
; 128-byte 6800 systems leave 35(+1+2) bytes for user ($BC-$DE) assuming 60-byte stack AND sysvars space ($80-$BB)
; without multitasking, $DF (z_used) COULD be free, and so are $FE-FF (sys_sp)
ZP_AVAIL	=	$23


; *** speed definitions ***
; ** master Phi-2 clock speed, used to compute remaining values! **
PHI2	=	921600		; clock speed in Hz

; ** jiffy interrupt frequency **
IRQ_FREQ =	150			; general case
; T1_DIV no longer specified, should be computed elsewhere
; could be PHI2/IRQ_FREQ-2

; speed code in fixed-point format
SPEED_CODE =	$0F		; 921.6 kHz system
; could be computed as PHI2*16/1000000
