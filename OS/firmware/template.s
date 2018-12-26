; firmware for minimOS·63
; sort-of generic template, but intended for KERAton
; v0.6a18
; (c)2017-2018 Carlos J. Santisteban
; last modified 20181226-1438

#define		FIRMWARE	_FIRMWARE

; in case of standalone assembly
#include "../usual.h"

; *** first some ROM identification ***
; this is expected to be loaded at an aligned address anyway
#ifndef	NOHEAD
	; standard page alignment for CPP-MASM
	ORG		*-1&$FF00+$100	; eeeeeek
fw_start:
	FCB		0
	FCB		'm'
	FCB		CPU_TYPE			; executable for testing TBD
	FCC		"****"				; flags TBD
	FCB		CR
	FCC		"boot"				; mandatory filename for firmware
	FCB		0
fw_splash:
	FCC		"0.6a16 firmware for "	; version in comment
fw_mname:
	FCC		MACHINE_NAME
	FCB		0

; advance to end of header
	FILL	$FF, fw_start+$F8-*		; padding

; *** date & time in MS-DOS format at byte 248 ($F8) ***
	FDB		$5800				; time, 11.00
	FDB		$4ACE				; date, 2017/6/14

; filesize in top 32 bits NOT including header, new 20161216
	FDB		$10000-fw_start-256	; firmware size excluding header 
	FDB		0					; 64K space does not use upper 16-bit
; *** end of standard header ***
#else
; if case of no headers, at least keep machine name somewhere
fw_splash:
	FCC		"0.6a16 firmware for "
fw_mname:
	FCC		MACHINE_NAME
	FCB 	0
#endif

; 6800 architecture needs JuMP table at fixed address!

; **************************
; **************************
; ****** cold restart ******
; **************************
; **************************

reset:
; *** basic init ***
#include "firmware/modules/basic_init.s"


; ******************************
; *** minimal hardware setup ***
; ******************************

; check for peripherals and disable all interrupts *** will differ widely in MCUs
#include "firmware/modules/check_irq.s"

; *********************************
; *** optional firmware modules ***
; *********************************

; optional boot selector
;#include "firmware/modules/bootoff.s"

; might check ROM integrity here
;#include "firmware/modules/romcheck.s"

; some systems might copy ROM-in-RAM and continue at faster speed!
;#include "firmware/modules/rominram.s"

; startup beep
;#include "firmware/modules/beep.s"	; basic standard beep

; SRAM test
;#include "firmware/modules/ramtest.s"

; ********************************
; *** hardware interrupt setup ***
; ********************************

; peripheral initialisation (and stop beeping)
#include "firmware/modules/pia_init.s"

; ***********************************
; *** firmware parameter settings ***
; ***********************************

; *** set default CPU type ***
; just set expected default type as defined in options.h...
;#include "firmware/modules/default_cpu.s"
; ...or actually check for it!
#include "firmware/modules/cpu_check.s"
; do NOT include both files at once!

; *** continue parameter setting ***
; preset kernel start address
#include "firmware/modules/kern_addr.s"

; preset default SWI handler
#include "firmware/modules/swi_addr.s"

; no need to set NMI as it will be validated


; preset jiffy irq frequency
#include "firmware/modules/jiffy_hz.s"

; reset jiffy count
#include "firmware/modules/jiffy_rst.s"

; reset last installed kernel (new)
#include "firmware/modules/rst_lastk.s"

; *** direct print splash string ***
#include "firmware/modules/splash.s"

; *** optional network booting ***
; might modify the contents of fw_warm
;#include "firmware/modules/netboot.s"

; ************************
; *** start the kernel ***
; ************************
start_kernel:
#include "firmware/modules/start.s"


; ********************************
; ********************************
; ****** interrupt handlers ******
; ********************************
; ********************************

; **********************************************
; *** vectored NMI handler with magic number ***
; **********************************************
nmi:
#include "firmware/modules/nmi_hndl.s"

; ****************************
; *** vectored IRQ handler ***
; ****************************
; nice to be here, but might go elsewhere in order to save space, like between FW interface calls
irq:
#include "firmware/modules/irq_hndl.s"

; ***************************
; *** minimOS BRK handler ***
; ***************************
swi_hndl:				; label from vector list
#include "firmware/modules/swi_hndl.s"

; ********************************
; *** administrative functions ***
; ********************************

; *** generic functions ***

; *********************************
; GESTALT, get system info, API TBD
; *********************************
gestalt:
#include "firmware/modules/gestalt.s"

; ***********************
; SET_ISR, set IRQ vector
; ***********************
set_isr:
#include "firmware/modules/set_isr.s"

; ********************************
; SET_NMI, set NMI handler routine
; ********************************
set_nmi:
#include "firmware/modules/set_nmi.s"

; ********************************
; SET_DBG, set SWI handler routine
; ********************************
set_dbg:
#include "firmware/modules/set_dbg.s"

; *** interrupt related ***

; ***************************
; JIFFY, set jiffy IRQ period
; ***************************
jiffy:
#include "firmware/modules/jiffy.s"

; ****************************************
; IRQ_SRC, investigate source of interrupt
; ****************************************
; notice non-standard ABI!
irq_src:
#include "firmware/modules/irq_src.s"

; *** hardware specific ***

; **********************
; POWEROFF, shutdown etc *** TBD
; **********************
poweroff:
#include "firmware/modules/poweroff.s"

; ***********************************
; FREQ_GEN, generate frequency at PB7 *** TBD
; ***********************************
freq_gen:
;#include "firmware/modules/freq_gen.s"
	_DR_ERR(UNAVAIL)	; not yet implemented

; *** for higher-specced systems ***
; but a reduced, non patchable INSTALL is available instead
#ifndef	LOWRAM

; **************************
; INSTALL, supply jump table
; **************************
install:
#include "firmware/modules/install.s"

; ****************************
; PATCH, patch single function
; ****************************
patch:
#include "firmware/modules/patch.s"

#else

; these functions will not work on 128-byte systems!
; **************************
; INSTALL, supply jump table
; **************************
install:
#include "firmware/modules/install_lr.s"

; ****************************
; PATCH, patch single function
; ****************************
patch:
	_DR_ERR(UNAVAIL)	; not available

#endif

; *** experimental lock with flashing LED on keyboard ***
; assume PIA on KERAton

led_lock:
; make sure port B is ready to access keyboard
	LDAA PIA+CRB		; port B control register
	ANDA #251			; clear DDR access
	STAA PIA+CRB		; DDRB mode
	LDAB PIA+PRB		; previous direction
	ORAB #$F9			; these bits as output, at least
	STAB PIA+PRB
	EORA #4				; toggle DDR access
	STAA PIA+CRB		; now it is data register B
	LDAB #$B8			; initial value (LED on, LCD disabled)
	STAB PIA+PRB		; all set
; now a loop for LED blinking, toggling PB3
ll_loop:
; no need to preset regs as full range of X gives ~0.56s @ 921 kHz
			INX					; count (4)
			BNE ll_loop			; until expired (4)
		EORB #8				; toggle PB3
		STAB PIA+PRB		; set new value to LED
		BRA ll_loop			; continue forever

; ****************************
; *** some firmware tables ***
; ****************************

; ****** some odds ******

; temporary empty memory map
fw_map:
	FCB	0


; *********************************
; *** administrative JUMP table ***
; *********************************
	ORG	adm_ptr			; must be set in S19 format!

; generic functions, esp. interrupt related
	JMP gestalt			; GESTALT get system info (renumbered) @ $FFC0
	JMP set_isr			; SET_ISR set IRQ vector +3
	JMP set_nmi			; SET_NMI set (magic preceded) NMI routine +6
	JMP set_dbg			; SET_DBG set debugger, new 20170517 +9
	JMP jiffy			; JIFFY set jiffy IRQ speed, ** TBD ** +C
	JMP irq_src			; *** IRQ_SRC get interrupt source for total ISR independence +F

; pretty hardware specific
	JMP poweroff		; POWEROFF power-off, suspend or cold boot +12
	JMP freq_gen		; *** FREQ_GEN frequency generator hardware interface, TBD +15

; a reduced INSTALL is available for LOWRAM system, calling it is mandatory!
	JMP fw_install		; INSTALL copy jump table +18
#ifndef	LOWRAM
; not for LOWRAM systems
	JMP fw_patch		; PATCH patch single function (renumbered) +1B
#endif

; ****** at the NEW standard address $FFC0, this will be at $FFDE ******

; *****************************
; *** standard ROM inteface ***
; *****************************

; *** panic routine, locks at very obvious address ($FFE1-$FFE2) ***
	ORG	lock			; must be set in S19 format!
	SEI					; locks at about the same address as 6502
panic_loop:
		BRA panic_loop		; always OK

; ****** might use some space for interrupt handlers ******

; **********************************
; ****** hardware ROM vectors ******
; **********************************

; *** Hitachi ROM vectors ***
	ORG	$FFEE			; must be set in S19 format!

	FDB	nmi				; TRAP	@ $FFEE

; *** Microcontroller ROM vectors ***
	ORG	$FFF0			; should be already at it

	FDB	nmi				; SCI	@ $FFF0
	FDB	nmi				; TOF	@ $FFF2
	FDB	nmi				; OCF	@ $FFF4
	FDB	nmi				; ICF	@ $FFF6

; *** 6800 ROM vectors ***
	ORG	$FFF8			; just in case

	FDB	irq				; IRQ @ $FFF8
	FDB	swi_hndl		; SWI @ $FFFA
	FDB	nmi				; NMI @ $FFFC
	FDB	reset			; RES @ $FFFE
