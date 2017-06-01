; firmware for minimOS-63
; sort-of generic template
; v0.6a1
; (c)2017 Carlos J. Santisteban
; last modified 20170601-1743

#define		FIRMWARE	_FIRMWARE

; in case of standalone assembly
#include "usual.h"

; *** first some ROM identification ***
; this is expected to be loaded at an aligned address anyway
#ifndef	NOHEAD
fw_start:
	.asc	0, "m", CPU_TYPE	; special system wrapper
	.asc	"****", CR			; flags TBD
	.asc	"boot", 0			; mandatory filename for firmware
fw_splash:
	.asc	"0.6 firmware for "
fw_mname:
	.asc	MACHINE_NAME, 0

; advance to end of header
	.dsb	fw_start + $F8 - *, $FF	; for ready-to-blow ROM, advance to time/date field

; *** date & time in MS-DOS format at byte 248 ($F8) ***
	.word	$8800			; time, 17.00
	.word	$4AC1			; date, 2017/6/1

fwSize	=	$10000 - fw_start - 256	; compute size NOT including header!

; filesize in top 32 bits NOT including header, new 20161216
	.word	fwSize			; filesize
	.word	0				; 64K space does not use upper 16-bit
; *** end of standard header ***
#endif

; ********************
; *** cold restart ***
; ********************
; basic init
reset:
	SEI				; cold boot
	LDS #SPTR		; initialise stack pointer, machine-dependent

; *** optional firmware modules ***
post:

; might check ROM integrity here
;#include "firmware/modules/romcheck.s"

; SRAM test
;#include "firmware/modules/ramtest.s"

; *** set default CPU type ***
	LDAA #'X'			; default 6800 installed
	STAA fw_cpu			; store variable
; ...but check it for real afterwards
#include	"firmware/modules/cpu_check.s"

; *** preset kernel start address (standard label from ROM file) ***
	LDX #kernel			; get full address
	STX fw_warm			; store in sysvars, really needed?

	LDX #IRQ_FREQ	; interrupts per second
	STX irq_freq	; store speed...

	LDX #ticks			; first address in uptime seconds AND ticks, assume contiguous
res_sec:
		CLR 0, X		; reset byte
		INX					; next
		CPX #ticks+5		; first invalid address
		BNE res_sec			; continue until reached
;	LDAA #$C0			; enable T1 (jiffy) interrupt only
;	STAA VIA_J + IER


; ******* debug code, direct print some string *******
	LDX #fw_splash				; reset index
fws_loop:
		LDAA 0, X	; get char
			BEQ fws_cr			; no more to print
;		STX kern_ptr				; keep reg (not really needed)
;		JSR $c0c2			; Eh output
;		LDX					; restore index
		INX					; next char
		BRA fws_loop	
fws_cr:
	LDAA #LF				; trailing CR, needed by console!
;	JSR $c0c2			; direct print
; ******* end of debug code **************************

; *** firmware ends, jump into the kernel ***
start_kernel:
	LDX fw_warm		; get pointer
	JMP 0, X			; jump there!

; *** vectored NMI handler with magic number ***
nmi:
; registers are saved! but check system pointers

	PSHA					; save registers
	PSHB
	
; make NMI reentrant
	LDY sysptr			; get original word 
	LDX sysptr+1
	LDA systmp			; this byte only
	_PHY					; store them in similar order
	_PHX
	PHA
	
; prepare for next routine

	LDX fw_nmi			; get vector to supplied routine
; check whether user NMI pointer is valid
	LDX #3				; offset for (reversed) magic string, no longer preloaded (2)
	LDY #0				; offset for NMI code pointer (2)
nmi_chkmag:
		LDA (sysptr), Y		; get code byte (5)
		CMP fw_magic, X		; compare with string (4)
			BNE rst_nmi			; not a valid routine (2/3)
		INY					; another byte (2)
		DEX					; internal string is read backwards (2)
		BPL nmi_chkmag		; down to zero (3/2)
do_nmi:
	LDX #0				; null offset
	JSR nmi_call		; in case no 816 is used!
; *** here goes the former nmi_end routine ***
nmi_end:
	PLA					; retrieve saved vars
	_PLX
	_PLY
	STA systmp			; only this byte
	STX sysptr+1

	RTI					; resume normal execution, hopefully

nmi_call:
	_JMPX(fw_nmi)		; call actual code, ending in RTS (6)

fw_magic:
	.asc	"*jNU"		; reversed magic string

; *** execute standard NMI handler ***
rst_nmi:
	LDA #>nmi_end-1		; prepare return address
	PHA
	LDA #<nmi_end-1		; now LSB (safer than PEA)
	PHA
; ...will continue thru subsequent standard handler, its RTS will get back to ISR exit

; *** default code for NMI handler, if not installed or invalid, should end in RTS ***
std_nmi:
#include "firmware/modules/std_nmi.s"


; *** administrative functions ***
; install jump table
; kerntab <- address of supplied jump table
fw_install:
	_ENTER_CS			; disable interrupts! (5)
	LDY #0				; reset index (2)
fwi_loop:
		LDA (kerntab), Y	; get word from table as supplied (5)
		STA fw_table, Y		; copy where the firmware expects it (4)
		INY					; advance one byte
		BNE fwi_loop		; until whole page is done (3/2)
	_EXIT_CS			; restore interrupts if needed, will restore size too (4)
	_DR_OK				; all done (8)


; set IRQ vector
; kerntab <- address of ISR
fw_s_isr:
	_ENTER_CS				; disable interrupts and save sizes!
	LDX kerntab				; get pointer
	STX fw_isr				; store for firmware
	_EXIT_CS				; restore interrupts if needed
	_DR_OK					; done (8)


; set NMI vector
; kerntab <- address of NMI code (including magic string)
; might check whether the pointed code starts with the magic string
; no need to disable interrupts as a partially set pointer would be rejected
fw_s_nmi:
#ifdef	SAFE
	LDX #3					; offset to reversed magic string
	LDY #0					; reset supplied pointer
fw_sn_chk:
		LDA (kerntab), Y		; get pointed handler string char
		CMP fw_magic, X			; compare against reversed string
		BEQ fw_sn_ok			; no problem this far...
			_DR_ERR(CORRUPT)		; ...or invalid NMI handler
fw_sn_ok:
		INY						; try next one
		DEX
		BPL fw_sn_chk			; until all done
#endif
; transfer supplied pointer to firmware vector
	LDX kerntab				; get pointer
	STX fw_nmi				; store for firmware
	_DR_OK					; done (8)

; patch single function
; kerntab <- address of code
; Y <- function to be patched
fw_patch:
#ifdef		LOWRAM
	_DR_ERR(UNAVAIL)		; no way to patch on 128-byte systems
#else
	_ENTER_CS				; disable interrupts and save sizes! (5)
	LDA kerntab				; get full pointer
	LDX kerntab+1
	STA fw_table, Y			; store into firmware
	TXA
	STA fw_table+1, Y
	_EXIT_CS				; restore interrupts and sizes (4)
	_DR_OK					; done (8)
#endif


; get system info, API TBD
; zpar -> available pages of (kernel) SRAM
; zpar+2.W -> available BANKS of RAM
; zpar2.B -> speedcode
; zpar2+2.B -> CPU type
; zpar3.W/L -> points to a string with machine name
; *** WILL change ABI/API ***REVISE
fw_gestalt:
	LDA himem		; get pages of kernel SRAM (4)
	STA zpar		; store output (3)
	_STZX zpar+2	; no bankswitched RAM yet (4)
	_STZX zpar3+2	; same for string address (4)
	LDA #>fw_mname	; get string pointer
	LDY #<fw_mname
	STA zpar3+1		; put it outside
	STY zpar3
	LDA #SPEED_CODE	; speed code as determined in options.h (2+3)
	STA zpar2
	LDA fw_cpu		; get kind of CPU (previoulsy stored or determined) (4+3)
	STA zpar2+2
	_DR_OK			; done (8)

; poweroff etc
; Y <- mode (0 = suspend, 2 = warmboot, 4 = coldboot, 6 = poweroff)
; C -> not implemented
fw_power:
	TYA					; get subfunction offset
	TAX					; use as index
	_JMPX(fwp_func)		; select from jump table

fwp_off:
	_PANIC("{OFF}")		; just in case is handled

fwp_cold:
	LDX $FFFE			; 6800 RES vector
	JMP 0, X			; go!

fwp_susp:
	_DR_OK				; just continue execution

; *** temporary labels for unimplemented functions ***
fw_s_brk:
fw_jiffy:
fw_i_src:
fw_fgen:
fw_ctx:
	_DR_ERR(UNAVAIL)	; not yet implemented
; *** end of temporary labels ***

; sub-function jump table (eeeek)
fwp_func:
	.word	fwp_susp	; suspend	+FW_STAT
	.word	kernel		; shouldn't use this, just in case
	.word	fwp_cold	; coldboot	+FW_COLD
	.word	fwp_off		; poweroff	+FW_OFF

; *********************************
; *** administrative jump table *** changing
; *********************************
fw_admin:
; generic functions, esp. interrupt related
	.word	fw_gestalt	; GESTALT get system info (renumbered)
	.word	fw_s_isr	; SET_ISR set IRQ vector
	.word	fw_s_nmi	; SET_NMI set (magic preceded) NMI routine
	.word	fw_s_brk	; *** SET_BRK set debugger, new 20170517
	.word	fw_jiffy	; *** JIFFY set jiffy IRQ speed, ** TBD **
	.word	fw_i_src	; *** IRQ_SOURCE get interrupt source in X for total ISR independence

; pretty hardware specific
	.word	fw_power	; POWEROFF power-off, suspend or cold boot
	.word	fw_fgen		; *** FREQ_GEN frequency generator hardware interface, TBD

; not for LOWRAM systems
	.word	fw_install	; INSTALL copy jump table
	.word	fw_patch	; PATCH patch single function (renumbered)
	.word	fw_ctx		; *** CONTEXT context bankswitching

; *** minimOS BRK handler *** might go elsewhere
brk_hndl:		; label from vector list
; much like the ISR start
	PHA						; save registers
	_PHX
	_PHY
	JSR brk_handler			; standard label from IRQ
	_PLY					; restore status and return
	_PLX
	PLA
	RTI

; if case of no headers, at least keep machine name somewhere
#ifdef	NOHEAD
fw_splash:
	.asc	"0.6 firmware for "
fw_mname:
	.asc	MACHINE_NAME, 0
#endif

; filling for ready-to-blow ROM
#ifdef		ROM
	.dsb	kernel_call-*, $FF
#endif

; *** minimOS·65 function call WRAPPER ($FFC0) ***
* = kernel_call
cop_hndl:		; label from vector list
	_JMPX(fw_table)		; the old fashioned way

; filling for ready-to-blow ROM
#ifdef		ROM
	.dsb	admin_call-*, $FF
#endif

; *** administrative meta-kernel call primitive ($FFD0) ***
* = admin_call
	_JMPX(fw_admin)		; takes 5 clocks


; *** vectored IRQ handler ***
; might go elsewhere, especially on NMOS systems
irq:
	JMP (fw_isr)	; vectored ISR (6)

; filling for ready-to-blow ROM
#ifdef	ROM
	.dsb	lock-*, $FF
#endif

; ********** Standard ROM vectors **********
; *** panic routine, locks at very obvious address ($FFE1-$FFE2) ***
* = lock
	NOP					; locks at same address as 6502
panic_loop:
	BRA panic_loop	; always OK

; filling for ready-to-blow ROM
#ifdef	ROM
	.dsb	$FFEE-*, $FF
#endif

; *** Hitachi ROM vectors ***
* = $FFEE				; should be already at it
	.word	nmi			; TRAP	@ $FFEE

; *** Microcontroller ROM vectors ***
* = $FFF0				; should be already at it
	.word	nmi			; SCI	@ $FFF0
	.word	nmi			; TOF	@ $FFF2
	.word	nmi			; OCF	@ $FFF4
	.word	nmi			; ICF	@ $FFF6

; *** 6800 ROM vectors ***
* = $FFF8				; just in case
	.word	irq			; IRQ @ $FFF8
	.word	brk_hndl	; SWI @ $FFFA
	.word	nmi			; NMI @ $FFFC
	.word	reset		; RES @ $FFFE
