; firmware for minimOS·63
; sort-of generic template, but intended for KERAton
; v0.6a10
; (c)2017 Carlos J. Santisteban
; last modified 20170821-1807
; MASM compliant 20170614

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
	FCC		"0.6a10 firmware for "	; version in comment
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
#endif

; **************************
; **************************
; ****** cold restart ******
; **************************
; **************************
; basic init
reset:
	SEI					; cold boot
	LDS #SPTR			; initialise stack pointer, machine-dependent

; *********************************
; *** optional firmware modules ***
; *********************************
post:

; might check ROM integrity here
;#include "firmware/modules/romcheck.s"

; SRAM test
;#include "firmware/modules/ramtest.s"

; ***********************************
; *** firmware parameter settings ***
; ***********************************
; *** set default CPU type ***
	LDAA #'M'			; default 6800 installed
	STAA fw_cpu			; store variable
; ...but check it for real afterwards, at least rejecting 6809
#include	"modules/cpu_check.s"

; *** preset kernel start address (standard label from ROM file) ***
	LDX #kernel			; get full address
	STX fw_warm			; store in fwvars, really needed?

; *** preset SWI & NMI handlers ***
	LDX #std_nmi		; default routine just like firmware-supplied NMI
	STX fw_brk			; new fwvar
; as NMI will be validated, no need to preinstall it!

; *** preset jiffy irq frequency ***
; should get accurate values from options.h
	LDX #150		; this is freq, not period!
	STX 
; *** reset jiffy count ***
	LDX #ticks			; first address in uptime seconds AND ticks, assume contiguous
res_sec:
		CLR 0,X				; reset byte
		INX					; next
		CPX #ticks+5		; first invalid address
		BNE res_sec			; continue until reached

; ********************************
; *** hardware interrupt setup ***
; ********************************
; KERAton has no VIA, jiffy IRQ is likely to be fixed on, perhaps enabling counter input on PIA
;	LDAA #$C0			; enable T1 (jiffy) interrupt only
;	STAA VIA_J+IER

; **********************************
; *** direct print splash string ***
; **********************************
	LDX #fw_splash		; reset index
fws_loop:
		LDAA 0,X			; get char
			BEQ fws_cr			; no more to print
		STAA $0F			; visual 6800 output
		INX					; next char
		BRA fws_loop
fws_cr:
	LDAA #CR			; trailing CR, needed by console!
	STAA $0F			; visual 6800 output

; *** might try to download a kernel just here, updating fw_warm accordingly ***

; ************************
; *** start the kernel ***
; ************************
start_kernel:
	LDX fw_warm			; get pointer
	JMP 0,X				; jump there!

; ********************************
; ********************************
; ****** interrupt handlers ******
; ********************************
; ********************************

; **********************************************
; *** vectored NMI handler with magic number ***
; **********************************************
nmi:
; registers are saved! but check system pointers
; make NMI reentrant *** could use some 6801 optimisation
#ifdef	MC6801
	LDX systmp			; get word...
	PSHX				; ...and keep it
#else
	LDAA systmp			; get original word
	LDAB systmp+1
	PSHB				; store them in similar order
	PSHA
#endif
; prepare for next routine
	LDX fw_nmi			; get vector to supplied routine
#ifdef	SAFE
; check whether user NMI pointer is valid
; as the magic string will NOT be "executed", can remain the same as 6502
	LDAA 0,X			; get first char
	CMPA #'U'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
	LDAA 1,X			; get second char
	CMPA #'N'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
	LDAA 2,X			; get third char
	CMPA #'j'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
	LDAA 3,X			; get fourth char
	CMPA #'*'			; matches magic string?
		BNE rst_nmi			; error, use standard handler
#endif
	JSR 4,X				; routine OK, skip magic string!
; *** here goes the former nmi_end routine ***
nmi_end:
#ifdef	MC6801
	PULX				; retrieve...
	STX systmp			; ...and restore
#else
	PULA				; retrieve saved vars
	PULB
	STAB systmp+1		; restore values
	STAA systmp
#endif
	RTI					; resume normal execution, hopefully

; *** execute standard NMI handler, if magic string failed ***
rst_nmi:
	BSR std_nmi		; call standard routine
	BRA nmi_end		; and finish, much simpler

; *** default code for NMI handler, if not installed or invalid, should end in RTS ***
std_nmi:
#include "modules/std_nmi.s"


; ********************************
; *** administrative functions ***
; ********************************

; *** generic functions ***

; GESTALT, get system info, API TBD
; cpu_ll	= CPU type
; c_speed	= speed code
; str_pt	= points to a string with machine name
; ex_pt		= points to a map of default memory conf ???
; k_ram		= available pages of (kernel) SRAM
; *** MAY change ABI/API ***REVISE

fw_gestalt:

	LDAB fw_cpu			; get kind of CPU (previoulsy stored or determined) (3)
	LDAA #SPEED_CODE	; speed code as determined in options.h (2)
	STAB cpu_ll			; set outputs (4+4)
	STAA c_speed
	LDAA himem			; get pages of SRAM??? (3)
	STAA k_ram			; store output (4)
; no "high" RAM on this architecture
	LDX #fw_mname		; get string pointer (3)
	STX str_pt			; put it outside (5)
	LDX #fw_map			; pointer to standard map TBD ???? (3)
	STX ex_pt			; set output (5)
	_DR_OK				; done (7)


; SET_ISR, set IRQ vector
;		INPUT
; kerntab	= address of ISR (will take care of all necessary registers)
;		0 means read current!

fw_s_isr:
; no CS as STX is atomic!
	LDX kerntab			; get pointer
	BEQ fw_r_isr			; in case of read...
		STX fw_isr			; ...or store for firmware
		_DR_OK				; done
fw_r_isr:
	LDX fw_isr			; get current
	STX kerntab			; and return it
	_DR_OK				; done


; SET_NMI, set NMI vector
;		INPUT
; kerntab	= address of NMI code (including magic string, ends in RTS)
;		0 means read current!

; might check whether the pointed code starts with the magic string
; as the magic string will NOT get "executed", can safely be the same as 6502
; no CS as STX is atomic!

fw_s_nmi:
	LDX kerntab			; get pointer to supplied code + magic string
		BEQ fw_r_nmi			; in case of read...
#ifdef	SAFE
	LDAA 0,X			; first char
	CMPA #'U'			; is it correct?
		BNE fsn_err			; not!
	LDAA 1,X			; second char
	CMPA #'N'			; correct?
		BNE fsn_err			; not!
	LDAA 2,X			; third char
	CMPA #'j'			; correct?
		BNE fsn_err			; not!
	LDAA 3,X			; last char
	CMPA #'*'			; correct?
	BEQ fsn_valid		; yeah, proceed!
fsn_err:
		_DR_ERR(CORRUPT)	; ...or invalid NMI handler
fsn_valid:
#endif
; transfer supplied pointer to firmware vector
	STX fw_nmi			; store for firmware
	_DR_OK				; done
fw_r_nmi:
	LDX fw_nmi			; get current
	STX kerntab			; and return it
	_DR_OK				; done


; SET_DBG, set SWI handler
;		INPUT
; kerntab	= address of SWI routine (ending in RTS)
;		0 means read current!
; no CS as STX is atomic!

fw_s_brk:
	LDX kerntab			; get pointer
	BEQ fw_r_brk			; in case of read...
		STX fw_brk			; store for firmware
		_DR_OK				; done
fw_r_brk:
	LDX fw_brk			; get current
	STX kerntab			; and return it
	_DR_OK				; done


; JIFFY, set jiffy IRQ frequency
;		INPUT
; irq_hz	= frequency in Hz (0 means no change)
;		OUTPUT
; irq_hz	= actually set frequency (in case of error or no change)
; C			= could not set (KERAton)

fw_jiffy:
; GENERIC!!!
; if could not change, then just set return parameter and C
	LDX irq_hz			; get input values
		BNE fj_set			; not just checking
	LDX irq_freq		; get current frequency
	STX irq_hz			; set return values
fj_end:
	_DR_OK
fj_set:
	STX irq_freq		; store in sysvars
; ***** must set counters accordingly! *****
	BRA fj_end			; all done, no need to update as will be OK


; IRQ_SOURCE, investigate source of interrupt
;		OUTPUT
; *** X	= 0 (periodic), 2 (async IRQ @ 65xx) *********************** 6502 ************
; *** notice NON-standard output register for faster indexed jump! ***
; other even values hardware dependent
fw_i_src:
; ****** TO BE DONE ******
	_DR_ERR(UNAVAIL)	; not yet implemented

; *** hardware specific ***

; POWEROFF, poweroff etc
; acc B <- mode (0 = suspend, 2 = warmboot, 4 = coldboot, 6 = poweroff)
; *** special codes for SWI/NMI triggering (10 = NMI, 12 = SWI) ***
; C -> not implemented

fw_power:
	TSTB				; is it zero? could be CMPB #PW_STAT
	BNE fwp_ns			; not suspend
; this would be suspend code...
fwp_exit:
		_DR_OK				; just continue execution
fwp_ns:
	CMPB #PW_WARM		; warm?
	BNE fwp_nw			; not
		JMP start_kernel		; warm boot!
fwp_nw:
	CMPB #PW_COLD		; cold?
	BNE fwp_nc			; not
		JMP reset			; or go to internal reset!
fwp_nc:
	CMPB #PW_HARD		; NMI?
	BNE fwp_nhi			; not
		JMP nmi				; NMI!
fwp_nhi:
	CMPB #PW_SOFT		; SWI? Not sure if really needed...
	BNE fwp_nsi			; not
		JMP brk_hndl			; SWI!
fwp_nsi:
	CMPB #PW_OFF		; off?
	BNE fwp_exit		; not recognised
; should add here shutdown code
		_PANIC("{OFF}")		; placeholder if not implemented


; FREQ_GEN, frequency generator hardware interface, TBD
fw_fgen:
; ****** TO BE DONE ******
	_DR_ERR(UNAVAIL)	; not yet implemented

; *** for higher-specced systems ***
; but a reduced, non patchable INSTALL is available instead
#ifndef	LOWRAM


; INSTALL, copy jump table
;		INPUT
; kerntab	= address of supplied JUMP table

; CS is 28+3 bytes, (6+)18+ 256*43 = (6+) 11026 cycles
fw_install:
	_CRITIC				; disable interrupts!
	PSHA				; EEEEEEEEEEEK
	CLRB				; reset counter (2)
	LDX #fw_table		; get destination pointer (3) eeeeeeeek
	STX systmp			; store temporarily (5)
fwi_loop:
		LDX kerntab			; set origin pointer (4)
		LDAA 0,X			; get byte from table as supplied (5)
		INX					; increment (4)
		STX kerntab			; ...and update (5)
		LDX systmp			; switch to destination (4)
		STAA 0,X			; copy the byte (6)
		INX					; increment (4)
		STX systmp			; ...and update (5)
		INCB				; advance counter (2)
		BNE fwi_loop		; until whole page is done (4)
;	DEC kerntab			; restore original page, if needed (6)
	LDX #fw_table		; the firmware table will be pointed... (3)
	STX kern_ptr		; ...from the standard address (5)
	PULA				; EEEEEEEEEEEK
	_NO_CRIT			; restore interrupts if needed
	_DR_OK				; all done


; PATCH, patch single function
; kerntab <- address of code
; acc B <- function to be patched

fw_patch:
	_CRITIC			; disable interrupts, respects A
#ifdef	MC6801
	PSHA				; eeeek
	LDX #fw_table		; take base address
	ABX					; destination entry computed!
	LDD kerntab			; get targeted function pointer
	STD 0,X				; updated for firmware
	PULA				; eeeek
#else
	ADDB #<fw_table		; compute final LSB
	STAB systmp+1		; set temporary pointer
	LDAB #>fw_table		; now for MSB
	ADCB #0				; propagate carry
	STAB systmp			; pointer is ready
	LDX systmp			; set as index
	LDAB kerntab		; get function MSB
	STAB 0,X			; store into firmware
	LDAB kerntab+1		; and LSB
	STAB 1,X
#endif
	_NO_CRIT			; restore interrupts
	_DR_OK				; done


; CONTEXT, zeropage & stack bankswitching
; *** TO BE DONE ****** TO BE DONE ***
fw_ctx:
	_DR_ERR(UNAVAIL)	; not yet implemented

#else
; these functions will not work on 128-byte systems!
fw_install:
; *** limited version ***
#ifdef	SAFE
; no CS as STX is atomic!
	LDX kerntab			; the supplied table will be pointed...
	STX kern_ptr		; ...from the standard address
	_DR_OK				; done
#endif

fw_patch:
fw_ctx:
	_DR_ERR(UNAVAIL)	; not available

#endif

; ****************************
; *** some firmware tables ***
; ****************************

; ****** some odds ******

; temporary empty memory map
fw_map:
	FCB		0

; if case of no headers, at least keep machine name somewhere
#ifdef	NOHEAD
fw_splash:
	FCC		"0.6a9 firmware for "
fw_mname:
	FCC		MACHINE_NAME
	FCB 	0
#endif

; *********************************
; *** administrative JUMP table ***
; *********************************
	ORG		admin_ptr	; must be set in S19 format!

; generic functions, esp. interrupt related
	JMP fw_gestalt		; GESTALT get system info (renumbered) @0
	JMP fw_s_isr		; SET_ISR set IRQ vector +3
	JMP fw_s_nmi		; SET_NMI set (magic preceded) NMI routine +6
	JMP fw_s_brk		; SET_DBG set debugger, new 20170517 +9
	JMP fw_jiffy		; JIFFY set jiffy IRQ speed, ** TBD ** +C
	JMP fw_i_src		; *** IRQ_SRC get interrupt source for total ISR independence +F

; pretty hardware specific
	JMP fw_power		; POWEROFF power-off, suspend or cold boot +12
	JMP fw_fgen			; *** FREQ_GEN frequency generator hardware interface, TBD +15

; a reduced INSTALL is available for LOWRAM system, calling it is mandatory!
	JMP fw_install		; INSTALL copy jump table +18
#ifndef	LOWRAM
; not for LOWRAM systems
	JMP fw_patch		; PATCH patch single function (renumbered) +1B
	JMP fw_ctx			; *** CONTEXT context bankswitching +1E
#endif

; ****** at the NEW standard address $FFC0, this will be at $FFDE ******

; *****************************
; *** standard ROM inteface ***
; *****************************

; *** panic routine, locks at very obvious address ($FFE1-$FFE2) ***
	ORG	lock			; must be set in S19 format!
	SEI					; locks at same address as 6502
panic_loop:
		BRA panic_loop		; always OK

; ****** use some space for interrupt handlers ******
; *** vectored IRQ handler *** $FFE3
; might go elsewhere
irq:
	LDX fw_isr			; vectored ISR
	JMP 0,X			; MUST end in RTI

; *** minimOS SWI handler *** $FFE8
; might go elsewhere
brk_hndl:		; label from vector list
	LDX fw_brk			; get vectored pointer
	JMP 0,X			; MUST end in RTI

; $FFED...

; **********************************
; ****** hardware ROM vectors ******
; **********************************

; *** Hitachi ROM vectors ***
	ORG		$FFEE		; must be set in S19 format!

	FDB		nmi			; TRAP	@ $FFEE

; *** Microcontroller ROM vectors ***
	ORG		$FFF0		; should be already at it

	FDB		nmi			; SCI	@ $FFF0
	FDB		nmi			; TOF	@ $FFF2
	FDB		nmi			; OCF	@ $FFF4
	FDB		nmi			; ICF	@ $FFF6

; *** 6800 ROM vectors ***
	ORG		$FFF8		; just in case

	FDB		irq			; IRQ @ $FFF8
	FDB		brk_hndl	; SWI @ $FFFA
	FDB		nmi			; NMI @ $FFFC
	FDB		reset		; RES @ $FFFE
