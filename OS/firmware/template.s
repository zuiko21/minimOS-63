; firmware for minimOS-63
; sort-of generic template
; v0.6a3
; (c)2017 Carlos J. Santisteban
; last modified 20170605-0935

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

; **************************
; **************************
; ****** cold restart ******
; **************************
; **************************
; basic init
reset:
	SEI				; cold boot
	LDS #SPTR		; initialise stack pointer, machine-dependent

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
	LDAA #'X'			; default 6800 installed
	STAA fw_cpu			; store variable
; ...but check it for real afterwards, at least rejecting 6809
#include	"firmware/modules/cpu_check.s"

; *** preset kernel start address (standard label from ROM file) ***
	LDX #kernel			; get full address
	STX fw_warm			; store in sysvars, really needed?

; *** preset SWI & NMI handlers ***
	LDX #swi_routine	; standard routine/label?
	STX fw_swi			; new sysvar
; as NMI will be validated, no need to preinstall it!

; ********************************
; *** hardware interrupt setup ***
; ********************************
;	LDX #IRQ_FREQ	; interrupts per second
;	STX irq_freq	; store speed...

	LDX #ticks			; first address in uptime seconds AND ticks, assume contiguous
res_sec:
		CLR 0, X		; reset byte
		INX					; next
		CPX #ticks+5		; first invalid address
		BNE res_sec			; continue until reached

; KERAton has no VIA, jiffy IRQ is likely to be fixed on, perhaps enabling counter input on PIA
;	LDAA #$C0			; enable T1 (jiffy) interrupt only
;	STAA VIA_J + IER

; **********************************
; *** direct print splash string ***
; **********************************
	LDX #fw_splash				; reset index
fws_loop:
		LDAA 0, X	; get char
			BEQ fws_cr			; no more to print
		STAA $0F			; visual 6800 output
		INX					; next char
		BRA fws_loop
fws_cr:
	LDAA #LF				; trailing CR, needed by console!
	STAA $0F				; visual 6800 output

; ************************
; *** start the kernel ***
; ************************
start_kernel:
	LDX fw_warm		; get pointer
	JMP 0, X			; jump there!

; *** vectored NMI handler with magic number ***
nmi:
; registers are saved! but check system pointers
; make NMI reentrant
	LDAA systmp			; get original word
	LDAB systmp+1
	PSHB					; store them in similar order
	PSHA
; prepare for next routine
	LDX fw_nmi			; get vector to supplied routine
; check whether user NMI pointer is valid
	LDAA 0, X			; get first char
	CMPA #'U'			; matches magic string? REVISE
		BNE rst_nmi			; error, use standard handler
	LDAA 1, X			; get second char
	CMPA #'N'			; matches magic string? REVISE
		BNE rst_nmi			; error, use standard handler
	LDAA 2, X			; get third char
	CMPA #'j'			; matches magic string? REVISE
		BNE rst_nmi			; error, use standard handler
	LDAA 3, X			; get fourth char
	CMPA #'*'			; matches magic string? REVISE
		BNE rst_nmi			; error, use standard handler
	JSR 4, X			; routine OK, skip magic string!
; *** here goes the former nmi_end routine ***
nmi_end:
	PULA					; retrieve saved vars
	PULB
	STAB systmp+1			; restore values
	STAA systmp+1
	RTI					; resume normal execution, hopefully

; *** execute standard NMI handler ***
rst_nmi:
	LDAA #<nmi_end		; prepare return address
	PSHA
	LDAA #>nmi_end		; now MSB
	PSHA
; ...will continue thru subsequent standard handler, its RTS will get back to ISR exit

; *** default code for NMI handler, if not installed or invalid, should end in RTS ***
std_nmi:
#include "firmware/modules/std_nmi.s"


; ********************************
; *** administrative functions ***
; ********************************

; *** generic functions ***

; GESTALT, get system info, API TBD
; zpar -> available pages of (kernel) SRAM
; zpar+2.W -> available BANKS of RAM
; zpar2.B -> speedcode
; zpar2+2.B -> CPU type
; zpar3.W/L -> points to a string with machine name
; *** WILL change ABI/API ***REVISE
fw_gestalt:
;6502
;	LDA himem		; get pages of kernel SRAM (4)
;	STA zpar		; store output (3)
;	_STZX zpar+2	; no bankswitched RAM yet (4)
;	_STZX zpar3+2	; same for string address (4)
;	LDA #>fw_mname	; get string pointer
;	LDY #<fw_mname
;	STA zpar3+1		; put it outside
;	STY zpar3
;	LDA #SPEED_CODE	; speed code as determined in options.h (2+3)
;	STA zpar2
;	LDA fw_cpu		; get kind of CPU (previoulsy stored or determined) (4+3)
;	STA zpar2+2
	_DR_OK			; done (8)

; SET_ISR, set IRQ vector
; kerntab <- address of ISR
fw_s_isr:
	_ENTER_CS				; disable interrupts and save sizes!
	LDX kerntab				; get pointer
	STX fw_isr				; store for firmware
	_EXIT_CS				; restore interrupts if needed
	_DR_OK					; done (8)

; SET_NMI, set NMI vector
; kerntab <- address of NMI code (including magic string)
; might check whether the pointed code starts with the magic string
; no need to disable interrupts as a partially set pointer would be rejected
fw_s_nmi:
	LDX kerntab			; get pointer to supplied code + magic string
#ifdef	SAFE
	LDAA 0, X			; first char
	CMPA #'U'			; is it correct? TBD
		BNE fsn_err			; not!
	LDAA 1, X			; second char
	CMPA #'N'			; correct?
		BNE fsn_err			; not!
	LDAA 2, X			; third char
	CMPA #'j'			; correct?
		BNE fsn_err			; not!
	LDAA 3, X			; last char
	CMPA #'*'			; correct?
	BEQ fsn_valid		; yeah, proceed!
fsn_err:
		_DR_ERR(CORRUPT)		; ...or invalid NMI handler
fsn_valid:
#endif
; transfer supplied pointer to firmware vector
	STX fw_nmi				; store for firmware
	_DR_OK					; done (8)

; SET_BRK, set BRK handler
; kerntab <- address of BRK code
fw_s_brk:
	_ENTER_CS				; disable interrupts!
	LDX kerntab				; get pointer
	STX fw_brk				; store for firmware
	_EXIT_CS				; restore interrupts if needed
	_DR_OK					; done

; JIFFY, set jiffy IRQ frequency
fw_jiffy:
; ****** TO BE DONE ******

; IRQ_SOURCE, get source of interrupt
fw_i_src:
; ****** TO BE DONE ******
	_DR_ERR(UNAVAIL)	; not yet implemented

; *** hardware specific ***

; POWEROFF, poweroff etc
; B <- mode (0 = suspend, 2 = warmboot, 4 = coldboot, 6 = poweroff)
; C -> not implemented
fw_power:
	TSTB				; is it zero?
	BNE fwp_ns			; not suspend
; this would be suspend code...
fwp_exit:
		_DR_OK				; just continue execution
fwp_ns:
	CMPB #2				; warm?
	BNE fwp_nw			; not
		LDX fw_warm			; get restart vector
		JMP 0, X			; warm boot!
fwp_nw:
	CMPB #4				; cold?
	BNE fwp_nc			; not
		LDX $FFFE			; 6800 RES vector
		JMP 0, X			; go!
fwp_nc:
	CMPB #6				; off?
	BNE fwp_exit			; not recognised
		_PANIC("{OFF}")		; just in case is handled

; FREQ_GEN, frequency generator hardware interface, TBD
fw_fgen:
; ****** TO BE DONE ******
	_DR_ERR(UNAVAIL)	; not yet implemented

; *** for higher-specced systems ***
; but a reduced, non patchable INSTALL is available instead
#ifndef	LOWRAM

; INSTALL, copy jump table
; kerntab <- address of supplied jump table
fw_install:
	_ENTER_CS			; disable interrupts!
	LDAB #0				; reset counter
	LDX fw_table			; get destination pointer
	STX systmp			; store temporarily
fwi_loop:
		LDX kerntab		; set origin pointer
		LDAA 0, X		; get byte from table as supplied
		INX			; increment
		STX kerntab		; ...and update
		LDX systmp		; switch to destination
		STAA 0, X		; copy the byte
		INX			; increment
		STX systmp		; ...and update
		INCB			; advance counter
		BNE fwi_loop		; until whole page is done
;	DEC kerntab		; restore original value
	LDX #fw_table		; the firmware table will be pointed...
	STX $FC			; ...from the standard address
	_EXIT_CS			; restore interrupts if needed
	_DR_OK				; all done

; PATCH, patch single function
; kerntab <- address of code
; acc B <- function to be patched
fw_patch:
	_ENTER_CS				; disable interrupts
	ADDB #<fw_table				; compute final LSB
	STAB systmp+1				; set temporary pointer
	LDAB #>fw_table				; now for MSB
	ADCB #0					; propagate carry
	STAB systmp				; pointer is ready
	LDX systmp				; set as index
	LDAA kerntab				; get function MSB
	LDAB kerntab+1				; and LSB
	STAA 0, X				; store into firmware
	STAB 1, X
	_EXIT_CS				; restore interrupts
	_DR_OK					; done
; CONTEXT to do to do **********
fw_ctx:
	_DR_ERR(UNAVAIL)	; not yet implemented
#else
; these functions will not work on 128-byte systems!
fw_install:
; limited version
	_ENTER_CS				; disable interrupts
	LDX kerntab		; the supplied table will be pointed...
	STX $FC			; ...from the standard address
	_EXIT_CS				; restore interrupts
	_DR_OK					; done

fw_patch:
fw_ctx:
	_DR_ERR(UNAVAIL)	; not available

#endif

; ****************************
; *** some firmware tables ***
; ****************************

; power sub-function pointer table (eeeek)
fwp_func:
	.word	fwp_susp	; suspend	+FW_STAT
	.word	kernel		; shouldn't use this, just in case
	.word	fwp_cold	; coldboot	+FW_COLD
	.word	fwp_off		; poweroff	+FW_OFF

; ****** some odds ******

; if case of no headers, at least keep machine name somewhere
#ifdef	NOHEAD
fw_splash:
	.asc	"0.6 firmware for "
fw_mname:
	.asc	MACHINE_NAME, 0
#endif

; filling for ready-to-blow ROM
#ifdef		ROM
	.dsb	admin_ptr-*, $FF
#endif

; *********************************
; *** administrative jump table ***
; *********************************

* = admin_ptr

; generic functions, esp. interrupt related
	JMP fw_gestalt	; GESTALT get system info (renumbered) @0
	JMP fw_s_isr	; SET_ISR set IRQ vector +3
	JMP fw_s_nmi	; SET_NMI set (magic preceded) NMI routine +6
	JMP fw_s_brk	; *** SET_BRK set debugger, new 20170517 +9
	JMP fw_jiffy	; *** JIFFY set jiffy IRQ speed, ** TBD ** +C
	JMP fw_i_src	; *** IRQ_SOURCE get interrupt source for total ISR independence +F

; pretty hardware specific
	JMP fw_power	; POWEROFF power-off, suspend or cold boot +12
	JMP fw_fgen		; *** FREQ_GEN frequency generator hardware interface, TBD +15

; a reduced INSTALL is available for LOWRAM system, calling it is mandatory!
	JMP fw_install	; INSTALL copy jump table +18
#ifndef	LOWRAM
; not for LOWRAM systems
	JMP fw_patch	; PATCH patch single function (renumbered) +1B
	JMP fw_ctx		; *** CONTEXT context bankswitching +1E
#endif

; ****** at the NEW standard address $FFC0, this will be at $FFDE ******

; filling for ready-to-blow ROM
#ifdef	ROM
	.dsb	lock-*, $FF
#endif

; *****************************
; *** standard ROM inteface ***
; *****************************

; *** panic routine, locks at very obvious address ($FFE1-$FFE2) ***
* = lock
	SEI					; locks at same address as 6502
panic_loop:
	BRA panic_loop	; always OK

; ****** use some space for interrupt handlers ******
; *** vectored IRQ handler *** $FFE3
; might go elsewhere
irq:
	LDX fw_isr			; vectored ISR
	JMP 0, X			; MUST end in RTI

; *** minimOS BRK handler *** %FFE8
; might go elsewhere
brk_hndl:		; label from vector list
	LDX fw_brk			; get vectored pointer
	JMP 0, X			; MUST end in RTI

; $FFED...

; filling for ready-to-blow ROM
#ifdef	ROM
	.dsb	$FFEE-*, $FF
#endif

; **********************************
; ****** hardware ROM vectors ******
; **********************************

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
