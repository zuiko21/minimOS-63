; firmware for minimOS·63
; sort-of generic template, but intended for KERAton
; v0.6a15
; (c)2017-2018 Carlos J. Santisteban
; last modified 20180219-0924

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
	FCC		"0.6a14 firmware for "	; version in comment
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
	FCC		"0.6a14 firmware for "
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
;#include "firmware/modules/rst_lastk.s"

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
	BSR std_nmi			; call standard routine
	BRA nmi_end			; and finish, much simpler

; *** default code for NMI handler, if not installed or invalid, should end in RTS ***
std_nmi:
#include "modules/std_nmi.s"


; ********************************
; *** administrative functions ***
; ********************************

; *** generic functions ***

; GESTALT, get system info, API TBD
; cpu_ll	= CPU type
; c_speed	= speed code (16b)
; str_pt	= points to a string with machine name
; ex_pt		= points to a map of default memory conf ???
; k_ram		= available pages of (kernel) SRAM
; *** MAY change ABI/API ***REVISE

gestalt:

	LDAB fw_cpu			; get kind of CPU (previoulsy stored or determined) (3)
	LDX #SPEED_CODE		; speed code as determined in options.h ()
	STAB cpu_ll			; set outputs (4+)
	STX c_speed
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

set_isr:
; no CS as STX is atomic!
	LDX kerntab			; get pointer
	BEQ fw_r_isr		; in case of read...
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

set_nmi:
	LDX kerntab			; get pointer to supplied code + magic string
		BEQ fw_r_nmi		; in case of read...
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

set_dbg:
	LDX kerntab			; get pointer
	BEQ fw_r_brk		; in case of read...
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
; irq_hz	= actually set frequency (in case of no change)
; C			= could not set

jiffy:
	LDX irq_hz			; get input values
	BNE fj_set			; not just checking
		LDX irq_freq		; get current frequency
		STX irq_hz			; set return values
fj_end:
; reload in case of successful change
; supress in case of fixed IRQ (KERAton)
		LDX irq_hz			; get requested
		STX irq_freq		; set, will not harm
		_DR_OK
fj_set:
; generic code, fixed IRQ systems just notice error!
; multiply irq_hz (16b) by SPD_CODE (16b), then shift 12b right
; MCUs do much better...
#ifdef	MC6801
; use several MUL to get the 32b result, IJxKL
; first JxL, no shift
	LDAA irq_hz+1		; J
	LDAB #<SPD_CODE		; L
	MUL					; JxL
; as the result LSB is to be discarded, just keep the high part!
	STAA local1			; JL.high
; now IxL, shift one byte
	LDAA irq_hz			; I
	LDAB #<SPD_CODE		; L again
	MUL					; IxL
	STD local1+2		; IL, will shift 1
; now JxK, shift one too
	LDAA irq_hz+1		; J
	LDAB #>SPD_CODE		; K now
	MUL					; JxK
	STD local2			; JK, will shift 1 too
; finally IxK will be shifted two bytes!
	LDAA irq_hz			; I
	LDAB #>SPD_CODE		; K again
	MUL					; IxK
	STD local2+2		; IK, will shift 2 bytes
; now add things, up to three bytes!
;  ••JL
;  •IL•
;  •JK•
; +IK••
;  123x
	LDD local1+2		; full 'IL' on second line
	ADDB local1			; first add 'J' on first product
	ADCB local2+1		; add with carry 'K' on 3rd line
; accumulator B is ready
	ADCA local2 		; for mid byte add 'J' on 3rd line
	ADCA local2+3 		; and 'K' on 4th line
	STD local3+1		; lowest 16-bit ready!
	CLRB				; for first byte...
	ADCB local2+2		; ...add 'I' from 4th line
	STAB local3			; full result is ready!!!
#else
; do russian-style multiply!
	LDX irq_hz			; get 1st factor
	STX local1+2		; store local copy...
	LDX #0
	STX local1			; ...with room to be shifted left
	STX local3			; clear result too
	STX local3+2
	LDX #SPD_CODE		; second factor
	STX local2			; will be shifted right
fj_mul:
		LSR local2			; extract lsb
		ROR local2+1
		BCC fj_next			; was clear, do not add
; add current 1st factor to result, with loop takes 16b/111t
			LDAA #4				; otherwise set loop (2)
			LDX #local1+3		; set pointer to LSB (3)
			CLC					; loop does not tell ADD from ADC (2)
fj_add:
				LDAB 0,X			; get 1st (5)
				ADCB 8,X			; add result (5)
				STAB 8,X			; update (6)
				DEX					; point to previous byte (4)
				DECA				; one less (2+4)
				BNE fj_add
; loopless alternative, takes 24b/40t
;			LDAA local1+3		; get 1st LSB (3)
;			ADDA local3+3		; add result, first with no carry! (3)
;			STAA local3+3		; update (4)
;			LDAA local1+2		; same for remaining bytes
;			ADCA local3+2		; but add with carry (3)
;			STAA local3+2
;			LDAA local1+1
;			ADCA local3+1
;			STAA local3+1
;			LDAA local1
;			ADCA local3
;			STAA local3			; done
fj_next:
		ASL local1+3		; shift 1st factor left
		ROL local1+2
		ROL local1+1
		ROL local1
; check remaining bits on 2nd factor
		LDX local2			; full 16-bit check
		BNE fj_mul			; continue until done
#endif
; time to shift 4 bits to the right
#ifdef	MC6801
; MCU is 14b/71t
	LDX #4				; preset counter (3)
	LDD local3			; put highest in D (4)
fj_shft:
		LSRD				; shift 2 bytes... (3)
		ROR local3+2		; ...plus last (6)
		DEX
		BNE fj_shft			; until done (3+3)
	STD local3			; update full value (4)
#else
; classic 6800 is 14b/98t
;	LDAB #4				; preset counter (2)
;fj_shft:
;		LSR local3			; shift first... (6)
;		ROR local3+1		; ...into 2nd... (6)
;		ROR local3+2		; ...plus last (6)
;		DECB
;		BNE fj_shft			; until done (2+4)
; alternative 6800 is 16b/88t
	LDAB #4				; preset counter (2)
	LDAA local3			; put MSB in A (3)
fj_shft:
		LSRA				; shift 1st byte... (2)
		ROR local3+1		; ...plus 2nd... (6)
		ROR local3+2		; ...and last (6)
		DECB
		BNE fj_shft			; until done (2+4)
	STAA local3			; must update, unfortunately (3)
; another 6800 is 19b/87t
;	LDX #4				; preset counter (3)
;	LDAA local3			; put MSB in A (3)
;	LDAB local3+1		; middle byte in B (3)
;fj_shft:
;		LSRA				; shift 1st byte... (2)
;		LSRB				; ...plus 2nd... (2)
;		ROR local3+2		; ...and last (6)
;		DEX
;		BNE fj_shft			; until done (4+4)
;	STAA local3			; must update, unfortunately (3+3)
;	STAB local3+1
#endif
; if carry is set, must add 1 before subtracting 2
; prepare accurate value for VIA
#ifdef	MC6801
	LDD local3+1		; get full value
#else
	LDAA local3+1		; get full value
	LDAB local3+2
#endif
	BCS fj_rnd			; C was set, subtract 1 only
		SUBB #2				; otherwise round low
		BRA fj_msb			; and go for next byte
fj_rnd:
	SUBB #1				; round up
fj_msb:
	SBCA #0				; propagate borrow
	BCC fj_nbor			; no more to check
		DEC local3			; rare but possible
fj_nbor:
#ifdef	MC6801
	STD local3+1		; update full value
#else
	STAA local3+1		; update full value
	STAB local3+2
#endif
; finally check whether fits range
	LDAA local3			; check MSB, must be zero
	BNE fj_over			; no, out of range
; otherwise get computed value...
#ifdef	MC6801
		LDD local3+1		; get result, big-endian
#else
		LDAA local3+1		; otherwise get result
		LDAB local3+2		; big-endian!
#endif
; ...and set it into counters, converting endianness!
		STAB VIA_J+T1CL		; VIA is little-endian!
		STAA VIA_J+T1CH		; start counting
		BRA fj_end			; success
fj_over:
	_DR_ERR(INVALID)


; IRQ_SOURCE, investigate source of interrupt
;		OUTPUT
; *** X	= 0 (periodic), 2 (async IRQ @ 65xx) *********************** 6502 ************
; *** notice NON-standard output register for faster indexed jump! ***
; other even values hardware dependent
irq_src:
; ****** TO BE DONE ******
	_DR_ERR(UNAVAIL)	; not yet implemented

; *** hardware specific ***

; POWEROFF, poweroff etc
; acc B <- mode (0 = suspend, 2 = warmboot, 4 = coldboot, 6 = poweroff)
; *** special codes for SWI/NMI triggering (10 = NMI, 12 = SWI) ***
; C -> not implemented

poweroff:
	TSTB				; is it zero? could be CMPB #PW_STAT
	BNE fwp_ns			; not suspend
; this would be suspend code...
fwp_exit:
		_DR_OK				; just continue execution
fwp_ns:
	CMPB #PW_WARM		; warm?
	BNE fwp_nw			; not
		JMP start_kernel	; warm boot!
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
		JMP brk_hndl		; SWI!
fwp_nsi:
	CMPB #PW_OFF		; off?
	BNE fwp_exit		; not recognised
; should add here shutdown code
		_PANIC("{OFF}")		; placeholder if not implemented


; FREQ_GEN, frequency generator hardware interface, TBD
freq_gen:
; ****** TO BE DONE ******
	_DR_ERR(UNAVAIL)	; not yet implemented

; *** for higher-specced systems ***
; but a reduced, non patchable INSTALL is available instead
#ifndef	LOWRAM


; INSTALL, copy jump table
;		INPUT
; kerntab	= address of supplied JUMP table

; CS is 28+3 bytes, (6+)18+ 256*43 = (6+) 11026 cycles
install:
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

patch:
	_CRITIC				; disable interrupts, respects A
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

#else
; these functions will not work on 128-byte systems!
install:
; *** limited version ***
#ifdef	SAFE
; no CS as STX is atomic!
	LDX kerntab			; the supplied table will be pointed...
	STX kern_ptr		; ...from the standard address
	_DR_OK				; done
#endif

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
	ORG	admin_ptr		; must be set in S19 format!

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
	SEI					; locks at same address as 6502
panic_loop:
		BRA panic_loop		; always OK

; ****** use some space for interrupt handlers ******
; *** vectored IRQ handler *** $FFE3
; might go elsewhere
irq:
	LDX fw_isr			; vectored ISR
	JMP 0,X				; MUST end in RTI

; *** minimOS SWI handler *** $FFE8
; might go elsewhere
brk_hndl:				; label from vector list
	LDX fw_brk			; get vectored pointer
	JMP 0,X				; MUST end in RTI

; $FFED...

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
	FDB	brk_hndl		; SWI @ $FFFA
	FDB	nmi				; NMI @ $FFFC
	FDB	reset			; RES @ $FFFE
