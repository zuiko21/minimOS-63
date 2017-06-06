; minimOS-63 generic Kernel
; v0.6a3
; (c) 2017 Carlos J. Santisteban
; last modified 20170606-1240

; avoid standalone definitions
#define		KERNEL	_KERNEL

; uncomment in case of separate, downloadable jump & boot files
; should assume standalone assembly!!! (will get drivers anyway)
; ROMable kernels cannot be downloaded, though
#ifndef	ROM
;#define		DOWNLOAD	_DOWNLOAD
#endif

; in case of standalone assembly
#ifndef	HEADERS
#include "usual.h"
#ifdef		DOWNLOAD
* = $0400				; safe address for patchable 2 kiB systems, change if required
#else
; standalone kernels need to keep track of drivers_ad label!
.data
#include "drivers/config/DRIVER_PACK.s"
.text
#endif
#endif

; *** standard header, at least for testing ***
#ifndef	NOHEAD
	.dsb	$100*((* & $FF) <> 0) - (* & $FF), $FF	; page alignment!!! eeeeek
kern_head:
	BRK
	.asc	"m", CPU_TYPE	; executable for testing TBD
	.asc	"****", 13		; flags TBD
	.asc	"kernel", 0		; filename
kern_splash:
	.asc	"minimOS-63 0.6a3", 0	; version in comment

	.dsb	kern_head + $F8 - *, $FF	; padding

	.word	$5280	; time, 10.20
	.word	$4AC6	; date, 2017/6/6

kern_siz = kern_end - kern_head - $FF

	.word	kern_siz, 0	; kernel size excluding header
#endif

; **************************************************
; *** kernel begins here, much like a warm reset ***
; **************************************************

warm:
	SEI				; interrupts off, just in case (2)

; install kernel jump table if not previously loaded
; LOWRAM systems will use a reduced, non-patchable INSTALL function
#ifndef		DOWNLOAD
	LDX #k_vec			; get table address (3)
	STX kerntab			; store parameter (5)
	_ADMIN(INSTALL)		; copy jump table
#endif

; install ISR code (as defined in "isr/irq.s" below)
	LDX #k_isr			; get address
	STX ex_pt			; no need to know about actual vector location
	_ADMIN(SET_ISR)		; install routine

; Kernel no longer supplies default NMI, but could install it otherwise

; *** default action in case the scheduler runs out of tasks ***
	LDAA #PW_STAT		; default action upon complete task death
	STAA sd_flag			; this is important to be clear (PW_STAT) or set as proper error handler

; *****************************
; *** memory initialisation ***
; *****************************

; this should take a basic memory map from firmware, perhaps via the GESTALT function
#ifndef		LOWRAM
; ++++++
	LDX #256*FREE_RAM+END_RAM	; get status of both ends
	STX ram_stat		; as it is the first entry, no index needed
	LDAA #>user_sram	; beginning of available ram, as defined... in rom.s
	LDAB #<user_sram	; LSB misaligned?
	BEQ ram_init		; nothing to align
		INCA				; otherwise start at next page
ram_init:
	STAA ram_pos			; store it, this is PAGE number
	LDAA #SRAM			; number of SRAM pages as defined in options.h *** revise
	STAA ram_pos+1		; store second entry and we are done!
; ++++++
#endif

; ************************************************
; *** intialise drivers from their jump tables ***
; ************************************************
; sometime will create API entries for these, but new format is urgent!
; * will also initialise I/O lock arrays! * 20161129

; *** 1) initialise stuff ***
; clear some bytes
	CLRB				; reset driver index (2)
	CLR queues_mx		; reset all indexes (6+6)
	CLR queues_mx+1

#ifdef LOWRAM
; ------ low-RAM systems have no direct tables to reset ------
; ** maybe look for fast tables in ROM **
	CLR drv_num			; single index of, not necessarily SUCCESSFULLY, detected drivers, updated 20150318 (4)
; ------
#else
; ++++++ new direct I/O tables for much faster access 20160406 ++++++
	CLR run_pid			; new 170222, set default running PID *** this must be done BEFORE initing drivers as multitasking should place appropriate temporary value via SET_CURR!
	LDAA #>dr_error		; make unused entries point to a standard error routine
	LDAB #<dr_error		; the LSB too
; *** reset all tables ***
	LDX #drv_opt		; output routines handler
dr_oclear:
		BSR dr_clrio		; shared code!
		CPX #drv_opt+256	; all done?
		BNE dr_oclear
	LDX #drv_ipt		; input routines handler
dr_iclear:
		BSR dr_clrio		; shared code!
		CPX #drv_ipt+256	; all done?
		BNE dr_iclear
	LDX #cio_lock		; will clear I/O locks and binary flags!
	CLRA				; zero is to be stored, both arrays interleaved!
	CLRB
dr_lclear:
		BSR dr_clrio		; shared code!
		CPX #cio_lock+256	; all done?
		BNE dr_lclear
; ++++++
#endif

; *** 2) prepare access to each driver header ***
; first get the pointer to each driver table
drv_aix = systmp		; temporary pointer, *** might go into locals ***

	LDX #drivers_ad		; driver table in ROM
dr_loop:
		STX drv_aix			; reset index (may change in future as X)
		LDX 0, X			; get current pointer
		BNE dr_inst			; cannot be zero, in case is too far for BEQ dr_ok
			JMP dr_ok			; all done otherwise

; *********************************************************
; *** subroutine for putting A as MSB and B as LSB, X+2 ***
dr_clrio:
	STAA 0, X			; set MSB for output
	STAB 1, X			; and LSB
	INX					; next word
	INX
	RTS
; *** perhaps in another place, try to use BSR ***
; ************************************************

dr_inst:
		STX da_ptr			; store pointer to header, *** but check new variable conflicts! ***
; create entry on IDs table
		LDAA D_ID, X		; get ID code (5)
		STAA dr_id			; keep in local variable as will be often used
#ifdef	SAFE
		BMI dr_phys			; only physical devices (3/2)
			JMP dr_abort		; reject logical devices (3)
dr_phys:
#endif
		LDAA D_AUTH, X		; get provided features
		STAA dr_feat		; another commonly used value (will stay during check)

; *** 3) before registering, check whether the driver COULD be successfully installed ***
; that means 1) there must be room enough on the interrupt queues for its tasks, if provided
; and 2) the D_INIT routine succeeded as usual
; otherwise, skip the installing procedure altogether for that driver
		ASLA				; extract MSb (will be A_POLL first) *** no longer using dr_aut
		BCC dr_nptsk		; skip verification if task not enabled
			LDAB queues_mx+1	; get current tasks in P queue
			CMPB #MAX_QUEUE		; room for another?
			BCC dr_ntsk			; yeah! will this work on 6800 too??
dr_nabort:
				JMP dr_abort		; or did not checked OK
dr_nptsk:
		ASL					; extract MSb (now is A_REQ) *** no longer using dr_aut
		BCC dr_nrtsk		; skip verification if task not enabled
			LDAB queues_mx	; get current tasks in R queue
			CMPB #MAX_QUEUE		; room for another?
				BCS dr_nabort			; did not check OK
dr_nrtsk:
; if arrived here, there is room for interrupt tasks, but check init code
;		STAA dr_aut			; *** will save it here for later...
		JSR D_INIT, X		; like dr_icall, call D_INIT routine!
			BCS dr_nabort		; no way, forget about this
; *** 4) driver should be OK to install, just check whether this ID was not in use ***

#ifndef	LOWRAM
; ++++++ new faster driver list ++++++
		BSR dr_outptr		; get pointer to output routine
		CPX #dr_error		; check whether something was installed
			BNE dr_busy			; pointer was not empty
		BSR dr_inptr		; get pointer to input routine
		CPX #dr_error		; check whether something was installed
		BEQ dr_empty		; it is OK to set
dr_busy:
			JMP dr_abort		; already in use

; ********************************************
; *** subroutine for copy dr_iopt into (X) ***
; uses acc B and restores X as da_ptr
dr_setpt:
	LDAB dr_iopt		; get MSB
	STAB 0, X			; write it!
	LDAB dr_iopt+1		; LSB too
	STAB 1, X
	LDX da_ptr			; restore header pointer
	RTS

; *** locate entry for input according to ID ***
dr_inptr:
	LDAB dr_id			; take original ID... or preset parameter!
	ASLB				; times two, as is index to pointers
	ADDB #<drv_ipt		; add to LSB
	STAB systmp+1		; *** is this already used??? ***
	LDAB #>drv_ipt		; now for MSB
	BRA dr_iopx			; common ending!

; *** locate entry for output according to ID ***
dr_outptr:
	LDAB dr_id			; take original ID... or preset parameter!
	ASLB				; times two, as is index to pointers
	ADDB #<drv_opt		; add to LSB
	STAB systmp+1		; *** is this already used??? ***
	LDAB #>drv_opt		; now for MSB
dr_iopx:
	ADCB #0				; propagate carry
	STAB systmp
	LDX systmp			; get final pointer
	RTS
; *** end of subroutines ***
; **************************

; *** continue installation ***
dr_empty:

; *** 4b) Set I/O pointers (if memory allows) ***
; might check here whether I/O are provided!
;		LDAA dr_aut			; *** continue with bit shifting!
;		ASLA				; look for CIN
;		BCC dr_seto			; no input for this!
			LDX D_CIN, X		; get input routine address, this X=6502 sysptr
			STX dr_iopt			; *** new temporary, will hold address to write into entry
			BSR dr_inptr		; locate entry for input according to ID! X points to entry
			BSR dr_setpt		; using B, copy dr_iopt into (X)
dr_seto:
;		ASL dr_aut			; look for COUT (5)
;		BCC dr_nout			; no output for this!
			LDX D_COUT, X		; get output routine address, this X=6502 sysptr
			STX dr_iopt			; *** new temporary, will hold address to write into entry
			BSR dr_outptr		; locate entry for output according to ID! X points to entry
			BSR dr_setpt		; using B, copy dr_iopt into (X)
dr_nout:
; ++++++
#else
; ------ IDs table filling for low-RAM systems ------
; check whether the ID is already in use
		LDX #drivers_id		; beginning of ID list
		LDAA dr_id			; fetch aspiring ID
		LDAB drv_num		; number of drivers registered this far
		BEQ dr_eol			; already at end of list
dr_scan:
#ifdef	SAFE
			CMPA 0, X			; compare with list entry
				BEQ dr_abort		; already in use, don't register
#endif
			INX					; otherwise try next one
			DECB				; one less to go
			BNE dr_scan
dr_eol:
; supposedly no conflicting IDs found, or at least pointer set after last one
		STAA 0, X			; store in list, now in RAM
; ------
#endif

; *** 5) register interrupt routines *** new, much cleaner approach ***** REVISE
		LDAA dr_feat		; get original auth code
		STAA dr_aut			; and keep for later!
; time to get a pointer to the-block-of-pointers (source)
		LDX da_ptr			; work with current header
		LDX D_POLL, X		; get this pointer
		STX sysptr			; store it ***** check!
; also a temporary pointer to the particular queue
		LDX #drv_poll		; must be the first one?
		STX dq_ptr			; store temporarily
; new functionality 170519, pointer to (interleaved) task enabling queues
		LDX #drv_p_en		; this is the second one, will be decremented for async?
		STX dte_ptr			; yet another temporary pointer...
; all set now, now easier to use a loop, not sure on 6800

		LDAB #1				; index for loop (2)

; ***** 6502 ***************** 6502 ***************** 6502 *****
dr_iqloop:
;			ASL dr_aut			; extract MSB (will be A_POLL first, then A_REQ)
;			BCC dr_noten		; skip installation if task not enabled
; prepare another entry into queue
;				LDY queues_mx, X	; get index of free entry!
;				STY dq_off			; worth saving on a local variable
;				INC queues_mx, X	; add another task in queue
;				INC queues_mx, X	; pointer takes two bytes
; install entry into queue
;				JSR dr_itask		; install into queue
; save for frequency queue, flags must be enabled for this task!
;				LDY dq_off			; get index of free entry!
;				LDA dr_id			; use ID as flags, simplifies search and bit 7 hi (as per physical device) means enabled by default
;				STA (dte_ptr), Y	; set default flags
; let us see if we are doing periodic task, in case frequency must be set also
;				TXA					; doing periodic?
;					BEQ dr_next			; if zero, is doing async queue, thus skip frequencies (in fact, already ended)
;				JSR dr_nextq		; advance to next queue (frequencies)
;				JSR dr_itask		; same for frequency queue
;				_BRA dr_doreq		; nothing to skip, go for async queue
dr_noten:
;			JSR dr_nextq		; if periodic was not enabled, this will skip frequencies queue
dr_doreq:
; as this will get into async, switch enabling queue
;			LDA dte_ptr			; check previous LSB
;			BNE dr_neqnw		; will wrap upon decrement?
;				DEC dte_ptr+1		; if so, precorrect MSB
dr_neqnw:
;			DEC dte_ptr			; one before as it is interleaved
; continue into async queue
;			JSR dr_nextq		; go for next queue
;			DEX					; now 0, index for async queue (2)
;			BPL dr_iqloop		; eeeeek

; *** 6) continue initing drivers ***
		BRA dr_next		; if arrived here, did not fail initialisation

dr_abort:
; *** if arrived here, driver initialisation failed in some way! ***
#ifdef	LOWRAM
; ------ low-RAM systems keep count of installed drivers ------
			LDX #drivers_id		; beginning of ID list
			LDAB drv_num		; number of drivers registered this far
dr_ablst:
				INX					; advance pointer
				DECB				; one less
				BNE dr_ablst		; acc B will never start at 0!
			LDAB #DEV_NULL		; invalid proper value...
			STAB 0, X			; ...as unreachable entry
; ------
#endif
dr_next:
#ifdef	LOWRAM
; ------ low-RAM systems keep count of installed drivers ------
		INC drv_num			; update SINGLE index
; ------
#endif
; in order to keep drivers_ad in ROM, can't just forget unsuccessfully registered drivers...
; in case drivers_ad is *created* in RAM, dr_abort could just be here, is this OK with new separate pointer tables?
		LDX drv_aix			; retrieve saved index
		INX					; update ADDRESS index, even if unsuccessful
		INX					; eeeeeeeek! pointer arithmetic!
		JMP dr_loop			; go for next

; ***************************
; *** points of no return ***
; ***************************
dr_error:
	_DR_ERR(N_FOUND)	; standard exit for non-existing drivers!

; *****************************************
; *** some driver installation routines ***
; *****************************************

; ***** 6502 ********************* 6502 ************************* 6502 *****
; * routine for advancing to next queue *
; both pointers in dq_ptr (whole queue) and sysptr (pointer in header)
dr_nextq:
;	LDA dq_ptr			; get original queue pointer
;	CLC
;	ADC #MAX_QUEUE		; go to next queue
;	STA dq_ptr
;	BCC dnq_nw			; no carry...
;		INC dq_ptr+1		; ...or update MSB
dnq_nw:
;	LDA sysptr			; increment the origin pointer!
;	CLC
;	ADC #2				; next pointer in header
;	STA sysptr			; eeeeeeeeeeek
;	BCC dnq_snw			; no carry...
;		INC sysptr+1		; ...or update MSB
dnq_snw:
	RTS

; * routine for copying a pointer from header into a table *
; sysptr & dq_ptr (updated) set as usual
dr_itask:
; *** preliminary 6800 version, room to optimise ***
; read pointer from header
	LDX sysptr			; get set pointer
	LDAA 0, X			; indirect read MSB
	LDAB 1, X			; and LSB
; write pointer into queue
	LDX dq_ptr			; switch pointer (will be updated, not using dq_off)
	STAA 0, X			; store fetched task pointer
	STAB 1, X			; LSB too
	INX					; go for next entry
	INX
	STX dq_ptr			; this has to be updated
	RTS

; ***************************************************************
; *** drivers already installed, clean up things and continue ***
; ***************************************************************
dr_ok:					; *** all drivers inited ***
; really no need to terminate ID list on LOWRAM systems

; **********************************
; ********* startup code ***********
; **********************************

; in case no I/O lock arrays were initialised... only for LOWRAM
#ifdef	LOWRAM
	CLR cin_mode		; single flag for non-multitasking systems
#endif

; startup code, revise ASAP
; *** set default I/O device ***
	LDX #DEVICE*257			; as defined in options.h
	STX default_in		; set word

; *** interrupt setup no longer here, firmware did it! ***

; new, show a splash message ever the kernel is restarted!
	JSR ks_cr			; leading newline
	LDX #kern_splash	; get pointer
	STX str_pt			; set parameter
	LDAB #DEVICE			; eeeeeek
	_KERNEL(STRING)		; print it!
	JSR ks_cr			; trailing newline

; ******************************
; **** launch monitor/shell ****
; ******************************
sh_exec:
	LDX #shell			; get pointer to built-in shell
	STX ex_pt			; set execution address
	LDX #DEVICE*257			; *** revise
	STX def_io			; default local I/O
	_KERNEL(B_FORK)		; reserve first execution braid, no direct call as could be PATCHED!
	_KERNEL(B_EXEC)		; go for it! no direct call as could be PATCHED!
; singletask systems will not arrive here, ever!
	_KERNEL(B_YIELD)	; ** get into the working code ASAP! ** no direct call as could be PATCHED!
here:
	BRA here			; ...as the scheduler will detour execution

; a quick way to print a newline on standard device
ks_cr:
	LDAA #CR			; leading newline
	STAA io_c
	LDAB #DEVICE
	_KERNEL(COUT)		; print it
	RTS

; ***********************************************
; *** generic kernel routines, separate files ***
; ***********************************************
#ifndef		LOWRAM
	.asc	"<API>"		; debug only
#include "api.s"
#else
#include "api_lowram.s"
#endif

; *********************************
; *** interrupt service routine ***
; *********************************
; will include BRK handler!

k_isr:
#include "isr/irq.s"
; default NMI-ISR is on firmware!


; in headerless builds, keep at least the splash string
#ifdef	NOHEAD
kern_splash:
	.asc	"minimOS-63 0.6a3", 0
#endif

kern_end:		; for size computation
; ***********************************************
; ***** end of kernel file plus API and IRQ *****
; ***********************************************

; **********************************************************************************

; **********************************************************************************
; *** place here the shell code, must end in FINISH macro, currently with header ***
; **********************************************************************************
; must NOT include external shell label!!!
; but MUST make page alignment HERE, the bulit-in one into shell file will fo nothing as already algined

; first determine actual shell address, no longer internally defined!
#ifdef	NOHEAD
shell:					; no header to skip
#else
	.dsb	$100*((* & $FF) <> 0) - (* & $FF), $FF	; page alignment!!! eeeeek
shell	= * + 256		; skip header
#endif

#include "shell/SHELL"

; ************************************************************
; ****** Downloaded kernels add driver staff at the end ******
; ************************************************************
#ifdef	DOWNLOAD
#include "drivers/config/DRIVER_PACK.s"	; this package will be included with downloadable kernels
.data
; downloadable system have ALL system & driver variables AFTER the kernel/API
sysvars:
#include "sysvars.h"
; driver-specific system variables, located here 20170207
dr_vars:
#include "drivers/config/DRIVER_PACK.h"
.text					; eeeeeek
-user_sram = *			; the rest of available SRAM
#endif
