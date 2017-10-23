; minimOS·63 generic Kernel
; v0.6a17
; MASM compliant 20170614
; (c) 2017 Carlos J. Santisteban
; last modified 20171023-1000

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
	ORG		$0400			; safe address for patchable 2 kiB systems, change if required
#else
; standalone kernels need to keep track of drivers_ad label!
#include DRIVER_PACK_s
	ORG		ROM_BASE
#endif
#endif

; *** standard header, at least for testing ***
#ifndef	NOHEAD
; standard page alignment for CPP-MASM
	ORG		*-1&$FF00+$100	; eeeeeek
kern_head:
	FCB		0
	FCB		'm'
	FCB		CPU_TYPE		; executable for testing TBD
	FCC		"****"			; flags TBD
	FCB		CR
	FCC		"kernel"		; filename
	FCB		0
kern_splash:
	FCC		"minimOS·63 0.6a17"	; version in comment
	FCB		0

	FILL	$FF, kern_head+$F8-*		; padding

	FDB		$5800			; time, 11.00
	FDB		$4ACE			; date, 2017/6/14

	FDB		kern_end-kern_head-256		; kernel size excluding header 
	FDB		0
#endif

; **************************************************
; *** kernel begins here, much like a warm reset ***
; **************************************************

warm:
	SEI					; interrupts off, just in case (2)

; install kernel jump table if not previously loaded
; LOWRAM systems will use a reduced, non-patchable INSTALL function...
; ...or directly set mOS interface!
#ifndef		DOWNLOAD
	LDX #k_vec			; get table address (3)
#ifdef		LOWRAM
	STX kern_ptr		; skimming bytes and cycles! (5)
#else
	STX kerntab			; store parameter (5)
	_ADMIN(INSTALL)		; copy jump table
#endif
#endif

; install ISR code (as defined in "isr/irq.s" below)
	LDX #k_isr			; get address (3)
	STX kerntab			; no need to know about actual vector location (5)
	_ADMIN(SET_ISR)		; install routine

; install SWI code (as defined in "isr/swi.s" below)
	LDX #k_swi			; get address (3)
	STX kerntab		; no need to know about actual vector location (5)
	_ADMIN(SET_DBG)		; install routine

; Kernel no longer supplies default NMI, but could install it otherwise

; *** default action in case the scheduler runs out of tasks ***
	LDAA #PW_STAT		; default action upon complete task death (2)
	STAA sd_flag		; this is important to be clear (PW_STAT) or set as proper error handler (5)

; *****************************
; *** memory initialisation ***
; *****************************

; this should take a basic memory map from firmware, perhaps via the GESTALT function
#ifndef		LOWRAM
; ++++++
	LDX #256*FREE_RAM+END_RAM	; get status of both ends (3)
	STX ram_stat		; as it is the first entry, no index needed (6)
	LDAA #>user_ram		; beginning of available ram, as defined... in rom.s (2)
	LDAB #<user_ram		; LSB misaligned? (2)
	BEQ ram_init		; nothing to align (4)
		INCA				; otherwise start at next page (2)
ram_init:
	LDAB #SRAM			; number of SRAM pages as defined in options.h (2) *** revise
#ifdef	MC6801
	STD ram_pos			; full free RAM start address (5)
#else
	STAA ram_pos		; store it, this is PAGE number (5)
	STAB ram_pos+1		; store second entry and we are done! (5)
#endif
; ++++++
#endif

; ************************************************
; *** intialise drivers from their jump tables ***
; ************************************************
; sometime will create API entries for these, but new format is urgent!
; * will also initialise I/O lock arrays! * 20161129
; completely separate standard & LOWRAM versions 20170902

#ifndef	LOWRAM
; ++++++ ++++++ standard version ++++++ ++++++
; *** 1) initialise stuff ***
; clear some bytes
	CLRB				; reset driver index (2)
	STAB queue_mx		; reset all indexes, faster than CLR (5+5)
	STAB queue_mx+1		; not worth doing LDX#/STX as same bytes and only one cycle faster

; ++++++ new direct I/O tables for much faster access 20160406 ++++++
	STAB run_pid		; new 170222, set default running PID
; this must be done BEFORE initing drivers as multitasking should place appropriate temporary value via SET_CURR! (5)
#ifdef	MC6801
	LDD #dr_error		; make unused entries point to a standard error routine (3)
#else
	LDAA #>dr_error		; make unused entries point to a standard error routine (2)
	LDAB #<dr_error		; the LSB too (2)
#endif
; *** reset all tables ***
	LDX #drv_opt		; output routines array (3)
dr_oclear:
		BSR dr_clrio		; shared code! (8+25)
		CPX #drv_opt+256	; all done? (3+4) might use reduced array
		BNE dr_oclear
	LDX #drv_ipt		; input routines array (3)
dr_iclear:
		BSR dr_clrio		; shared code! (8+25)
		CPX #drv_ipt+256	; all done? (3+4) might use reduced array
		BNE dr_iclear
	LDX #cio_lock		; will clear I/O locks and binary flags! (3)
	CLRA				; zero is to be stored, both arrays interleaved! (2+2)
	CLRB
dr_lclear:
		BSR dr_clrio		; shared code! (8+25)
		CPX #cio_lock+256	; all done? (3+4)
		BNE dr_lclear

#ifdef	MUTABLE
	LDX #drv_ads		; will clear header array, if mutable (3)
dr_dclear:
		BSR dr_clrio		; shared code! (8+25)
		CPX #drv_ads+256	; all done? (3+4)
		BNE dr_dclear
#endif

; *** 2) prepare access to each driver header ***
; first get the pointer to it

drv_aix	EQU		systmp	; temporary pointer, *** not needed into locals ***

	LDX #drvrs_ad		; driver table in ROM (3)
dr_loop:
		STX drv_aix			; reset index (5)
		LDX 0,X				; get current pointer (6)
		BNE dr_inst			; cannot be zero, in case is too far for BEQ dr_ok (4)
			JMP dr_ok			; all done otherwise (3)

; ****************************************************************
; *** subroutine for putting A as MSB and B as LSB, then X=X+2 *** (25, 16 for MCUs)
dr_clrio:
#ifdef	MC6801
	STD 0,X				; store whole word (5)
#else
	STAA 0,X			; set MSB for output (6)
	STAB 1,X			; and LSB (6)
#endif
	INX					; next word (4+4+5)
	INX
	RTS
; *** located here in order to use BSR, above instr jumped anyway ***
; *******************************************************************

dr_inst:
		STX da_ptr			; store pointer to header (5)
		_KERNEL(DR_INST)	; call API
dr_next:
; in order to keep drivers_ad in ROM, can't just forget unsuccessfully registered drivers...
; in case drivers_ad is *created* in RAM, dr_abort could just be here, is this OK with new separate pointer tables?
		LDX drv_aix			; retrieve saved index (4)
		INX					; update ADDRESS index, even if unsuccessful (4)
		INX					; eeeeeeeek! pointer arithmetic! (4)
		JMP dr_loop			; go for next (3)

; ++++++ ++++++ end of standard version ++++++ ++++++

#else

; ------ ------ LOWRAM version complete here ------ ------
; *** 1) initialise stuff ***
; clear some bytes
	CLRB				; reset driver index (2)
	STAB queue_mx		; reset all indexes, faster than CLR (5+5)
	STAB queue_mx+1		; not worth doing LDX#/STX as same bytes and only one cycle faster
; ------ low-RAM systems have no direct tables to reset ------
; ** maybe look for fast tables in ROM **
	STAB drv_num		; single index of, not necessarily SUCCESSFULLY, detected drivers, updated 20150318 (5)
; *** 2) prepare access to each driver header ***
; first get the pointer to it

drv_aix	EQU		systmp	; temporary pointer, *** might go into locals ***

	LDX #drvrs_ad		; driver table in ROM (3)
dr_loop:
		STX drv_aix			; keep index (5)
		LDX 0,X				; get current pointer (6)
		BNE dr_inst			; cannot be zero, in case is too far for BEQ dr_ok (4)
			JMP dr_ok			; all done otherwise (3)
dr_inst:
		STX da_ptr			; store pointer to header (5) *** but check new variable conflicts! ***
; *** external API function here ***
;	LDX da_ptr			; pointer as parameter
; create entry on IDs table
		LDAA D_ID,X			; get ID code (5)
		STAA dr_id			; keep in local variable as will be often used (4)
#ifdef	SAFE
		BMI dr_phys			; only physical devices (4)
; function returns INVALID
			JMP dr_iabort		; reject logical devices (3)
dr_phys:
#endif
		LDAA D_AUTH,X		; get provided features (5)
		STAA dr_aut		; another commonly used value (will stay during check) (4)
; *** 3) before registering, check whether the driver COULD be successfully installed ***
; that means 1) there must be room enough on the interrupt queues for its tasks, if provided
; and 2) the D_INIT routine succeeded as usual
; otherwise, skip the installing procedure altogether for that driver
		ASLA				; extract MSb (will be A_POLL first) (2)
		BCC dr_nptsk		; skip verification if task not enabled (4)
			LDAB queue_mx+1		; get current tasks in P queue (4)
			CMPB #MAX_QUEUE		; room for another? (2)
				BCC dr_nabort		; if not, just abort! (4)
dr_nptsk:
		ASLA				; extract MSb (now is A_REQ) (2)
		BCC dr_nrtsk		; skip verification if task not enabled (4)
			LDAB queue_mx		; get current tasks in A queue (4)
			CMPB #MX_QUEUE		; room for another? (2)
				BCC dr_nabort		; did not check OK (4)
dr_nrtsk:
; if arrived here, there is room for interrupt tasks, but check init code
		LDX D_INIT, X		; get pointer first EEEEEEEEEEEEEEEEEEK (5)
		JSR 0,X				; like dr_icall, call D_INIT routine! (8...)
		BCC dr_succ			; successfull, proceed (4)
; function returns UNAVAIL error
			JMP dr_uabort		; did not check OK (3)
dr_nabort:
; function returns FULL error
			JMP dr_fabort		; no room (3)
dr_succ:

; *** 4) driver should be OK to install, just check whether this ID was not in use ***
; ------ IDs table filling for low-RAM systems ------
; check whether the ID is already in use
		LDX #id_list		; beginning of ID list (3)
		LDAA dr_id			; fetch aspiring ID (3)
		LDAB drv_num		; number of drivers registered this far (3)
		BEQ dr_eol			; already at end of list (4)
dr_scan:
#ifdef	SAFE
			CMPA 0,X			; compare with list entry (5)
			BNE dr_nused			; free to use
				JMP dr_babort		; already in use, don't register (4)
dr_nused:
#endif
			INX					; otherwise try next one (4)
			DECB				; one less to go (2+4)
			BNE dr_scan
dr_eol:
; supposedly no conflicting IDs found, or at least pointer set after last one
		STAA 0,X			; store in list, now in RAM (6)

; *** 5) register interrupt routines *** new, much cleaner approach ***** REVISE
; preliminary 6800 does NOT use a loop
; let us go for P-queue first
		ASL dr_aut			; extract MSB (will be A_POLL first, then A_REQ)
		BCC dr_notpq		; skip installation if task not enabled (4)
; time to get a pointer to the-block-of-pointers (source)
			LDX da_ptr			; work with current header (4)
			LDX D_POLL,X		; get this pointer (6)
			STX pfa_ptr			; store it (5)
; prepare another entry into queue
			LDAA queue_mx+1		; get index of free P-entry! (4)
			TAB					; two copies (2)
#ifdef	MC6801
; MCUs do much better
			LDX #drv_poll		; whole pointer... (3)
			ABX					; ...gets increased... (3)
			STX dq_ptr			; ...and stored (4)
#else
			ADDB #<drv_poll		; add to base of queue (2)
			STAB dq_ptr+1		; LSB is ready (4)
			LDAB #>drv_poll		; go for MSB (2)
			ADCB #0				; propagate carry (2)
			STAB dq_ptr			; complete pointer (4)
#endif
; flags must be enabled for this task!
#ifdef	MC6801
; again, MCUs do much better
			LDX #drv_p_en		; whole pointer... (3)
			ABX					; ...gets increased (3)
#else
			TAB					; get queue index again (2)
			ADDB #<drv_p_en		; add to base of queue (2)
			STAB dte_ptr+1		; LSB is ready (4)
			LDAB #>drv_p_en		; go for MSB (2)
			ADCB #0				; propagate carry (2)
			STAB dte_ptr		; complete pointer (4)
; pointer to enable-array is ready, fill it!
			LDX dte_ptr			; MCUs waive this (4)
#endif
			LDAB dr_id			; use ID as enabling value, as has bit 7 high (3)
			STAB 0,X			; enabled! (6)
; an entry is being inserted, thus update counter
			ADDA #2				; another entry added (A had original count) (2)
			STAA queue_mx+1		; update P-counter (5)
; insert task and advance queue
			BSR dr_itask		; install entry into queue (8+35, 8+23 MCU)
			BSR dr_nextq		; and advance to next! (frequency) (8+40, 8+28 MCU)
; the currently pointed word is freq value, just copy it into currently pointed queue
			BSR dr_itask		; install entry into queue, no need to advance (8+35, 8+23 MCU)
			INC 0,X				; *** increment MSB as expected by ISR implementation ***
; ...but copy too into drv_count! eeeeeeeeeeeeeeeeeeek
			LDAB 0,X			; get original MSB (increased)
			STAB MX_QUEUE*4,X	; ...and preset counters!!!
			LDAB 1,X			; same for LSB
			STAB MX_QUEUE*4+1,X
; P-queue is done, let us go to simpler R-queue
dr_notpq:
		ASL dr_aut			; extract MSB (now is A_REQ) (6)
		BCC dr_notrq		; skip installation if task not enabled (4)
; worth advancing just the header pointer
			LDX da_ptr			; work with current header (4)
			LDX D_ASYN,X		; get this pointer (6)
			STX pfa_ptr			; store it (5)
; prepare another entry into queue
			LDAA queue_mx		; get index of free A-entry! (4)
			TAB					; two copies (2)
#ifdef	MC6801
; MCUs do much better
			LDX #drv_poll		; whole pointer... (3)
			ABX					; ...gets increased... (3)
			STX dq_ptr			; ...and stored (4)
#else
			ADDB #<drv_poll		; add to base of queue (2)
			STAB dq_ptr+1		; LSB is ready (4)
			LDAB #>drv_poll		; go for MSB (2)
			ADCB #0				; propagate carry (2)
			STAB dq_ptr			; complete pointer (4)
#endif
; flags must be enabled for this task!
#ifdef	MC6801
; again, MCUs do much better
			LDX #drv_p_en		; whole pointer... (3)
			ABX					; ...gets increased... (3)
#else
			TAB					; get queue index again (2)
			ADDB #<drv_r_en		; add to base of queue (2)
			STAB dte_ptr+1		; LSB is ready (4)
			LDAB #>drv_r_en		; go for MSB (2)
			ADCB #0				; propagate carry (2)
			STAB dte_ptr		; complete pointer (4)
; pointer to enable-array is ready, fill it!
			LDX dte_ptr			; MCUs waive this (4)
#endif
			LDAB dr_id			; use ID as enabling value, as has bit 7 high (3)
			STAB 0,X			; enabled! (6)
; an entry is being inserted, thus update counter
			ADDA #2				; another entry added (A had original count) (2)
			STAA queue_mx		; update A-counter (5)
; insert task, no need to advance queues any more
			BSR dr_itask		; install entry into queue (8+35, 8+23 MCU)
dr_notrq:


; *** 6) continue initing drivers ***
;		BRA dr_ended			; if arrived here, did not fail initialisation (4)
	_EXIT_OK
dr_iabort:
		LDAB #INVALID
		BRA dr_abort
dr_fabort:
		LDAB #FULL
		BRA dr_abort
dr_babort:
		LDAB #BUSY			;**********
		BRA dr_abort
dr_uabort:
		LDAB #UNAVAIL
dr_abort:
; *** if arrived here, driver initialisation failed in some way! ***
			LDX #id_list		; beginning of ID list (3)
			LDAA drv_num		; number of drivers registered this far (3)
dr_ablst:
				INX					; advance pointer (4)
				DECA				; one less (2)
				BNE dr_ablst		; acc B will never start at 0! (4)
			LDAA #DEV_NULL		; invalid proper value... (2)
			STAB 0,X			; ...as unreachable entry (6)
; standard error, no macro
		SEC
		RTS
; *** *** end of function *** ***

dr_next:
; ------ low-RAM systems keep count of installed drivers ------
		INC drv_num			; update SINGLE index (6)
; in order to keep drivers_ad in ROM, can't just forget unsuccessfully registered drivers...
; in case drivers_ad is *created* in RAM, dr_abort could just be here, is this OK with new separate pointer tables?
		LDX drv_aix			; retrieve saved index (4)
		INX					; update ADDRESS index, even if unsuccessful (4)
		INX					; eeeeeeeek! pointer arithmetic! (4)
		JMP dr_loop			; go for next (3)

; *****************************************
; *** some driver installation routines ***
; *****************************************

; * routine for copying a pointer from header into a table, plus advance to next queue *
; pfa_ptr & dq_ptr (will NOT get updated) set as usual
dr_itask:
; *** preliminary 6800 version, room to optimise ***
; read address from header and update it!
	LDX pfa_ptr			; get set pointer (4)
#ifdef	MC6801
	LDD 0,X				; MCUs get the whole word! (5)
	LDX dq_ptr			; switch pointer (4)
	STD 0,X				; MCUs store whole word! (5)
#else
	LDAA 0,X			; indirect read MSB (5)
	LDAB 1,X			; and LSB (5)
	LDX dq_ptr			; switch pointer (4)
	STAA 0,X			; store fetched task pointer (6)
	STAB 1,X			; LSB too (6+5)
#endif
; in any case, exits with X pointing as dq_ptr *** needed after freq setting
; write pointer into queue
	RTS

; * routine for advancing to next queue *
; both pointers in dq_ptr (whole queue size) and sysptr (next word in header)
; likely to get inlined as used only once just after dr_itask
dr_nextq:
; update the original pointer
	LDAB #MX_QUEUE		; amount to add... (2)
#ifdef	MC6801
; MCUs do it much better!
	ABX					; ...to X which had dq_ptr as per previous dr_itask... (3)
	STX dq_ptr			; ...gets updated! (4)
#else
	ADDB dq_ptr+1		; ...to previous LSB (3)
	LDAA dq_ptr			; MSB too (3)
	ADCA #0				; propagate carry (2)
	STAB dq_ptr+1		; update values (4)
	STAA dq_ptr			; ready! (4)
#endif
; and now the header reading pointer
	LDX pfa_ptr			; get set pointer (4)
	INX					; go for next entry (4+4)
	INX
	STX pfa_ptr			; this has to be updated (5+5)
	RTS

; in case no I/O lock arrays were initialised... only for LOWRAM
	CLR cin_mode		; single flag for non-multitasking systems (6)

; ------ ------ end of LOWRAM version------ ------
#endif

; ***************************************************************
; *** drivers already installed, clean up things and continue ***
; ***************************************************************
dr_ok:
; *** all drivers inited ***
; really no need to terminate ID list on LOWRAM systems

; **********************************
; ********* startup code ***********
; **********************************

; startup code, revise ASAP
; *** set GLOBAL default I/O device ***
	LDX #DEVICE*257		; as defined in options.h (3)
	STX dflt_in		; set word (6)

; *** interrupt setup no longer here, firmware did it! ***

; new, show a splash message ever the kernel is restarted!
	BSR ks_cr			; leading newline (8+...)
	LDX #kern_splash	; get pointer (3)
	STX str_pt			; set parameter (5)
	LDAB #DEVICE		; eeeeeek (2)
	_KERNEL(STRING)		; print it! (...)
	BSR ks_cr			; trailing newline (8+...)

; ******************************
; **** launch monitor/shell ****
; ******************************
sh_exec:
; must use LOADLINK for this
	LDX #shell			; get pointer to built-in shell (3)
	STX ex_pt			; set execution address (5)
	LDX #DEVICE*257		; *** revise (3)
	STX def_io			; default local I/O (5)
	_KERNEL(B_FORK)		; reserve first execution braid, no direct call as could be PATCHED!
	_KERNEL(B_EXEC)		; go for it! no direct call as could be PATCHED!
; singletask systems will not arrive here, ever!
	_KERNEL(B_YIELD)	; ** get into the working code ASAP! ** no direct call as could be PATCHED!
here:
	BRA here			; ...as the scheduler will detour execution (4)

; a quick way to print a newline on standard device
ks_cr:
	LDAA #CR			; leading newline (2+4)
	STAA io_c
	LDAB #DEVICE		; standard output *** revise (2)
	_KERNEL(COUT)		; print it
	RTS

; ***************************
; *** points of no return ***
; ***************************
dr_error:
	_DR_ERR(N_FOUND)	; standard exit for non-existing drivers!

; ***********************************************
; *** generic kernel routines, separate files *** not yet implemented
; ***********************************************
;#ifndef		LOWRAM
	FCC		"<API>"		; debug only
;#include "api.s"
;#else
#include "api_lowram.s"
;#endif

; *********************************
; *** interrupt service routine ***
; *********************************

k_isr:
#include "isr/irq.s"
; default NMI-ISR is on firmware!

; *** 6800 will handle a separate SWI handler ***
k_swi:
#include "isr/swi.s"

; in headerless builds, keep at least the splash string
#ifdef	NOHEAD
kern_splash:
	FCC		"minimOS·63 0.6a17"
	FCB		0
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
; standard page alignment for CPP-MASM
	ORG		*-1&$FF00+$100	; eeeeeek
shell		EQU	*+256		; skip header
#endif

#include SHELL

; ************************************************************
; ****** Downloaded kernels add driver staff at the end ******
; ************************************************************
#ifdef	DOWNLOAD
; this package will be included with downloadable kernels
#include	DRIVER_PACK_s
; downloadable system have ALL system & driver variables AFTER the kernel/API
sysvars:
#include "sysvars.h"
; driver-specific system variables, located here 20170207
dr_vars:
#include	DRIVER_PACK_h
user_sram:				; the rest of available SRAM
#endif
