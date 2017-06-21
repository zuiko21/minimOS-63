; minimOS·63 ROM template
; v0.6a1
; (c) 2017 Carlos J. Santisteban
; last modified 20170621-1312

; create ready-to-blow ROM image
#define		ROM		_ROM

; ***** include files *****
; *** options.h is machine-dependent, copy or link appropriate file from options/ ***
#include "usual.h"

; ************************
; ***** ROM contents *****
; ************************

	ORG	ROM_BASE		; as defined in options.h

; *** minimOS volume header, new 20150604 ***
; not final as current (0.6) LOAD_LINK will not recognise it!
; might use NOHEAD option for systems without any filesystem, but current LOAD_LINK needs it

#ifndef	NOHEAD
sysvol:
	FCB		0			; don't enter here! NUL marks beginning of header
	FCC		"aV"		; minimOS system volume ID, TBD
	FCC		"****"		; some flags TBD
	FCB		CR
	FCC		"sys"		; volume name (mandatory)
	FCB		0
; *** ROM identification string as comment (highly recommended) ***
	FCC		"minimOS·63 0.6 for "
	FCC		MACHINE_NAME			; system version and machine
	FCB		CR
	FCC		"20170616-0857"			; build date and time
	FCB		0

	FILL	$FF, sysvol+$F8-*		; for ready-to-blow ROM, advance to time/date field

	FDB		$4800		; time, 9.00
	FDB		$4AD0		; date, 2017/06/16

;	FDB		$FF00-ROM_BASE			; volume size (for future support)
;	FDB		0						; 64k only
; FAKE file "size" in order to be LOAD_LINK savvy...
	FDB		0
	FDB		0			; nothing inside, skip to adjacent header
#endif

; **************************************
; *** the GENERIC kernel starts here ***
; **************************************

; mandatory kernel label now defined HERE!
#ifdef	NOHEAD
kernel:
#else
kernel	EQU *+256		; skip header
#endif

; includes appropriate shell with its own header
#include "kernel.s"

; **************************
; *** I/O device drivers ***
; **************************
; ### should include a standard header here! ###
#ifndef	NOHEAD
	; standard page alignment for CPP-MASM
	ORG		*-1&$FF00+$100	; eeeeeek
drv_file:
	FCB		0
	FCC		"aD****"	; driver pack file plus flags TBD
	FCB		CR
	FCC		"drivers"	; filename and empty comment
	FDB		0

	FILL	$FF, drv_file+$F8-*			; padding

	FDB		$4800		; time, 09.00
	FDB		$4ACF		; date, 2017/06/15

	FDB		drv_end-drv_file-$100
	FDB		0
#endif
; ### end of minimOS header ###

; after header goes the binary blob
#include	DRIVER_PACK_s
drv_end:				; for easier size computation

; *********************************************
; *** include rest of the supplied software ***
; *********************************************
; with their own headers, these must be page aligned!!!
;#include "../apps/ls.s"
#ifndef		LOWRAM
;#include "../apps/pmap.s"
#endif
;#include "../apps/lined.s"
;#include "../apps/sigtest.s"

; ****** skip I/O area for more ******
; ##### empty header #####
#ifndef	NOHEAD
	; standard page alignment for CPP-MASM
	ORG		*-1&$FF00+$100			; eeeeeek
empty_head:
	FCB		0			; don't enter here! NUL marks beginning of header
	FCC		"aS****"	; just reserved SYSTEM space
	FCB		CR
	FCC		"[I/O]"		; file name (mandatory) and empty comment
	FDB		0
; advance to end of header
	FILL	$FF, empty_head+$FC-*	; for ready-to-blow ROM, advance to size

; *** no valid date & time ***
; filesize in top 32 bits NOT including header, new 20161216
	FDB		afterIO-empty_head-256	; filesize
	FDB		0						; 64K space does not use upper 16-bit
#endif
; ##### end of minimOS header #####

; *** blank space for I/O area skipping ***
afterIO		EQU $E000	; assume I/O ends at $DFFF
	ORG		afterIO		; OK to do this way in S19 format!

; *************************************
; ****** more software after I/O ******
; *************************************
; ...could add more software up to $FC00
;#include "shell/monitor.s"	; SUPPOSEDLY INCLUDED WITHIN KERNEL
;#include "shell/miniMoDA.s"

; ****** skip rest of unused ROM until firmware ******
; ##### empty header #####
#ifndef	NOHEAD
	; standard page alignment for CPP-MASM
	ORG		*-1&$FF00+$100	; eeeeeek
free_head:
	FCB		0			; don't enter here! NUL marks beginning of header
	FCC		"aS****"	; just reserved SYSTEM space
	FCB		CR
	FCC		"[R"
	FCC		"OM]"		; file name (mandatory) and empty comment *** note macro savvy
	FDB		0
; advance to end of header
	FILL	$FF, free_head+$FC-*	; for ready-to-blow ROM, advance to size

; *** no valid date & time ***
; filesize in top 32 bits NOT including header, new 20161216
	FDB		FW_BASE-free_head-256	; filesize
	FDB		0						; 64K space does not use upper 16-bit
#endif
; ##### end of minimOS header #####

; ***************************************
; *** make separate room for firmware ***
; ***************************************
	ORG	FW_BASE			; done this way in S19 format!

; ***********************************
; *** hardware-dependent firmware ***
; ***********************************
#include	ARCH_s
