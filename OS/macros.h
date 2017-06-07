; minimOS·63 0.6a3 MACRO definitions
; (c) 2017 Carlos J. Santisteban
; last modified 20170607-0910

; **************************
; *** standard addresses ***
; **************************

kern_ptr	=	$FC		; pointer to jump table, routines ending in RTS
admin_ptr	=	$FFC0	; NEW fixed jump table, routines ending in RTS, intended for kernel/drivers ONLY

; unified address (will lock at $FFE1-2 anyway)
lock		=	$FFE0	; just after the above

; *****************************
; *** common function calls ***
; *****************************

; system calling interface ***
#define		_KERNEL(a)		LDX kern_ptr: JSR a, X
#define		_ADMIN(a)		LDX #admin_ptr: JSR a, X
; Kernel/firmware function numbers are multiples of 3!!! Each entry is JMP ext
; besides zpar* parameters, accumulator B is used (like Y on 6502)

; *** function endings ***
; if Carry is set, acc B holds error code
; * all the same, keep all names for 65xx easier porting *
; for kernel API
#define		_EXIT_OK	CLC: RTS
#define		_ERR(a)		LDAB #a: SEC: RTS

; for apps
#define		_FINISH		CLC: RTS
#define		_ABORT(a)	LDAB #a: SEC: RTS

; for most code, incl. drivers
; such code without error signaling (eg. shutdown, jiffy interrupt) may just end on RTS no matter the CPU
#define		_DR_OK		CLC: RTS
#define		_DR_ERR(a)	LDAB #a: SEC: RTS

; *** special functions ***
; new exit for asynchronous driver routines when not satisfied
#define		_NEXT_ISR	SEC: RTS
#define		_ISR_DONE	CLC: RTS

; new macros for critical sections, do not just rely on SEI/CLI
; * accumulator A must be kept! Otherwise add PSHA after entering and PULA before exit! *
#define		_ENTER_CS	TPA: SEI
#define		_EXIT_CS	TAP

; *** panic call, now using SWI in case of error display *** new SWI handled 20161010
#define		_PANIC(a)	SWI: .asc a, 0

; **************************************************************
; *** device numbers for optional pseudo-driver modules, TBD ***
; **************************************************************

;TASK_DEV	=	128		; no longer needed, may displace the following
WIND_DEV	=	129		; new name 20161017, might suffer the same fate!
FILE_DEV	=	130		; *** this will be sticked somewhere as no patchable API entries for it! Perhaps #128

; *****************************
; *** usual ASCII constants ***
; *****************************

#define		CR		13
#define		LF		10
#define		BS		8
#define		TAB		9
#define		BEL		7
#define		ESC		27

