; minimOS-63 generic Kernel API for LOWRAM systems
; v0.6a3
; (c) 2017 Carlos J. Santisteban
; last modified 20170606-0858

; *** dummy function, non implemented ***
unimplemented:		; placeholder here, not currently used
; *** MALLOC, reserve memory ***
; *** MEMLOCK, reserve some address ***
; *** FREE, release memory ***
; *** RELEASE, release ALL memory for a PID ***
; *** TS_INFO, get taskswitching info for multitasking driver ***
; *** SET_CURR, set internal kernel info for running task ***
; not for 128-byte systems
malloc:
memlock:
free:
release:
ts_info:
set_curr:

; *** FUTURE IMPLEMENTATION ***
aqmanage:
pqmanage:

	_ERR(UNAVAIL)	; go away!

; ********************************
; *** COUT, output a character ***
; ********************************
;		INPUT
; acc B		= dev
; io_c	= char
;		OUTPUT
; C = I/O error
;		USES da_ptr, iol_dev, plus whatever the driver takes

cio_of = da_ptr			; parameter switching between CIN and COUT
; da_ptr globally defined, cio_of not needed upon calling dr_call!

cout:
	LDAA #D_COUT		; only difference from cin (2)
	STAA cio_of			; store for further indexing (3)
	TSTB				; for indexed comparisons (2)
	BNE co_port			; not default (3/2)
		LDAB stdout			; default output device (3)
	BNE co_port			; eeeeeeeeeek
		LDAB #DEVICE		; *** somewhat ugly hack ***
co_port:
	BMI cio_phys		; not a logic device (3/2)
; no need to check for windows or filesystem
; investigate rest of logical devices
		CMPB #DEV_NULL		; lastly, ignore output
			BNE cio_nfound		; final error otherwise
		_EXIT_OK			; "/dev/null" is always OK
cio_phys:
;6502******************************************************************
	LDX drv_num			; number of drivers (3)
		BEQ cio_nfound		; no drivers at all! (2/3)
cio_loop:
		CMP drivers_id-1, X	; get ID from list, notice trick (4)
			BEQ cio_dev			; device found! (2/3)
		DEX					; go back one (2)
		BNE cio_loop		; repeat until end, will reach not_found otherwise (3/2)
cio_nfound:
	_ERR(N_FOUND)		; unknown device, needed before cio_dev in case of optimized loop
cio_dev:
	DEX					; needed because of backwards optimized loop (2)
	TXA					; get index in list (2)
	ASL					; two times (2)
	TAX					; index for address table!
	LDY cio_of			; get offset (3)
	LDA drivers_ad, X	; take table LSB (4)
	STA da_ptr			; store pointer (3)
	LDA drivers_ad+1, X	; same for LSB (4+3)
	STA da_ptr+1		; cannot use neater way but no longer needs cio_of!
	JMP dr_call			; re-use routine (3...)

; *****************************
; *** CIN,  get a character ***
; *****************************
;		INPUT
; acc B	= dev
;		OUTPUT
; io_c	= char
; C		= not available
;		USES iol_dev, and whatever the driver takes
; cin_mode is a kernel variable

cin:
	LDAA #D_CIN			; only difference from cout
	STAA cio_of			; store for further addition
	TSTB				; for indexed comparisons
	BNE ci_port			; specified
		LDAB std_in			; default input device
	BNE ci_port			; eeeeeeeeeek
		LDAB #DEVICE		; *** somewhat ugly hack ***
ci_port:
	BPL ci_nph			; logic device
		JSR cio_phys		; check physical devices... but come back for events! new 20150617
			BCS ci_exit			; some error, send it back
; ** EVENT management **
; this might be revised, or supressed altogether!
		LDAA io_c			; get received character
		CMPA #' '-1			; printable?
			BLS ci_manage		; if not, might be an event
ci_exitOK:
		CLC					; otherwise, no error --- eeeeeeeek!
ci_exit:
		RTS					; above comparison would set carry
; ** continue event management **
ci_manage:
; check for binary mode
	TST cin_mode		; get flag, new sysvar 20150617
	BEQ ci_event		; should process possible event
		CLR cin_mode		; back to normal mode
		BRA ci_exitOK		; and return whatever was received
ci_event:
	CMPA #16			; is it DLE?
	BNE ci_notdle		; otherwise check next
		STAA cin_mode		; set binary mode! SAFER!
		BNE ci_abort		; and supress received character, no need for BRA
ci_notdle:
	CMPA #3				; is it ^C? (TERM)
	BNE ci_exitOK		; otherwise there's no more to check -- only signal for single-task systems!
		LDAB #SIGTERM
		STAB b_sig			; set signal as parameter
#ifdef	SAFE
		CLRB				; sent to all, this is the only one (skimming a couple of bytes!)
#endif
		JSR signal			; send signal FASTER
ci_abort:
		_ERR(EMPTY)			; no character was received

ci_nph:
; only logical devs, no need to check for windows or filesystem
	CMPB #DEV_RND		; getting a random number?
		BEQ ci_rnd			; compute it!
	CMPB #DEV_NULL		; lastly, ignore input
		BNE cio_nfound		; final error otherwise
	_EXIT_OK			; "/dev/null" is always OK

ci_rnd:
; *** generate random number (6800) ***
	LDAA ticks+1		; simple placeholder
	STAA io_c			; eeeeeeek
	_EXIT_OK


; **************************************
; *** OPEN_W, get I/O port or window ***
; **************************************
;		INPUT
; w_rect	= 16b size VV.HH
; w_rect+2	= 16b pos VV.HH
; str_pt	= pointer to title string, NONE yet used
;		OUTPUT
; acc B	= dev
; C	= not supported/not available

open_w:
	LDX w_rect			; asking for some size? takes 16-bit
	BEQ ow_no_window	; wouldn't do it
		_ERR(NO_RSRC)
ow_no_window:
; ...and continue into following functions, returning just 0 in acc B

; *************************************
; ***** GET_PID, get current PID ******
; *** B_FORK, reserve available PID ***
; *************************************
;		OUTPUT
; acc B = PID (0 means singletask system)
; *********************************************
; *** B_YIELD, Yield CPU time to next braid ***
; *********************************************
; (no interface needed)
; ********************************************************
; *** CLOSE_W,  close window *****************************
; *** FREE_W, release window, will be closed by kernel ***
; ********************************************************
;		INPUT
; acc B = dev

get_pid:
b_fork:
	LDAB #0				; no multitasking, system reserved PID
yield:
close_w:
free_w:
	_EXIT_OK


; **************************************
; *** UPTIME, get approximate uptime ***
; **************************************
;		OUTPUT
; up_ticks	= 16b ticks, new standard format 20161006
; up_sec	= 32b uptime in seconds

uptime:
	_ENTER_CS			; do not change while copying, A is preserved (4)
	LDX ticks			; get system variable word (5)
	STX up_ticks		; and store them in output parameter (5)
	LDX ticks+2			; get system variable word (5)
	STX up_sec			; and store them in output parameter (5)
	LDX ticks+4			; get system variable last word (5)
	STX up_sec+2		; and store it in output parameter (5)
	_EXIT_CS			; A was preserved (2)
	_EXIT_OK


; *****************************************
; *** B_EXEC, launch new loaded process ***
; *****************************************
;		INPUT
; acc B		= PID (0 for singletask only)
; ex_pt		= execution pointer
; def_io	= 16b default std_in (MSB) & stdout (LSB)
;
; API still subject to change... (rendez-vous mode TBD)

b_exec:
; non-multitasking version
#ifdef	SAFE
	TSTB				; should be system reserved PID
	BEQ ex_st			; OK for single-task system
		_ERR(NO_RSRC)		; no way without multitasking
ex_st:
#endif
	LDS #SPTR			; init stack
; set default SIGTERM handler! eeeeeeeeeeeeeeeeeeeeek
	LDX #sig_kill		; default TERM
	STX mm_sterm		; set variable
; this is how a task should replace the shell
	LDAA #ZP_AVAIL		; eeeeeeeeeeek
	STAA z_used			; otherwise SAFE will not work
; and set default devices!!! eeeeeeeeeeeeeeeeeeeeeeek
; in case of LOWRAM, this will alter default global devices, is that OK?
	LDX def_io			; standard input/output
	STX std_in			; set at GLOBAL
; *** soon will preset registers according to new API ***
; at last, launch code
	LDX ex_pt			; where to jump
	CLI					; time to do it!
	JSR 0, X			; call and return to SIGKILL

; *** SIGKILL standard handler ***
sig_kill:
; systems without memory management have nothing to free...
	TST sd_flag			; some pending action?
	BEQ rst_shell		; if not, just restart the shell
		LDAB #PW_CLEAN		; or go into second phase...
		JSR shutdown		; ...of shutdown procedure (could use JMP)
; if none of the above, a single task system can only restart the shell!
rst_shell:
	LDS #SPTR			; init stack again (in case SIGKILL was called)
	JMP sh_exec			; back to kernel shell!


; **************************************************
; *** B_SIGNAL, send UNIX-like signal to a braid ***
; **************************************************
;		INPUT
; b_sig	= signal to be sent
; acc B	= PID (0 means TO ALL)

signal:
#ifdef	SAFE
	TSTB				; check correct PID, really needed?
		BNE sig_pid			; strange error?
#endif
	LDAA b_sig			; get the signal
	CMPA #SIGTERM		; clean shutdown?
	BNE sig_suic
; 6800 SIGTERM handlers end in RTS, thus a pretty standard jump is OK
		LDX mm_sterm		; pointer to handler
		JMP 0, X			; will return to caller
sig_suic:
	CMPA #SIGKILL		; suicide?
		BEQ sig_kill
sig_pid:
	_ERR(INVALID)		; unrecognised signal


; ************************************************
; *** B_STATUS, get execution flags of a braid ***
; ************************************************
;		INPUT
; acc B	= addressed braid
;		OUTPUT
; acc B	= flags ***TBD
; C		= invalid PID

status:
#ifdef	SAFE
	TSTB				; check PID
		BNE sig_pid			; only 0 accepted
#endif
	LDAB #BR_RUN		; single-task systems are always running
; might include some architecture information...
sig_exit:
	_EXIT_OK


; **************************************************************
; *** SET_HNDL, set SIGTERM handler, default is like SIGKILL ***
; **************************************************************
;		INPUT
; acc B	= PID (0 means to myself)
; ex_pt	= SIGTERM handler routine (ending in RTS)
;		OUTPUT
; C		= bad PID

set_handler:
#ifdef	SAFE
	TSTB				; check PID
		BNE sig_pid			; only 0 accepted
#endif
	LDX ex_pt			; get pointer
	STX mm_sterm		; store in single variable (from unused table)
	_EXIT_OK


; ***************************************************************
; *** LOAD_LINK, get address once in RAM/ROM (in development) ***
; ***************************************************************
;		INPUT
; str_pt	= pointer to filename path (will be altered!)
;		OUTPUT
; ex_pt		= pointer to executable code
;		USES rh_scan AND loc_str (new 20170606)

load_link:
; *** look for that filename in ROM headers ***
; get initial scanning address as local variable
	LDX #ROM_BASE		; begin of ROM contents
	STX	rh_scan			; set local pointer
ll_geth:
; no need to correct parameter pointer as will use independent pointers anyway
		LDX str_pt			; get string pointer
		STX loc_str			; temporary internal pointer
		LDX rh_scan			; *** reload scanning address ***
; ** check whether we are on a valid header!!! **
		LDAA 0, X			; first of all should be a NUL
#ifdef	SAFE
		ORAA 254, X			; last word is CLEAR on 6800 architecture
		ORAA 255, X			; ***or put the whole 32-bit as big-endian?***
#endif 
			BNE ll_nfound		; link was lost, no more to scan
		LDAA 7, X			; after type and size, get eigth byte in header
		CMPA #CR			; was it a CR?
			BNE ll_nfound		; if not, go away
; look for the name
ll_nloop:
			LDAA 8, X			; get character in found name from its offset
			LDX loc_str			; switch to name pointer
			CMPA 0, X			; compare with what we are looking for
				BNE ll_nthis		; difference found
			INX					; advance this local name pointer...
			STX loc_str			; ...update stored...
			LDX rh_scan			; ...and switch back to header pointer
			TST 8, X			; otherwise check whether at EOL
				BEQ ll_found		; all were zero, both ended names are the same! note different offset
			INX					; otherwise continue scanning
			STX rh_scan			; ...and update...
			BRA ll_nloop		; will not do forever until aborted
ll_nthis:
; not this one, correct local pointer for the next header
		CLR rh_scan+1		; reset LSB, assuming page-aligned headers!
		LDX rh_scan			; get full pointer to current header
		LDAA 252, X			; relative offset to number of pages
		LDAB 253, X			; also number of bytes (***last word is unused!***)
		BEQ ll_bound		; if it does not cross boundary, do not advance page
			INCA				; otherwise goes into next page
ll_bound:
		INCA				; skip header too!
		ADDA rh_scan		; add to previous value
		STAA rh_scan		; update pointer
		CLR rh_scan+1		; keep it page-aligned!
		BCC ll_geth			; inspect new header (if no overflow!)
ll_nfound:
	_ERR(N_FOUND)		; all was scanned and the query was not found
ll_found:
; from original LOADLINK code
	CLR rh_scan+1		; reset LSB, assuming page-aligned headers!
	LDX rh_scan			; only header pointer will get used from here
	LDAA 1, X			; check filetype
	CMPA #'m'			; must be minimOS app!
		BNE ll_wrap		; error otherwise
	LDAA 2, X			; next byte is CPU type
; check compability of supplied code against present CPU
	LDAB fw_cpu			; *** UGLY HACK, this is a FIRMWARE variable ***
	CMPB #'H'			; is it a 68HC11 MCU?
		BEQ ll_hc11			; all Motorola is OK, but not Hitachi!
	CMPB #'K'			; Hitachi microcontroller?
		BEQ ll_hitachi		; native and 6803 down is OK
	CMPB #'U'			; Motorola 6801/6803 microcontroller?
		BEQ ll_mcu			; 6800-6803 only
	CMPB #'M'			; old plain 6800/6802/6808?
		BEQ ll_basic			; only basic code will do
	_PANIC("{CPU?}")	; *** should NEVER arrive here, unless firmware variables are corrupt! ***
ll_hc11:
	CMPA #'H'			; code has HC11 extensions?
		BEQ ll_valid		; that is OK...
		BRA ll_mcu			; ...but skip Hitachi code
ll_hitachi:
	CMPA #'K'			; Hitachi code?
		BEQ ll_valid		; this and old MCU code will do
ll_mcu:
	CMPA #'U'			; 6801/6803 code?
		BEQ ll_valid		; will tolerate old 6800 too
ll_basic:
	CMPA #'M'			; every supported CPU can run 6800 code
	BEQ ll_valid		; otherwise is code for another architecture!
ll_wrap:
		_ERR(INVALID)		; something was wrong
; present CPU is able to execute supplied code
ll_valid:
	LDAA rh_scan		; get pointer MSB
;	LDAB rh_scan+1		; and MSB (should be zero!)
	INCA				; start from next page, skipping header
	STAA ex_pt			; save execution pointer
;	STAB ex_pt+1		; LSB too
	CLR ex_pt+1			; assuming page-aligned headers
	_EXIT_OK


; *********************************
; *** STRING, prints a C-string ***
; *********************************
;		INPUT
; acc B		= dev
; str_pt	= pointer to string
;		OUTPUT
; C = device error
;		USES iol_dev and whatever COUT takes

string:
;6502******************************************************************
	STY iol_dev			; save Y
	LDY #0				; reset new index
	LDA str_pt+1		; get older MSB in case it changes
	PHA					; save it somewhere!
str_loop:
		LDA (str_pt), Y		; get character, new approach
			BEQ str_end			; NUL = end-of-string
		STA io_c			; store output character for COUT
		_PHY				; save current index
		LDY iol_dev			; retrieve device number
		_KERNEL(COUT)		; call routine
#ifdef	SAFE
		BCC str_nerr		; extra check
			PLA					; cleanup stack
			_BRA str_exit		; return error code (and restore pointer)
str_nerr:
#endif
		_PLY				; retrieve index
		INY					; eeeeeeeek!
		BNE str_loop		; repeat, will later check for termination
	INC str_pt+1		; next page, unfortunately
	BNE str_loop		; no need for BRA
str_end:
	CLC					; no errors
str_exit:
	PLA					; get MSB back
	STA str_pt+1		; restore it
	RTS					; return error code


; ******************************
; *** READLN, buffered input *** new 20161223
; ******************************
;		INPUT
; acc B		= dev
; str_pt	= buffer address
; ln_siz	= max offset (byte)
;		USES rl_dev, rl_cur and whatever CIN takes

readLN:
;6502******************************************************************
	STY rl_dev			; preset device ID!
	_STZY rl_cur		; reset variable
rl_l:
		JSR yield			; always useful!
		LDY rl_dev			; use device
		JSR cin				; get one character
		BCC rl_rcv			; got something
			CPY #EMPTY			; otherwise is just waiting?
		BEQ rl_l			; continue then
			LDA #0
			_STAX(str_pt)		; if any other error, terminate string
			RTS					; and return whatever error
rl_rcv:
		LDA io_c			; get received
		LDY rl_cur			; retrieve index
		CMP #CR				; hit CR?
			BEQ rl_cr			; all done then
		CMP #BS				; is it backspace?
		BNE rl_nbs			; delete then
			TYA					; check index
				BEQ rl_l			; ignore if already zero
			DEC rl_cur			; otherwise reduce index
			_BRA rl_echo		; and resume operation
rl_nbs:
		CPY ln_siz			; overflow? EEEEEEEEEEK
			BCS rl_l			; ignore if so
		STA (str_pt), Y		; store into buffer
		INC	rl_cur			; update index
rl_echo:
		LDY rl_dev			; retrieve device
		JSR cout			; echo received character
		_BRA rl_l			; and continue
rl_cr:
	LDA #CR				; newline
	LDY rl_dev			; retrieve device
	JSR cout			; print newline (ignoring errors)
	LDY rl_cur			; retrieve cursor!!!!!
	LDA #0				; no STZ indirect indexed
	STA (str_pt), Y		; terminate string
	_EXIT_OK			; and all done!


; **************************************************
; *** SET_FG, enable/disable frequency generator *** TO BE REVISED
; **************************************************
;		INPUT
; zpar.w = dividing factor (times two?)
;		OUTPUT
; C = busy
;
; should use some firmware interface, just in case it doesn't affect jiffy-IRQ!
; should also be Phi2-rate independent... input as Hz, or 100uS steps?
; *******TO BE REVISED*********
set_fg:
;6502******************************************************************
	LDA zpar
	ORA zpar+1
		BEQ fg_dis		; if zero, disable output
	LDA VIA+ACR		; get current configuration
		BMI fg_busy	; already in use
	LDX VIA+T1LL	; get older T1 latch values
	STX old_t1		; save them
	LDX VIA+T1LH
	STX old_t1+1
; *** TO_DO - should compare old and new values in order to adjust quantum size accordingly ***
	LDX zpar			; get new division factor
	STX VIA+T1LL	; store it
	LDX zpar+1
	STX VIA+T1LH
	STX VIA+T1CH	; get it running!
	ORA #$C0		; enable free-run PB7 output
	STA VIA+ACR		; update config
fg_none:
	_EXIT_OK		; finish anyway
fg_dis:
	LDA VIA+ACR		; get current configuration
		BPL fg_none	; it wasn't playing!
	AND #$7F		; disable PB7 only
	STA VIA+ACR		; update config
	LDA old_t1		; older T1L_L
	STA VIA+T1LL	; restore old value
	LDA old_t1+1
	STA VIA+T1LH	; it's supposed to be running already
; *** TO_DO - restore standard quantum ***
		_BRA fg_none
fg_busy:
	_ERR(BUSY)		; couldn't set


; ***********************************************************
; *** SHUTDOWN, proper shutdown, with or without poweroff ***
; ***********************************************************
;		INPUT
; acc B	= subfunction code
;		OUTPUT
; C	= not implemented?
;		USES b_sig (calls B_SIGNAL)
; sd_flag is a kernel variable

shutdown:
	CMPB #PW_STAT		; is it going to suspend?
		BEQ sd_stat			; don't shutdown system then!
	CMPB #PW_CLEAN		; from end of main task
		BEQ sd_2nd			; continue with second stage
	STAB sd_flag		; store mode for later, first must do proper 
system shutdown, note long addressing
; ask THE braid to terminate
	CLRB				; PID=0 means ALL braids
	LDAA #SIGTERM		; will be asked to terminate
	STAA b_sig			; store signal type
	JMP signal			; ask braids to terminate, needs to return to task until the end
; ** the real stuff starts here **
sd_2nd:
; now let's disable all drivers
	SEI					; disable interrupts
; call each driver's shutdown routine
;6502******************************************************************
	LDA drv_num			; get number of installed drivers
	ASL					; twice the value as a pointer
	TAX					; use as index
; first get the pointer to each driver table
sd_loop:
; get address index
		DEX					; go back one address
		DEX
		LDA drivers_ad+1, X	; get address MSB (4)
		BEQ sd_done			; not in zeropage
		STA da_ptr+1		; store pointer (3)
		LDA drivers_ad, X	; same for LSB (4+3)
		STA da_ptr
		_PHX				; save index for later
		LDY #D_BYE			; offset for shutdown routine --- eeeeeek!
		JSR dr_call			; call routine from generic code!
		_PLX				; retrieve index
		BNE sd_loop			; repeat until zero
; ***************************************************

; ** system cleanly shut, time to let the firmware turn-off or reboot **
sd_done:
	LDAB sd_flag		; retrieve mode
	CMPB #PW_STAT		; suspend?
		BEQ sd_fw			; tell firmware!
	CMPB #PW_OFF		; poweroff?
		BEQ sd_fw			; tell firmware!
	CMPB #PW_COLD		; cold boot?
		BEQ sd_fw			; tell firmware!
	CMPB #PW_WARM		; just a warm restart?
	BEQ sd_warm			; will not tell firmware, just jump there
		_ERR(INVALID)		; unrecognised command!
sd_warm:
	JMP warm			; firmware no longer should take pointer, generic kernel knows anyway
sd_fw:
	_ADMIN(POWEROFF)	; except for suspend, shouldn't return...
	RTS					; for suspend or not implemented

; *******************************
; *** end of kernel functions ***
; *******************************

; *******************************
; *** other data and pointers ***
; *******************************

; ****************************
; *** end of kernel tables ***
; ****************************

; **************************************************
; *** JUMP table, if not in separate 'jump' file ***
; **************************************************

; *** order MUST match abi.h ***
;-fw_table:				; 128-byte systems' firmware get unpatchable table from here, new 20150318
; 6800 LOWRAM systems may just update the kern_ptr variable, without a firmware-owned table
k_vec:
; basic I/O
	JMP	cout		; output a character
	JMP	cin			; get a character
	JMP	string		; prints a C-string
	JMP	readLN		; buffered input
; simple windowing system (placeholders)
	JMP	open_w		; get I/O port or window
	JMP	close_w		; close window
	JMP	free_w		; will be closed by kernel
; other generic functions
	JMP	uptime		; approximate uptime in ticks
	JMP	set_fg		; enable frequency generator (VIA T1@PB7)
	JMP	shutdown	; proper shutdown procedure
	JMP	load_link	; get addr. once in RAM/ROM
; simplified task management
	JMP	b_fork		; get available PID ***returns 0
	JMP	b_exec		; launch new process ***simpler
	JMP	signal		; send UNIX-like signal to a braid ***SIGTERM & SIGKILL only
	JMP	status		; get execution flags of a task ***eeeeeeeeeek
	JMP	get_pid		; get PID of current braid ***returns 0
	JMP	set_handler	; set SIGTERM handler
	JMP	yield		; give away CPU time for I/O-bound process ***does nothing
; new functionalities TBD
	JMP	aqmanage	; manage asynchronous task queue
	JMP	pqmanage	; manage periodic task queue
; *** unimplemented functions ***
#ifdef	SAFE
	JMP	malloc		; reserve memory
	JMP	memlock		; reserve some address
	JMP	free		; release memory
	JMP	release		; release ALL memory for a PID
	JMP	ts_info		; get taskswitching info
	JMP	set_curr	; set internal kernel info for running task
#endif
