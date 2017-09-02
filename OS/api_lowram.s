; minimOS·63 generic Kernel API for LOWRAM systems
; v0.6a13
; (c) 2017 Carlos J. Santisteban
; last modified 20170902-1920
; MASM compliant 20170614

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
dr_install:
dr_shutdown:

	_ERR(UNAVAIL)		; go away!

; ********************************
; *** COUT, output a character ***
; ********************************
;		INPUT
; acc B		= dev
; io_c		= char
;		OUTPUT
; C 		= I/O error
;		USES BOUT

cout:
	LDX #io_c			; parameter address
	STX bl_ptr
	LDX #1				; single byte
	STX bl_siz
; ...and fall into BOUT

; **************************
; *** BOUT, block output ***
; **************************
;		INPUT
; acc B		= dev
; bl_ptr	= pointer to block
; bl_siz	= block size
;		OUTPUT
; bl_siz	= REMAINING size
; C 		= I/O error
;		USES da_ptr, dr_id, plus whatever the driver takes

cio_of		EQU dr_id	; parameter switching between BLIN and BOUT, must leave da_ptr clear!

blout:

#ifdef	MC6801
	LDAA #D_BOUT		; offset to be added (2)
	STAA cio_of			; store safely (4)
#else
	CLR cio_of			; zero indicates OUT, special faster value (6)
#endif
	TSTB				; for indexed comparisons (2)
	BNE co_port			; not default (4)
		LDAB stdout			; default output device (3)
		BNE co_port			; eeeeeeeeeek (4)
			LDAB #DEVICE		; *** somewhat ugly hack *** (2)
co_port:
	BMI cio_phys		; not a logic device (4)
; no need to check for windows or filesystem
; investigate rest of logical devices
		CMPB #DEV_NULL		; lastly, ignore output (2)
			BNE cio_nfound		; final error otherwise (4)
; DEV_NULL must reset transfer size!
		LDX #0
		STX bl_siz			; transfer complete
		_EXIT_OK			; "/dev/null" is always OK (7)
cio_phys:
	LDX #id_list		; pointer to ID list (3)
	LDAA drv_num		; number of drivers (3)
		BEQ cio_nfound		; no drivers at all! (4)
cio_loop:
		CMPB 0,X			; get ID from list (5)
			BEQ cio_dev			; device found! (4)
		INX					; go for next (4)
		DECA				; one less to go (2)
		BNE cio_loop		; repeat until end, will reach not_found otherwise (4)
cio_nfound:
	_ERR(N_FOUND)		; unknown device (9)
cio_dev:
	NEGA				; driver countdown will be subtracted! (2)
	ADDA drv_num		; compute driver position in list (3)
	ASLA				; two times is offset LSB for drivers_ad list (2)
#ifdef	MC6801
	TAB					; get offset here (2)
	LDX #drvrs_ad		; base address (3)
	ABX					; done! (3)
#else
	ADDA #<drvrs_ad		; take table LSB (2)
	LDAB #>drvrs_ad		; same for MSB (2)
	ADCB #0				; propagate carry (2)
	STAB da_ptr			; store pointer to list entry (4+4)
	STAA da_ptr+1
	LDX da_ptr			; use as base index (4)
#endif
	LDX 0,X				; now X holds the header address (6)
; here must jump to appropriate driver routine
#ifdef	MC6801
;  MCU version
	LDAB cio_of			; get offset, must hold an appropriate D_x offset (3)
	ABX					; GREAT, pointer to routine is ready (3)
	LDX 0,X				; ready to jump there (6)
#else
; generic way follows
	TST cio_of			; input or output? (6)
	BEQ cio_out			; was output eeeeeek (4)
		LDX D_BLIN,X			; otherwise get input routine pointer (6)
		JMP 0,X				; not worth reusing code (4...)
cio_out:
	LDX D_BOUT,X		; faster getting output routine pointer (6)
#endif
	JMP 0,X				; will return to caller (4...)

; ****************************
; *** CIN, get a character *** and manage events!
; ****************************
;		INPUT
; acc B	= dev
;		OUTPUT
; io_c	= char
; C		= not available
;		USES BLIN
cin:
	LDX #io_c			; parameter address
	STX bl_ptr
	LDX #1				; single byte
	STX bl_siz
	JSR blin			; non-patchable call
		BCS ci_exit			; error code must be kept
; now process possible event?
; ** EVENT management **
	LDAA io_c			; get received character (3)
	CMPA #' '-1			; printable? (2)
	BLS ci_manage		; if not, might be an event (4)
ci_exitOK:
		CLC					; otherwise, no error --- eeeeeeeek! (2)
ci_exit:
		RTS					; above comparison would set carry (5)
; ** continue event management **
ci_manage:
; check for binary mode first
	TST cin_mode		; was it in binary mode? (6)
	BEQ ci_event		; if not, should process possible event (4)
		CLR cin_mode		; otherwise back to normal mode (6)
		_EXIT_OK			; and return whatever was received (7)
; standard mode must check for events
ci_event:
	CMPA #16			; is it DLE? (2)
	BNE ci_notdle		; otherwise check next (4)
		STAA cin_mode		; set binary mode! SAFER! (4)
		BRA ci_abort		; and supress received character (4)
ci_notdle:
	CMPA #3				; is it ^C/TERM? (2)
	BNE ci_exitOK		; otherwise there's no more to check -- only signal for single-task systems!
		LDAB #SIGTERM		; signal to be sent (2)
		STAB b_sig			; set signal as parameter (4)
#ifdef	SAFE
		CLRB				; sent to all, this is the only one (2)
#endif
		JSR signal			; send signal (9)
ci_abort:
		_ERR(EMPTY)			; no character was received (9)


; ***********************
; *** BLIN, get block ***
; ***********************
;		INPUT
; acc B		= dev
; bl_ptr	= buffer address
; bl_siz	= maximum transfer
;		OUTPUT
; bl_siz	= REMAINING bytes
; C		= not available
;		USES dr_id and whatever the driver takes
; cin_mode is a kernel variable

blin:
	LDAA #D_CIN			; only difference from cout, as long as is not zero! (2)
	STAA cio_of			; store for further addition/selection (4)
	TSTB				; for indexed comparisons (2)
	BNE ci_port			; specified (4)
		LDAB std_in			; default input device (3)
	BNE ci_port			; eeeeeeeeeek (4)
		LDAB #DEVICE		; *** somewhat ugly hack *** (2)
ci_port:
	BPL ci_nph			; logic device (4)
		JSR cio_phys		; check physical devices and try to get char... but come back for events! (9)
			BCS ci_exit			; some error, send it back (4)
; *** check for some logical devices ***
ci_nph:
; only logical devs, no need to check for windows or filesystem
	CMPB #DEV_RND		; getting a random number? (2)
		BEQ ci_rnd			; compute it! (4)
	CMPB #DEV_NULL		; lastly, ignore input (2)
		BNE cio_nfound		; final error otherwise (4)
; otherwise must fill buffer with zeroes like /dev/zero!!!
	BSR ci_last		; get end address (+1)
	LDAA #0			; STAA is faster than CLR
ci_nl:
		DEX			; reverse loop
		STAA 0,X		; clear byte
		CPX bl_ptr		; compare against buffer start
		BNE ci_nl		; until done
cinr_end:
	LDX #0			; no remaining bytes
	STX bl_siz
	_EXIT_OK			; "/dev/null" and "/dev/rnd" are always OK (7)

ci_rnd:
; *** generate random number *** placeholder
	BSR ci_last		; get end address (+1)
ci_rl:
		DEX			; reverse loop
		LDAA ticks+3		; simple placeholder (3)
		STAA 0,X		; clear byte
		CPX bl_ptr		; compare against buffer start
		BNE ci_rl		; until done
	BRA binr_end		; clear remaining and exit (4)

; *** common routine (NULL & RND) for computing last address (+1) in buffer ***
ci_last:
#ifdef	MC6801
	LDD bl_ptr		; get pointer
	ADDD bl_siz		; add size for last address
	STD local1		; store pointer for X...
#else
	LDAA bl_ptr		; get pointer
	LDAB bl_ptr+1
	ADDB bl_siz+1		; add size for last address
	ADCA bl_siz
	STAA local1		; store pointer for X...
	STAB local1+1
#endif
	LDX local1		; get end address (+1)
	RTS

; **************************************
; *** OPEN_W, get I/O port or window ***
; **************************************
;		INPUT
; w_rect	= 16b size VV.HH
; w_rect+2	= 16b pos VV.HH
; str_pt	= pointer to title string, NONE yet used
;		OUTPUT
; acc B	= dev
; C		= not supported/not available

open_w:
	LDX w_rect			; asking for some size? takes 16-bit (4)
	BEQ ow_no_window	; wouldn't do it (4)
		_ERR(NO_RSRC)		; no windows (9)
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
	LDAB #0				; no multitasking, system reserved PID (2)
yield:
close_w:
free_w:
	_EXIT_OK			; all done (7)


; **************************************
; *** UPTIME, get approximate uptime ***
; **************************************
;		OUTPUT
; up_ticks	= 32b ticks, new format 20170822
; up_sec	= 24b approximate uptime in seconds for API compatibility

uptime:
	_CRITIC				; do not change while copying, A is preserved (4)
	LDX ticks			; get system variable word (4)
	STX up_ticks		; and store it in output parameter (5)
	LDX ticks+2			; get system variable word (4)
	STX up_ticks+2			; and store it in output parameter (5)
	_NO_CRIT			; A was preserved (2)
	_EXIT_OK			; (7)


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
	TSTB				; should be system reserved PID (2)
	BEQ ex_st			; OK for single-task system (4)
		_ERR(NO_RSRC)		; no way without multitasking (9)
ex_st:
#endif
	LDS #SPTR			; init stack (3)
; set default SIGTERM handler! eeeeeeeeeeeeeeeeeeeeek
	LDX #sig_kill		; default TERM (3)
	STX mm_sterm		; set variable (5)
; this is how a task should replace the shell
	LDAA #ZP_AVAIL		; eeeeeeeeeeek (2)
	STAA z_used			; otherwise SAFE will not work (4)
; and set default devices!!! eeeeeeeeeeeeeeeeeeeeeeek
; in case of LOWRAM, this will alter default global devices, is that OK?
	LDX def_io			; standard input/output (4)
	STX std_in			; set as GLOBAL (5)
; *** soon will preset registers according to new API ***
; at last, launch code
	LDX ex_pt			; where to jump (4)
	CLI					; time to do it! (2)
	JSR 0,X				; call and return to SIGKILL (4...)

; *** SIGKILL standard handler ***
sig_kill:
; systems without memory management have nothing to free...
	TST sd_flag			; some pending action? (6)
	BEQ rst_shell		; if not, just restart the shell (4)
		LDAB #PW_CLEAN		; or go into second phase... (2)
		JSR shutdown		; ...of shutdown procedure (could use JMP) (9)
; if none of the above, a single task system can only restart the shell!
rst_shell:
	LDS #SPTR			; init stack again (in case SIGKILL was called) (3)
	JMP sh_exec			; back to kernel shell! (3)


; **************************************************
; *** B_SIGNAL, send UNIX-like signal to a braid ***
; **************************************************
;		INPUT
; b_sig	= signal to be sent
; acc B	= PID (0 means TO ALL)

signal:
#ifdef	SAFE
	TSTB				; check correct PID, really needed? (2)
		BNE sig_pid			; strange error? (4)
#endif
	LDAA b_sig			; get the signal (3)
	CMPA #SIGTERM		; clean shutdown? (2)
	BNE sig_suic		; no, check for KILL then (4)
; 6800 SIGTERM handlers end in RTS, thus a pretty standard jump is OK
; please note that it should clear C in case of no problems!
		LDX mm_sterm		; pointer to handler (4)
		JMP 0,X				; will return to caller (4)
sig_suic:
	CMPA #SIGKILL		; suicide? (2)
		BEQ sig_kill		; it was, thus terminate as usual (4)
sig_pid:
	_ERR(INVALID)		; unrecognised signal (9)


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
	TSTB				; check PID (2)
		BNE sig_pid			; only 0 accepted (4)
#endif
	LDAB #BR_RUN		; single-task systems are always running (2)
; might include some architecture information...
sig_exit:
	_EXIT_OK			; done so far (7)


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
	TSTB				; check PID (2)
		BNE sig_pid			; only 0 accepted (4)
#endif
	LDX ex_pt			; get pointer (4)
	STX mm_sterm		; store in single variable (5)
	_EXIT_OK			; done (7)


; **************************************************************
; *** LOADLINK, get address once in RAM/ROM (in development) ***
; **************************************************************
;		INPUT
; str_pt	= pointer to filename path (will be altered!)
;		OUTPUT
; ex_pt		= pointer to executable code
;		USES rh_scan (modifies it!) AND loc_str (new 20170606)
; might change API somehow...

load_link:
; *** look for that filename in ROM headers ***
; get initial scanning address as local variable
	LDX #ROM_BASE		; begin of ROM contents (3)
	STX	rh_scan			; set local pointer (5)
ll_geth:
; no need to correct parameter pointer as will use independent pointers anyway
		LDX str_pt			; get string pointer (4)
		STX loc_str			; temporary internal pointer (5)
		LDX rh_scan			; *** reload scanning address *** (4)
; ** check whether we are on a valid header!!! **
		LDAA 0,X			; first of all should be a NUL (5)
#ifdef	SAFE
		ORAA 254,X			; last word is CLEAR on 6800 architecture (5)
		ORAA 255,X			; ***or put the whole 32-bit as big-endian?*** (5)
#endif
			BNE ll_nfound		; link was lost, no more to scan (4)
		LDAA 7,X			; after type and size, get eigth byte in header (5)
		CMPA #CR			; was it a CR? (2)
			BNE ll_nfound		; if not, go away (4)
; look for the name
ll_nloop:
			LDAA 8,X			; get character in found name from its offset (5)
			LDX loc_str			; switch to name pointer (4)
			CMPA 0,X			; compare with what we are looking for (5)
				BNE ll_nthis		; difference found (4)
			INX					; advance this local name pointer... (4)
			STX loc_str			; ...update stored... (5)
			LDX rh_scan			; ...and switch back to header pointer (4)
			TST 8,X				; otherwise check whether at EOL (7)
				BEQ ll_found		; all were zero, both ended names are the same! note different offset (4)
			INX					; otherwise continue scanning (4)
			STX rh_scan			; ...and update... (5)
			BRA ll_nloop		; will not do forever until aborted (4)
ll_nthis:
; not this one, correct local pointer for the next header
		CLR rh_scan+1		; reset LSB, assuming page-aligned headers! (6)
		LDX rh_scan			; get full pointer to current header (4)
		LDAA 252,X			; relative offset to number of pages ***check endianness*** (5)
		LDAB 253,X			; also number of bytes (***last word is unused!***) (5)
		BEQ ll_bound		; if it does not cross boundary, do not advance page (4)
			INCA				; otherwise goes into next page (2)
ll_bound:
		INCA				; skip header too! (2)
		ADDA rh_scan		; add to previous value (3)
		STAA rh_scan		; update pointer (4)
		CLR rh_scan+1		; keep it page-aligned! (6)
		BCC ll_geth			; inspect new header, if no overflow! (4)
ll_nfound:
	_ERR(N_FOUND)		; all was scanned and the query was not found (9)
ll_found:
; from original LOADLINK code
	CLR rh_scan+1		; reset LSB, assuming page-aligned headers! (6)
	LDX rh_scan			; only header pointer will get used from here (4)
	LDAA 1,X			; check filetype (5)
	CMPA #'m'			; must be minimOS app! (2)
		BNE ll_wrap		; error otherwise (4)
	LDAA 2,X			; next byte is CPU type (5)
; check compability of supplied code against present CPU
	_ADMIN(GESTALT)		; fetch system info (3+8+43)
	LDAB cpu_ll			; the parameter we are looking for, was fw_cpu (3)
	CMPB #'H'			; is it a 68HC11 MCU? (2)
		BEQ ll_hc11			; all Motorola is OK, but not Hitachi! (4)
	CMPB #'K'			; Hitachi microcontroller? (2)
		BEQ ll_hitachi		; native and 6803 down is OK (4)
	CMPB #'U'			; Motorola 6801/6803 microcontroller? (2)
		BEQ ll_mcu			; 6800-6803 only (4)
	CMPB #'M'			; old plain 6800/6802/6808? (2)
		BEQ ll_basic			; only basic code will do (4)
	_PANIC("{CPU?}")	; *** should NEVER arrive here, unless firmware variables are corrupt! ***
ll_hc11:
	CMPA #'H'			; code has HC11 extensions? (2)
		BEQ ll_valid		; that is OK... (4)
		BRA ll_mcu			; ...but otherwise skip Hitachi code (4)
ll_hitachi:
	CMPA #'K'			; Hitachi code? (2)
		BEQ ll_valid		; this and old MCU code will do (4)
ll_mcu:
	CMPA #'U'			; 6801/6803 code? (2)
		BEQ ll_valid		; will tolerate old 6800 too (4)
ll_basic:
	CMPA #'M'			; every supported CPU can run 6800 code (2)
	BEQ ll_valid		; otherwise is code for another architecture! (4)
ll_wrap:
		_ERR(INVALID)		; something was wrong (9)
; present CPU is able to execute supplied code
ll_valid:
	LDAA rh_scan		; get pointer MSB (3)
;	LDAB rh_scan+1		; and MSB (should be zero!)
	INCA				; start from next page, skipping header (2)
	STAA ex_pt			; save execution pointer (4)
;	STAB ex_pt+1		; LSB too
	CLR ex_pt+1			; assuming page-aligned headers (6)
	_EXIT_OK			; ready to go (7)


; *********************************
; *** STRING, prints a C-string ***
; *********************************
;		INPUT
; acc B		= dev
; str_pt	= pointer to string
;		OUTPUT
; C 		= device error
;		USES iol_dev, loc_str and whatever COUT takes

string:
;	STAB iol_dev		; save device (4)
	LDX #0			; clear measured size (3+5)
	STX bl_siz
	LDX str_pt		; get pointer (4)
;	STX bl_ptr		; not needed if the same parameter!
str_loop:
		LDAA 0,X			; get character (5)
			BEQ str_end			; NUL = end-of-string (4)
		INX				; try next otherwise
		INC bl_siz+1			; increment LSB
			BNE str_loop			; contimue if no wrap
		INC bl_siz			; or update MSB
		BRA str_loop
str_end:
	JMP blout		; proceed and return!

; ******************************
; *** READLN, buffered input *** new 20161223
; ******************************
;		INPUT
; acc B		= dev
; str_pt	= buffer address
; ln_siz	= max offset (byte)
;		USES rl_dev, rl_cur, loc_str and whatever CIN takes

readLN:
	STAB rl_dev			; preset device ID! (4)
	LDX str_pt			; destination buffer... (4)
	STX loc_str			; ...set local copy (5)
	CLR rl_cur			; reset cursor (6)
rl_l:
; no-one to yield to!
		LDAB rl_dev			; use device (3)
		JSR cin				; get one character (...)
		BCC rl_rcv			; got something (4)
			CMPB #EMPTY			; otherwise is just waiting? (2)
		BEQ rl_l			; continue then (4)
			LDX str_pt			; back to beginning (4)
			CLR 0,X				; terminate string (7)
			SEC					; MUST set carry again! (2)
			RTS					; and return whatever error (5)
rl_rcv:
		LDAA io_c			; get received char (3)
		LDAB rl_cur			; retrieve cursor (3)
		LDX loc_str			; retrieve index (4)
		CMPA #CR			; hit CR? (2)
			BEQ rl_cr			; all done then (4)
		CMPA #BS			; is it backspace? (2)
		BNE rl_nbs			; delete then (4)
			TSTB				; something to delete? (2)
				BEQ rl_l			; ignore if already zero (4)
			DEX					; otherwise reduce indexes (4+6)
			DEC rl_cur
			BRA rl_echo			; and resume operation (4)
rl_nbs:
		CMPB ln_siz			; overflow? EEEEEEEEEEK (3)
			BCC rl_l			; ignore if so (4)
		STAA 0,X			; store into buffer (6)
		INC	rl_cur			; update indexes (6+4)
		INX
rl_echo:
		STX loc_str			; update pointer (5)
		LDAB rl_dev			; retrieve device (3)
		JSR cout			; echo received character (...)
		BRA rl_l			; and continue (4)
rl_cr:
	LDAA #CR			; newline (2)
	STAA io_c			; EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEK (4)
	LDAB rl_dev			; retrieve device (3)
	JSR cout			; print newline ignoring errors (...)
	LDX loc_str			; retrieve cursor!!!!! (4)
	CLR 0,X				; terminate string (7)
	_EXIT_OK			; and all done! (7)


; **************************************************
; *** SET_FG, enable/disable frequency generator *** TO BE REVISED
; **************************************************
;		INPUT
; zpar.w	= dividing factor (times two?) IN LITTLE ENDIAN???
;		OUTPUT
; C			= busy
;
; should use some firmware interface, just in case it doesn't affect jiffy-IRQ!
; should also be Phi2-rate independent... input as Hz, or 100uS steps?
; *******TO BE REVISED*********
set_fg:
; *** preliminary 6800 code ***
	LDAA VIA+ACR		; get current configuration (4)
	LDX VIA+T1LL		; get older T1 latch values (5)
	STX old_t1			; save them (5)
; *** TO_DO - should compare old and new values in order to adjust quantum size accordingly ***
	LDX zpar			; get new division factor (4) *** LITTLE ENDIAN
		BEQ fg_dis			; if zero, disable output (4)
	TSTA				; check current configuration (2)
		BMI fg_busy			; already in use (4)
	STX VIA+T1LL		; store it (6)
	STX VIA+T1CL		; get it running! (6)
	ORAA #$C0			; enable free-run PB7 output (2)
fg_exit:
	STAA VIA+ACR		; update config (5)
fg_none:
	_EXIT_OK			; finish anyway (7)
fg_dis:
	TSTA				; check current configuration (2)
		BPL fg_none			; it wasn't playing! (4)
	ANDA #$7F			; disable PB7 only (2)
	LDX old_t1			; older T1L (4)
	STX VIA+T1LL		; restore old value, supposed to be running already (6)
; *** TO_DO - restore standard quantum ***
	BRA fg_exit			; usual ending (4)
fg_busy:
	_ERR(BUSY)			; couldn't set (9)


; ***********************************************************
; *** SHUTDOWN, proper shutdown, with or without poweroff ***
; ***********************************************************
;		INPUT
; acc B	= subfunction code
;		OUTPUT
; C		= not implemented?
;		USES da_ptr & b_sig (calls B_SIGNAL)
; sd_flag is a kernel variable

shutdown:
	CMPB #PW_STAT		; is it going to suspend? (2)
		BEQ sd_fw			; don't shutdown system then! (4)
	CMPB #PW_CLEAN		; from end of main task (2)
		BEQ sd_2nd			; continue with second stage (4)
	STAB sd_flag		; store mode for later, first must do proper (4)
; ask THE braid to terminate
	CLRB				; PID=0 means ALL braids (2)
	LDAA #SIGTERM		; will be asked to terminate (2)
	STAA b_sig			; store signal type (4)
	JMP signal			; ask braids to terminate, will return to task until the end (...)
; ** the real stuff starts here **
sd_2nd:
; now let's disable all drivers
	SEI					; disable interrupts (2)
; call each driver's shutdown routine
	LDX drivers_ad		; preset index (5)
; first get the pointer to each driver table
sd_loop:
; get address index
		STX da_ptr			; local index copy (5)
		LDX 0,X				; get pointer to header (6)
			BEQ sd_done			; not ended (4)
		JSR D_BYE,X			; call exit routine! cannot use da_ptr (8...)
		LDX da_ptr			; retrieve list pointer (4)
		INX					; advance to next entry (4+4)
		INX
		BRA sd_loop			; repeat until zero (4)
; ** system cleanly shut, time to let the firmware turn-off or reboot **
sd_done:
	LDAB sd_flag		; retrieve mode (3)
	CMPB #PW_STAT		; suspend? (2)
		BEQ sd_fw			; tell firmware! (4)
	CMPB #PW_OFF		; poweroff? (2)
		BEQ sd_fw			; tell firmware! (4)
	CMPB #PW_COLD		; cold boot? (2)
		BEQ sd_fw			; tell firmware! (4)
	CMPB #PW_HARD		; interrupt? (2)
		BGE sd_fw			; tell firmware! (4)
	CMPB #PW_WARM		; just a warm restart? (2)
	BEQ sd_warm			; will not tell firmware, just jump there (4)
		_ERR(INVALID)		; unrecognised command! (9)
sd_warm:
	JMP warm			; firmware no longer should take pointer, generic kernel knows anyway (3)
sd_fw:
	_ADMIN(POWEROFF)	; except for suspend, shouldn't return...
	RTS					; for suspend or not implemented

; *******************************
; *** end of kernel functions ***
; *******************************


; **************************************************
; *** JUMP table, if not in separate 'jump' file ***
; **************************************************

; *** order MUST match abi.h ***
;-fw_table:				; 128-byte systems' firmware get unpatchable table from here, new 20150318
; 6800 LOWRAM systems may just update the kern_ptr variable, without a firmware-owned table
k_vec:
; basic I/O
	JMP	cout			; output a character
	JMP	cin				; get a character
	JMP	string			; prints a C-string
	JMP	readLN			; buffered input
; block-oriented I/O
	JMP	blout			; standard block output
	JMP	blin			; standard block input
	JMP	dev_config		; device configuration
	JMP	dev_status		; device status
; simple windowing system (placeholders)
	JMP	open_w			; get I/O port or window
	JMP	close_w			; close window
	JMP	free_w			; will be closed by kernel
; other generic functions
	JMP	uptime			; approximate uptime in ticks
	JMP	set_fg			; enable frequency generator (VIA T1@PB7)
	JMP	shutdown		; proper shutdown procedure
	JMP	load_link		; get address once in RAM/ROM
; simplified task management
	JMP	b_fork			; get available PID ***returns 0
	JMP	b_exec			; launch new process ***simpler
	JMP	signal			; send UNIX-like signal to a braid ***SIGTERM & SIGKILL only
	JMP	status			; get execution flags of a task ***eeeeeeeeeek
	JMP	get_pid			; get PID of current braid ***returns 0
	JMP	set_handler		; set SIGTERM handler
	JMP	yield			; give away CPU time for I/O-bound process ***does nothing
; new functionalities TBD
	JMP	aqmanage		; manage asynchronous task queue
	JMP	pqmanage		; manage periodic task queue
; *** unimplemented functions in LOWRAM systems ***
#ifdef	SAFE
	JMP	dr_install		; install driver
	JMP	dr_shutdown		; shutdown driver
	JMP	malloc			; reserve memory
	JMP	memlock			; reserve some address
	JMP	free			; release memory
	JMP	release			; release ALL memory for a PID
	JMP	ts_info			; get taskswitching info
	JMP	set_curr		; set internal kernel info for running task
#endif
