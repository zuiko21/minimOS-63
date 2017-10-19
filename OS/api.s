; minimOS·63 generic Kernel API
; ****** originally copied from LOWRAM version, must be completed from 6502 code *****
; v0.6a8
; (c) 2017 Carlos J. Santisteban
; last modified 20171019-1316
; MASM compliant 20170614

; *** dummy function, non implemented ***
unimplemented:		; placeholder here, not currently used
; *** MEMLOCK, reserve some address ***
; *** RELEASE, release ALL memory for a PID ***
; *** TS_INFO, get taskswitching info for multitasking driver ***
; *** SET_CURR, set internal kernel info for running task ***
memlock:
release:
ts_info:
set_curr:

; *** FUTURE IMPLEMENTATION ***
aqmanage:
pqmanage:
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

;cio_of		EQU dr_id	; parameter switching between BLIN and BOUT, must leave da_ptr clear!

blout:
	TSTB				; for indexed comparisons (2)
	BNE co_port			; not default (4)
		LDAB stdout			; default output device (3)
		BNE co_port			; eeeeeeeeeek (4)
			LDAB defltout		; otherwise get global device (4)
co_port:
	BMI cio_phys		; not a logic device (4)
; will need to check for windows or filesystem...
		CMPB #64				; first file-dev??? ***
			BLT co_win			; below that, should be window manager
; ** optional filesystem access **
#ifdef	FILESYSTEM
		CMPB #64+MX_FILES	; still within file-devs?
			BGE co_log			; that value or over, not a file
; *** manage here output to open file ***
#endif
; ** end of filesystem access **
co_win:
; *** virtual windows manager TO DO ***
		_ERR(NO_RSRC)		; not yet implemented ***placeholder***
co_log:
; investigate rest of logical devices
		CMPB #DEV_NULL		; lastly, ignore output (2)
		BNE cio_nfound		; final error otherwise (4)
; DEV_NULL must reset transfer size!
			LDX #0
			STX bl_siz			; transfer complete
			_EXIT_OK			; "/dev/null" is always OK (7)
cio_nfound:
		_ERR(N_FOUND)		; unknown device (9)
cio_phys:
	ASLB				; convert (negative) ID into table offset (2)
	STAB iol_dev		; eeeeeeeeeeeeeeeeeeeeeeeeeek
#ifdef	MC6801
	LDX #cio_lock		; get locks table base pointer (3)
	ABX					; compute entry address! ()
	STX local1			; eeeeeeeeeek
#else
	ADDB #<cio_lock		; add offset to base LSB... ()
	STAB local1+1		; ...save temporarily... (check conflicts!!!)
	LDAB #0
	ADCB #>cio_lock		; ...and propagate carry to MSB ()
	STAB local1			; pointer is complete (see above)
#endif
	_CRITIC
	PSHA				; *** just in case B_YIELD touches it ***
co_loop:
		LDX local1			; pointer was ready to use
		LDAB 0,X			; get pointed entry in locks list
			BEQ co_lckd			; exit loop if free
		_KERNEL(B_YIELD)	; otherwise yield CPU time and repeat
		BRA co_loop
cp_lckd:
	LDAB run_pid		; current task...
	LDX local1			; ...just in case...
	STAB 0,X			; ...is now the owner of this resource
	PULA				; *** retrieve saved flags ***
	_NO_CRIT
; now compute direct driver address
	LDAB iol_dev		; retrieve this index!
#ifdef	MC6801
	LDX #drv_opt		; get output routines table base pointer (3)
	ABX					; compute entry address! ()
#else
	ADDB #<drv_opt		; add offset to base LSB... ()
	STAB local2+1		; ...save temporarily... (check conflicts!!!)
	LDAB #0
	ADCB #>drv_opt		; ...and propagate carry to MSB ()
	STAB local2			; pointer is complete... (see above)
	LDX local2			; ...and ready to use
#endif
	LDX 0,X				; eeeeeeeeeeeeeeeeeeeeek
	JSR 0,X				; call driver and return
cio_unlock:
	LDX local1			; lock pointer was ready to use
	CLR 0,X				; make entry unlocked
	RTS					; return whatever error

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
	PSHB				; MUST keep device, much safer this way
	_KERNEL(BLIN)		; patchable call!
	BCC ci_nerror
		PULA				; must discard saved device while keeping error code...
		RTS					; error code must be kept
ci_nerror:
; ** EVENT management **
	LDAA io_c			; get received character (3)
	CMPA #' '-1			; printable? (2)
	BLS ci_manage		; if not, might be an event (4)
		PULA				; again, discard saved device...
ci_exitOK:
		CLC					; otherwise, no error --- eeeeeeeek! (2)
ci_exit:
		RTS					; above comparison would set carry (5)
; ** continue event management **
ci_manage:
; check for binary mode first
; must compute entry on cin_mode table!
	PULB				; retrieve device ID
#ifdef	MC6801
	LDX #cin_mode		; get mode table base pointer (3)
	ABX					; compute entry address! ()
#else
	ADDB #<cin_mode		; add offset to base LSB... ()
	STAB local1+1		; ...save temporarily... (check conflicts!!!)
	LDAB #0
	ADCB #>cin_mode		; ...and propagate carry to MSB ()
	STAB local1			; pointer is complete (see above)
	LDX local1			; ...and ready to use
#endif
	LDAB 0,X			; was it in binary mode? ()
	BEQ ci_event		; if not, should process possible event (4)
		CLR 0,X				; otherwise back to normal mode (6)
		_EXIT_OK			; and return whatever was received (7)
ci_event:
	CMPA #16			; is it DLE?
	BNE ci_notdle		; otherwise check next
		STAA 0,X		; *set binary mode! puts 16, safer and faster! eeeeeeeeeeeek
		_ERR(EMPTY)			; and supress received character (will NOT stay locked!)******************
ci_notdle:
	CMPA #3				; is it ^C? (TERM)
	BNE ci_noterm		; otherwise check next
		LDAA #SIGTERM
		BRA ci_signal		; send signal
ci_noterm:
	CMPA #4				; is it ^D? (KILL) somewhat dangerous...
	BNE ci_nokill		; otherwise check next
		LDAA #SIGKILL
		BRA ci_signal		; send signal
ci_nokill:
	CMPA #26			; is it ^Z? (STOP)
		BNE ci_exitOK		; otherwise there is no more to check
	LDAA #SIGSTOP		; last signal to be sent
ci_signal:
	STAA b_sig			; set signal as parameter
	LDAB run_pid		; faster GET_PID
	_KERNEL(B_SIGNAL)	; send signal to myself
; continue after having filtered the error
ci_error:
	_ERR(EMPTY)			; no character was received


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
		LDAB deflt_in		; get system global instead (3)
ci_port:
	BPL ci_nph			; logic device (4)
; new MUTEX for CIN, physical devs only!
		ASLB				; convert to proper physdev index (2)
		STAB iol_dev		; keep physdev temporarily, worth doing here (3)
; now must compute pointer to locks array
#ifdef	MC6801
		LDX #cio_lock		; get locks table base pointer (3)
		ABX					; compute entry address! ()
		STX local1			; eeeeeeeeeek
#else
		ADDB #<cio_lock		; add offset to base LSB... ()
		STAB local1+1		; ...save temporarily... (check conflicts!!!)
		LDAB #0
		ADCB #>cio_lock		; ...and propagate carry to MSB ()
		STAB local1			; pointer is complete (see above)
#endif
; * this has to be done atomic! *
		_CRITIC
		PSHA				; will need to keep status????????????
ci_loop:
			LDX local1			; point to lock, check conflicts!
			LDAB 0,X			; *check whether THAT device in use ()
				BEQ ci_lckd			; resume operation if free ()
; otherwise yield CPU time and repeat
; but first check whether it was me (waiting on binary mode)
			LDAB run_pid		; who am I?
			CMPB 0,X			; *was it me who locked? ()
				BEQ ci_lckdd		; *if so, resume execution ()
; if the above, could first check whether the device is in binary mode, otherwise repeat loop!
; continue with regular mutex
			_KERNEL(B_YIELD)	; otherwise yield CPU time and repeat *** could be patched!
			BRA ci_loop			; try again! (4)
ci_lckd:
		LDAB run_pid		; who is me?
		LDX local1			; ...just in case
		STAB 0,X			; *reserve this (4)
ci_lckdd:
		PULA				; retrieve flags for end of critical section
		_NO_CRIT
; * end of atomic operation *
; now compute direct driver address
		LDAB iol_dev		; retrieve this index!
#ifdef	MC6801
		LDX #drv_opt		; get output routines table base pointer (3)
		ABX					; compute entry address! ()
#else
		ADDB #<drv_opt		; add offset to base LSB... ()
		STAB local2+1		; ...save temporarily... (check conflicts!!!)
		LDAB #0
		ADCB #>drv_opt		; ...and propagate carry to MSB ()
		STAB local2			; pointer is complete... (see above)
		LDX local2			; ...and ready to use
#endif
		LDX 0,X				; eeeeeeeeeeeeeeeeeeeeek
		JSR 0,X				; call driver and return
			BCS cio_unlock		; clear MUTEX and return whatever error!

; ** EVENT management no longer here **

; logical devices management, * placeholder
ci_nph:
	CMPB #64			; within window devices?
		BLT ci_win			; below that, should be window manager
; ** optional filesystem access **
#ifdef	FILESYSTEM
	CMPB #64+MX_FILES	; still within file-devs?
		BGE ci_log			; that or over, not a file
; *** manage here input from open file ***
#endif
; *** virtual window manager TO DO ***
ci_win:
	_ERR(NO_RSRC)		; not yet implemented ***placeholder***
; manage logical devices...
ci_log:
	CMPB #DEV_RND		; getting a random number?
		BEQ ci_rnd			; compute it!
	CMPB #DEV_NULL		; lastly, ignore input...
		BEQ ci_ok			; but work like "/dev/zero"
	JMP cio_nfound		; final error otherwise

ci_null:
; reading from DEV_NULL works like /dev/zero, must fill buffer with zeroes!
; must fill buffer with zeroes like /dev/zero!!!
	BSR ci_last			; get end address (+1)
	LDAA #0				; STAA is faster than CLR
ci_nl:
		DEX					; reverse loop
		STAA 0,X			; clear byte
		CPX bl_ptr			; compare against buffer start
		BNE ci_nl			; until done
cinr_end:
	LDX #0				; no remaining bytes
	STX bl_siz
ci_ok:
	_EXIT_OK			; "/dev/null" and "/dev/rnd" are always OK (7)

ci_rnd:
; *** generate random number *** placeholder
	BSR ci_last			; get end address (+1)
ci_rl:
		DEX					; reverse loop
		LDAA ticks+3		; simple placeholder (3)
		STAA 0,X			; clear byte
		CPX bl_ptr			; compare against buffer start
		BNE ci_rl			; until done
	BRA cinr_end		; clear remaining and exit (4)

; *** common routine (NULL & RND) for computing last address (+1) in buffer ***
ci_last:
#ifdef	MC6801
	LDD bl_ptr			; get pointer
	ADDD bl_siz			; add size for last address
	STD local1			; store pointer for X...
#else
	LDAA bl_ptr			; get pointer
	LDAB bl_ptr+1
	ADDB bl_siz+1		; add size for last address
	ADCA bl_siz
	STAA local1			; store pointer for X...
	STAB local1+1
#endif
	LDX local1			; get end address (+1)
	RTS

; ******************************
; *** MALLOC, reserve memory *** translating from 6502
; ******************************
;		INPUT
; ma_rs		= size (0 means reserve as much memory as available)
; ma_align	= page mask (0=page/not aligned, 1=512b, $FF=bank aligned)
;		OUTPUT
; ma_pt	= pointer to reserved block
; ma_rs	= actual size (esp. if ma_rs was 0, but check LSB too)
; C		= not enough memory/corruption detected
;		USES ma_ix.b
; best to assume fixed distance between system arrays!!!
; ram_pos, ram_stat (+MAX_LIST), ram_pid (+2*MAX_LIST)

malloc:
	LDX #ram_pos		; MUST be the first array in memory (assume MAX_LIST << 85)
	LDAA ma_rs+1		; check individual bytes, just in case, MUCH faster than TST
	BEQ ma_nxpg			; no extra page needed
		INC ma_rs			; otherwise increase number of pages
		CLR ma_rs+1		; ...and just in case, clear asked bytes!
ma_nxpg:
	_CRITIC				; this is dangerous! enter critical section, new 160119
; new code will hopefully keep A
	LDAB ma_rs			; get number of asked pages
	BNE ma_scan			; work on specific size
; otherwise check for biggest available block
ma_biggest:
#ifdef	SAFE
		CPX #ram_pos+MAX_LIST		; already past?
			BEQ ma_corrupt		; something was wrong!!!
; *** self-healing feature for full memory assignment! ***
; X already points to ram_pos array
		LDAB 1,X			; get end position
		SUBB 0,X			; subtract current for size!
			BCS ma_corrupt		; corruption detected!
#endif
; ram_stat is just MAX_LIST bytes ahead
		LDAB MAX_LIST,X			; get status of block
;		CMPB #FREE_RAM		; not needed if FREE_RAM is zero! (2)
		BNE ma_nxbig		; go for next as this one was not free
			BSR ma_alsiz		; **compute size according to alignment mask**
			CMPB ma_rs			; compare against current maximum (3)
			BLT ma_nxbig		; this was not bigger
				STAB ma_rs			; otherwise keep track of it... (3)
				STX ma_ix			; ...and its index!
ma_nxbig:
		INX					; advance index
		LDAB MAX_LIST,X		; peek next status
		CMPB #END_RAM		; check whether at end (2)
			BNE ma_biggest		; or continue
; is there at least one available block?
		LDAB ma_rs			; should not be zero
		BNE ma_fill			; there is at least one block to allocate
			_NO_CRIT			; eeeeeeek! we are going
			_ERR(FULL)			; otherwise no free memory!
; report allocated size
ma_fill:
		LDX ma_ix			; retrieve index
		BRA ma_falgn		; nothing to scan, only if aligned eeeeeek
ma_scan:
; *** this is the place for the self-healing feature! ***
#ifdef	SAFE
		CPX #ram_pos+MAX_LIST		; already past?
			BEQ ma_corrupt		; something was wrong!!!
; check UNALIGNED size for self-healing feature! worth a routine?
		LDAB 1,X		; get end position
		SUBB 0,X		; subtract current for size!
		BCC ma_nobad		; no corruption was seen
ma_corrupt:
			LDAB #<user_ram		; LSB misaligned?
			BEQ ma_zlsb			; nothing to align
				LDAB #1			; ...or start at next page
ma_zlsb:
			ADDB #>user_ram		; this page is beginning of user RAM
			STAB ram_pos		; create values
			LDAB #LOCK_RAM		; ...that will become locked (new value)
			STAB ram_stat		; **should it clear the PID field too???**
			LDAB #SRAM			; physical top of RAM...
			STAA ram_pos+1		; create second set of values
			LDAB #END_RAM		; ...as non-plus-ultra
			STAB ram_stat+1
			_NO_CRIT			; eeeeeeeeeek
			_ERR(CORRUPT)		; report but do not turn system down
ma_nobad:
#endif
		LDAB MAX_LIST,X		; get state of current entry
;		CMPB #FREE_RAM		; looking for a free one (2) not needed if free is zero
			BEQ ma_found		; got one
		CMPB #END_RAM		; got already to the end? (2)
			BEQ ma_nobank		; could not found anything suitable
ma_cont:
		INX					; increase index
		BRA ma_scan			; will exit somewhere else
ma_nobank:
	_NO_CRIT			; non-critical when aborting!
	_ERR(FULL)			; no room for it!
ma_found:
	BSR ma_alsiz		; **compute size according to alignment mask**
	CMPB ma_rs			; compare
		BCS ma_cont			; smaller, thus continue searching (2/3)
; here we go! first of all check whether aligned or not
ma_falgn:
	PSHB					; save current size
; is X pointing to current ram_pos entry???
	LDAB 0,X		; check start address for alignment failure
	BITB ma_align		; any offending bits?
	BEQ ma_aok			; already aligned, nothing needed
		ORAB ma_align		; set disturbing bits...
		INCB				; ...and reset them after increasing the rest
		PSHB					; need to keep the new aligned pointer!
		BSR ma_adv			; create room for assigned block (BEFORE advancing eeeeeeeek)
		INX					; skip the alignment blank
		PULB					; retrieve aligned address
; ram_pos pointer is preserved on ma_adv
		STAB 0,X		; update pointer on assigned block
ma_aok:
	PULB					; retrieve size
; make room for new entry... if not exactly the same size
	CMPB ma_rs			; compare this block with requested size eeeeeeeek
	BEQ ma_updt			; was same size, will not generate new entry
; **should I correct stack balance for safe mode?
		BSR ma_adv			; make room otherwise, and set the following one as free padding
; create after the assigned block a FREE entry!
; access to indexed ram_pos
		LDAB 0,X		; newly assigned slice will begin there eeeeeeeeeek
		ADDB ma_rs			; add number of assigned pages
		STAB 1,X		; update value
		LDAB #FREE_RAM		; let us mark it as free, PID is irrelevant!
; offset to access ram_stat
		STAB MAX_LIST+1,X	; next to the assigned one
ma_updt:
	CLR ma_pt+1			; clear pointer LSB
	LDAB 0,X		; get address of block to be assigned
	STAB ma_pt			; note this is address of PAGE
	LDAB #USED_RAM		; now is reserved
	STAB MAX_LIST,X		; update ram_stat table entry
; ** new 20161106, store PID of caller **
	LDAB run_pid			; who asked for this? FASTER
	STAB 2*MAX_LIST,X		; store PID in ram_pid array
; theoretically we are done, end of CS
	_NO_CRIT			; end of critical section, new 160119
	_EXIT_OK			; we're done

; **** routine for aligned-block size computation ****
; returns found size in B, sets C if OK, error otherwise (C clear!)
ma_alsiz:
	LDAB 0,X			; get bottom address
	BITB ma_align		; check for set bits from mask
	BEQ ma_fit			; none was set, thus already aligned
		ORAB ma_align		; set masked bits...
		INCB				; ...and increase address for alignment
ma_fit:
	EORB #$FF			; invert bits as will be subtracted to next entry
	SEC					; needs one more for twos-complement
	ADCB 1,X			; compute size from top ptr MINUS bottom one
	RTS

; **** routine for making room for an entry ****
ma_adv:
	STX ma_ix			; store current index
ma_2end:
		INX					; previous was free, thus check next
#ifdef	SAFE
		CPX #ram_pos+MAX_LIST-1		; just in case, check offset!!! (needs -1?)
		BLT ma_notend		; could expand
			PULB					; discard return address
			PULB
			BRA ma_nobank		; notice error
ma_notend:
#endif
		LDAB MAX_LIST,X		; check status of block
		CMPB #END_RAM		; scan for the end-of-memory marker
		BNE ma_2end			; hope will eventually finish!
ma_room:
		LDAB 0,X		; get block address
		STAB 1,X		; one position forward
		LDAB MAX_LIST,X		; get block status
		STAB MAX_LIST+1,X	; advance it
		LDAB 2*MAX_LIST,X	; same for PID, non-interleaved!
		STAB 2*MAX_LIST+1,X	; advance it
		DEX					; down one entry
		CPX ma_ix			; position of updated entry
		BNE ma_room			; continue until done
; no longer creates at the beginning of the moved block a FREE entry!
	RTS


; ****************************
; *** FREE, release memory ***
; ****************************
;		INPUT
; ma_pt = addr
;		OUTPUT
; C = no such used block
;
; ram_pos & ram_stat are kernel structures
; see MALLOC for array requisites

free:
#ifdef	SAFE
	LDAA ma_pt			; LSB currently not implemented
		BNE fr_nos			; could not find
#endif
	LDX #ram_pos			; reset index, first array!
	LDAB ma_pt			; get comparison PAGE eeeeeeeeek
	_CRITIC			; supposedly dangerous
fr_loop:
		CMPB 0,X		; is what we are looking for?
			BEQ fr_found		; go free it!
		INX					; advance index
		LDAB MAX_LIST,X		; anyway check status
		CMPB #END_RAM		; no more in list?
		BNE fr_loop			; continue until end
; was not found, thus exit CS and abort
fr_no:
	_NO_CRIT
fr_nos:
	_ERR(N_FOUND)		; no block to be freed!
fr_found:
	LDAB MAX_LIST,X		; only used blocks can be freed!
	CMPB #USED_RAM		; was it in use?
		BNE fr_no			; if not, cannot free it!
	LDAB #FREE_RAM		; most likely zero, might use STZ instead
	STAB MAX_LIST,X		; this block is now free, but...
; really should join possible adjacent free blocks
	LDAB MAX_LIST+1, X	; check status of following entry
;	CMPB #FREE_RAM		; was it free? could be supressed if value is zero
	BNE fr_notafter		; was not free, thus nothing to optimise forward
		BSR fr_join			; integrate following free block
	BEQ fr_ok			; if the first block, cannot look back eeeeeeeeeek
fr_notafter:
	CPX #ram_pos				; check whether it was the first block
		BEQ fr_ok			; do not even try to look back eeeeeeeeeeek
	DEX					; let us have a look to the previous block
	LDAB MAX_LIST,X		; is this one free?
;	CMPB #FREE_RAM		; could be supressed if value is zero
	BNE fr_ok			; nothing to optimise backwards
		BSR fr_join			; otherwise integrate it too
; ** already optimized **
fr_ok:
	_NO_CRIT
	_EXIT_OKW

; routine for obliterating the following empty entry
fr_join:
		LDAB 2,X		; get following address
		STAB 1,X		; store one entry below
		LDAB 2*MAX_LIST+2,X	; copy PID of following, but keep status for last!
		STAB 2*MAX_LIST+1,X		; no longer interleaved
		LDAB MAX_LIST+2,X	; check status of following!
		STAB MAX_LIST+1,X		; store one entry below
		CMPB #END_RAM		; end of list?
		BNE fr_join			; repeat until done
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
	LDAB #DEVICE		; new standard device ID, revise!
; ...and continue into following functions for exit!

; ********************************************************
; *** CLOSE_W,  close window *****************************
; *** FREE_W, release window, will be closed by kernel ***
; ********************************************************
;		INPUT
; acc B = dev

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


; *************************************
; ***** GET_PID, get current PID ******
; *** B_FORK, reserve available PID ***
; *************************************
;		OUTPUT
; acc B = PID (0 means singletask system)
get_pid:
b_fork:
	LDAB #0				; no multitasking, system reserved PID (2)
; ...and go into subsequent EXIT_OK from B_YIELD

; *********************************************
; *** B_YIELD, Yield CPU time to next braid ***
; *********************************************
; (no interface needed)
yield:
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
		_KERNEL(SHUTDOWN)		; ...of shutdown procedure (could be patched)
; if none of the above, a single task system can only restart the shell!
rst_shell:;********revise
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


; *************************************
; ***** GET_PID, get current PID ******
; *************************************
get_pid:
	LDAB run_pid				; no multitasking will give a system reserved PID
	_EXIT_OK

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
	_KERNEL(BOUT)		; proceed...
	RTS			; ...and return!

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
		_KERNEL(CIN)				; get one character (...)
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
		_KERNEL(COUT)			; echo received character (...)
		BRA rl_l			; and continue (4)
rl_cr:
	LDAA #CR			; newline (2)
	STAA io_c			; EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEK (4)
	LDAB rl_dev			; retrieve device (3)
	_KERNEL(COUT)			; print newline ignoring errors (...)
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
	_KERNEL(B_SIGNAL)		; ask braids to terminate, will return
	RTS
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
		LDX D_BYE,X			; eeeeeeeeeeeeeeek (5)
		JSR 0,X				; call exit routine! cannot use da_ptr (8...)
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
; *** DR_INST, install driver ***
; *******************************
;		INPUT
; da_ptr	= pointer to the proposed driver header
;		OUTPUT
; acc. B	= actually assigned ID
; C			= could not install driver (ID in use or invalid, queue full, init failed)

dr_install:
	LDX da_ptr			; function gets standard parameter
; is it worth storing ID?
		LDAA D_ID,X			; get ID code (5)
; unlike the 65xx version, keeping an ID copy is needed
		STAA dr_id			; keep in local variable as will be often used (4)
#ifdef	SAFE
		BMI dr_phys			; only physical devices (4)
; otherwise function returns INVALID code
			JMP dr_iabort		; reject logical devices (3)
dr_phys:
#endif
		LDAA D_AUTH,X		; get provided features (5)
		STAA dr_aut		; keep for later use (4)

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
		LDX D_INIT,X		; EEEEEEEEEEEEEEEEEEEEEEEEEEK (5)
		JSR 0,X				; like dr_icall, call D_INIT routine! (8...)
		BCC dr_succ			; successfull, proceed (4)
; init code failed otherwise, function returns UNAVAIL error code
			JMP dr_uabort
dr_nabort:
; no space on queue, function returns FULL error
			JMP dr_fabort		; (3)
dr_succ:

; *** 4) driver should be OK to install, just check whether this ID was not in use ***
; ++++++ new faster driver list ++++++
;		BSR dr_outptr		; get pointer to output routine (8+28)
;		CPX #dr_error		; check whether something was installed (3)
;			BNE dr_inuse		; pointer was not empty (4)
;		BSR dr_inptr		; get pointer to input routine (8+34)
;		CPX #dr_error		; check whether something was installed (3)
;			BNE dr_inuse		; already in use (4)
;		BRA dr_empty		; was empty, OK (4)
; ****** new, much faster approach, mutable-ID savvy ******
		LDAB dr_id			; *take original ID... or preset parameter! (3)
		ASLB				; *times two, as is index to pointers (2)
#ifdef	MC6801
; MCUs simply add B to X
		LDX #drv_ads		; get base pointer (3)
		ABX					; *add entry offset and we are done! (3)
		STX pfa_ptr			; temporary storage
#else
; compute using local, then load into X
		ADDB #<drv_ads		; add to LSB (2)
		STAB pfa_ptr+1		; *** is this already used??? *** (4)
		LDAB #>drv_ads		; now for MSB (2)
dr_iopx:
		ADCB #0				; propagate carry (2+4)
		STAB pfa_ptr
;		LDX pfa_ptr			; get final pointer (4+5)
#endif
		LDAB pfa_ptr		; check MSB only
		BEQ dr_empty		; if it is clear, is OK to install
dr_inuse:
; already in use, function returns BUSY error or something...
			JMP dr_babort		; ID already in use

; ********************************************
; *** subroutine for copy dr_iopt into (X) ***
; uses acc B and restores X as da_ptr
dr_setpt:
#ifdef MC6801
; MCU version affects A!!!
	LDD dr_iopt			; get word (4)
	STD 0,X				; write it! (4)
#else
	LDAB dr_iopt		; get MSB (3)
	STAB 0,X			; write it! (6)
	LDAB dr_iopt+1		; LSB too (3+6)
	STAB 1,X
#endif
	LDX da_ptr			; restore header pointer (4+5)
	RTS

; *** locate entry for input according to ID ***
; might skim a few bytes, but in a non-6800-compatible way
dr_inptr:
	LDAB dr_id			; *take original ID... or preset parameter! (3)
	ASLB				; *times two, as is index to pointers (2)
#ifdef	MC6801
	LDX #drv_ipt		; get base pointer (3)
	ABX					; *add entry offset and we are done! (3)
	RTS					; no shared code here (5)
#else
	ADDB #<drv_ipt		; add to LSB (2)
	STAB pfa_ptr+1		; *** is this already used??? *** (4)
	LDAB #>drv_ipt		; now for MSB (2)
	BRA dr_iopx			; common ending! (4+17)
#endif

; *** locate entry for output according to ID ***
dr_outptr:
	LDAB dr_id			; *take original ID... or preset parameter! (3)
	ASLB				; *times two, as is index to pointers (2)
#ifdef	MC6801
; MCUs simply add B to X
	LDX #drv_opt		; get base pointer (3)
	ABX					; *add entry offset and we are done! (3)
#else
; compute using local, then load into X
	ADDB #<drv_opt		; add to LSB (2)
	STAB pfa_ptr+1		; *** is this already used??? *** (4)
	LDAB #>drv_opt		; now for MSB (2)
dr_iopx:
	ADCB #0				; propagate carry (2+4)
	STAB pfa_ptr
	LDX pfa_ptr			; get final pointer (4+5)
#endif
	RTS

; *** routine for copying a pointer from header into a table, plus advance to next queue ***
; sysptr & dq_ptr (will NOT get updated) set as usual
dr_itask:
; * preliminary 6800 version, room to optimise *
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

; *** routine for advancing to next queue ***
; both pointers in dq_ptr (whole queue size) and pfa_ptr (next word in header)
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
	LDX pfa_ptr			; get set pointer***** (4)
	INX					; go for next entry (4+4)
	INX
	STX pfa_ptr			; this has to be updated (5+5)
	RTS
; *** end of subroutines ***
; **************************

; *** continue installation ***
dr_empty:

; *** 4b) Set I/O pointers (if memory allows) ***
; no longer checks I/O availability as all drivers must provide at least dummy pointers
		LDX D_BLIN,X		; get input routine address, this X=6502 sysptr (6)
		STX dr_iopt			; *** new temporary, will hold address to write into entry (5)
		BSR dr_inptr		; locate entry for input according to ID! X points to entry (8+34)
		BSR dr_setpt		; using B, copy dr_iopt into (X) (8+29)
		LDX D_BOUT,X		; get output routine address, this X=6502 sysptr (6)
		STX dr_iopt			; *** new temporary, will hold address to write into entry (5)
		BSR dr_outptr		; locate entry for output according to ID! X points to entry (8+28)
		BSR dr_setpt		; using B, copy dr_iopt into (X) (8+29)

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
			LDAB queue_mx+1		; get index of free P-entry! (4)
			TBA				; save for later (2)
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
; again, MCUs do much better, B was kept too
			LDX #drv_p_en		; whole pointer... (3)
			ABX					; ...gets increased (3)
#else
			TAB			; get queue index again, saving 1 byte (2)
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
; ...no errors happened
;	_EXIT_OK
; *** error handling ***
dr_iabort:
		LDAB #INVALID			; logical IDs not allowed
		BRA dr_abort
dr_fabort:
		LDAB #FULL			; no room in queue
		BRA dr_abort
dr_babort:
		LDAB #BUSY			; ID already used******
		BRA dr_abort
dr_uabort:
		LDAB #UNAVAIL			; INIT failed, not available
dr_abort:
; standard error handling, no macro allowed
		SEC
		RTS
dr_ended:
; *** *** end of standard function*** ***
	LDAB dr_id			; get current if mutable
	_EXIT_OK

; ******************************
; *** DR_SHUT, remove driver *** TBD
; ******************************
; interface TBD ****

dr_shutdown:
	_ERR(UNAVAIL)		; go away! PLACEHOLDER ********* TBD


; ***************************************************************
; *** TS_INFO, get taskswitching info for multitasking driver ***
; ***************************************************************
;		OUTPUT
; B			= number of bytes
; ex_pt		= pointer to the proposed stack frame

ts_info:
	LDX #tsi_str			; pointer to proposed stack frame
	STX ex_pt				; store it
	LDAB #tsi_end-tsi_str	; number of bytes
	_EXIT_OK

tsi_str:
; pre-created reversed stack frame for firing tasks up, regardless of multitasking driver implementation
	FDB	isr_schd	; corrected reentry address, NEW standard label from ISR
	FCB	1				; stored X value, best if multitasking
driver is the first one EEEEEEEEEEEK not zero!
tsi_end:
; end of stack frame for easier size computation


; ***********************************************************
; *** RELEASE, release ALL memory for a PID, new 20161115 ***
; ***********************************************************
;		INPUT
; B		= PID, 0 means myself
;		USES ma_pt and whatever takes FREE (will call it)

release:
	TSTB				; check whether asking for myself
	BNE rls_pid			; was it a valid PID?
		LDAB run_pid			; otherwise, get mine
rls_pid:
	LDX #ram_pos			; reset index for first array
rls_loop:
		LDAA MAX_LIST,X		; will check stat of this block
		CMPA #USED_RAM
			BNE rls_oth			; it is not in use
		CMPB 2*MAX_LIST,X	; check whether mine!
		BNE rls_oth			; it is not mine
			PSHB				; otherwise save status
			STX local1		; check possible conflicts!!!
			LDAB 0,X		; get pointer to targeted block
			STAB ma_pt+1			; will be used by FREE
			CLR ma_pt+1
			_KERNEL(FREE)			; release it!
			LDX local1			; retrieve status
			PULB
			BCC rls_next		; keep index IF current entry was deleted!
rls_oth:
		INX					; advance to next block
rls_next:
		LDAA MAX_LIST,X		; look status only
		CMPA #END_RAM		; are we done?
		BNE rls_loop		; continue if not yet
	_EXIT_OK			; no errors...


; ***********************************************************
; *** SET_CURR, set internal kernel info for running task ***
; ***********************************************************
;		INPUT
; B			= PID
; affects internal sysvar run_pid
; run_arch not supported in 8-bit mode

set_curr:
; does not check for valid PID... hopefully the multitasking driver (the only one expected to call this) does
	STAB run_pid			; store PID into kernel variables (4)
	_EXIT_OK

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
