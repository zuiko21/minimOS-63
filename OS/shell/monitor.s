; Monitor shell for minimOS·63 (simple version)
; v0.6a5
; last modified 20170829-1237
; (c) 2017-2022 Carlos J. Santisteban

#include "../usual.h"

; *** uncomment for narrow (20-char) displays ***
;#define	NARROW	_NARROW

; *** constant definitions ***
#define	BUFSIZ		12
; bytes per line in dumps 4 or 8/16
#ifdef	NARROW
#define		PERLINE		4
#else
#define		PERLINE		8
#endif

; ##### include minimOS headers and some other stuff #####
#ifndef	NOHEAD
	; standard page alignment for CPP-MASM
	ORG		*-1&$FF00+$100	; eeeeeek
mon_head:
; *** header identification ***
	FCB		0
	FCB		'm'			; minimOS app!
	FCB		CPU_TYPE
	FCC		"****"		; some flags TBD
	FCB		CR

; *** filename and optional comment ***
montitle:
	FCC		"monitor"	; file name (mandatory)
	FCB		0
	FCC		"v0.6 for MC6800 and later!"	; comment
	FCB		0

; advance to end of header
	FILL	$FF, mon_head+$F8-*		; for ready-to-blow ROM, advance to time/date field

; *** date & time in MS-DOS format at byte 248 ($F8) ***
	FDB		$7000		; time, 14.00
	FDB		$4ACF		; date, 2017/6/15

; filesize in top 32 bits NOT including header, new 20161216
	FDB		mon_end-mon_head-256	; filesize
	FDB		0						; 64K space does not use upper 16-bit
#endif
; ##### end of minimOS executable header #####

; *** declare zeropage variables ***
; ##### uz_top is last available zeropage byte, z_used goes next #####
ptr		EQU z_used-2	; current address pointer
_pc		EQU ptr			; as per new model, filled by NMI hnadler
siz		EQU ptr-2		; number of bytes to copy or transfer ('n')
lines	EQU siz-1		; lines to dump ('u')
_a		EQU lines-1		; A register
_b		EQU _a-1		; B register
_x		EQU _b-2		; X register
_sp		EQU _x-2		; stack pointer
_psr	EQU _sp-1		; status register
cursor	EQU _psr-2		; storage for full X cursor
buffer	EQU cursor-BUFSIZ	; storage for input line (BUFSIZ chars)
tmp		EQU buffer-2	; temporary storage
smc_pt	EQU tmp-1		; for SMC!!! (JMP 'tmp')
tmp2	EQU tmp-2		; for hex dumps, LSB also used for SMC (JMP 'tmp')
iodev	EQU tmp2-1		; standard I/O ##### minimOS specific #####

; ******************************
; *** initialise the monitor ***
; ******************************

; ##### minimOS specific stuff #####
	LDAA #z_used-iodev	; zeropage space needed
; check whether has enough zeropage space
#ifdef	SAFE
	CMPA z_used			; check available zeropage space
	BLS go_mon			; enough space
		_ABORT(FULL)		; not enough memory otherwise (rare)
go_mon:
#endif
; proceed normally
	STAA z_used			; set needed ZP space as recommended by minimOS
	LDX #0				; worth doing for...
	STX w_rect			; ...no screen size required
	LDX #montitle		; window title pointer
	STX str_pt			; set parameter
	_KERNEL(OPEN_W)		; ask for a character I/O device
	BCC open_mon		; no errors
		_ABORT(NO_RSRC)		; abort otherwise! proper error code
open_mon:
	STAB iodev			; store device!!!
; ##### end of minimOS specific stuff #####
; print splash message, just the first time!
	LDX #mon_splash		; address of splash message
	JSR prnStr			; print the string!

; *** store current stack pointer as it will be restored upon JMP ***
; no other relevant registers to be initialised
get_sp:
	STS _sp				; store current stack pointer
; does not really need to set PC/ptr

; *** these ought to be initialised after calling a routine! ***
	LDAA #z_used-iodev	; zeropage space needed
	STAA z_used			; set needed ZP space as required by minimOS ####
; global variables
	LDAA #4				; standard number of lines
	STAA lines			; set variable
	STAA siz+1			; also default transfer size
	CLR siz				; clear copy/transfer size MSB

; ********************
; *** begin things ***
; ********************
main_loop:
; put current address before prompt
		LDAB ptr			; MSB goes first
		JSR prnHex			; print it
		LDAB ptr+1			; same for LSB
		JSR prnHex
		LDAB #'>'			; prompt character
		JSR prnChar			; print it
		JSR getLine			; input a line
		LDX #buffer-1		; getNextChar will advance it to initial position!
		JSR gnc_do			; get first character on string, without the variable
		TSTA				; recheck flags!
			BEQ main_loop		; ignore blank lines!
;		STX cursor			; save cursor!
		CMPA #'X'			; compare against last command?
			BGT bad_cmd			; past it, unrecognised
#ifdef	SAFE
		SUBA #'?'			; first available command
#else
		SUBA #'A'			; first available command
#endif
			BMI bad_cmd			; ...cannot be lower
		ASLA				; times two to make it pointer offset
; *** call command routine, pointer offset in A ***
#ifdef	MC6801
		TAB					; the place to be added from
		LDX #cmd_ptr		; base address
		ABX					; compute entry address
#else
		ADDA #<cmd_ptr		; add to table LSB
		STAA tmp+1			; store partial result
		LDAA #>cmd_ptr		; go for MSB
		ADCA #0				; propagate carry
		STAA tmp			; pointer is complete
		LDX tmp				; get temporary pointer
#endif
		LDX 0,X				; get indirect address from table entry
		JSR 0,X				; call and return here!
		BRA main_loop		; continue forever

; *** generic error handling ***
_unrecognised:
	INS					; discard main loop return address
	INS
bad_cmd:
	LDX #err_bad		; address of error message
d_error:
	JSR prnStr			; display error
	BRA main_loop		; continue

; ****************************************************
; *** command routines, named as per pointer table ***
; ****************************************************

; ** .A = set accumulator A **
set_A:
	JSR fetch_byte		; get operand in A
	STAA _a				; set accumulator
	RTS

; ** .B = set accumulator B **
set_B:
	JSR fetch_byte		; get operand in A
	STAA _b				; set accumulator
	RTS

; ** .T = store byte **
store_byte:
	JSR fetch_byte		; get operand in A
	LDX ptr				; pointer position
	STAA 0,X			; set byte in memory
	INX					; advance pointer
	STX ptr				; update current value
sb_end:
	RTS

; ** .C = call address **
call_address:
	JSR fetch_value		; get operand address
#ifdef	SAFE
	TST tmp2			; at least one?
		BEQ _unrecognised	; reject zero loudly
#endif
; setting SP upon call makes little sense...
	LDAA iodev			; must push default device for later
	PSHA
	JSR do_call			; *** set regs and jump! ***
; ** should record current registers here **
	PSHA				; save A as will take PSR
	TPA					; get current status
	STAA _psr			; and store it
	PULA				; retrieve A accumulator
	STAA _a				; store in array
	STAB _b
	STX _x
; hopefully no stack imbalance was caused, otherwise will not resume monitor!
	PULA				; this (eeeeek) will take previously saved default device
	STAA iodev			; store device!!!
	INS					; must discard previous return address, as will reinitialise stuff!
	INS
	JMP get_sp			; hopefully context is OK, will restore as needed

; ** .J = jump to an address **
jump_address:
	JSR fetch_value		; get operand address
#ifdef	SAFE
	TST tmp2			; at least one?
		BEQ _unrecognised	; reject zero loudly
#endif
; restore stack pointer...
	LDS _sp				; get stored value (word)
; SP restored
; restore registers and jump
do_call:
	LDAA #$7E			; *** opcode for JMP ext ***
	STAA smc_pt			; ...stored just in front of desired address!
	LDX _x				; retrieve registers
	LDAB _b				; accumulator B
	LDAA _a				; get saved accumulator A...
	PSHA				; ...into stack!
	LDAA _psr			; status is differently set...
	TAP					; ...from A
	PULA				; lastly retrieve accumulator (does not alter status)
	JMP smc_pt			; *** go via SMC! *** might return somewhere else

; ** .E = examine 'u' lines of memory **
examine:
	JSR fetch_value		; get address
	LDX tmp				; save tmp elsewhere
	STX tmp2
	LDAA lines			; get original counter
ex_l:
		PSHA				; save counter
		LDAB tmp2			; address MSB
		JSR prnHex			; print it
		LDAB tmp2+1			; same for LSB
		JSR prnHex
		LDX #dump_in		; address of separator
		JSR prnStr			; print it
		; loop for 4/8 hex bytes
		LDAA #0				; reset offset
ex_h:
			PSHA				; save offset
; space only when wider than 20 char AND if not the first
#ifndef	NARROW
			TSTA				; check offset
			BEQ ex_ns			; no space if the first one
				JSR vr_spc			; common space print!
ex_ns:
#endif
			LDX tmp2			; set pointer...
			LDAB 0,X			; ...and get byte
			JSR prnHex			; print it in hex
			PULA				; retrieve offset
			INX					; next byte
			INCA
			CMPA #PERLINE		; bytes per line (8 if not 20-char)
			BNE ex_h			; continue line
		LDX #dump_out		; address of separator
		JSR prnStr			; print it
		; loop for 4/8 ASCII
		LDAA #0				; reset offset...
		LDAB tmp2+1			; ...and for pointer...
		SUBB #PERLINE		; ...decrement it back to start of line
		STAB tmp2+1
		BCC ex_a			; no borrow to propagate into MSB...
			DEC tmp2			; ...or move to PREVIOUS page
ex_a:
			PSHA				; save offset BEFORE!
			LDX tmp2			; set pointer...
			LDAB 0,X			; ...and get byte
			CMPB #127			; check whether printable
				BGT ex_np
			CMPB #' '
				BLT ex_np
			BRA ex_pr			; it is printable
ex_np:
				LDAB #'.'			; otherwise, substitute
ex_pr:		JSR prnChar			; print it
			PULA				; retrieve index
			INX					; next byte
			INCA
			CMPA #PERLINE		; bytes per line (8 if not 20-char)
			BNE ex_a			; continue line
		LDAB #CR				; print newline
		JSR prnChar
; pointer was incremented thru second loop!
		PULA				; retrieve counter!!!!
		DECA				; one line less
		BNE ex_l			; continue until done
	RTS

; ** .G = set stack pointer **
set_SP:
	JSR fetch_word		; get operand in @tmp
	LDX tmp				; this was the value
	STX _sp				; set stack pointer
	RTS

; ** .? = show commands **
#ifdef	SAFE
help:
	LDX #help_str		; help string
	JMP prnStr			; print it, and return to main loop
#endif

; ** .K = keep (load or save)
; ### highly system dependent ###
; placeholder will send (-) or read (+) raw data to/from indicated I/O device
ext_bytes:
; *** set labels from miniMoDA ***

count	EQU	tmp2+1		; subcommand
temp	EQU	cursor		; will store I/O channel
oper	EQU	tmp			; 16-bit counter
; try to get subcommand, then device
	JSR getNextChar		; check for subcommand
	TSTA					; already at the end?
	BNE ex_noni			; not yet
ex_abort:
		JMP _unrecognised			; fail loudly otherwise
ex_noni:
; there is subcommand, let us check target device ###placeholder
	STAA count			; first save the subcommand!
	JSR fetch_byte		; read desired device
		BCS ex_abort		; could not get it
	STAA temp			; set as I/O channel
	LDAB #0				; reset counter!
	STAB oper			; also reset forward counter, decrement is too clumsy!
	STAB oper+1
; check subcommand
	LDAA count			; restore subcommand
	CMPA #'+'			; is it load?
		BEQ ex_load			; OK then
#ifdef	SAFE
	CMPA #'-'			; is it save? (MARATHON MAN)
		BNE ex_abort		; if not, complain
#endif
; assume save!
; 6800 should get X ready!
	LDX ptr				; current pointer
; save raw bytes
		LDAA 0,X		; get source data
		STAA io_c			; set parameter
		STX ptr				; keep index updated
		LDAB temp			; get target device
		_KERNEL(COUT)		; send raw byte!
			BCS ex_err			; aborted!
		BCC ex_next			; otherwise continue, no need for BRA
; load raw bytes
ex_load:
		LDAB temp			; get target device
		_KERNEL(CIN)		; get raw byte!
			BCS ex_err			; aborted!
		LDAA io_c			; get parameter
		LDX ptr				; retrieve pointer
		STAA 0,X		; write destination data
; go for next byte in any case
ex_next:
		LDX ptr				; worth reloading
		INX					; go for next
		STX ptr				; and update!
; 16-bit counter INcrement
		LDX oper			; get full value
		INX				; one more
		STX oper			; stay updated
; have we finished yet?
		CPX siz				; compare against desired size
		BNE ex_do			; continue until done
; transfer ended, show results
#ifdef	SAFE
	BRA ex_show			; skip error message
#endif
ex_err:
#ifdef	SAFE
; an I/O error occurred during transfer!
	LDX #io_err		; set message pointer
	JSR prnStr			; print it
#endif
ex_show:
	LDAA oper+1			; get LSB
	PSHA					; into stack
	LDAA oper			; get MSB
	PSHA					; same
	JMP nu_end			; and print it! eeeeeek return also

; ** .M = move (copy) 'n' bytes of memory **
move:
; preliminary 6800 code, forward only
	JSR fetch_value		; get operand word
#ifdef	SAFE
	TST tmp2			; at least one?
	BNE mv_ok
		JMP _unrecognised	; reject zero loudly
mv_ok:
#endif
	LDX siz				; copy original size...
		BEQ mv_end			; (unless nothing to do!)
	STX tmp2			; ...on a safe place
mv_loop:
		LDX ptr				; set source pointer
		LDAB 0,X			; get source byte
		INX					; one more...
		STX ptr				; ...and update
		LDX tmp				; switch to destination pointer
		STAB 0,X			; copy at destination
		INX					; one more...
		STX tmp				; ...and update
		LDX tmp2			; lets see the counter
		DEX					; one less to go (may set Z)
		STX tmp2			; updated...
		BNE mv_loop			; ...until expired
mv_end:
	RTS

; ** .N = set 'n' value **
set_count:
	JSR fetch_value		; get operand word
	LDX tmp				; whole word
#ifdef	SAFE
		BEQ sc_z			; there was nothing!
sc_ok:
#endif
	STX siz				; into destination variable
sc_z:
	LDAB siz+1			; check current or updated value LSB
	PSHB				; into stack
	LDAB siz			; same for MSB
	PSHB

; *** common ending for .N and .U ***
; 16-bit value on stack!
nu_end:
	LDX #set_str		; pointer to rest of message
	JSR prnStr			; print that
	PULB				; get pushed MSB
	JSR prnHex			; show in hex
	PULB				; also for LSB
	JSR prnHex			; show in hex
	BRA po_cr			; print trailing newline and return!

; ** .O = set origin **
origin:
	JSR fetch_value		; get operand word
	LDX tmp				; copy value...
	STX ptr				; into destination variable
	RTS

; ** .P = set status register **
set_PSR:
	JSR fetch_byte		; get operand in A
	STAA _psr			; set status
	RTS

; ** .Q = standard quit **
quit:
; will not check any pending issues
	INS					; discard main loop return address
	INS
	_FINISH				; exit to minimOS, proper error code

; ** .S = store raw string **
store_str:
		LDX cursor			; set pointer
		INX					; first skip the S, then go increasing
		LDAA 0,X			; get raw character
		STX cursor			; update...
		LDX ptr				; ...and switch pointer
		STAA 0,X			; store in place
			BEQ sstr_end		; until terminator, will be stored anyway
		CMPA #CR			; newline also accepted, just in case
			BEQ sstr_cr			; terminate and exit
		INX					; advance destination
		STX ptr				; update pointer
		BRA store_str		; should end sometime!
sstr_cr:
	CLR 0,X				; terminate string
sstr_end:
	RTS

; ** .U = set 'u' number of lines/instructions **
set_lines:
	JSR fetch_byte		; get operand in A
#ifdef	SAFE
	TSTA				; check value
		BEQ sl_z			; nothing to set
#endif
	STAA lines			; set number of lines
sl_z:
	PSHA				; to be displayed
	CLRA				; no MSB
	PSHA
	BRA nu_end

; ** .V = view register values **
view_regs:
	LDX #regs_head		; print header
	JSR prnStr
; since _pc and ptr are the same, no need to print it!
; preliminary non-loop version
; * A *
	LDAB _a				; get register...
	JSR prnHex			; show value in hex
#ifndef	NARROW
	BSR vr_spc			; add separation
#endif
; * B *
	LDAB _b				; get register...
	JSR prnHex			; show value in hex
	BSR vr_spc			; always add separation
; * X *
	LDAB _x				; get register MSB...
	JSR prnHex			; show value in hex
	LDAB _x+1			; get register LSB...
	JSR prnHex			; show value in hex
	BSR vr_spc			; always add separation
; * S *
	LDAB _sp			; get register MSB...
	JSR prnHex			; show value in hex
	LDAB _sp+1			; get register LSB...
	JSR prnHex			; show value in hex
#ifndef	NARROW
	BSR vr_spc			; add separation
#endif
; * bitwise P *
	LDAA #8				; bit to show from PSR
	LDAB _psr			; copy original status
	ASLB				; discard two irrelevant bits
	ASLB
vr_sb:
		PSHA				; keep counter!
		ASLB				; get leftmost bit
		PSHB				; keep remaining status (PSR unaffected)
		LDAB #' '			; default is off (space) C unaffected
		BCC vr_off			; was really off
			INCB				; otherwise turns into '!'
vr_off:
		JSR prnChar			; prints bit
		PULB				; retrieve remaining status...
		PULA				; ...and counter
		DECA				; one less
		BNE vr_sb			; until done
; newline-printing common routine
po_cr:
	LDAB #CR			; print newline
	JMP prnChar			; will return
; space-printing common routine
vr_spc:
	LDAB #' '			; print space
	JMP prnChar			; will return to caller


; ** .W = store word **
store_word:
	JSR fetch_value		; get operand word
	LDAA tmp			; get MSB
	LDX ptr				; also pointer
	STAA 0,X			; store in memory
	INX					; next byte
	LDAA tmp+1			; same for LSB
	STAA 0,X			; store in memory
	INX					; next byte
	STX ptr				; update incremented pointer
sw_end:
	RTS

; ** .X = set X register **
set_X:
	JSR fetch_word		; get operand @tmp
	LDX tmp
	STX _x				; set register
	RTS

; ** .R = reboot or shutdown **
reboot:
	JSR getNextChar		; is there an extra character?
	TSTA				; check whether end of buffer
		BEQ rb_exit			; no interactive option
	CMPA #'W'			; asking for warm boot?
	BNE rb_notw
		LDAB #PW_WARM		; warm boot request ## minimOS specific ##
		BRA fw_shut		; call firmware, could use BNE?
rb_notw:
	CMPA #'C'			; asking for cold boot?
	BNE rb_notc
		LDAB #PW_COLD		; cold boot request ## minimOS specific ##
		BRA fw_shut			; call firmware, could use BNE?
rb_notc:
	CMPA #'S'			; asking for shutdown?
	BNE rb_exit			; otherwise abort quietly
		LDAB #PW_OFF		; poweroff request ## minimOS specific ##
fw_shut:
		_KERNEL(SHUTDOWN)	; unified firmware call
rb_exit:
	RTS					; needs to return and wait for the complete shutdown!


; *** useful routines ***
; ** basic output and hexadecimal handling **

; might include this library when a portable, properly interfaced one is available!
;#include "libs/hexio.s"
; in the meanwhile, it takes these subroutines

; * print a byte in B as two hex ciphers *
prnHex:
	PSHB				; keep whole value
	LSRB				; shift right four times (just the MSB)
	LSRB
	LSRB
	LSRB
	BSR ph_b2a			; convert and print this cipher
	PULB 				; retrieve full value
	ANDB #$0F			; keep just the LSB... and repeat procedure
ph_b2a:
	CMPB #10			; will be a letter?
	BLT ph_n			; just a number
		ADDB #7				; convert to letter
ph_n:
	ADDB #'0'			; convert to ASCII
; ...and print it (will return somewhere)

; * print a character in B *
prnChar:
	STAB io_c			; store character
	LDAB iodev			; get device
	_KERNEL(COUT)		; output it ##### minimOS #####
; ignoring possible I/O errors
	RTS

; * print a NULL-terminated string pointed by X *
prnStr:
	STX str_pt			; store pointer
	LDAB iodev			; standard device
	_KERNEL(STRING)		; print it! ##### minimOS #####
; currently ignoring any errors...
	RTS

; * convert two hex ciphers into byte@tmp, A is current char, X is cursor *
; * new approach for hex conversion *
; * add one nibble from hex in current char!
; A is current char, returns result in value[0...1]
; does NOT advance any cursor (neither reads char from nowhere)
; MUST reset value previously!
hex2nib:
	SUBA #'0'			; convert from ASCII
		BMI h2n_err			; below number!
	CMPA #10			; already OK?
	BLT h2n_num			; do not convert from letter
		CMPA #23			; otherwise should be a valid hex
			BGT h2n_err			; or not!
		SUBA #7				; convert from hex
h2n_num:
	LDAB #4				; shifts counter, no longer X in order to save some pushing!
h2n_loop:
		ASL tmp+1			; current value will be times 16
		ROL tmp
		DECB				; next iteration
		BNE h2n_loop
	ORAA tmp+1			; combine with older value
	STAA tmp+1
	CLC					; all done without error
h2n_rts:
	RTS					; usual exit
h2n_err:
	SEC					; notify error!
	RTS

; ** end of inline library **

; * get input line from device at fixed-address buffer *
; new from API!
getLine:
	LDX #buffer			; get buffer address
	STX str_pt			; set parameter
	LDAA #BUFSIZ-1		; max index
	STAA ln_siz			; set value
	LDAB iodev			; use device
	_KERNEL(READLN)		; get line
	RTS					; and all done!

; * get clean character from buffer in A, pointer at X (stored in cursor) *
getNextChar:
	LDX cursor			; retrieve index
gnc_do:
	INX					; advance!
	LDAA 0,X			; get raw character
		BEQ gn_ok			; go away if ended
	CMPA #' '			; white space?
		BEQ gnc_do			; skip it!
	CMPA #'$'			; ignored radix?
		BEQ gnc_do			; skip it!
	CMPA #'a'			; not lowercase?
		BCC gn_ok			; all done! ***6800???
	CMPA #'z'+1			; still within lowercase?
		BCS gn_ok			; otherwise do not correct! ***6800???
	ANDA #%11011111		; remove bit 5 to uppercase
gn_ok:
	STX cursor			; eeeeeeeeeeeeeeeeeeeeek
	RTS

; * back off one character, skipping whitespace, use instead of DEC cursor! *
backChar:
	LDX cursor			; get current position
bc_loop:
		DEX					; back once
		LDAA 0,X			; check what is pointed
		CMPA #' '			; blank?
			BEQ bc_loop			; once more
		CMPA #TABU			; tabulation? **** 6800-savvy
			BEQ bc_loop			; ignore
		CMPA #'$'			; ignored radix?
			BEQ bc_loop			; also ignore
	STX cursor				; otherwise we are done
	RTS

; * fetch one byte from buffer, value in A and @value.b *6502
; newest approach as interface for fetch_value
fetch_byte:
	BSR fetch_value		; get whatever
	LDAA tmp2			; how many bytes will fit?
	INCA				; round up chars...
	LSRA				; ...and convert to bytes
	CMPA #1				; strictly one?
	BRA ft_check		; common check

; * fetch two bytes from hex input buffer, value @tmp *
fetch_word:
; another approach using fetch_value
	BSR fetch_value		; get whatever
	LDAA tmp2			; how many bytes will fit?
	INCA				; round up chars...
	LSRA				; ...and convert to bytes
	CMPA #2				; strictly two?
; common fetch error check
ft_check:
	BNE ft_err
		CLC					; if so, all OK
		LDAA tmp+1			; convenient!!! LSB in A
		RTS
; common fetch error discard routine
ft_err:
	LDAB tmp2			; check how many chars were processed eeeeeeek
	BEQ ft_clean		; nothing to discard eeeeeeeeek
ft_disc:
		BSR backChar		; should discard previous char!
		DECB				; one less to go
		BNE ft_disc			; continue until all was discarded
	CLR tmp2			; just in case!
ft_clean:
	SEC					; there was an error
	RTS

; * fetch typed value, no matter the number of chars *
fetch_value:
	LDX #0				; worth doing...
	STX tmp				; ...to clear full result
	STX tmp2			; no chars processed yet
; could check here for symbolic references...
ftv_loop:
		JSR getNextChar		; go to operand first cipher!
		JSR hex2nib			; process one char
			BCS ftv_bad			; no more valid chars
		INC tmp2			; otherwise count one
		BNE ftv_loop		; until no more valid, no real need for BRA
ftv_bad:
	BSR backChar		; should discard very last char! eeeeeeeek
	CLC					; always check temp=0 for errors!
	RTS

; *** pointers to command routines (? to X) ***
cmd_ptr:
#ifdef	SAFE
	FDB		help		; .?
	FDB			_unrecognised	; .@
#endif
	FDB		set_A		; .A
	FDB		set_B		; .B
	FDB		call_address	; .C
	FDB			_unrecognised	; .D
	FDB		examine		; .E
	FDB			_unrecognised	; .F
	FDB		set_SP		; .G
	FDB			_unrecognised	; .H
	FDB			_unrecognised	; .I
	FDB		jump_address	; .J
	FDB		ext_bytes	; .K
	FDB			_unrecognised	; .L
	FDB		move		; .M
	FDB		set_count	; .N
	FDB		origin		; .O
	FDB		set_PSR		; .P
	FDB		quit		; .Q
	FDB		reboot		; .R
	FDB		store_str	; .S
	FDB		store_byte	; .T
	FDB		set_lines	; .U
	FDB		view_regs	; .V
	FDB		store_word	; .W
	FDB		set_X		; .X


; *** strings and other data ***
#ifdef	NOHEAD
montitle:
	FCC		"monitor", 0
#endif

mon_splash:
	FCC		"minimOS·63 monitor"
	FCB		CR
	FCC		"v0.6, (c) 2017"
	FCB		CR
	FCC		"Carlos Santisteban"
	FCB		CR
	FCB		0

err_bad:
	FCC		"** Bad command **"
	FCB		CR
	FCB		0

regs_head:
#ifdef	NARROW
	FCC	"A,B, X,   S,  HINZVC"
#else
	FCC	"A, B, X,   S,   HINZVC"
	FCB		CR
#endif
	FCB		0

dump_in:
#ifdef	NARROW
	FCC		"["			; for 20-char version
#else
	FCC		" ["
#endif
	FCB		0

dump_out:
	FCC		"] "
	FCB		0

set_str:
	FCC		"-> $"
	FCB		0

#ifdef	SAFE
; I/O error message stripped on non-SAFE version
io_err:
	FCC		"*** I/O error ***"
	FCB		CR
	FCB		0


; online help only available under the SAFE option!
help_str:
	FCC 	"---Command list---"
	FCB		CR
	FCC 	"? = show this list"
	FCB		CR
	FCC 	"Ad = set A reg."
	FCB		CR
	FCC 	"Bd = set B reg."
	FCB		CR
	FCC 	"C* = call subroutine"
	FCB		CR
	FCC 	"E* = dump 'u' lines"
	FCB		CR
	FCC 	"Gd = set SP reg."
	FCB		CR
	FCC 	"J* = jump to address"
	FCB		CR
	FCC 	"Kcd=load/save n byt."
	FCB		CR
	FCC 	"   from/to device #d"
	FCB		CR
	FCC 	"Ma =copy n byt. to a"
	FCB		CR
	FCC 	"N* = set 'n' value"
	FCB		CR
	FCC 	"O* = set origin"
	FCB		CR
	FCC 	"Pd = set Status reg"
	FCB		CR
	FCC 	"Q = quit"
	FCB		CR
	FCC 	"Rx = reboot/poweroff"
	FCB		CR
	FCC 	"Ss = put raw string"
	FCB		CR
	FCC 	"Td = store byte"
	FCB		CR
	FCC 	"Ud = set 'u' lines"
	FCB		CR
	FCC 	"V = view registers"
	FCB		CR
	FCC 	"Wa = store word"
	FCB		CR
	FCC 	"Xa = set X reg."
	FCB		CR
	FCC 	"--- values ---"
	FCB		CR
	FCC 	"d => 2 hex char."
	FCB		CR
	FCC 	"a => 4 hex char."
	FCB		CR
	FCC 	"* => up to 4 char."
	FCB		CR
	FCC 	"s => raw string"
	FCB		CR
	FCC 	"c = +(load)/ -(save)"
	FCB		CR
	FCC 	"x=Cold/Warm/Shutdown"
	FCB		CR

#endif
	FCB		0

mon_end:				; for size computation
