; Monitor shell for minimOS-63 (simple version)
; v0.6a2
; last modified 20170615-1013
; (c) 2017 Carlos J. Santisteban

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
	FILL	$FF, $100*((* & $FF) <> 0) - (* & $FF), $FF	; page alignment!!! eeeeek
mon_head:
; *** header identification ***
	FCB	0
	FCC	"m"				; minimOS app!
	FCB	CPU_TYPE
	FCC	"****"			; some flags TBD
	FCB	CR

; *** filename and optional comment ***
montitle:
	FCC	"monitor"		; file name (mandatory)
	FCB	0
	FCC	"for MC6800 and later!"	; comment
	FCB	0

; advance to end of header
	FILL	$FF, mon_head + $F8 - *, $FF	; for ready-to-blow ROM, advance to time/date field

; *** date & time in MS-DOS format at byte 248 ($F8) ***
	FDB	$6800			; time, 13.00
	FDB	$4ABF			; date, 2017/5/31

monSize	EQU	mon_end - mon_head - 256	; compute size NOT including header!

; filesize in top 32 bits NOT including header, new 20161216
	FDB	monSize			; filesize
	FDB	0				; 64K space does not use upper 16-bit
#endif
; ##### end of minimOS executable header #####

; *** declare zeropage variables ***
; ##### uz_top is last available zeropage byte, z_used goes next #####
ptr		EQU z_used-1	; current address pointer
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
tmp2	EQU tmp-2		; for hex dumps
iodev	EQU tmp2-1		; standard I/O ##### minimOS specific #####

; *** initialise the monitor ***

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
; these ought to be initialised after calling a routine!
	LDAA #z_used-iodev	; zeropage space needed
	STAA z_used			; set needed ZP space as required by minimOS ####
; global variables
	LDAA #4				; standard number of lines
	STAA lines			; set variable
	STAA siz+1			; also default transfer size
	CLR siz				; clear copy/transfer size MSB

; *** begin things ***
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
; **************6502*************6502
		TAY					; just in case...
			BEQ main_loop		; ignore blank lines!
		STX cursor			; save cursor!
		CMP #'X'+1			; past last command?
			BCS bad_cmd			; unrecognised
#ifdef	SAFE
		SBC #'?'-1			; first available command (had borrow)
#else
		SBC #'A'-1			; first available command (had borrow)
#endif
			BCC bad_cmd			; cannot be lower
		ASL					; times two to make it index
		TAX					; use as index
		JSR call_mcmd		; call monitor command
		_BRA main_loop		; continue forever
; *** generic error handling ***
_unrecognised:
	PLA					; discard main loop return address
	PLA
bad_cmd:
	LDA #>err_bad		; address of error message
	LDY #<err_bad
d_error:
	JSR prnStr			; display error
	_BRA main_loop		; continue

; *** call command routine ***
call_mcmd:
	_JMPX(cmd_ptr)		; indexed jump macro, supposedly from bank 0 only!

; *** command routines, named as per pointer table ***

; ** .A = set accumulator **
set_A:
	JSR fetch_byte		; get operand in A
	STA _a				; set accumulator
	RTS

; ** .B = store byte **
store_byte:
	JSR fetch_byte		; get operand in A
	_STAY(ptr)			; set byte in memory
	INC ptr				; advance pointer
	BNE sb_end			; all done if no wrap
		INC ptr+1			; increase MSB otherwise
sb_end:
	RTS

; ** .C = call address **
call_address:
	JSR fetch_value		; get operand address
#ifdef	SAFE
	LDA tmp2			; at least one?
		BEQ _unrecognised	; reject zero loudly
#endif
; setting SP upon call makes little sense...
	LDA iodev			; *** must push default device for later ***
	PHA
	JSR do_call			; set regs and jump!
#ifdef	C816
	.xs: .as: SEP #$30	; *** make certain about standard size ***
#endif
; ** should record actual registers here **
	STA _a
	STX _x
	STY _y
	PHP					; get current status
	CLD					; eeeeeeeeeeeeeek
	PLA					; A was already saved
	STA _psr
; hopefully no stack imbalance was caused, otherwise will not resume monitor!
	PLA					; this (eeeeek) will take previously saved default device
	STA iodev			; store device!!!
	PLA					; must discard previous return address, as will reinitialise stuff!
	PLA
	JMP get_sp			; hopefully context is OK, will restore as needed

; ** .J = jump to an address **
jump_address:
	JSR fetch_value		; get operand address
#ifdef	SAFE
	LDA tmp2			; at least one?
		BEQ _unrecognised	; reject zero loudly
;	BNE jp_ok
;		JMP _unrecognised	; reject zero loudly
;jp_ok:
#endif
; restore stack pointer...
#ifdef	C816
	.xl: REP #$10		; *** essential 16-bit index ***
#endif
	LDX _sp				; get stored value (word)
	TXS					; set new pointer...
; SP restored
; restore registers and jump
do_call:
#ifdef	C816
	.xs: .as: SEP #$30	; *** make certain about standard size ***
#endif
	LDX _x				; retrieve registers
	LDY _y
	LDA _psr			; status is different
	PHA					; will be set via PLP
	LDA _a				; lastly retrieve accumulator
	PLP					; restore status
	JMP (tmp)			; go! might return somewhere else

; ** .E = examine 'u' lines of memory **
examine:
	JSR fetch_value		; get address
	LDY tmp				; save tmp elsewhere
	LDA tmp+1
	STY tmp2
	STA tmp2+1
	LDX lines			; get counter
ex_l:
		_PHX				; save counters
		LDA tmp2+1			; address MSB
		JSR prnHex			; print it
		LDA tmp2			; same for LSB
		JSR prnHex
		LDA #>dump_in		; address of separator
		LDY #<dump_in
		JSR prnStr			; print it
		; loop for 4/8 hex bytes
		LDY #0				; reset offset
ex_h:
			_PHY				; save offset
; space only when wider than 20 char AND if not the first
#ifndef	NARROW
			BEQ ex_ns			; no space if the first one
				_PHY				; please keep Y!
				LDA #' '			; print space, not in 20-char
				JSR prnChar
				_PLY				; retrieve Y!
ex_ns:
#endif
			LDA (tmp2), Y		; get byte
			JSR prnHex			; print it in hex
			_PLY				; retrieve index
			INY					; next byte
			CPY #PERLINE		; bytes per line (8 if not 20-char)
			BNE ex_h			; continue line
		LDA #>dump_out		; address of separator
		LDY #<dump_out
		JSR prnStr			; print it
		; loop for 4/8 ASCII
		LDY #0				; reset offset
ex_a:
			_PHY				; save offset BEFORE!
			LDA (tmp2), Y		; get byte
			CMP #127			; check whether printable
				BCS ex_np
			CMP #' '
				BCC ex_np
			_BRA ex_pr			; it is printable
ex_np:
				LDA #'.'			; substitute
ex_pr:		JSR prnChar			; print it
			_PLY				; retrieve index
			INY					; next byte
			CPY #PERLINE		; bytes per line (8 if not 20-char)
			BNE ex_a			; continue line
		LDA #CR				; print newline
		JSR prnChar
		LDA tmp2			; get pointer LSB
		CLC
		ADC #PERLINE		; add shown bytes (8 if not 20-char)
		STA tmp2			; update pointer
		BCC ex_npb			; skip if within same page
			INC tmp2+1			; next page
ex_npb:
		_PLX				; retrieve counter!!!!
		DEX					; one line less
		BNE ex_l			; continue until done
	RTS

; ** .G = set stack pointer **
set_SP:
	JSR fetch_byte		; get operand in A
	STA _sp				; set stack pointer (LSB only)
	RTS

; ** .? = show commands **
#ifdef	SAFE
help:
	LDA #>help_str		; help string
	LDY #<help_str
	JMP prnStr			; print it, and return to main loop
#endif

; ** .K = keep (load or save) **
; ### highly system dependent ###
; placeholder will send (-) or read (+) raw data to/from indicated I/O device
ext_bytes:
; *** set labels from miniMoDA ***
count	= tmp2+1		; subcommand
temp	= cursor		; will store I/O channel
oper	= tmp			; 16-bit counter
; try to get subcommand, then device
	JSR getNextChar		; check for subcommand
	TAY					; already at the end?
	BNE ex_noni			; not yet
ex_abort:
		JMP _unrecognised			; fail loudly otherwise
ex_noni:
; there is subcommand, let us check target device ###placeholder
	STA count			; first save the subcommand!
	JSR fetch_byte		; read desired device
		BCS ex_abort		; could not get it
	STA temp			; set as I/O channel
	LDY #0				; reset counter!
	STY oper			; also reset forward counter, decrement is too clumsy!
	STY oper+1
; check subcommand
#ifdef	SAFE
	LDA count			; restore subcommand
	CMP #'+'			; is it load?
		BEQ ex_do			; OK then
	CMP #'-'			; is it save? (MARATHON MAN)
		BNE ex_abort		; if not, complain
#endif
ex_do:
; decide what to do
	LDA count			; restore subcommand
	CMP #'+'			; is it load?
	BEQ ex_load			; OK then
; otherwise assume save!
; save raw bytes
		LDA (ptr), Y		; get source data
		STA io_c			; set parameter
		_PHY				; save index
		LDY temp			; get target device
		_KERNEL(COUT)		; send raw byte!
		_PLY				; restore index eeeeeeeeeek
			BCS ex_err			; aborted!
		BCC ex_next			; otherwise continue, no need for BRA
; load raw bytes
ex_load:
		_PHY				; save index
		LDY temp			; get target device
		_KERNEL(CIN)		; get raw byte!
		_PLY				; restore index
			BCS ex_err			; aborted!
		LDA io_c			; get parameter
		STA (ptr), Y		; write destination data
; go for next byte in any case
ex_next:
		INY					; go for next
		BNE ex_nw			; no wrap
			INC ptr+1
ex_nw:
; 16-bit counter INcrement
		INC oper			; one more
		BNE ex_sinc			; no wrap
			INC oper+1
ex_sinc:
; have we finished yet?
		LDA oper			; check LSB
		CMP siz				; compare against desired size
		BNE ex_do			; continue until done
			LDA oper+1			; check MSB, just in case
			CMP siz+1			; against size
		BNE ex_do			; continue until done
ex_ok:
; update PC LSB!
	TYA					; current offset
	CLC
	ADC ptr				; add base LSB
	STA ptr				; update
	BCC ex_show			; no wrap
		INC ptr+1			; or carry to MSB
ex_show:
; transfer ended, show results
#ifndef	SAFE
ex_err:					; without I/O error message, indicate 0 bytes transferred
#endif
	LDA oper			; get LSB
	PHA					; into stack
	LDA oper+1			; get MSB
	PHA					; same
	JMP nu_end			; and print it! eeeeeek return also
#ifdef	SAFE
ex_err:
; an I/O error occurred during transfer!
	LDA #>io_err		; set message pointer
	LDY #<io_err
	JSR prnStr			; print it and finish function afterwards
	_BRA ex_show		; there is nothing to increment!
#endif

; ** .M = move (copy) 'n' bytes of memory **
move:
; preliminary version goes forward only, modifies ptr.MSB and X!

	JSR fetch_value		; get operand word
#ifdef	SAFE
	LDA tmp2			; at least one?
	BNE mv_ok
		JMP _unrecognised	; reject zero loudly
mv_ok:
#endif
	LDY #0				; reset offset
	LDX siz+1			; check n MSB
		BEQ mv_l			; go to second stage if zero
mv_hl:
		LDA (ptr), Y		; get source byte
		STA (tmp), Y		; copy at destination
		INY					; next byte
		BNE mv_hl			; until a page is done
	INC ptr+1			; next page
	INC tmp+1
	DEX					; one less to go
		BNE mv_hl			; stay in first stage until the last page
	LDA siz				; check LSB
		BEQ mv_end			; nothing to copy!
mv_l:
		LDA (ptr), Y		; get source byte
		STA (tmp), Y		; copy at destination
		INY					; next byte
		CPY siz				; compare with LSB
		BNE mv_l			; continue until done
mv_end:
	RTS

; ** .N = set 'n' value **
set_count:
	JSR fetch_value		; get operand word
	LDA tmp				; copy LSB
#ifdef	SAFE
	BNE sc_ok			; not zero is OK
		LDX tmp+1			; check MSB otherwise
		BEQ sc_z			; there was nothing!
sc_ok:
#endif
	STA siz				; into destination variable
sc_z:
	PHA					; for common ending!
	LDA tmp+1			; and MSB
	STA siz+1
	PHA					; to be displayed right now...

; *** common ending for .N and .U ***
; 16-bit REVERSED value on stack!
nu_end:
	LDA #>set_str		; pointer to rest of message
	LDY #<set_str
	JSR prnStr			; print that
	PLA					; check current or updated value MSB
	JSR prnHex			; show in hex
	PLA					; same for LSB
	JSR prnHex			; show in hex
	JMP po_cr			; print trailing newline and return!

; ** .O = set origin **
origin:
	JSR fetch_value		; get operand word
	LDY tmp				; copy LSB
	LDA tmp+1			; and MSB
	STY ptr				; into destination variable
	STA ptr+1
	RTS

; ** .P = set status register **
set_PSR:
	JSR fetch_byte		; get operand in A
	ORA #$30			; *** let X & M set on 65816 ***
	STA _psr			; set status
	RTS

; ** .Q = standard quit **
quit:
; will not check any pending issues
	PLA					; discard main loop return address
	PLA
	_FINISH				; exit to minimOS, proper error code

; ** .S = store raw string **
store_str:
		INC cursor			; skip the S and increase, not INY
		LDY cursor			; allows NMOS macro!
		LDA buffer, Y		; get raw character
		_STAY(ptr)			; store in place, STAX will not work
#ifdef	NMOS
		TAY					; update flags altered by macro!
#endif
			BEQ sstr_end		; until terminator, will be stored anyway
		CMP #CR				; newline also accepted, just in case
			BEQ sstr_cr			; terminate and exit
		INC ptr				; advance destination
		BNE store_str		; boundary not crossed
	INC ptr+1			; next page otherwise
	BNE store_str		; continue, no real need for BRA
sstr_cr:
	_STZA buffer, X		; terminate string
sstr_end:
	RTS

; ** .U = set 'u' number of lines/instructions **
set_lines:
	JSR fetch_byte		; get operand in A
#ifdef	SAFE
	TAX					; check value
		BEQ sl_z			; nothing to set
#endif
	STA lines			; set number of lines
sl_z:
	PHA					; to be displayed
	LDA #0				; no MSB
	PHA
	_BRA nu_end

; ** .V = view register values **
view_regs:
	LDA #>regs_head		; print header
	LDY #<regs_head
	JSR prnStr
; since _pc and ptr are the same, no need to print it!
	LDX #0				; reset counter
vr_l:
		_PHX				; save index!
		LDA _a, X			; get value from regs
		JSR prnHex			; show value in hex
; without PC being shown, narrow displays will also put regular spacing
		LDA #' '			; space, not for 20-char
		JSR prnChar			; print it
		_PLX				; restore index
		INX					; next reg
		CPX #4				; all regs done?
		BNE vr_l			; continue otherwise
	LDX #8				; number of bits
	STX tmp				; temp counter
	LDA _psr			; copy original value
	STA tmp+1			; temp storage
vr_sb:
		ASL tmp+1			; get highest bit
		LDA #' '			; default is off (space)
		BCC vr_off			; was off
			_INC				; otherwise turns into '!'
vr_off:
		JSR prnChar			; prints bit
		DEC tmp				; one less
		BNE vr_sb			; until done
po_cr:
	LDA #CR				; print newline
	JMP prnChar			; will return

; ** .W = store word **
store_word:
	JSR fetch_value		; get operand word
	LDA tmp				; get LSB
	_STAY(ptr)			; store in memory
	INC ptr				; next byte
	BNE sw_nw			; no wrap
		INC ptr+1			; otherwise increment pointer MSB
sw_nw:
	LDA tmp+1			; same for MSB
	_STAY(ptr)
	INC ptr				; next byte
	BNE sw_end			; no wrap
		INC ptr+1			; otherwise increment pointer MSB
sw_end:
	RTS

; ** .X = set X register **
set_X:
	JSR fetch_word		; get operand in X
	STX _x				; set register
	RTS

; ** .R = reboot or shutdown **
reboot:
	JSR getNextChar		; is there an extra character?
	TAX					; check whether end of buffer
		BEQ rb_exit			; no interactive option
	CMP #'W'			; asking for warm boot?
	BNE rb_notw
		LDY #PW_WARM		; warm boot request ## minimOS specific ##
		_BRA fw_shut		; call firmware, could use BNE?
rb_notw:
	CMP #'C'			; asking for cold boot?
	BNE rb_notc
		LDY #PW_COLD		; cold boot request ## minimOS specific ##
		_BRA fw_shut		; call firmware, could use BNE?
rb_notc:
	CMP #'S'			; asking for shutdown?
	BNE rb_exit			; otherwise abort quietly
		LDY #PW_OFF			; poweroff request ## minimOS specific ##
fw_shut:
		_KERNEL(SHUTDOWN)	; unified firmware call
rb_exit:
	RTS					; needs to return and wait for the complete shutdown!


; *** useful routines ***
; ** basic output and hexadecimal handling **

; might include this library when a portable, properly interfaced one is available!
;#include "libs/hexio.s"
; in the meanwhile, it takes these subroutines

; * print a byte in B as two hex ciphers *6502*********
prnHex:
	STAB tmp2			; keep whole value
	LSRB				; shift right four times (just the MSB)
	LSRB
	LSRB
	LSRB
	JSR ph_b2a			; convert and print this cipher
	LDAB tmp2			; retrieve full value
	ANDB #$0F			; keep just the LSB... and repeat procedure
ph_b2a:
	CMPB #10			; will be a letter?
	BCC ph_n			; just a number
		ADCB #6				; convert to letter (plus carry)
ph_n:
	ADCB #'0'			; convert to ASCII (carry is clear)
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

; * convert two hex ciphers into byte@tmp, A is current char, X is cursor * 6502*6502*6502******
; * new approach for hex conversion *
; * add one nibble from hex in current char!
; A is current char, returns result in value[0...1]
; does NOT advance any cursor (neither reads char from nowhere)
; MUST reset value previously!
hex2nib:
	SEC					; prepare for subtract
	SBC #'0'			; convert from ASCII
		BCC h2n_err			; below number!
	CMP #10				; already OK?
	BCC h2n_num			; do not convert from letter
		CMP #23				; otherwise should be a valid hex
			BCS h2n_rts			; or not! exits with C set
		SBC #6				; convert from hex (C is clear!)
h2n_num:
	LDY #4				; shifts counter, no longer X in order to save some pushing!
h2n_loop:
		ASL tmp				; current value will be times 16
		ROL tmp+1
		DEY					; next iteration
		BNE h2n_loop
	ORA tmp				; combine with older value
	STA tmp
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
	LDAA 0, X			; get raw character
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
		LDAA 0, X			; check what is pointed
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
	JSR fetch_value		; get whatever
	LDAA tmp2			; how many bytes will fit?
	INCA				; round up chars...
	LSRA				; ...and convert to bytes
	CMPA #1				; strictly one?
	BRA ft_check		; common check

; * fetch two bytes from hex input buffer, value @tmp *
fetch_word:
; another approach using fetch_value
	JSR fetch_value		; get whatever
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
		JSR backChar		; should discard previous char!
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
	JSR backChar		; should discard very last char! eeeeeeeek
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
	FCC	"A:B: X:   S:  HINZVC"
#else
	FCC	"A: B: X:   S:   --HINZVC"
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
