; minimOS·63 firmware module
; (C) 2018-2020 Carlos J. Santisteban
; last modified 20180219-0948

; JIFFY, set jiffy IRQ frequency
;		INPUT
; irq_hz	= frequency in Hz (0 means no change)
;		OUTPUT
; irq_hz	= actually set frequency (in case of no change)
; C			= could not set

-jiffy:
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
