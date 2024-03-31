; Hitachi 6301 ROM extractor
; (c) 2024 Carlos J. Santisteban
; last modified 20210331-0013

; *** usual definitions ***
hd1ddr	EQU		$0
hd2ddr	EQU		$1
hd1ior	EQU		$2
hd2ior	EQU		$3

hd_ram	EQU		$14

	ORG $B000			; 4 kiB ROM
; *** code start ***
reset:
	SEI
	LDS #$FF			; reset stack pointer
	JSR config
	CLR hd1ior
	CLR hd2ior			; turn off LEDs quickly
;
stop:
	BRA stop			; lock for now, waiting for NMI

; *** useful routines ***
config:
	LDAA #$FF			; all outputs for port 1 (data LEDs)
	LDAB #$08			; P23 is the only output on port 2 (strobe LED)
	STAA hd1ior
	STAA hd2ior			; set all output bits for a while
	STAA hd1ddr
	STAB hd2ddr			; single output bit on Port 2
	LDX #10000			; about 100 ms delay
;	JSR delay
;	RTS
delay:
		JMP there		; (3+3) *** approx. 10 µs times X with 4.43 MHz XTAL ***
here:
		NOP				; (1)
		DEX				; (1)
		BNE delay		; (3)
	RTS

there:
	JMP here			; (3)

; *** interrupt handlers ***
swi_hndl:
nmi:
; infinite loop making all data LEDs blink!
	JSR config			; restore ports, just in case
	LDX #0				; ~.24s cycle with 4.43 MHz crystal
;	LDAA #$FF			; all LEDs on initially
	CLR hd2ior			; turn off strobe LED
;	STAA hd1ior			; data LEDs on port 1
lock:
			DEX
			BNE lock
		NEGA			; invert all bits
		STAA hd1ior
		BRA lock		; forever!
irq:
	RTI					; do nothing so far

end:
	FILL	$FF, $BFF8-end		; padding
; *** 6800 ROM vectors *** note remapping of a 27C32, mirrored into 16 kiB block
	ORG	$BFF8			; just in case

	FDB	irq				; IRQ @ $FFF8
	FDB	swi_hndl		; SWI @ $FFFA
	FDB	nmi				; NMI @ $FFFC
	FDB	reset			; RES @ $FFFE
