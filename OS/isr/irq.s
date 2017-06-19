; ISR for minimOS•63
; v0.6a1, should match kernel.s
; features TBD
; (c) 2017 Carlos J. Santisteban
; last modified 20170619-2034

#define		ISR		_ISR

#include "../usual.h"

; **** the ISR code ****
; registers are saved but keep temp vars!
#ifdef	MC6801
	LDX systmp			; get this word (4)
	PSHX				; easily stored (4)
#else
	LDAA systmp			; get this word (3+3)
	LDAB systmp+1
	PSHB				; store similarly (4+4)
	PSHA
#endif
; *** place here HIGH priority async tasks, if required ***

; check whether from VIA, BRK...
;	_ADMIN(IRQ_SOURCE)		; future, proper way... perhaps offset in X?
	LDAA VIA+IFR			; have a look at bit 6 (4+2)
	ANDA #%01000000
		BNE periodic		; from T1 (4)

; *** async interrupt otherwise ***
; execute D_REQ in drivers, whenever enabled
; *** should be rewritten for new 0.6 functionality ***
	LDAB queues_mx			; get queue size, CHECK OFFSET (4)
		BEQ isr_done			; no drivers to call (4)
	LDX #drv_async			; otherwise get queue start
i_req:
		STX systmp			; keep index! (5+4)
		PSHB
; *** include here disable control in 0.6 ***
; proceed to call
		LDX 0,X				; solve indirect (6)
		JSR 0,X				; call from table (...)
i_nxreq:
		LDX systmp			; restore pointer... (4)
		INX				; ...to next value (4+4)
		INX
		PULB				; restore counter too (4)
			BCC isr_done			; driver satisfied, thus go away
		DECB				; go backwards to be faster! (2)
		BNE i_req			; until zero is done (4)
; go away
isr_done:
second:
#ifdef	MC6801
	PLX				; retrieve word (5)
	STX				; easily restored (4)
#else
	PULA				; retrieve word (4+4)
	PULB
	STAA				; store (4+4)
	STAB
#endif
	RTI

; *** here goes the periodic interrupt code ***
periodic:
	LDAA VIA+T1CL		; acknowledge periodic interrupt!!! (4)

; *** scheduler no longer here, just an optional driver! But could be placed here for maximum performance ***

; execute D_POLL code in drivers
; *** should be rewritten for new 0.6 functionality ***
	LDAB queues_mx+1		; get queue size, CHECK OFFSET (4)
		BEQ ip_done			; no drivers to call (4)
	LDX #drv_poll			; otherwise get queue start
i_poll:
		STX systmp			; keep index! (5+4)
		PSHB
; *** include here disable control in 0.6 ***
; *** and frequency setting ***
; proceed to call
		LDX 0,X				; solve indirect (6)
		JSR 0,X				; call from table (...)
; *** here is the return point needed for B_EXEC in order to create the stack frame ***
isr_sched_ret:				; *** take this standard address!!! ***
		LDX systmp			; restore pointer... (4)
		INX				; ...to next value (4+4)
		INX
		PULB				; restore counter too (4)
		DECB				; go backwards to be faster! (2)
		BNE i_poll			; until zero is done (4)

ip_done:
; update uptime
	INC ticks+1			; increase uptime count (6)
	BNE isr_nw			; did not wrap (4)
		INC ticks			; otherwise carry (6)
isr_nw:
	LDAA ticks+1			; check LSB first (4)
	CMPA irq_freq		; possible end? (4)
		BNE isr_done		; no second completed yet *** revise for load balancing (4)
	LDAA ticks			; go for MSB (4)
	CMPA irq_freq		; second completed? (4)
		BNE isr_done		; no second completed yet *** revise for load balancing (4)
	LDX #0			; otherwise reset values (3+5)
	STX ticks
	INC ticks+5			; one more second (6)
		BNE second			; no wrap (4)
	INC ticks+4			; 256 more second (6)
		BNE second			; no wrap (4)
	INC ticks+3			; 64K more seconds (6)
		BNE second			; no wrap (4)
	INC ticks+2			; 16M more seconds (6)
	BRA isr_done		; go away (3)


