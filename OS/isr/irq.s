; ISR for minimOS•63
; v0.6a3, should match kernel.s
; features TBD
; (c) 2017 Carlos J. Santisteban
; last modified 20170620-2053

#define		ISR		_ISR

#include "../usual.h"

; **** the ISR code ****
; registers are saved but keep temp vars!
#ifdef	MC6801
	LDX systmp			; get this word (4)
	PSHX				; easily stored (4)
;	LDX sys_sp			; get this word (4)///only if MAX_QUEUES>84
;	PSHX				; easily stored (4)///
#else
	LDAA systmp			; get this word (3+3)
	LDAB systmp+1
	PSHB				; store similarly (4+4)
	PSHA
;	LDAA sys_sp			; get this word (3+3)///
;	LDAB sys_sp+1
;	PSHB				; store similarly (4+4)///
;	PSHA
#endif
; *** place here HIGH priority async tasks, if required ***

; check whether from VIA, BRK...
;	_ADMIN(IRQ_SOURCE)		; future, proper way... perhaps offset in X?
	LDAA VIA+IFR			; have a look at bit 6 (4+2) *** NOT for KERAton
	ANDA #%01000000
		BNE periodic		; from T1 (4)

; *** async interrupt otherwise ***
; execute D_REQ in drivers, whenever enabled
	LDAB queues_mx			; get R-queue size (4)
		BEQ isr_done			; no drivers to call (4)
;	LDX #drv_r_en			; otherwise get enable array///
;	STX sys_sp				; temporary storage///
	LDX #drv_async			; and get R-queue start
i_req:
		STX systmp			; keep index! (5+4)
		PSHB
; *** include here disable control in 0.6 ***
;		LDX sys_sp			; pointer to enable entry///
		LDAA MAX_QUEUE,X		; R-entry for this task///or 0 if using sys_sp
		BPL i_nxreq			; temporarily disabled, skip or...
; *** proceed to call ***
;			LDX systmp			; pointer to routine queue///
			LDX 0,X				; solve indirect (6)
			JSR 0,X				; call from table (...)
i_nxreq:
		PULB				; restore counter too (4)
			BCC isr_done			; driver satisfied, thus go away (4)
; *** here comes part from the disable control in 0.6 ***///
;		LDX sys_sp			; advance enable queue too! (4+4+4)///
;		INX
;		INX
;		STX sys_sp			; needs to be restored (5)///
; *** end of 0.6 extras ***
		LDX systmp			; restore pointer... (4)
		INX					; ...to next value (4)
		INX					; will be saved upon next iteration (4)
		DECB				; go backwards to be faster! (2)
		BNE i_req			; until zero is done (4)
; go away
isr_done:
second:
#ifdef	MC6801
;	PLX				; retrieve word (5)///
;	STX sys_sp		; easily restored (4)///
	PLX				; retrieve word (5)
	STX systmp		; easily restored (4)
#else
;	PULA				; retrieve word (4+4)///
;	PULB
;	STAA sys_sp			; store (4+4)///
;	STAB sys_sp+1
	PULA				; retrieve word (4+4)
	PULB
	STAA systmp			; store (4+4)
	STAB systmp+1
#endif
	RTI

; *** here goes the periodic interrupt code ***
periodic:
	LDAA VIA+T1CL		; acknowledge periodic interrupt!!! (4)***NOT for KERAton

; *** scheduler no longer here, just an optional driver! But could be placed here for maximum performance ***

; execute D_POLL code in drivers
; *** should be rewritten for new 0.6 functionality ***
	LDAB queues_mx+1		; get P-queue size (4)
		BEQ ip_done			; no drivers to call (4)
;	LDX #drv_p_en			; otherwise get enable array///
;	STX sys_sp				; temporary storage///
	LDX #drv_poll			; and get P-queue start
i_poll:
		STX systmp			; keep index! (5+4)
		PSHB
; *** include here disable control in 0.6 ***
;		LDX sys_sp			; pointer to enable entry///
		LDAA MAX_QUEUE*3+1,X	; P-entry for this task///or 0 if using sys_sp
			BPL isr_sched_ret	; temporarily disabled, skip or...
; *** and frequency setting ***
;		LDX systmp			; restore queue pointer (4)///
		DEC MAX_QUEUE+1,X	; countdown, note offset (7)
		BNE i_nxpll			; not yet, save some time, X has systmp
			DEC MAX_QUEUE, X	; try MSB, must be set +1!!! (7)
		BNE i_nxpll			; not yet, save some time, X has systmp
			LDAA irq_freq		; otherwise, counter must be reset!
			LDAB irq_freq+1
			INCA				; as required by routine, please take into account on startup!!!
			STAA MAX_QUEUE, X	; update counter
			STAB MAX_QUEUE+1, X
; *** proceed to call ***
			LDX 0,X				; solve indirect (6)
			JSR 0,X				; call from table (...)
; *** here is the return point needed for B_EXEC in order to create the stack frame ***
isr_sched_ret:				; *** take this standard address!!! ***
		LDX systmp			; restore pointer... (4)
i_nxpll:
		INX					; ...to next value (4+4)
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


