
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
-set_fg:
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

