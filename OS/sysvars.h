; minimOS·63 0.6a1 System Variables
; (c) 2012-2017 Carlos J. Santisteban
; last modified 20170621-1052

; **** I/O management ****
; ** pointer tables for drivers, new order suggested for alternative version **
#ifndef	LOWRAM
drv_opt		RMB 256				; full page of output driver pointers, new direct scheme 160406
drv_ipt		RMB 256				; full page of input driver pointers, new direct scheme 160406
#else
drv_num		FCB 0				; number of installed drivers
drivers_id	RMB MAX_DRIVERS		; space for reasonable number of drivers
#endif

; ** I/O flags and locks **
; mandatory order!!!
#ifndef	LOWRAM
cio_lock	RMB 256				; PID-reserved MUTEX for CIN & COUT, per-phys-driver & interleaved with CIN binary mode flag
cin_mode	EQU cio_lock+1		; interleaved
#else
cin_mode	RMB	1				; only this for low ram systems
#endif

; **** interrupt queues **** new format 20170518
queues_mx	RMB 2				; array with max offset for both Periodic[1] & Async[0] queues
drv_poll	RMB MAX_QUEUE		; space for periodic task pointers
drv_freq	RMB MAX_QUEUE		; array of periodic task frequencies (word?)
drv_async	RMB MAX_QUEUE		; space for async task pointers
drv_r_en	RMB MAX_QUEUE		; interleaved array of async interrupt task flags
drv_p_en	EQU drv_r_en+1		; ditto for periodic tasks (interleaved)

; *** single-task sigterm handler separate again! ***
; multitasking should provide appropriate space!
mm_sterm	RMB 2				; 16-bit pointer
; no longer mm_term et al here!

; **** new memory management table 150209, revamped 161106 ****
#ifndef		LOWRAM
ram_pos		RMB MAX_LIST		; location of blocks, new var 20161103
ram_stat	RMB MAX_LIST		; status of each block, non interleaved
ram_pid		RMB MAX_LIST		; non-interleaved PID array
#endif

; *************************************************
; ** these are the older variables, up to 150126 **
; *************************************************
irq_freq	FDB	200		; IRQs per second (originally set from options.h)
ticks		RMB	6		; second fraction in jiffy IRQs, then approximate uptime in seconds (2+4 bytes) new format 161006
sd_flag		RMB	1		; default task upon no remaining braids! 160408
#ifndef	LOWRAM
default_in	RMB	1		; GLOBAL default devices, EXCEPT for LOWRAM systems
default_out	RMB	1
; no way for multitasking in LOWRAM systems
run_pid		FCB	0		; current PID running for easy kernel access, will be set by new SET_CURR
#else
default_in	EQU std_in	; in LOWRAM systems, both global and local standard devices are the same!
default_out	EQU stdout
#endif
old_t1		RMB	2		; *** keep old T1 latch value for FG, revised 150208 *** might be revised or moved to firmware vars!

; ** driver-specific system variables come after this one, in main source **
