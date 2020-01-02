; firmware variables for minimOS·63
; sort-of template, possibly fine for KERAton
; v0.6a6
; (c) 2017-2020 Carlos J. Santisteban
; last modified 20180219-0929

#ifndef	LOWRAM
fw_table	FILL $FF, API_SIZE	; NOT available in 128-byte systems
#endif
fw_lastk	RMB 2				; address of last installed kernel jump table! new 20180116
fw_isr		RMB 2				; ISR vector
fw_nmi		RMB 2				; NMI vector, fortunately checks for integrity
fw_dbg		RMB 2				; SWI vector, new 20170602
fw_warm		RMB 2				; start of kernel, new 20150220
fw_cpu		FCB 'M'				; CPU type ('M'= 6800)
himem		RMB 1				; number of available 'kernel-RAM' pages, 0 means 128-byte RAM
irq_freq	FDB	150				; IRQs per second... or period?
old_t1		RMB	2				; keep old T1 latch value for FREQ_GEN
