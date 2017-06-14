; firmware variables for minimOS·63
; sort-of template, possibly fine for KERAton
; v0.6a1
; (c) 2017 Carlos J. Santisteban
; last modified 20170614-1207

#ifndef	LOWRAM
fw_table	FILL	$FF, 256		; new address 150204, avoid speed penalty, move towards the end otherwise, NOT available in 128-byte systems
#endif
fw_isr		RMB 2				; ISR vector
fw_nmi		RMB 2				; NMI vector, fortunately checks for integrity
fw_brk		RMB 2				; SWI vector, new 20170602
fw_warm		RMB 2				; start of kernel, new 20150220
fw_cpu		FCB 'M'				; CPU type ('M'= 6800)
himem		RMB 1				; number of available 'kernel-RAM' pages, 0 means 128-byte RAM
