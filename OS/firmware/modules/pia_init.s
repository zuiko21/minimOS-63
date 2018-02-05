; minimOS·63 firmware module
; (C) 2017-2018 Carlos J. Santisteban
; last modified 20180205-1046

; *** hardware interrupt setup ***
; no interface needed

; KERAton has no VIA, jiffy IRQ is likely to be fixed on, perhaps enabling counter input on PIA
;	LDAA #$C0			; enable T1 (jiffy) interrupt only
;	STAA VIA_J+IER
