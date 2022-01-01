; includes for minimOS·63 drivers
; KERAton specific configuration! but visual6800 support this far
; v0.6a3
; (c) 2017-2022 Carlos J. Santisteban
; last modified 20170808-2229

#define		DRIVERS		_DRIVERS

; in case of standalone assembly
#include	"../../usual.h"

; *** specific driver code ***
driver0:
#include "../visual6800.s"

; *** boot driver list ***
drvrs_ad:
	FDB		driver0		; generic entry
	FDB		0			; terminate list *** essential ***
