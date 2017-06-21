; includes for minimOS·63 drivers
; KERAton specific configuration! but visual6800 support this far
; v0.6a2
; (c) 2017 Carlos J. Santisteban
; last modified 20170621-1252

#define		DRIVERS		_DRIVERS

; in case of standalone assembly
#include	"../../usual.h"

; *** specific driver code ***
driver0:
#include "../visual6800.s"

; *** boot driver list ***
drivers_ad:
	FDB		driver0		; generic entry
	FDB		0			; terminate list *** essential ***
