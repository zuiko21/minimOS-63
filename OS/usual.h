; usual header includes for minimOS-63
; last modified 20170808-2147
; (c) 2012-2020 Carlos J. Santisteban

#ifndef	HEADERS
; avoid further redefinitions
#define	HEADERS	_HEADERS
; usual support files
#include "options.h"
#include "macros.h"
#include "abi.h"
#include "zeropage.h"
	ORG		SYSRAM				; as defined in options.h
; firmware and system variables
#include ARCH_h
#ifndef	DOWNLOAD
sysvars:
#include "sysvars.h"
; driver-specific system variables, located here 20170207
dr_vars:
#include DRIVER_PACK_h
#endif
; points to the beginning of free SRAM
user_ram:
#ifndef	ROM
; placeholder for standalone assembly
	ORG		ROM_BASE
kernel:
#endif
#endif
