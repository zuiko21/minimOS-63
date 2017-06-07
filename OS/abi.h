; minimOS·63 0.6a2 API/ABI
; *** for Motorola 6800/6802/6808 & derived microcontrollers ***
; (c) 2017 Carlos J. Santisteban
; last modified 20170607-1644

; *************************************************
; *************************************************
; ***** kernel function codes for system call *****
; *************************************************
; *************************************************

; basic I/O
COUT		=   0	; character output
CIN			=   3	; character input
STRING		=   6	; output a C-string
READLN		=   9	; read input into supplied buffer

; basic windowing system
OPEN_W		=  12	; open window or get I/O devices
CLOSE_W		=  15	; close a window or release device and its buffer
FREE_W		=  18	; release a window but let it on screen, keeping its buffer, may be closed by kernel

; other generic functions
UPTIME		=  21	; give uptime in ticks and seconds
SET_FG		=  24	; set PB7 frequency generator ***firmware implementation
SHUTDOWN	=  27	; proper shutdown, with or without power-off ***might be used for NMI/SWI invocation
LOAD_LINK	=  30	; get an executable from its path, and get it loaded into primary memory, maybe relocated

; mainly for multitasking use, but also for simplified single task management
B_FORK		=  33	; reserve a free braid
B_EXEC		=  36	; get code at some address running into a previously reserved braid
B_SIGNAL	=  39	; send UNIX_like signal to a braid
B_STATUS	=  42	; get execution flags of a braid
GET_PID		=  45	; get current braid PID
SET_HNDL	=  48	; set SIGTERM handler
B_YIELD		=  51	; give away CPU time, not really needed but interesting anyway

; some new functionalities, perhaps OK with LOWRAM systems
AQ_MANAGE	=  54	; get asyncronous task status, or enable/disable it!
PQ_MANAGE	=  57	; get periodic task status, enable/disable it or set frequency!

; not for LOWRAM systems
MALLOC		=  60	; allocate memory
MEMLOCK		=  63	; allocate memory at a certain address, new 20170524
FREE		=  66	; release memory block
RELEASE		=  69	; release ALL memory blocks belonging to some PID, new 20161115
TS_INFO		=  72	; get taskswitching info for multitasking driver
SET_CURR	=  75	; set internal kernel info for running task (PID & architecture) new 20170222


; ***********************
; ***********************
; ***** error codes *****
; ***********************
; ***********************
END_OK	=   0		; not needed on 65xx, CLC instead
UNAVAIL	=   1		; unavailable on this version
TIMEOUT	=   2		; try later
FULL	=   3		; not enough memory, try less
N_FOUND	=   4		; try another
NO_RSRC	=   5		; no resource, try a different way
EMPTY	=   6		; put some and retry
INVALID	=   7		; invalid argument
BUSY	=   8		; cannot use it now, free it or wait
CORRUPT	=   9		; data corruption

; ************************************
; ************************************
; ** firmware interface calls (TBD) **
; ************************************
; ************************************

; generic functions, esp. interrupt related
GESTALT		=  0	; get system info (renumbered)
SET_ISR		=  3	; set IRQ vector
SET_NMI		=  6	; set (magic preceded) NMI routine
SET_BRK		=  9	; set debugger, new 20170517
JIFFY		= 12	; set jiffy IRQ speed, ** TBD **
IRQ_SOURCE	= 15	; get interrupt source in X for total ISR independence

; pretty hardware specific
POWEROFF	= 18	; power-off, suspend or cold boot
FREQ_GEN	= 21	; frequency generator hardware interface, TBD

; reduced version for LOWRAM systems, otherwise standard and mandatory use
INSTALL		= 24	; copy jump table

; not for LOWRAM systems
PATCH		= 27	; patch single function (renumbered)
CONTEXT		= 30	; context bankswitching

; **************************
; ** Driver table offsets **
; **************************
D_ID	=  0		; driver ID
D_AUTH	=  1		; authorization mask
D_CIN	=  2		; character input code
D_COUT	=  4		; character output code
D_INIT	=  6		; device reset procedure
D_POLL	=  8		; periodic interrupt task
D_FREQ	= 10		; frequency for periodic task, new 20170517
D_REQ	= 12		; asynchronous interrupt task
D_BLIN	= 14		; block input, new names 20150304, also for control purposes
D_BLOUT	= 16		; block output, new names 20150304, also for control purposes
D_BYE	= 18		; shutdown procedure
D_INFO	= 20		; points to a C-string with driver info
D_MEM	= 22		; NEW, required variable space (if relocatable) (WORD)

; ** Driver feature mask values **
A_POLL	= %10000000	; D_POLL routine available
A_REQ	= %01000000	; D_REQ routine available
A_CIN	= %00100000	; D_CIN capability
A_COUT	= %00010000	; D_COUT capability
A_BLIN	= %00001000	; D_BLIN capability
A_BLOUT	= %00000100	; D_BLOUT capability
A_SEC	= %00000010	; *** no longer available, RESERVED ***
A_MEM	= %00000001	; D_MEM dynamically linked, on-the-fly loadable driver

; ** VIA 65(C)22 registers, just for convenience **
; offsets from base address (add to base in options.h)
IORB	= $0
IORA	= $1
DDRB	= $2
DDRA	= $3
T1CL	= $4
T1CH	= $5
T1LL	= $6
T1LH	= $7
T2CL	= $8
T2CH	= $9
SR		= $A
ACR		= $B
PCR		= $C
IFR		= $D
IER		= $E
NHRA	= $F		; IRA/ORA without handshake

; **********************************
; ** values for RAM chunck status **
; **********************************
; make certain FREE_RAM is zero for easier scan (BEQ)
; even numbers just in case indexed jump is used
FREE_RAM	=	0
USED_RAM	=	2
END_RAM		=	4	; new label 20161103
LOCK_RAM	=	6	; new label 20161117

; some kernel-related definitions
#ifndef	LOWRAM
			MAX_QUEUE	=	16	; maximum interrupt task queue size
			MAX_DRIVERS	=	16	; maximum number of drivers, independent as of 20170207
			MAX_LIST	=	32	; number of available RAM blocks *** might increase this value in 65816 systems!
#else
			MAX_QUEUE	=	6	; much smaller queues in 128-byte systems, note unified jiffy & slow queues!
			MAX_DRIVERS	=	4	; maximum number of drivers, independent as of 20170207
			MAX_LIST	=	0	; no memory management for such systems
#endif

; multitasking subfunctions, no longer needed as will patch regular kernel!

; ** multitasking status values **
BR_FREE		= 192	; free slot, non-executable
BR_RUN		=   0	; active process, may get CPU time, should be zero for quick evaluation
BR_STOP		= 128	; paused process, will not get CPU until resumed
BR_END		=  64	; ended task, waiting for rendez-vous
; might add a fifth state for forked-but-not-yet-loaded braids (in order NOT to start them abnormally)
BR_MASK		= 192	; as it set both bits but NOT those for SIGTERM handler, new 20161117

; ** multitasking signals **
SIGKILL		=  0	; immediately kill braid, will go BR_FREE... maybe after BR_END
SIGTERM		=  2	; ask braid to terminate in an orderly fashion, default handler is SIGKILL
SIGSTOP		=  4	; pause braid, will go BR_STOP
SIGCONT		=  6	; resume a previously paused braid, will go BR_RUN

; MAX_BRAIDS should be system variable, as defined by firmware and/or multitasking driver
; default updateable value = 1 (no multitasking)
; if defined in firmware, think about a gestalt-like function for reading/setting it!
; QUANTUM_COUNT no longer defined here

; ********************************************************
; *** subfunction codes for task queues management TBR ***
; ********************************************************
TQ_STAT		=   0	; read status (and frequency)
TQ_EN		=   2	; enable task
TQ_DIS		=   4	; disable task
TQ_FREQ		=   6	; set task frequency (periodic tasks only)

; *********************************************************
; ** power control values, valid for kernel and firmware **
; *********************************************************
PW_STAT		=  0	; suspend (go static) if available, or no pending action, best if zero
PW_WARM		=  2	; warm reset (needs no firmware) renumbered 150603
PW_COLD		=  4	; cold reset (needed for compatibility with other architectures) renumbered 150603
PW_OFF		=  6	; power off
PW_CLEAN	=  8	; scheduler detected system is clean for poweroff! new 20160408

; *******************************************
; ** optional windowing system values, TBD **
; *******************************************
W_OPEN		=   0	; active window in use
W_CLOSE		= 192	; closed, free window
W_FREE		= 128	; no longer in use, may be closed by kernel itself
W_REQ		=  64	; requested to close, will send SIGTERM to creator braid

; ************************
; ** logic devices, TBD **
; ************************
DEV_RND		= 126	; get a random number
DEV_NULL	= 127	; ignore I/O

; ** physical device numbers, TBD **
; these are likely to become 'logical' port IDs like rs0, rs1, ss0... regardless of actual implementation
DEV_LED		= 252	; LED keypad on VIAport
DEV_LCD		= 210	; Hitachi LCD module, TO_DO
DEV_ACIA	= 236	; ACIA, currently 6551
DEV_SS22	= 250	; SS-22 port
DEV_ASCII	= 241	; ASCII keyboard on VIAport, TO_DO
DEV_DEBUG	= 255	; Bus sniffer, NEW 20150323
DEV_CONIO	= 132	; for Kowalski & run816 simulator, NEW 20160308
DEV_VGA		= 192	; integrated VGA-compatible Tijuana, NEW 20160331

; more temporary IDs

; lcd-4-bit @ascii = 240
; acia 2651 = 237
; VIA PA parallel port = 243
; VDU @ VIAport = 242
; (d)uart-16c550-1 = 232 hehehe
; duart-16c552-0 (or 2) = 224
; rtc 146818 (pseudo-driver?) = 208
; duart 2681-1 = 235 (or 227)
; duart 2681-2 = 227 (or 235)

