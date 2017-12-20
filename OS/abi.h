; minimOS·63 0.6a9 API/ABI
; *** for Motorola 6800/6802/6808 & derived microcontrollers ***
; (c) 2017 Carlos J. Santisteban
; last modified 20171220-1437
; MASM compliant 20170614, 8-char max names 20170805

; *************************************************
; *************************************************
; ***** kernel function codes for system call *****
; *************************************************
; *************************************************

; basic I/O
COUT		EQU	0		; character output
CIN			EQU	COUT+3	; character input
STRING		EQU	CIN+3		; output a C-string
READLN		EQU	STRING+3	; read input into supplied buffer

; new block I/O
BLOUT		EQU	READLN+3	; block output
BLIN		EQU	BLOUT+3		; block input
BL_CNFG		EQU	BLIN+3		; device configuration
BL_STAT		EQU	BL_CNFG+3	; device status

; basic windowing system
OPEN_W		EQU	BL_STAT+3	; open window or get I/O devices
CLOSE_W		EQU	OPEN_W+3	; close a window or release device and its buffer
FREE_W		EQU	CLOSE_W+3	; release a window but let it on screen, keeping its buffer, may be closed by kernel

; other generic functions
UPTIME		EQU	FREE_W+3	; give uptime in ticks and seconds
SET_FG		EQU	UPTIME+3	; set frequency generator ***firmware implementation
SHUTDOWN	EQU	SET_FG+3	; proper shutdown, with or without power-off ***might be used for NMI/SWI invocation
LOADLINK	EQU	SHUTDOWN+3	; get an executable from its path, and get it loaded into primary memory, maybe relocated

; mainly for multitasking use, but also for simplified single task management
B_FORK		EQU	LOADLINK+3	; reserve a free braid
B_EXEC		EQU	B_FORK+3	; get code at some address running into a previously reserved braid
B_SIGNAL	EQU	B_EXEC+3	; send UNIX_like signal to a braid
B_FLAGS		EQU	B_SIGNAL+3	; get execution flags of a braid
SET_HNDL	EQU	B_FLAGS+3	; set SIGTERM handler
B_YIELD		EQU	SET_HNDL+3	; give away CPU time, not really needed but interesting anyway
GET_PID		EQU	B_YIELD+3	; get current braid PID

; some new driver functionalities, perhaps OK with LOWRAM systems
AQ_MNG		EQU	GET_PID+3	; get asyncronous task status, or enable/disable it!
PQ_MNG		EQU	AQ_MNG+3	; get periodic task status, enable/disable it or set frequency!

; not for LOWRAM systems
; drivers...
DR_INST		EQU	PQ_MNG+3	; install and enable driver
DR_SHUT		EQU	DR_INST+3	; shut down driver
; memory...
MALLOC		EQU	DR_SHUT+3	; allocate memory
MEMLOCK		EQU	MALLOC+3	; allocate memory at a certain address, new 20170524
FREE		EQU	MEMLOCK+3	; release memory block
RELEASE		EQU	FREE+3		; release ALL memory blocks belonging to some PID, new 20161115
; multitasking...
TS_INFO		EQU	RELEASE+3	; get taskswitching info for multitasking driver
SET_CURR	EQU	TS_INFO+3	; set internal kernel info for running task (PID & architecture) new 20170222


; ***********************
; ***********************
; ***** error codes *****
; ***********************
; ***********************
END_OK	EQU   0		; not needed on 65xx/68xx, CLC instead
UNAVAIL	EQU   1		; unavailable on this version
TIMEOUT	EQU   2		; no timely reply, try later
FULL	EQU   3		; not enough memory, try less
N_FOUND	EQU   4		; not found, try another
NO_RSRC	EQU   5		; no resource, try a different way
EMPTY	EQU   6		; put some and retry
INVALID	EQU   7		; invalid argument
BUSY	EQU   8		; cannot use it now, free it or wait
CORRUPT	EQU   9		; data corruption

; ************************************
; ************************************
; ** firmware interface calls (TBD) **
; ************************************
; ************************************

; generic functions, esp. interrupt related
GESTALT		EQU	0		; get system info (renumbered)
SET_ISR		EQU	GESTALT+3	; set IRQ vector
SET_NMI		EQU	SET_ISR+3	; set (magic preceded) NMI routine
SET_DBG		EQU	SET_NMI+3	; set debugger, new 20170517, renamed 20170805
JIFFY		EQU	SET_DBG+3	; set jiffy IRQ speed, ** TBD **
IRQ_SRC		EQU	JIFFY+3		; get interrupt source for total ISR independence

; pretty hardware specific
POWEROFF	EQU	IRQ_SRC+3	; power-off, suspend or cold boot (invoke debuggers?)
FREQ_GEN	EQU	POWEROFF+3	; frequency generator hardware interface, TBD

; reduced version for LOWRAM systems, otherwise standard and mandatory use
INSTALL		EQU	FREQ_GEN+3	; copy jump table

; not for LOWRAM systems
PATCH		EQU	INSTALL+3	; patch single function (renumbered)
CONTEXT		EQU	PATCH+3		; context bankswitching

; **************************
; ** Driver table offsets **
; **************************
D_ID	EQU  0		; driver ID
D_AUTH	EQU  1		; authorization mask
D_BLIN	EQU  2		; BLOCK input code
D_BOUT	EQU  4		; BLOCK output code
D_INIT	EQU  6		; device reset procedure
D_POLL	EQU  8		; periodic interrupt task
D_FREQ	EQU 10		; frequency for periodic task, new 20170517
D_ASYN	EQU 12		; asynchronous interrupt task, renamed 20170830
D_CNFG	EQU 14		; device configuration, new
D_STAT	EQU 16		; device status, new
D_BYE	EQU 18		; shutdown procedure
D_INFO	EQU 20		; points to a C-string with driver info
D_MEM	EQU 22		; NEW, required variable space (if relocatable) (WORD)

; ** Driver feature mask values **
A_POLL	EQU %10000000	; D_POLL routine available
A_REQ	EQU %01000000	; D_REQ routine available
A_BLIN	EQU %00100000	; D_BLIN capability
A_BOUT	EQU %00010000	; D_BOUT capability
A_CNFG	EQU %00001000	; D_CNFG capability
A_STAT	EQU %00000100	; D_STAT capability
A_RSVD	EQU %00000010	; *** no longer available, RESERVED ***
A_MEM	EQU %00000001	; D_MEM dynamically linked, on-the-fly loadable driver

; ** VIA 65(C)22 registers, just for convenience **
; offsets from base address (add to base in options.h)
IORB	EQU $0
IORA	EQU $1
DDRB	EQU $2
DDRA	EQU $3
T1CL	EQU $4
T1CH	EQU $5
T1LL	EQU $6
T1LH	EQU $7
T2CL	EQU $8
T2CH	EQU $9
VSR		EQU $A	; renamed 20170805
ACR		EQU $B
PCR		EQU $C
IFR		EQU $D
IER		EQU $E
NHRA	EQU $F		; IRA/ORA without handshake

; **********************************
; ** values for RAM chunck status **
; **********************************
; make certain FREE_RAM is zero for easier scan (BEQ)
; even numbers just in case indexed jump is used
FREE_RAM	EQU	0
USED_RAM	EQU	2
END_RAM		EQU	4	; new label 20161103
LOCK_RAM	EQU	6	; new label 20161117

; some kernel-related definitions *** renamed
#ifndef	LOWRAM
MX_QUEUE	EQU	16	; maximum interrupt task queue size
MX_DRVRS	EQU	16	; maximum number of drivers, independent as of 20170207
MAX_LIST	EQU	32	; number of available RAM blocks *** might increase this value in 65816 systems!
#else
MX_QUEUE	EQU	6	; much smaller queues in 128-byte systems, note unified jiffy & slow queues!
MX_DRVRS	EQU	4	; maximum number of drivers, independent as of 20170207
MAX_LIST	EQU	0	; no memory management for such systems
#endif

; multitasking subfunctions, no longer needed as will patch regular kernel!

; ** multitasking status values **
BR_FREE		EQU 192	; free slot, non-executable
BR_RUN		EQU   0	; active process, may get CPU time, should be zero for quick evaluation
BR_STOP		EQU 128	; paused process, will not get CPU until resumed
BR_END		EQU  64	; ended task, waiting for rendez-vous
; might add a fifth state for forked-but-not-yet-loaded braids (in order NOT to start them abnormally)
BR_MASK		EQU 192	; as it set both bits but NOT those for SIGTERM handler, new 20161117

; ** multitasking signals **
SIGKILL		EQU  0	; immediately kill braid, will go BR_FREE... maybe after BR_END
SIGTERM		EQU  2	; ask braid to terminate in an orderly fashion, default handler is SIGKILL
SIGSTOP		EQU  4	; pause braid, will go BR_STOP
SIGCONT		EQU  6	; resume a previously paused braid, will go BR_RUN

; MAX_BRAIDS should be system variable, as defined by firmware and/or multitasking driver
; default updateable value EQU 1 (no multitasking)
; if defined in firmware, think about a gestalt-like function for reading/setting it!
; QUANTUM_COUNT no longer defined here

; ********************************************************
; *** subfunction codes for task queues management TBR ***
; ********************************************************
TQ_STAT		EQU   0	; read status (and frequency)
TQ_EN		EQU   2	; enable task
TQ_DIS		EQU   4	; disable task
TQ_FREQ		EQU   6	; set task frequency (periodic tasks only)

; *********************************************************
; ** power control values, valid for kernel and firmware **
; *********************************************************
PW_STAT		EQU  0	; suspend (go static) if available, or no pending action, best if zero
PW_WARM		EQU  2	; warm reset (needs no firmware) renumbered 150603
PW_COLD		EQU  4	; cold reset (needed for compatibility with other architectures) renumbered 150603
PW_OFF		EQU  6	; power off
PW_CLEAN	EQU  8	; scheduler detected system is clean for poweroff! new 20160408
; new interrupt invoking codes
PW_HARD		EQU 10	; invoke hardware interrupt (NMI-like)
PW_SOFT		EQU 12	; invoke software interrupt, not really needed as SWI directly handled

; *******************************************
; ** optional windowing system values, TBD **
; *******************************************
W_OPEN		EQU   0	; active window in use
W_CLOSE		EQU 192	; closed, free window
W_FREE		EQU 128	; no longer in use, may be closed by kernel itself
W_REQ		EQU  64	; requested to close, will send SIGTERM to creator braid

; ************************
; ** logic devices, TBD **
; ************************
DEV_RND		EQU 126	; get a random number
DEV_NULL	EQU 127	; ignore I/O

; ** physical device numbers, TBD **
; these are likely to become 'logical' port IDs like rs0, rs1, ss0... regardless of actual implementation
DEV_LED		EQU 252	; LED keypad on VIAport
DEV_LCD		EQU 210	; Hitachi LCD module, TO_DO
DEV_ACIA	EQU 236	; ACIA, currently 6551
DEV_SS22	EQU 250	; SS-22 port
DEV_ASC		EQU 241	; ASCII keyboard on VIAport, TO_DO
DEV_DBG		EQU 255	; Bus sniffer, NEW 20150323
DEV_CNIO	EQU 132	; for Kowalski & run816 simulator, NEW 20160308
DEV_VGA		EQU 192	; integrated VGA-compatible Tijuana, NEW 20160331

; more temporary IDs

; lcd-4-bit @ascii EQU 240
; acia 2651 EQU 237
; VIA PA parallel port EQU 243
; VDU @ VIAport EQU 242
; (d)uart-16c550-1 EQU 232 hehehe
; duart-16c552-0 (or 2) EQU 224
; rtc 146818 (pseudo-driver?) EQU 208
; duart 2681-1 EQU 235 (or 227)
; duart 2681-2 EQU 227 (or 235)

