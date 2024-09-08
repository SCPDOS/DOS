# SCP/DOS OEM Init implementers overview guide 

It is assumed that an OEM implementer of SCP/DOS is using a non SCP/BIOS Hardware Abstraction Layer or bootstrap program.
Thus there are a number of modifications that need to be made to the full SYSINIT routine to allow DOS to load.
The modifications are herein listed and any directives MUST be followed to allow for easy porting of DOS.

Please note, OEMINIT MUST NOT use ANY DOS calls at all.

======================================================================

## Brief guidelines for writing OEMINIT

SYSINIT doesnt care about the internal structure of the OEMINIT module. Thus, an OEM is free to arrange code and data within the OEMINIT module, as they please. OEMINIT is always the first module linked to in the DOS binary blob file. SYSINIT starts being invoked only once OEMINIT jumps to the symbol SYSENTRY. OEMINIT can even be an .EXE or .ELF executable if the firmware allows it, as long as it can link with SYSINIT by EXPORTING and IMPORTING the right symbols, its ok! The DOS linker script will place the OEMINIT module at the start of the output executable file, with the default kernel drivers being the at the end, after the DOS, in the segments kDrvText (code segment), kDrvData (data segment) and kDrvBSS (BSS segment). OEMINIT has no BSS segment, but has otext (code segment) and odata (data segment) segments where it can link itself into. Information on writing replacement kernel drivers can be found in the driver writing guide.

## PUBLIC PROCEDURES needed to link with SYSINIT
<pre>
- SYSENTRY  : Entry point for SYSINIT. Must be jumped to!
- OEMMCBINIT: Does MCB chain building as SYSINIT doesn't know how to read any memory maps. It is on the OEM to parse and build the MCB chain for us.
- OEMHALT   : If anything goes wrong during the initial phase of SYSINIT, it will use this routine to print a message and halt the machine.
- OEMCALLBK : Used to finalise any setup before transferring control to "SHELL= ". At this point, DOS is ready to be used.
</pre>

## EXTERN VARS needed to link with SYSINIT
<pre>
The following vars need to be initialised before jumping to SYSENTRY.
- FINALDOSPTR dq ?    ;Pointer to where dSeg should be loaded
- FILES       db ?    ;Default number of FILES
- BUFFERS     db ?    ;Default number of BUFFERS
- DFLTDRIVE   db ?    ;Default drive number (0-25), this is the boot drive
- LASTDRIVE   db ?    ;Default last drive number (0-25)
- OEMBIOS     db ?    ;Set if to use IO.SYS or clear if to use SCPBIOS.SYS
- OEMDRVCHAIN dq ?    ;Pointer to the uninitialised device drivers
- OEMPTR      dq ?    ;Pointer to store at biosPtr
- OEMVERSION  dd ?    ;BIOS number, to be used by drivers for id-ing

These vars are initialised by SYSINIT, to be used in OEMMCBINIT. These vars are undefined outside of OEMMCBINIT.
- MCBANCHOR   dq ?    ;Pointer to the Anchor MCB, part of dSEg

These vars are initialised by SYSINIT, to be used in OEMCALLBK. These vars are undefined outside of OEMCALLBK.
- OEMMEMPTR   dq ?    ;Var to save ptr to the 64Kb block passed to OEMCALLBK
</pre>
