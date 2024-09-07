SCP/DOS OEM Init guide main points:
It is assumed that an OEM implementer of SCP/DOS is using a non SCP/BIOS Hardware Abstraction Layer or bootstrap program.
Thus there are a number of modifications that need to be made to the full SYSINIT routine to allow DOS to load.
The modifications are herein listed and any directives MUST be followed to allow for easy porting of DOS.

Please note, OEMINIT MUST NOT use ANY DOS calls at all.

MUST, MAY and SHOULD can be defined as usual.

The following routines must be marked as global:
- OEMRELOC -> Does any necessary relocations.
- OEMINIT -> Initialises the load-time variables and prepares the drivers for init.
- OEMMCBINIT -> Provides an MCB chain for use by DOS.
- OEMHALT -> Called by DOS if DOS wants to abort load.
- OEMCALLBK -> Called after processing CONFIG.SYS. Gives access to an initialised DOS. Can make final modifications to the way that DOS initialised the system.

These symbols must be declared as extern:

- BYTE [DFLTDRIVE] = Default boot drive, between 0 and 25
- BYTE [FILES]	= Default value of FILES, between 1 and 254
- BYTE [BUFFERS]  = Default value of BUFFERS, between 1 and 99
- BYTE [LASTDRIVE] = Default value for LASTDRIVE, between 0 and 25
- QWORD [OEMDRVCHAIN] = Pointer to the kernel driver chain
- QWORD [FINALDOSPTR] = Address of the DOS load address
- BYTE [OEMBIOS] = Set means using non SCP name for BIOS (IO.SYS)
- QWORD [OEMPTR] = Ptr to BIOS/HAL internal data structures to be used by drivers/utilities. Set to 0 means no pointer.
- DWORD [OEMVERSION] = Set to indicate type of BIOS/HAL (for drivers):
	- 0 = CSM SCP/BIOS
 	- 1 = UEFI Boot services x SCP/BIOS
  	- 2 = UEFI Runtime services x SCP/BIOS
  	- 3-99 = Reserved by SCP/BIOS
  	- 99-2^32-1 = For use by OEMs. Please register your allocation with the SCP OEM registry.

======================================================================

Kernel Drivers are mildly different in that they must be compiled
with all their data area intatct i.e. no BSS symbols.

Aside from that, SYSINIT will initialise kernel drivers in 
much the same way as normal and due to the fact that kernel
drivers all live in the same chain as far as the SYSINIT routine
is concerned, the pointer returned by the last driver in the chain
will be used to denote the end of the driver allocation space.

Please note that drivers may be flat binaries or PE executables. Upon 
completing INIT please return in INIT the pointer to the first byte 
that is no longer needed by the allocation.
Flat binary drivers are NOT bound by the 64Kb limit that .COM files 
are bounded by.

# Brief guidelines for writing OEMINIT

SYSINIT doesnt care about the internal structure of the OEMINIT module.
Thus, an OEM is free to arrange code and data within the OEMINIT module,
 as they please. OEMINIT is always the first module linked to in the DOS
 binary blob file and therefore an OEM can guarantee that the first byte 
 of the OEMINIT module will be the first byte executed by the machine.
SYSINIT starts being invoked only once OEMINIT jumps to the symbol SYSENTRY.
OEMINIT can even be an .EXE or .ELF executable if the firmware allows it, 
 as long as it can link with SYSINIT by EXPORTING and IMPORTING the right
 symbols, its ok! Also, the DOS linker script requires that the OEMINIT 
 module be the first thing in the executable file, with the default
 kernel drivers being the at the end, after the DOS, in the segment
 kDrvText, kDrvData and kDrvBSS.
OEMINIT has no BSS segment, but has otext and odata where it can link itself into.


# PUBLIC PROCEDURES needed to link with SYSINIT
- OEMMCBINIT -> Does MCB chain building as SYSINIT doesn't know how to read any memory maps. Thats on the OEM to parse and build for us.
- OEMHALT -> If anything goes wrong during the initial phase of SYSINIT, it will use this routine to print a message and halt the machine.
- OEMCALLBK -> Used to finalise any setup before xfring control to SHELL= . At this point, DOS is ready to be used.


# EXTERN VARS needed to link with SYSINIT
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
