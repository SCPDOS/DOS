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
BYTE [DFLTDRIVE] = Default boot drive, between 0 and 25
BYTE [FILES]	= Default value of FILES, between 1 and 254
BYTE [BUFFERS]  = Default value of BUFFERS, between 1 and 99
BYTE [LASTDRIVE] = Default value for LASTDRIVE, between 0 and 25
QWORD [OEMDRVCHAIN] = Pointer to the kernel driver chain
QWORD [FINALDOSPTR] = Address of the DOS load address
BYTE [OEMBIOS] = Set means using non SCP name for BIOS (IO.SYS)
QWORD [OEMPTR] = Ptr to BIOS/HAL internal data structures to be used by 
				 drivers/utilities. Set to 0 means No pointer.
DWORD [OEMVERSION] = Set to indicate type of BIOS/HAL (for drivers)
					 0 = CSM SCP/BIOS
					 1 = UEFI Boot services x SCP/BIOS
					 2 = UEFI Runtime services x SCP/BIOS
					 3-99 = Reserved by SCP/BIOS
					 99-2^32-1 = For use by OEMs. Please register 
					 your allocation with the SCP OEM registry.

======================================================================

Kernel Drivers are mildly different in that they must be compiled
with all their data area intatct i.e. no BSS symbols.

Aside from that, SYSINIT will initialise kernel drivers in 
much the same way as normal and due to the fact that kernel
drivers all live in the same chain as far as the SYSINIT routine
is concerned, the pointer returned by the last driver in the chain
will be used to denote the end of the driver allocation space.

Please note that drivers may be flat binaries or .EXE files. Upon 
completing INIT please return in INIT the pointer to the first byte 
that is no longer needed by the allocation.
Flat binary drivers are NOT bound by the 64Kb limit that .COM files 
are bounded by.

Order of OEMINIT and SYSINIT interplay
O1) OEMRELOC - Load and relocate DOS to its final resting place.
				MUST RETURN TO DOS THE VALUE OF THE MAX
				CONTIUGOUS USABLE MEMORY FROM THE DOSLOAD ADDRESS
				TO THE FIRST MEMORY HOLE/END OF MEMORY. 

				Please ensure to leave AT LEAST 20Kb of space
				for DOS to use to build internal data structures
				within this contiguous region. Failure to do so
				will result in DOS halting boot.

				Kernel drivers may NOT use more memory than the 
				end of their initial allocation but may
				resize their allocation to jettison their init code.

				Thus kernel drivers must be written with all 
				their work space built in. 

				Jumps to SYSINIT once complete!

O2) OEMINIT - Add page additional tables if needed and any additional
				OEM specific housekeeping such as providing a 
				sufficient stack if one is not already available and
				setting the OEM specific DOS variable defaults.

S1) Interrupt init - SYSINIT will setup the interrupts vectors
				for DOS now pointing to the routines in the 
				relocated DOS segment. This includes all CPU exception
				handlers and the DOS Interrupts.  

S2) Driver init - SYSINIT will initialise the Kernel drivers, giving
				them enough space unless there is not enough space 
				available in the current coniguous region or the
				kernel drivers return that they want to halt in 
				which case DOS halts init. Kernel drivers can use
				DOS functions 25h and 35h to install and get 
				interrupts ONLY and may not use any other DOS functions
				as DOS is not sufficiently configured at this stage
				to support this.
				
				Please note that only num_disk disk devices are 
				available until CONFIG.SYS is processed.
				Also, note that you may jettison the startup code
				as the file drvinit.asm is placed at the end of the 
				kernel module.

				Kernel drivers shall halt boot by indicating that
				they failed to initialise.

				The kernel driver chain must be in the following order:
				CON->AUX->PRN->CLOCK$->Any block or additional character devices.
				

O3) MCB init - OEM's must implement the MCB CHAIN starting from the 
				address provided by DOS to the MCB INIT routine in MCBANCHOR.
				The MCB INIT routine recieves the first MCB already filled in
				by DOS, which accounts for DOS and builds the chain from there.
				Please only allocate space as either FREE or MEMORY_HOLE 
				respectively.

S3) PSP init - SYSINIT will setup the default Kernel PSP and 
				additional DOS variables now.

S4) SFT init - SYSINIT will setup the default SFT entries now.

S5) Hook init - SYSINIT will setup the kernel hooks to defaults.
				These can be overridden by drivers and/or programs
				that might be loaded in CONFIG.SYS and/or the 
				COMSPEC= program. We suggest any multitaskers be loaded
				as COMSPEC= and then load a command interpreter as the
				child of the multitasker.

----DOS IS NOW COMPLETELY READY FOR USE----

S6) CONFIG.SYS - SYSINIT will process CONFIG.SYS

S7) Welcome - SYSINIT will print a default welcome message

S8) Reset SFT's - SYSINIT will now reopen handles to allow any 
				replacement drivers for the kernel drivers to take over 
				the default devices.

O4) OEMCALBK - A final callback as decsribed above. This can be used
				for example to correct anything that might've been 
				set in CONFIG.SYS that you don't agree with.
				You may also HALT system loading here by returning
				with CF=CY.

S9) Transfer to COMSPEC= - Transfer control to the master 
				Command interpreter. If one cannot be found, 
				print an error message and halt the system.

Since OEMINIT is called by SYSINIT immediately after OEMRELOC
DOS will permit the passing of arguments from one module to the 
next in registers. No further passing of arguments in registers
is permitted however.