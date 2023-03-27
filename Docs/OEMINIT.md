SCP/DOS OEM Init guide main points:
It is assumed that an OEM implementer of SCP/DOS is using a non SCP/BIOS Hardware Abstraction Layer or bootstrap program.
Thus there are a number of modifications that need to be made to the full SYSINIT routine to allow DOS to load.
The modifications are herein listed and any directives MUST be followed to allow for easy porting of DOS.

Please note, OEMINIT MUST NOT use ANY DOS calls at all.

MUST, MAY and SHOULD can be defined as usual.

0) Hardware setup.
	DOS needs a minimum of 128Kb to load and run successfully though we suggest a bit more.
	DOS uses the load address of tasks as their Process ID's and as such, for Version 1, 
	the entire memory space that is to be used by DOS MUST be identity mapped or at least,
	mapped in such a way that all tasks live in a single address space. This may be changed 
	in later versions, though please note that the kernel will always pass virtual addresses 
	to drivers in all future versions.

1) DOS Load.
	DOS MUST be loaded to the lowest possible address in memory. This MAY be address 0.
	No assumptions about DOS's load position are made within the SYSINIT module and as such
	you are free to load DOS wherever you wish in memory. Please note, the main DOS Code 
	section is loaded after the DOS data area. This includes any device drivers you might add.
	Please refer to the file scpdos.asm for information about in what order files are 
	loaded and how DOS is built.
	SYSINIT will not do any further relocation of DOS once OEMINIT has loaded DOS into memory.
	Thus, please be attentive to how you structure your routines to reduce DOS's memory footprint.
	DOS will place all buffers and do all scratch work in the memory immediately following DOS's 
	load location.

	When jumping to SYSINT from OEMINIT, you MUST pass the base address of the DOS data area 
	in the variable QWORD [INITBASE]. You MUST also pass the end address of the DOS allocation 
	in the variable QWORD [INITEND]. This value MUST point to the first byte of the a memory 
	control block's memory arena. This memory control block MAY be the anchor MCB.

2) Memory Control Block Chain init.
	An implementation of OEMINIT MUST create a valid Memory Control Block (MCB) chain. The MCB structure 
	definition can be found in dosStruc.inc. Each Memory Control Block is split into two regions:
	- The Arena Header, a 16 byte region defined in dosStruc.inc. 
		Any memory holes which are reserved for hardware or for use by BIOS can either be hidden behind 
		the first MCB (known as the anchor MCB) or placed in a memory arena that is given a mcb.owner ID
		of 7 (meaning memory hole). Blocks allocated with this owner signature cannot be freed or reallocated.
	- The Memory Arena, a variable sized region, of maximum size 4,294,967,295 (FFFFFFFFh) 16 byte paragraphs.

	All MCBs MUST be paragraph aligned. There is no maximum as to how many MCBs there needs to be in the system
	though there needs to be a minimum of one, the anchor MCB. 
	OEMINIT MUST form an MCB chain, where the anchor MCB is the first MCB in the chain. Each MCB follows the previous 
	one with the final MCB in the chain being marked with an End of chain marker. The MCBs are placed contiguously 
	in memory with the computation to go from one to the other being made by taking the Memory Arena pointer from the 
	current MCB in question and adding the number of bytes in the arena to that pointer. This value can be obtained 
	from the DWORD field mcb.blockSize as a number of paragraphs in the Memory Arena. This computation will take you 
	to the next MCB's arena header. 
	Memory holes, such as those found in PCI systems, where a PCI device might reserve some of the system memory 
	space for device MMIO registers, MUST also be placed in MCBs since MCBs form a contiguous partitioning of 
	memory. Memory holes have their own special identifying marker. This special marker prevents DOS from 
	reallocating or freeing these MCBs thus protecting the integrity of the data within the memory hole.

	To summarise:
	You MUST mark all free space as not owned by any process. 
	You MUST ONLY mark all space owned by DOS as being owned by DOS. 
	You MUST ONLY mark all memory holes with the special memory hole marker.
	You MUST NOT not mark any allocations to DOS as memory holes.

	A suggestion for simplicity and safety is to place the Anchor MCB AFTER DOS, and to not place DOS itself in 
	an MCB as DOS allocations can be resized and a malicious actor can end up freeing any DOS allocations.

	Finally, when jumping to SYSINIT from OEMINIT, you MUST pass a pointer to the Anchor MCB in QWORD [INITMCB].

3) Device Init.
	An implementation of OEMINIT MUST implement a minimum of three devices: CON (The Console Device), CLOCK$ (The Clock
	Device) and one Block Device Driver capable of supporting between 2 and 5 block devices. 
	CLOCK$ MUST NOT fail in an implementation of SCP/DOS and requests to it will not be checked for failure.
	Other optional devices that MAY be implemented are AUX (The Auxilliary I/O Device), and PRN (The Printer Device)
	DOS itself defines a device called NUL (The Null device) which can be used as a bit bucket and OEM Implementers
	need not implement such a device. Additionally, it MAY be useful to define additional Serial and Parallel devices 
	such as COM1-4 and PRN1-3 and perhaps more block devices. An implementer MAY additionally implement devices such 
	as ZERO$ or RND$ to provide a stream of zero bytes and a stream of random bytes respectively, such as those special 
	files found on UNIX based systems. 
	In the face of modernisation, it might also be prudent to add network drivers. The DOS device driver model is 
	flexible enough to support this. Trust us! Suggested names for the main device driver might be something like 
	NET$ or NETSTAK$. There is an 8 character limit for device names and it is suggested that common device names are 
	suffixed with a $-sign to avoid the DOS filesystem from mistakening the device name with a file or folder.

	OEMINIT needs not do anything particular to initialise any devices, instead passing a pointer to a linked list
	of device driver headers. The addresses in the chain MUST be relative to QWORD [INITBASE]. SYSINIT will
	add this address to each element in the linked list of driver headers and to the Strat and Int fields of each 
	header. DOS will call the init routine of each device driver in the order that they encountered in the linked 
	list.
	ATTENTION!! 
		The linked list of device driver headers MUST be within the DOS code/data area, i.e. MUST come
		AFTER the address that is passed in QWORD [INITBASE] and remain resident.

	When jumping to SYSINIT from OEMINIT, you MUST pass a pointer to the Device Driver Chain in QWORD [INITDEVDRV]
	
4) System Data.
	An implementation of OEMINIT MUST pass the default values for FILES and BUFFERS in the variables BYTE [INITFILE]
	and BYTE [INITBUF].
	An implementation MAY optionally also pass a value in BYTE [INITDEFAULT] for the start default drive (i.e. the 
	boot drive) and QWORD [INITMEMSZ] for a total count of bytes available for use by DOS though DOS will never 
	use this as it does all memory based based on the MCB chain.

5) Final Callback Hook.
	Should your implementation of OEMINIT require a some additional work before DOS loads the command interpreter
	you MUST hook the variable QWORD [INITCALLBACK] with an absolute pointer to the routine you wish to be called. 
	If your implemntation has no need of this functionality, the passed pointer MUST point solely to a return 
	instruction. 
	
	This routine will be called once CONFIG.SYS has been processed by DOS, to allow your implementation to adjust 
	any values and do any final system initialisation. When this function is called, r8 will be pointing to a 64Kb 
	block of memory. Please do not lose this pointer. Your function will be running as an extension of DOS. 
	You MAY reallocate your allocation, and if you do not wish to keep the assigned block in memory, please free the 
	block before you return to DOS.
	You MUST NOT use the following DOS functions to terminate excution: Int 40h, Int 41h/AH=00h/31h/44h/4Ch, Int 47h.
	To return to DOS, simply return from the called routine. On return, ALL REGISTERS MUST BE PRESERVED!
	If you wish to HALT DOS's init for some reason, please return with the carry flag set, i.e. CF=CY.

All OEM variables are visible across OEMinit.

- OEMSTATS -> Returns a few values in standard form for DOS
Fill the follwing variables:
BYTE [INITFILE]	= Default number of FILES
BYTE [INITBUF]  = Default number of Buffers
BYTE [INITDEFAULT] = Start drive for the boot
QWORD [INITMEMSZ] = Number of bytes usable by DOS
BYTE [OEMBIOS] = Set means using non SCP name for BIOS (IO.SYS)

- OEMCALLBK -> Access to initialised DOSSEG and SDA. Can make final 
modifications to the way that DOS initialised the system.
======================================================================

Kernel Drivers are mildly different in that they must be compiled
with all their data area intatct i.e. no BSS symbols.

Aside from that, SYSINIT will initialise kernel drivers in 
much the same way as normal and due to the fact that kernel
drivers all live in the same chain as far as the SYSINIT routine
is concerned, the pointer returned by the last driver in the chain
will be used to denote the end of the driver allocation space.

Please note that all drivers must be flat binary files and that whilst 
you may have CODE and DATA segments, please return in INIT the pointer
to the first byte that is no longer needed by the allocation.
Drivers are NOT bound by the 64Kb limit that .COM files are bounded by.

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
				

O3) MCB init - OEM's must implement the MCB CHAIN starting from the 
				address provided by DOS to the MCB INIT routine. This 
				is to account for the kernel drivers required space.
				The Anchor MCB must come after DOS i.e after any 
				allocated space for the kernel drivers. Please
				only allocate space as either FREE or MEMORY_HOLE 
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