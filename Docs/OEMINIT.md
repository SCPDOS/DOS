SCP/DOS OEM Init guide main points:
It is assumed that an OEM implementer of SCP/DOS is using a non SCP/BIOS Hardware Abstraction Layer or bootstrap program.
Thus there are a number of modifications that need to be made to the full SYSINIT routine to allow DOS to load.
The modifications are herein listed and any directives must be followed to allow for easy porting of DOS.

Please note, OEMINIT MUST NOT use ANY DOS calls at all.

MUST, MAY and SHOULD can be defined as usual.

0) Hardware setup.
	DOS needs a minimum of 128Kb to load or run successfully though we suggest a bit more.
	DOS uses the load address of tasks as their Process ID's and as such, for Version 1, 
	the entire memory space that is to be used by DOS must be identity mapped or at least,
	mapped in such a way that all tasks live in a single address space. This may be changed 
	in later versions, though please note that the kernel will always pass virtual addresses 
	to drivers in all future versions.

1) DOS Load.
	DOS must be loaded to the lowest possible address in memory. This may be address 0.
	No assumptions about DOS's load position are made within the SYSINIT module and as such
	you are free to load DOS wherever you wish in memory. Please note, the main DOS Code 
	section is loaded after the DOS data area. This includes any device drivers you might add.
	Please refer to the file scpdos.asm for information about in what order files are 
	loaded and how DOS is built.
	SYSINIT will not do any further relocation of DOS once OEMINIT has loaded DOS into memory.
	Thus, please be attentive to how you structure your routines to reduce DOS's memory footprint.
	DOS will place all buffers and do all scratch work in the memory immediately following DOS's 
	load location.

	When jumping to SYSINT from OEMINIT, you must pass the base address of the DOS data area 
	in the variable QWORD [INITBASE]. You must also pass the end address of the DOS allocation 
	in the variable QWORD [INITEND].

2) Memory Control Block Chain init.
	An implementation of OEMINIT must create a valid Memory Control Block (MCB) chain. The MCB structure 
	definition can be found in dosStruc.inc. Each Memory Control Block is split into two regions:
	- The Arena Header, a 16 byte region defined in dosStruc.inc. 
		Any memory holes which are reserved for hardware or for use by BIOS can either be hidden behind 
		the first MCB (known as the anchor MCB) or placed in a memory arena that is given a mcb.owner ID
		of 7 (meaning memory hole). Blocks allocated with this owner signature cannot be freed or reallocated.
	- The Memory Arena, a variable sized region, of maximum size 4,294,967,295 (FFFFFFFFh) 16 byte paragraphs.

	All MCBs must be paragraph aligned. There is no maximum as to how many MCBs there needs to be in the system
	though there needs to be a minimum of one, the anchor MCB. 
	OEMINIT must form an MCB chain, where the anchor MCB is the first MCB in the chain. Each MCB follows the previous 
	one with the final MCB in the chain being marked with an End of chain marker. The MCBs are placed contiguously 
	in memory with the computation to go from one to the other being made by taking the Memory Arena pointer from the 
	current MCB in question and adding the number of bytes in the arena to that pointer. This value can be obtained 
	from the DWORD field mcb.blockSize as a number of paragraphs in the Memory Arena. This computation will take you 
	to the next MCB's arena header. 
	Memory holes, such as those found in PCI systems, where a PCI device might reserve some of the system memory 
	space for device MMIO registers, must also be placed in MCBs since MCBs form a contiguous partitioning of 
	memory. Memory holes have their own special identifying marker. This special marker prevents DOS from 
	reallocating or freeing these MCBs thus protecting the integrity of the data within the memory hole.

	To summarise:
	You MUST mark all free space as not owned by any process. 
	You MUST ONLY mark all space owned by DOS as being owned by DOS. 
	You MUST ONLY mark all memory holes with the special memory hole marker.
	You MUST NOT not mark any allocations to DOS as memory holes.

	A suggestion for simplicity and safety is to place the Anchor MCB AFTER DOS, and to not place DOS itself in 
	an MCB as DOS allocations can be resized and a malicious actor can end up freeing any DOS allocations.

	Finally, when jumping to SYSINIT from OEMINIT, you must pass a pointer to the Anchor MCB in QWORD [INITMCB].

3) Device Init.
	An implementation of OEMINIT MUST implement a minimum of three devices: CON (The Console Device), CLOCK$ (The Clock
	Device) and one Block Device Driver capable of supporting between 2 and 5 block devices. 
	CLOCK$ MUST NOT fail in an implementation of SCP/DOS and requests to it will not be checked for failure.
	Other optional devices that MAY be implemented are AUX (The Auxilliary I/O Device), and PRN (The Printer Device)
	DOS itself defines a device called NUL (The Null device) which can be used as a bit bucket and OEM Implementers
	need not implement such a device. Additionally, it may be useful to define additional Serial and Parallel devices 
	such as COM1-4 and PRN1-3 and perhaps more block devices. An implementer may additionally implement devices such 
	as ZERO$ or RND$ to provide a stream of zero bytes and a stream of random bytes respectively, such as those special 
	files found on UNIX based systems. 
	In the face of modernisation, it might also be prudent to add network drivers. The DOS device driver model is 
	flexible enough to support this. Trust us! Suggested names for the main device driver might be something like 
	NET$ or NETSTAK$. There is an 8 character limit for device names and it is suggested that common device names are 
	suffixed with a $-sign to avoid the DOS filesystem from mistakening the device name with a file or folder.

	OEMINIT needs not do anything particular to initialise any devices, instead passing a pointer to a linked list
	of device driver headers. The addresses in the chain must be relative to QWORD [INITBASE]. SYSINIT will
	add this address to each element in the linked list of driver headers and to the Strat and Int fields of each 
	header. DOS will call the init routine of each device driver in the order that they encountered in the linked 
	list.
	ATTENTION!! 
		The linked list of device driver headers MUST be within the DOS code/data area, i.e. must come
		AFTER the address that is passed in QWORD [INITBASE] and remain resident.

	When jumping to SYSINIT from OEMINIT, you must pass a pointer to the Device Driver Chain in QWORD [INITDEVDRV]
	
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
	You may reallocate your allocation, and if you do not wish to keep the assigned block in memory, please free the 
	block before you return to DOS.
	You MUST NOT use the following DOS functions to terminate excution: Int 40h, Int 41h/AH=00h/31h/44h/4Ch, Int 47h.
	To return to DOS, simply return from the called routine.	
	If you wish to HALT DOS's init for some reason, please return with the carry flag set, i.e. CF=CY.

