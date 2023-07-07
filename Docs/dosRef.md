# Main DOS reference file for non-standard aspects of SCP/DOS
Where DOS is found, it is taken to mean SCP/DOS. All other versions of DOS are given their full names.
# A comment on driver development and porting of 16-bit device drivers to the 64-bit architecture.
-The following terminology is standard herein: Device drivers in the driver list that is passed to SYSINIT are called kernel (device) drivers. All other drivers are called installable (device) drivers.

General intro:
Device drivers may be either flat binaries or .EXE format files. All device drivers must be written such that their load address is 0. In MASM and NASM, this may be done by using the ORG 0 directive at the start of the module.

Installable drivers:
Installable device drivers under 16-bit MS-DOS were expected to return the end of their allocation to DOS in a pointer in the endptr field of the equivalent initReqPkt structure. In SCP/DOS this is no longer necessary as installable drivers are no longer permitted to simply assume that memory space is contiguous. As a result, installable drivers must use the pointer passed in the optptr field of the initReqPkt as a pointer to the start of their allocation and must use the DOS Kernel functions 48h (ALLOC), 49h (FREE) and 4Ah (REALLOC) to do any necessary memory management. 
Installable drivers must NOT free or attempt to change their base allocation address. They may nevertheless allocate as much additional memory as is needed and change the sizes of their allocations. However, be considerate to other drivers and programs on the system! Don't hog unneccesarry memory! It is highly recommended that drivers eject their init code before returning to DOS by reallocating their initial allocation to not include this section of memory.

Note that installable drivers may NOT allocate additional memory during system runtime. Please try to allocate as much memory as you need during installation!

Finally, installable drivers during INIT may use DOS functions 1-12, 25h, 2Ah-2Dh, 30h, 35h 48h, 49h and 4Ah only. If in doubt about the use of a DOS function during init of an installable driver, please contact a member of the DOS kernel development team.

Kernel drivers:
Kernel drivers are initialised in a more restricted way when comparison to installable drivers due to the fact that they are installed before DOS is completely initialised. As such, they are subject to more restrictions. 
1) Kernel drivers MUST be linked into the SYSINIT module and cannot be a separate executable.
2) Kernel drivers MUST present at least 5 devices with the driver chain linked as follows:
    CON->AUX->PRN->CLOCK$->Any block or additional character devices.
3) Kernel drivers MUST be compiled with all their data allocated in the binary.
4) Kernel drivers MUST NOT make any DOS calls except for 25h and 35h for interrupt installation, 30h for version checking and 2Ah-2Dh for Date/Time adjustment.
5) Kernel drivers MUST indicate the amount of memory they wish to eject using the endptr field of initReqPkt to indicate the first free byte after each driver. If there are many drivers in a single linked list, they should all point to the same address; the highest address of all the drivers. Returning a value larger than the end of the device driver module is not permitted.
6) Kernel drivers MUST NOT attempt to allocate more memory than what is linked into the object file. No uninitialised data sections may be present in a kernel driver.
7) Kernel drivers have no command line parameters passed to them. They must
be loaded with sensible defaults.

Aborting from a device driver installation:
If a device driver has assertained that it wishes to abort installation, then it indicates this to SCP/DOS in one of two ways:

1) Setting the drvErrStatus bit in the status word. Only errGF is considered a valid error code. All other error codes are undefined for the INIT routine. 
2) By returning the value 0 in the endptr and numunt field of the initReqPkt. 

If a kernel driver aborts installation, the system halts as these drivers are the fundamental connection between DOS and the hardware.
Otherwise, the driver is simply removed from memory and if there is a next driver to process, it is loaded. Hence, if the aborting installable driver has allocated any memory, it MUST free this memory before returning to DOS, otherwise this may result in a memory leak. 