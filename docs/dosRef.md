# Main DOS reference file for non-standard aspects of SCP/DOS
Where the word DOS is used, it is taken to mean SCP/DOS. All other versions of DOS are referred to by their full names.
## A comment on driver development and porting of 16-bit device drivers to the 64-bit architecture.
-The following terminology is standard herein: Device drivers in the driver list that is passed to SYSINIT are called kernel (device) drivers. All other drivers are called installable (device) drivers.

General intro:
All device drivers must be written such that their load address is 0. In MASM and NASM, this may be done by using the ORG 0 directive at the start of the module. When multiple drivers are contained in a file, each drivers' drvHdr.nxtPtr in the driver header MUST point to the next driver header in the file (or -1 if it is the last driver in the file).

Installable drivers:
Installable device drivers under 16-bit MS-DOS were expected to return the end of their allocation to DOS in a pointer in the endptr field of the equivalent initReqPkt structure. In SCP/DOS, installable drivers are no longer permitted to simply assume that memory space is contiguous. As a result, installable drivers must use the DOS Kernel functions 48h (ALLOC), 49h (FREE) and 4Ah (REALLOC) to do any memory management which results in further allocation. As in the case of kernel drivers, it is highly recommended that installable drivers eject their init code before returning to DOS. Installable drivers do so by placing a pointer in the endptr field of initReqPkt indicating the first free byte to eject within the driver. If there are many drivers in a single linked list, they should all point to the same address; the highest address of all the drivers. Returning a value larger than the end of the device driver module, 0 or -1 is not permitted and are ignored. Installable drivers must NOT free or attempt to change their base allocation address. They may however allocate as much additional memory as is needed and change the sizes of their allocations. However, be considerate to other drivers and programs on the system! Don't hog unneccesarry memory! 

Note that installable drivers may NOT allocate additional memory during system runtime. Please try to allocate as much memory as you need during installation!

Additionally, installable device drivers may be either flat binaries or PE format files. 
*-* If a driver is in PE format, the first segment in the PE executable must be a data segment, with the device driver header placed at the start of the segment. A driver may have arbitrary code/data/bss segments in the executable. If BSS segments are present, they are not initialised to 0. If necessary, this must be done by the driver's init routine.
*-* If a driver is a flat binary, the binary must start with the device driver header.

Finally, installable drivers during INIT may use DOS functions 1-12, 25h, 2Ah-2Dh, 30h, 35h, 48h, 49h, 4Ah and 52h only. If in doubt about the use of a DOS function during init of an installable driver, please contact a member of the DOS kernel development team.

Kernel drivers:
Kernel drivers are initialised in a more restricted way when compared to installable drivers due to the fact that they are installed before DOS is completely initialised. As such, they are subject to more restrictions. 
1) Kernel drivers must be provided as a linkable object file and cannot be written as a flat binary. 
*-* Kernel drivers MUST place all their code in the segment kDrvText.
*-* Kernel drivers MUST place all their allocated data in the segment kDrvDat.  This includes the device driver header. This must be the first piece of data in the data segment.
*-* We suggest all driver headers for the drivers in the kernel driver module be placed together in one long table at the start of the segment kDrvDat.
*-* Kernel drivers MUST place all their unallocated data in the segment kDrvBSS. Though support for a BSS segment is present, the segment itself is not initialised to 0. If necessary, this may be done during driver initialisation.

2) Kernel drivers MUST present at least 5 devices with the driver chain linked as follows:
    CON->AUX->PRN->CLOCK$->Any block or additional character devices.
3) Kernel drivers MUST be compiled with all their data allocated in the binary.
4) Kernel drivers MUST NOT make any DOS calls except for 25h and 35h for interrupt vector obtaining and installation, 30h for version checking, 2Ah-2Dh for Date/Time adjustment and 52h for SYSVARS pointer obtaining.
5) Kernel drivers MUST indicate the amount of memory they wish to eject using the endptr field of initReqPkt to indicate the first free byte after each driver. If there are many drivers in a single linked list, they should all point to the same address; the highest address of all the drivers. Returning a value larger than the end of the device driver module is not permitted.
6) Kernel drivers MUST NOT attempt to allocate more memory than what is linked into the object file.
7) Kernel drivers have no command line parameters passed to them. They must
be loaded with sensible defaults.

Aborting from a device driver installation:
If a device driver has assertained that it wishes to abort installation, then it indicates this to SCP/DOS in one of two ways:

1) Setting the drvErrStatus bit in the status word. Only errGF is considered a valid error code. All other error codes are undefined for the INIT routine. 
2) By returning the value 0 in the endptr and numunt field of the initReqPkt. 

If a kernel driver aborts installation, the system halts as these drivers are the fundamental connection between DOS and the hardware.
Otherwise, the driver is simply removed from memory and if there is a next driver to process, it is loaded. Hence, if the aborting installable driver has allocated any memory, it MUST free this memory before returning to DOS, otherwise this may result in a memory leak. 

For both kernel and installable drivers:
If a driver needs access to hardware HAL services through a global pointer, but is unsure of HAL version and/or otherwise cannot call a HAL system service to obtain a pointer, the driver may call DOS to get the DOS version number and based on that, work out the negative offset to the relevant data from the SYSVARS pointer, based on the structure of the DOS data area. The relevant data in the DOS data area is obtained from OEMINIT.

## CP/M features not present in DOS
To summarise: 
- The CALL 5 DOS entry point.
- All FCB File I/O functions are now considered reserved and will return an FCB error with error code 05h, Access Denied, if they are called. Those functions include:
  - FCB Create (21h/16h)
  - Open (21h/0Fh)
  - Close (21h/10h)
  - Sequential Read (21h/14h)
  - Sequential Write (21h/15h)
  - Random Read (21h/21h)
  - Random Write (21h/22h)
  - Random Block Read (21h/27h)
  - Random Block Write (21h/28h)
  - Set Random Record (21h/24h)
 
_Caveat: FCB Create (21h/16h) and Open (21h/0Fh) can be used for the creation of a volume label._

