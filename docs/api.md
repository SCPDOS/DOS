# Quick guide for how to use the DOS API

--------------------------------------------------------------------------------

## General comments for programming under SCP/DOS

The following points are necessary for safe operation under SCP/DOS.
- Programmer! You are operating in Ring-0 and have the ability to effectively make any change to the system you wish. If you wish to do so and/or ignore the below points, you are responsible for any changes that occur to the system and any data and/or physical damage that might occur to your system.
- Programmers **MAY** use SCP/DOS to bootstrap their own system software. However, by doing so, they may no longer rely on SCP/DOS to operate correctly. It is up to the programmer to ascertain what is available after what changes.

In normal operation, the following rules apply:
- Programmers **MUST NOT** modify the GDT or the GDTR.
- Programmers **MUST NOT** touch the IDT directly or modify the IDTR. IDT manipulation may be done only through DOS functions (21h/25h) and (21h/35h). 
- Programmers **MUST NOT** manipulate the page tables and assorted management registers such as those registers prefixed with __CR__.
- Programmers **MUST NOT** attempt to load _any_ segment registers with any other values. These values are set by the operating system. Exceptions to this are the hidden portions of the __FS__ and __GS__ registers, though neither are preserved by DOS.
- Programmers **MAY** use instructions _CLI_ and _STI_ for turning interrupts off and on. Please try to remain with interrupts off as little as possible.
- Programmers **MUST** remember; your code is not running on buggy 8088's. You do not need to turn interrupts off for changing the stack pointer!
  
--------------------------------------------------------------------------------

# DOS API Guide

--------------------------------------------------------------------------------

## Int 20h - Terminate Program

Calling this interrupt will terminate the current procedure. The parent program will not be table to get any information about the state of the program at termination. 

## Int 22h - Termination Address

This interrupt __must__ not be called directly. This interrupt contains a pointer to the address that will be returned to when the current program terminates. 

## Int 23h - The CRTL-C interface
This interrupt __must__ not be called directly. This interrupt is called by DOS upon detection of a CTRL+C (or CTRL+BREAK) keystroke on STDIN. Upon entering Int 23h, all registers are as they were upon entering DOS, the user stack is restored and DOS is no longer considered to be processing a system call. Therefore, all DOS system calls may be made from within an Int 23h handler however care must be taken to ensure that there are no re-entrancy issues. There is a caveat to the user stack restoration in that DOS does enter this call by calling Int 23h and as a result, five additional QWORDS are placed on the user stack which consitute the return data from the Int 23h call. These are:
- RSP + 32 : SS - Stack segment of DOS stack
- RSP + 24 : RSP - Stack pointer before invokation of the interrupt
- RSP + 16 : RFLAGS - Flags before entering interrupt
- RSP + 8 : CS - Code segment of DOS return address
- RSP + 0 : RIP - Instruction pointer of DOS return address
 
where RSP refers to the stack pointer upon entering the Int 23h handler. 

!!__WARNING__!!: Because this interrupt handler is entered by invoking an Interrupt instruction, if the stack pointer was not QWORD aligned at the time of the initial DOS system call invokation, upon entering Int 23h, the stack pointer _will_ be forcefully QWORD aligned, causing possible issues if trying to access data from the stack, or even stack corruption. Whilst it is not necessary to have a QWORD aligned stack when invoking DOS it is _highly_ recommended that the stack pointer is so to avoid such issues.

A user may exit in one of two ways from this system call. Note that when exiting, the following behaviour is sensitive to the value passed to DOS in the register __AH__:
- IRETQ - Execute the DOS call for the function number in __AH__.
- RET 8 - If CF=CY then terminate the program, indicating CTRL+C termination. Else, execute the DOS call for the function number in __AH__. _Note: The same effect may be achieved with RET 16 and RET 24._

Across this call, it is recommended the state of the registers and program is preserved in such a way that once the system call exits, the system is in a known state. This includes the value of AX. In most cases, you will want to restart the interrupted DOS call. As a result, it is _highly_ recommended you preserve at least __AH__ across the CTRL+C call, if you are not terminating the program.


## Int 24h - Critical Error Handler
This interrupt __must__ not be called directly. This interrupt is called upon detection of a fault with a device driver, thus giving the user program a chance to assertain what was the cause of the error and how to proceed.
The handling of this interrupt is almost exactly as it was in the case of previous versions of DOS, including the partial reentrancy considerations. The mjor difference is that now instead of **BP**:**SI** pointing to the device driver header, now **RSI** fills the role of the pointer. To ease with porting of any old INT 24h code, **RBP** is set to zero. See RBIL for further details.

<pre>
On entry:
               AH = Critical Error Bitfield
               Bit 7 = 0 - Disk Error, Bit 7 = 1 - Char Device Error
               Bit 6 - Reserved
               Bit 5 = 0 - IGNORE not allowed, Bit 5 = 1 - IGNORE allowed
               Bit 4 = 0 - RETRY not allowed, Bit 4 = 1 - RETRY allowed
               Bit 3 = 0 - FAIL not allowed, Bit 3 = 1 - FAIL allowed
               Bits [2-1] = Affected Disk Error
                     0 0   DOS area
                     0 1   FAT area
                     1 0   Directory area
                     1 1   Data area
               Bit 0 = 0 - Read Operation, Bit 0 = 1 - Write Operation
               AL  = Failing drive number if AH[7] = 0
               DIL = Error code for errorMsg
               RSI = EA of Device Header for which device the error occured
               RBP = 0
Return:
               AL = 0 - Ignore the Error       (Ignore)
                  = 1 - Retry the Operation    (Retry)
                  = 2 - Terminate the Program  (Abort)
                  = 3 - Fail the DOS call      (Fail)
</pre>

## Int 25h - Read Absolute Sectors
Reads sectors from a DOS drive to memory, bypassing the filesystem. This function should not be used as damage to the FAT may occur. Furthermore, in a multitasking environment, it may take time until the the device lock is given when making this call. Care must be taken as this call is a thread and DOS blocking call for the process.

Input: 
- __AL__ = Zero-based Drive Number (0 = A, 1 = B, ...)
- __RBX__ = Pointer to the start of memory where to read the data to
- __ECX__ = Number of sectors to read
- __RDX__ = Start sector number of read.

Output:
If CF is clear, the operation was successful.

If CF is set, then __AX__ contains an error code indicating the error.
- __AX__ = 80h: Device failed to respond
- __AX__ = 40h: Seek operation failure
- __AX__ = 20h: Controller failure
- __AX__ = 10h: Data error (Bad CRC)
- __AX__ = 08h: Direct Memory Access (DMA) failure
- __AX__ = 04h: Requested Sector not found
- __AX__ = 03h: Write-protect fault
- __AX__ = 02h: General device error
- __AX__ = 01h: Bad Command

## Int 26h - Write Absolute Sectors

As for Int 25h. Further care must be taken with this function as writing sectors using this endpoint may result in permanent data loss and filesystem corruption.

## Int 27h - Terminate and Stay Resident
Allows an application to terminate but retain some portion of the main program body in memory.

Input: __RDX__-Pointer to the last byte in the current program's memory space to keep.

Will preserve all memory from PSP[0] to __RDX__.

## Int 28h - DOS Idle interrupt

This interrupt can be hooked by applications, such as TSR's that need to know when DOS is idle to do processing. This interrupt is called by DOS whenever DOS assertains that it is in an "idle" loop, indicating to hooked processes that there is some time for them to do processing. Please ensure that your hooked function correctly jumps to the next hooked function as failure to do so correctly may result in catastrophic system failure.

## Int 29h - Reserved, Fast CON out

This interrupt is marked as reserved, though works exactly as documented in previous versions of DOS. This interrupt is marked as reserved for the endpoint may be obsoleted in future versions.

## Int 2Ah - Reserved

## Int 2Bh - Reserved

## Int 2Ch - Reserved

## Int 2Dh - Reserved

## Int 2Eh - Alternate DOS Multiplexer

## Int 2Fh - DOS Multiplexer

This section will be filled in later with details of the multiplexer and 2Fh/12h interface.

--------------------------------------------------------------------------------
# Int 21h - General DOS API endpoint

Much like in MS-DOS, SCP/DOS can be invoked from a program using the Int 21h interface or by calling PSP[50h], with the relevant function number being placed in register __AH__ to identify the function the program wishes invoked. Upon calling the operating system, registers __RAX__, __RBX__, __RCX__, __RDX__, __RSI__, __RDI__, __RBP__, __RSP__, __R8__ and __R9__ are saved on the callers stack, in that order after which the stack is switched to an internal DOS stack. Thus, the caller must ensure they have enough space for all of the aforementioned registers to be stored. 

Most system calls have been subtly updated in such a way to allow for ease of porting and to make use of the new architecture. In what follows we briefly describe most which should give a programmer a good idea of how to read the RBIL entries for MS-DOS and translate those requirements for SCP/DOS. In time, this section will be filled with a function-by-function specification.

The following are general instructions for using the DOS API.
- All functions and subfunction codes are passed in __AH__ and __AL__ respectively.
- RAX is to be considered trashed after a system call, except for where a system call specifically states a return value in __RAX__, or any part of it. In the partial case, the rest of __RAX__ is undefined.
- Most system calls which may return an error code, indicate an error by setting the Carry (CF) flag. The remaining FCB functions do so by setting the __AL__ register to -1.
- Generally, most system calls have been upgraded from using 16-bit registers to 32-bit registers. Thus where before one might have used __ZX__, this is replaced with __EZX__, where __Z__ is a placeholder. The only exception to this rule are pointers.
- Handles to files, where once would've been stored in 16-bit registers such as __BX__ and __AX__, are now stored in their corresponding 32-bit registers, __EBX__ and __EAX__. 
- Pointers to memory, where once would've been stored as far pointers, such as __DS__:__DX__ are now stored as near pointers in the 64-bit extension of the offset. That is to say, __DS__:__DX__ now becomes __RDX__.
- Pointers to memory segment, such as in FREE (21h/49h), where once mightve been stored in __ES__ are now stored as flat pointers in __R8__. No arguments are ever passed to and from DOS in __R9__.
- When doing file IO, one can transfer up to 4Gb of data in a single transer, using __ECX__ instead of the previous __CX__.
- File pointer manipulation using LSEEK (21h/42h) has an important caveat. Though at present, DOS only knows how to manipulate files which are at most 4Gb in size, using the redirector interface, DOS can in fact manipulate larger files. To facillitate this, LSEEK actually works by manipulating a _64-bit_ file pointer. The upper 32-bits of the file pointer are stored in __ECX__ with the lower 32-bits in __EDX__. Thus, the full pointer is stored in __ECX__:__EDX__.
- All undocumented Int 21h functions are implemented according to the above specification.
- Time and Date formats and structures remain unchanged.
- The maximum length of a path is 64 characters.
- The maximum length of a fully qualified file name is 67 characters.
- The PSP structure is considerably different in places. Efforts were made to keep the important documented and well used fields in place. Please refer to the NASM struct below:
<pre>
  struc psp
    .return     db 2 dup (?)  ;Should always be CDh 20h
    .allocSize  dd ?  ;Number of paras in init alloc or when exiting as TSR.
                dd ?  ;Reserved 4 bytes
    .oldInt22h  dq ?  
    .oldInt23h  dq ?  
    .oldInt24h  dq ?  
    .parentPtr  dq ?  ;Pointer to parent process PSP
    .jobFileTbl db 20 dup (?) 
    .envPtr     dq ?  ;Pointer to the environment
    .rspPtr     dq ?  ;Pointer to rsp on entry to Int 21h
    .jftSize    dw ?  ;JFT array size, 20 => PSP JFT in use
    .unixEntry  db 3 dup (?)  ;Must always be CDh 21h CBh
    .prevPSP    dq ?  ;Pointer to the previous PSP in chain (used by SHARE)
                db ?  ;Reserved byte
    .cmdLineArgPtr: 
    .fcb1       db 16 dup (?)  ;First FCB,    argument 1 
    .fcb2       db 20 dup (?)  ;Second FCB,   argument 2
    .dta:   ;Pointer to the default DTA in the PSP
    .parmList   db ?   ;Number of characters in command tail
    .progTail   db 127 dup (?) ;Default DTA/Program tail
endstruc
</pre>
- Previously undocumented DOS structures such as Drive Parameter Blocks (DPB), Current Direct Structures (CDS), System File Table (SFT) entries and disk buffer headers are all documented in the file ./src/Include/dosStruc.inc . Whilst it is _not_ suggested you make heavy use of these structures, if one chooses to, one should use the provided symbols properly, to allow for easy rebuilding of applications should the layouts of these structures change. Symbol names _may_ change, though it is unlikely.

## New Int 21h functions

- AH=61h - Process Management Services
  - AL=00h: Get pointer to the environment for current process. Returns a pointer to the environment for the current process in __RDX__.
  - AL=01h: Get pointer to the command line arguments for the current process. Returns a pointer to a command line arguments structure in __RDX__.
  - AL=02h: Get pointer to the filename of current process.
  <pre>
      On return: CF=NC : __RDX__ is a pointer to the filename.
                 CF=CY: No filename could be found and __RDX__ is a null pointer.
  </pre>
  - AL>03h: Reserved.

Note for reference, A command line arguments structure:
<pre>
struc cmdLineArgs  
  .fcb1       db 16 dup (?)  ;First command line argument parsed into FCB
  .fcb2       db 20 dup (?)  ;Second command line argument parsed into FCB
  .tailLen    db ?           ;Number of characters in command tail (up to 127)
  .progTail   db 127 dup (?) ;Command line tail, always with a terminating CR. CR is one char after tailLen number of chars.
endstruc
</pre> 
