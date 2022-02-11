[map all scpdos.map]
[DEFAULT REL]
BITS 64
%include "driverStruc.inc"
%include "fatStruc.inc"
%include "dosStruc.inc"

Segment dSeg nobits align=1 
    dosSegPtr   resq 1    ;Pointer to the data Segment itself
    bootDrive   resb 1    ;The Int 33h device we booted from
    charReqHdr  resb ioReqPkt_size  ;Character IO Request header
    diskReqHdr  resb ioReqPkt_size  ;Disk Action Request header
    ;The device driver header with space for the largest possible packet
    sysVarsPtr  resq 1    ;Pointer to dpbHeadPtr, head of Sys Vars struc below
    mcbChainPtr resq 1    ;Pointer to the MCB chain
    dpbHeadPtr  resq 1    ;Pointer to the first DPB in the DPB chain
    sftHeadPtr  resq 1    ;Pointer to the first SFT header in SFT chain
    clockPtr    resq 1    ;Pointer to the current active CLOCK$ device header
    ;                    The last driver loaded with the CLOCK$ bit[3] set 
    conPtr      resq 1    ;Pointer to the current active CON device header 
    ;                    The last driver loaded with the STDIN bit[0] set
    maxBytesSec resw 1    ;Maximum number of bytes per sector (size of buffers)
    bufHeadPtr  resq 1    ;Pointer to the head of the disk buffer chain
    cdsHeadPtr  resq 1    ;Pointer to the head of the CDS array
    sfcbHeadPTr resq 1    ;Pointer to the head of the System FCB chain
    numSafeSFCB resw 1    ;Number of protected FCBs (y in FCBS=x,y)
    numMSDdrv   resb 1    ;Number of mass storage devices detected in system
    lastdrvNum  resb 1    ;Value of LASTDRIVE (default = 5) [Size of CDS array]
    numJoinDrv  resb 1    ;Number of Joined Drives
    nulDevHdr   resb drvHdr_size

;Start of Swappable Data Area, this bit can remain static
    critErrFlag resb 1  ;Critical error flag, set on entry to INT 44h
    inDOS       resb 1  ;Inc on each DOS call, dec when leaving
    errorDrv    resb 1  ;Drive on which error occured or FFh
    errorLocus  resb 1  ;Where the error took place  
    errorExt    resw 1  ;Extended Error Code
    errorAction resb 1  ;Suggested action for error  
    errorClass  resb 1  ;Error Class

    currentDTA  resq 1  ;Address of the current DTA
    currentPSP  resq 1  ;Address of current PSP
    rdiErrorPtr resq 1  ;Saves RDI value of last error
    xInt43hRSP  resq 1  ;Saves RSP across an Int 43h call
    lastRetCode resw 1  ;Last return code returned by Int 41h/4Ch
    currentDrv  resb 1  ;Default, last accessed drive
    breakFlag   resb 1  ;If set, check for CTRL+C on all DOS calls
;SDA, needs to be replaced between processes
    xInt44hRSP  resq 1  ;RSP across an Int 44h call

    Int44RetVal resb 1  ;Saves a copy of the Int 44 return value
    Int44bitfld resb 1  ;Copies the bit field given to the Int 44h handler
    int48Flag   resb 1  ;If set, Int 48h should be called, if clear no
    oldoldRSP   resq 1  ;RSP at prev Int 41h entry if called from within Int 41h
    oldRSP      resq 1  ;RSP when entering Int 41h
    oldRBX      resq 1  ;Temp var to save value of rbx during an Int 41 call
;Time stuff
    CLOCKrecrd  resb 6  ;Clock driver record
    dayOfMonth  resb 1  ;1 - 31 BCD
    monthOfYear resb 1  ;1 - 12 BCD
    years       resw 1  ;0000 - 9999 BCD
    yearsOffset resw 1  ;Current Year - 1980
    daysOffset  resd 1  ;Days since 1-1-1980
    dayOfWeek   resb 1  ;0 = Sunday <-> 6 = Saturday

;Stacks
    critStack   resq 165
    critStakTop resq 1
    IOStack     resq 199
    IOStakTop   resq 1
    DiskStack   resq 199
    DiskStakTop resq 1
    dSegLen     equ     $

Segment .text align=1
; We arrive here with the following values in the registers.
; rbx =  LBA of first Logical Block after SCP/BIOS
; dx  = Int 33h boot device number
; fs  = userbase pointer (pointer to first usable block of RAM)
    dw 0AA55h           ;Initial signature
    mov byte fs:[bootDrive], dl ;Save the boot drive in memory

    mov ecx, 0C0000100h ;Read FS MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in

    mov qword fs:[dosSegPtr], rdi 
    mov rbp, rdi    ;Save the start of dosSeg in rdx 
    add rdi, dSegLen ;Move destination past end of data area
    lea rsi, section.resSeg.start  ;Get RIP relative address to copy high
    mov ecx, 1000h
    rep movsq

;Modify the pointers in nData before putting them in the data area
    add qword [nData + drvHdr.nxtPtr], rbp
    add qword [nData + drvHdr.strPtr], rbp
    add qword [nData + drvHdr.intPtr], rbp
;Copy the Null driver to its location in Sysvars
    mov ecx, drvHdr_size
    lea rsi, qword [nData]
    lea rdi, qword [rbp + nulDevHdr]
    rep movsb   

;Adjust the addresses in the other driver headers 
    mov rsi, conHdr ;Point to the first non-NUL dev in chain
    mov ecx, 12      ;12 drivers in data area
    lea rsi, qword [rsi + rbp]  ;Get effective addr of driver header
adjDrivers:
    call adjustDrvHdr
    loop adjDrivers

    ;Open NUL
    lea rbx, qword [rbp + nulDevHdr + drvHdr.strPtr]    ;Get ptr to strat ptr
    mov rbx, qword [rbx]    ;Get strat ptr
    xor al, al
    call rbx

    ;Open CON
    mov rbx, conDriver
    lea rbx, qword [rbp+rbx]
    xor al, al
    call rbx

    ;Open Mass Storage
    mov rbx, msdDriver
    lea rbx, qword [rbp+rbx]
    xor al, al
    call rbx
;Adjust Int 41h address table

adjInt41h:
    mov ecx, dispatchTableL/8 ;Number of elements in table
    mov rbx, functionDispatch.dispatchTable ;Get EA of table
    lea rbx, qword [rbp+rbx]    ;Point to the start of the relocated table 
.ai41h:
    add qword [rbx], rbp    ;Add base address value to entry in reloc table
    add rbx, 8              ;Each entry is size 8
    dec ecx
    jnz .ai41h  ;Keep looping until all entries have been adjusted

;Adjust Interrupt Entries Int 40h-49h
adjInts:
    mov bl, 40h
    mov eax, 0F007h ;Get the descriptor
    int 35h
    mov ecx, 40h    ;Start from interrupt 40h
    lea rdi, intData
    mov esi, eax    ;Move segment selector info to esi
.ai0:
    mov eax, 0F008h ;Set the descriptor
    mov rbx, qword [rdi]    ;Get address pointed to by rdi
    add rbx, rbp            ;Add the relocated base to rbx
    int 35h
    add rdi, 8
    inc ecx
    cmp ecx, 4Ah
    jne .ai0

;Test Error Case
    mov ah, 00110000b
    mov al, 00h
    mov edi, 0Ch
    int 44h


    lea rdx, qword [startmsg]   ;Get the absolute address of message
    mov ah, 09h
    int 41h

    mov rsi, fs:[nulDevHdr]
    mov eax, 0C501h ;Connect debugger
    int 35h
    xchg bx, bx
l1:
    mov eax, 0100h  ;Write with echo
    int 41h
    jmp short l1
adjustDrvHdr:
;Input: rsi = Effective address of driver in DOS segment
;       rbp = Ptr to the start of the DOS segment
;Output: rsi = EA of next header in DOS segment
    add qword [rsi + drvHdr.nxtPtr], rbp    ;Adjust address
    add qword [rsi + drvHdr.strPtr], rbp
    add qword [rsi + drvHdr.intPtr], rbp
    add rsi, drvHdr_size
    ret

startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,"$"
intData:
    dq terminateProcess ;Int 40h
    dq functionDispatch ;Int 41h
    dq terminateHandler ;Int 42h
    dq ctrlCHandler     ;Int 43h
    dq critErrorHandler ;Int 44h
    dq absDiskRead      ;Int 45h
    dq absDiskWrite     ;Int 46h
    dq terminateResident    ;Int 47h
    dq inDosHandler     ;Int 48h
    dq fastOutput       ;Int 49h
nData:
    dq conHdr
    dw 08004h
    dq nulStrat
    dq nulIntr
    db "NUL     " ;Default NUL data

Segment resSeg follows=.text align=1 vfollows=dSeg valign=1 
;-----------------------------------:
;       Misc System routines        :
;-----------------------------------:
findLRUBuffer: 
;Finds least recently used buffer, links it and returns ptr to it in rbx
;Input: Nothing
;Output: rbx = Pointer to the buffer to use
    push rdx
    mov rbx, qword [bufHeadPtr]
    cmp qword [rbx + bufferHdr.nextBufPtr], -1  ;Check if 1st entry is last
    jne .flb1
    pop rdx
    ret
.flb1:
    mov rdx, rbx    ;Save a ptr to the previous buffer header
    mov rbx, qword [rdx + bufferHdr.nextBufPtr] ;Get next buffer header ptr
    cmp qword [rbx + bufferHdr.nextBufPtr], -1 ;Check if at LRU buffer
    jne .flb1   ;If not LRU, keep walking, else process
    mov qword [rdx + bufferHdr.nextBufPtr], -1  ;Make prev node the LRU node
    mov rdx, qword [bufHeadPtr]    ;Now copy old MRU buffer ptr to rdx
    mov qword [bufHeadPtr], rbx    ;Sysvars to point to new buffer
    mov qword [rbx + bufferHdr.nextBufPtr], rdx
    pop rdx
    ret

findDPB:
;Finds the DPB for a given drive
;Input:   dl = Drive number (0=A, 1=B etc...)
;Output: al = 00, rbx = Pointer to the DPB
;        al = -1, Failed, no DPB for device, rbx destroyed
    mov rbx, qword [dpbHeadPtr]
.fd1:
    xor al, al
    cmp byte [rbx + dpb.bDriveNumber], dl
    je .fd2
    mov rbx, qword [rbx + dpb.qNextDPBPtr]
    mov al, -1
    cmp rbx, -1 ;If rbx followed last item in list, no DPB exists for dl
    jne .fd1
.fd2:
    ret
callCritError:
;Common Procedure to swap stacks and call Critical Error Interrupt
;-----------------------------------:
;       File System routines        :
;-----------------------------------:
name2Clust:
;Converts a file name to a first cluster number
;-----------------------------------:
;        Interrupt routines         :
;-----------------------------------:
terminateProcess:   ;Int 40h

functionDispatch:   ;Int 41h Main function dispatcher
;ah = Function number, all other registers have various meanings
    cli ;Halt external interrupts
    cld ;Ensure all string ops occur in the right direction
    cmp ah, dispatchTableL/8    ;Number of functions
    ja .fdExitBad
    ;Cherry pick functions
    cmp ah, 33h ;CTRL+BREAK check
    jb .fdInInt41   ;If below skip these checks
    je .ctrlBreakCheck
    cmp ah, 64h
    je .setDriverLookahead  ;Reserved, but avoids usual Int 41h spiel
    ja .fdInInt41   ;If above, do usual Int41 entry
    cmp ah, 51h
    je .getCurrProcessID    ;This an below are exactly the same
    cmp ah, 62h
    je .getPSPaddr          ;Calls the above function
    cmp ah, 50h
    je .setCurrProcessID
.fdInInt41:
    pushDOS ;Push the usual prologue registers
    mov rax, qword [oldRSP]
    mov qword [oldoldRSP], rax
    inc byte [inDOS]    ;Increment in DOS flag
    mov qword [oldRSP], rsp
    pop rax     ;Get old rax back
    push rax    ;and push it back onto the stack
    ;Here, we want to save oldRSP in the callers PSP
    lea rsp, critStakTop
    sti         ;Reenable interrupts

    mov byte [int48Flag], 1 ;Make it ok to trigger Int 48h

    mov qword [oldRBX], rbx ;Need to do this as I might switch stacks later
    movzx ebx, ah   ;Move the function number bl zero extended to rbx
    shl ebx, 3      ;Multiply the function number by 8 for offset into table
    push rax        ;Push rax onto the stack
    lea rax, qword [.dispatchTable]
    add rbx, rax    ;Add dispatch table offset into rbx
    pop rax
    mov rbx, qword [rbx]    ;Get the address from the dispatch table

    test ah, ah     ;Simple Terminate function?
    jz .fddiskOp
    cmp ah, 59h     ;Extended Error report?
    je .fdGoToFunction  ;Bypass code that clears the error report
    cmp ah, 0Ch     ;Are we a char function?
    ja .fddiskOp
;Char operations here
    test byte [critErrFlag], 1  ;Are we in critical error?
    jnz .fdGoToFunction         ;If we are, stay on Critical Error Stack
    lea rsp, IOStakTop          ;Otherwise, switch to IO stack
    jmp short .fdGoToFunction
.fddiskOp:
    ;Disk operations go here
    ;Clear up error info
    mov byte [errorLocus], 1    ;Reset to generic, unknown locus
    mov byte [critErrFlag], 0   ;Clear the Critical Error Flag
    mov byte [errorDrv], -1     ;Set the drive which caused the error to none

    mov byte [int48Flag], 0     ;Turn off the ability to trigger Int 48h
    lea rsp, DiskStakTop        ;Swap the stack to the Disk Transfer Stack
    test byte [breakFlag], -1   ;Test if set
    jz .fdGoToFunction
; HANDLE CTRL+BREAK HERE!
.fdGoToFunction:
    xchg rbx, qword [oldRBX]    ;Put the call addr in oldRBX and get oldRBX back
    ;Potentially point rbp to caller reg frame for easy access of registers 
    ;mov rbp, qword [oldRSP]    ;Move rsp on entry into rbp
    call qword [oldRBX]     ;Call the desired function, rax contains ret code
.fdExit:
    cli     ;Redisable interrupts
    ;???
    dec byte [inDOS]            ;Decrement the inDOS count
    mov rsp, qword [oldRSP]     ;Point rsp to old stack
    mov qword [rsp], rax    ;Put the ret code into its pos on the register frame
    mov rax, qword [oldoldRSP]
    mov qword [oldRSP], rax
    popDOS  ;Pop the frame
    iretq
.fdExitBad:
    mov ah, 0
    iretq
.simpleTerminate:     ;ah = 00h
    ret
.stdinReadEcho:     ;ah = 01h
    xor ah, ah
    int 36h
    int 49h ;Pass al to fast output
    ret
.stdoutWrite:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    push rax
    mov al, dl
    int 49h
    pop rax
    ret
.stdauxRead:        ;ah = 03h
.stdauxWrite:       ;ah = 04h
.stdprnWrite:       ;ah = 05h
.directCONIO:       ;ah = 06h
.waitDirectInNoEcho:;ah = 07h
.waitStdinNoEcho:   ;ah = 08h
    ret
.printString:       ;ah = 09h
    push rax
    push rdx
.ps0:
    mov al, byte [rdx]
    cmp al, "$"
    je .ps1
    inc rdx ;Goto next char
    int 49h ;Print char in al
    jmp short .ps0
.ps1:
    pop rdx
    pop rax
    ret
.buffStdinInput:    ;ah = 0Ah
.checkStdinStatus:  ;ah = 0Bh
.clearbuffDoFunc:   ;ah = 0Ch
.diskReset:         ;ah = 0Dh
.selectDisk:        ;ah = 0Eh
.openFileFCB:       ;ah = 0Fh
.closeFileFCB:      ;ah = 10h
.findFirstFileFCB:  ;ah = 11h
.findNextFileFCB:   ;ah = 12h
.deleteFileFCB:     ;ah = 13h
.sequentialReadFCB: ;ah = 14h
.sequentialWriteFCB:;ah = 15h
.createFileFCB:     ;ah = 16h
.renameFileFCB:     ;ah = 17h
                    ;ah = 18h unused
.getCurrentDisk:       ;ah = 19h, get current default drive
.setDTA:            ;ah = 1Ah
.FATinfoDefault:    ;ah = 1Bh
.FatinfoDevice:     ;ah = 1Ch
                    ;ah = 1Dh unused
                    ;ah = 1Eh unused
.getCurrentDPBptr:  ;ah = 1Fh, simply calls int 41h ah = 32h with dl = 0
                    ;ah = 20h unused
.randomReadFCB:     ;ah = 21h
.randomWriteFCB:    ;ah = 22h
.getFileSizeFCB:    ;ah = 23h
.setRelRecordFCB:   ;ah = 24h
.setIntVector:      ;ah = 25h
.createNewPSP:      ;ah = 26h
.randBlockReadFCB:  ;ah = 27h
.randBlockWriteFCB: ;ah = 28h
.parseFilenameFCB:  ;ah = 29h
.getDate:           ;ah = 2Ah
.setDate:           ;ah = 2Bh
.getTime:           ;ah = 2Ch
.setTime:           ;ah = 2Dh
.setResetVerify:    ;ah = 2Eh, turns ALL writes to write + verify
.getDTA:            ;ah = 2Fh
.getDOSversion:     ;ah = 30h
.terminateStayRes:  ;ah = 31h
.getDeviceDPBptr:   ;ah = 32h
.ctrlBreakCheck:    ;ah = 33h
.getInDOSflagPtr:   ;ah = 34h
.getIntVector:      ;ah = 35h
.getDiskFreeSpace:  ;ah = 36h
.getsetSwitchChar:  ;ah = 37h, allows changing default switch from / to anything
.getsetCountryInfo: ;ah = 38h, localisation info
.makeDIR:           ;ah = 39h
.removeDIR:         ;ah = 3Ah
.changeCurrentDIR:  ;ah = 3Bh, changes directory for current drive
.createFileHdl:     ;ah = 3Ch, handle function
.openFileHdl:       ;ah = 3Dh, handle function
.closeFileHdl:      ;ah = 3Eh, handle function
.readFileHdl:       ;ah = 3Fh, handle function
.writeFileHdl:      ;ah = 40h, handle function
.deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
.movFileReadPtr:    ;ah = 42h, handle function, LSEEK
.changeFileModeHdl: ;ah = 43h, handle function, CHMOD
.ioctrl:            ;ah = 44h, handle function
.duplicateHandle:   ;ah = 45h, handle function
.forceDuplicateHdl: ;ah = 46h, handle function
.getCurrentDIR:     ;ah = 47h
.allocateMemory:    ;ah = 48h
.freeMemory:        ;ah = 49h
.reallocMemory:     ;ah = 4Ah
.loadExecChild:     ;ah = 4Bh, EXEC
.terminateClean:    ;ah = 4Ch, EXIT
.getRetCodeChild:   ;ah = 4Dh, WAIT, get ret code of subprocess
.findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
.findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
.setCurrProcessID:  ;ah = 50h, set current process ID
.getCurrProcessID:  ;ah = 51h, get current process ID
.getSysVarsPtr:     ;ah = 52h
.createDPB:         ;ah = 53h, generates a DPB from a given BPB
.getVerifySetting:  ;ah = 54h
.createPSP:         ;ah = 55h, creates a PSP for a program
.renameFile:        ;ah = 56h
.getSetFileDateTime:;ah = 57h
.getsetMallocStrat: ;ah = 58h
.getExtendedError:  ;ah = 59h
.createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
.createNewFile:     ;ah = 5Bh
.lockUnlockFile:    ;ah = 5Ch
.getCritErrorInfo:  ;ah = 5Dh
.networkServices:   ;ah = 5Eh, do nothing
.networkRedirection:;ah = 5Fh, do nothing
.trueName:          ;ah = 60h, get fully qualified name
                    ;ah = 61h, reserved
.getPSPaddr:        ;ah = 62h, gives PSP addr/Process ID
                    ;ah = 63h, reserved
.setDriverLookahead:;ah = 64h, reserved
.getExtLocalInfo:   ;ah = 65h, Get Extended Country Info
.getsetGlobalCP:    ;ah = 66h, Get/Set Global Codepage, reserved
.setHandleCount:    ;ah = 67h
.commitFile:        ;ah = 68h, flushes buffers for handle to disk 
.getsetDiskSerial:  ;ah = 69h, get/set disk serial number
.return:
    ret


.dispatchTable:
    dq .simpleTerminate     ;AH = 00H, PROCESS MANAGEMENT
    dq .stdinReadEcho       ;AH = 01H, CHAR IO
    dq .stdoutWrite         ;AH = 02H, CHAR IO
    dq .stdauxRead          ;AH = 03H, CHAR IO
    dq .stdauxWrite         ;AH = 04H, CHAR IO
    dq .stdprnWrite         ;AH = 05H, CHAR IO
    dq .directCONIO         ;AH = 06H, CHAR IO
    dq .waitDirectInNoEcho  ;AH = 07H, CHAR IO
    dq .waitStdinNoEcho     ;AH = 08H, CHAR IO
    dq .printString         ;AH = 09H, CHAR IO
    dq .buffStdinInput      ;AH = 0AH, CHAR IO
    dq .checkStdinStatus    ;AH = 0BH, CHAR IO
    dq .clearbuffDoFunc     ;AH = 0CH, CHAR IO
    dq .diskReset           ;AH = 0DH, DISK MANAGEMENT
    dq .selectDisk          ;AH = 0EH, DISK MANAGEMENT
    dq .openFileFCB         ;AH = 0FH, FILE OPERATION       FCB
    dq .closeFileFCB        ;AH = 10H, FILE OPERATION       FCB
    dq .findFirstFileFCB    ;AH = 11H, FILE OPERATION       FCB
    dq .findNextFileFCB     ;AH = 12H, FILE OPERATION       FCB
    dq .deleteFileFCB       ;AH = 13H, FILE OPERATION       FCB
    dq .sequentialReadFCB   ;AH = 14H, RECORD OPERATION     FCB
    dq .sequentialWriteFCB  ;AH = 15H, RECORD OPERTAION     FCB
    dq .createFileFCB       ;AH = 16H, FILE OPERATION       FCB
    dq .renameFileFCB       ;AH = 17H, FILE OPERATION       FCB
    dq .return              ;AH = 18H, RESERVED
    dq .getCurrentDisk      ;AH = 19H, DISK MANAGEMENT
    dq .setDTA              ;AH = 1AH, RECORD OPERATION     F/H
    dq .FATinfoDefault      ;AH = 1BH, DISK MANAGEMENT
    dq .FatinfoDevice       ;AH = 1CH, DISK MANAGEMENT
    dq .return              ;AH = 1DH, RESERVED
    dq .return              ;AH = 1EH, RESERVED
    dq .getCurrentDPBptr    ;AH = 1FH, RESERVED INTERNAL, GET CURR DRIVE DPB PTR
    dq .return              ;AH = 20H, RESERVED
    dq .randomReadFCB       ;AH = 21H, RECORD OPERATION     FCB
    dq .randomWriteFCB      ;AH = 22H, RECORD OPERATION     FCB
    dq .getFileSizeFCB      ;AH = 23H, FILE OPERATION       FCB
    dq .setRelRecordFCB     ;AH = 24H, RECORD OPERATION     FCB
    dq .setIntVector        ;AH = 25H, MISC. SYS. FUNCTION
    dq .createNewPSP        ;AH = 26H, PROCESS MANAGEMENT
    dq .randBlockReadFCB    ;AH = 27H, RECORD OPERATION     FCB
    dq .randBlockWriteFCB   ;AH = 28H, RECORD OPERATION     FCB
    dq .parseFilenameFCB    ;AH = 29H, FILE OPERATION       FCB
    dq .getDate             ;AH = 2AH, TIME AND DATE
    dq .setDate             ;AH = 2BH, TIME AND DATE
    dq .getTime             ;AH = 2CH, TIME AND DATE
    dq .setTime             ;AH = 2DH, TIME AND DATE
    dq .setResetVerify      ;AH = 2EH, DISK MANAGEMENT
    dq .getDTA              ;AH = 2FH, RECORD OPERATION     F/H
    dq .getDOSversion       ;AH = 30H, MISC. SYS. FUNCTION
    dq .terminateStayRes    ;AH = 31H, PROCESS MANAGEMENT
    dq .getDeviceDPBptr     ;AH = 32H, RESERVED INTERNAL, GET DEVICE DPB PTR
    dq .ctrlBreakCheck      ;AH = 33H, MISC. SYS. FUNCTION
    dq .getInDOSflagPtr     ;AH = 34H, RESERVED INTERNAL, GET PTR TO INDOS FLAG
    dq .getIntVector        ;AH = 35H, MISC. SYS. FUNCTION
    dq .getDiskFreeSpace    ;AH = 36H, DISK MANAGEMENT
    dq .getsetSwitchChar    ;AH = 37H, RESERVED INTERNAL, CHANGE SWITCH CHAR
    dq .getsetCountryInfo   ;AH = 38H, MISC. SYS. FUNCTION
    dq .makeDIR             ;AH = 39H, DIRECTORY OPERATION
    dq .removeDIR           ;AH = 3AH, DIRECTORY OPERATION
    dq .changeCurrentDIR    ;AH = 3BH, DIRECTORY OPERATION
    dq .createFileHdl       ;AH = 3CH, FILE OPERATION       HANDLE
    dq .openFileHdl         ;AH = 3DH, FILE OPERATION       HANDLE
    dq .closeFileHdl        ;AH = 3EH, FILE OPERATION       HANDLE
    dq .readFileHdl         ;AH = 3FH, RECORD OPERATION     HANDLE
    dq .writeFileHdl        ;AH = 40H, RECORD OPERATION     HANDLE
    dq .deleteFileHdl       ;AH = 41H, FILE OPERATION       HANDLE
    dq .movFileReadPtr      ;AH = 42H, RECORD OPERATION     HANDLE
    dq .changeFileModeHdl   ;AH = 43H, FILE OPERATION       HANDLE
    dq .ioctrl              ;AH = 44H, MISC. SYS. FUNCTION
    dq .duplicateHandle     ;AH = 45H, FILE OPERATION       HANDLE
    dq .forceDuplicateHdl   ;AH = 46H, FILE OPERATION       HANDLE
    dq .getCurrentDIR       ;AH = 47H, DIRECTORY OPERATION
    dq .allocateMemory      ;AH = 48H, MEMORY MANAGEMENT
    dq .freeMemory          ;AH = 49H, MEMORY MANAGEMENT
    dq .reallocMemory       ;AH = 4AH, MEMORY MANAGEMENT
    dq .loadExecChild       ;AH = 4BH, PROCESS MANAGEMENT
    dq .terminateClean      ;AH = 4CH, PROCESS MANAGEMENT
    dq .getRetCodeChild     ;AH = 4DH, PROCESS MANAGEMENT
    dq .findFirstFileHdl    ;AH = 4EH, FILE OPERATION       HANDLE
    dq .findNextFileHdl     ;AH = 4FH, FILE OPERATION       HANDLE
    dq .setCurrProcessID    ;AH = 50H, RESERVED INTERNAL, SET CURRENT PROCESS ID
    dq .getCurrProcessID    ;AH = 51H, RESERVED INTERNAL, GET CURRENT PROCESS ID
    dq .getSysVarsPtr       ;AH = 52H, RESERVED INTERNAL, GET SYSVARS POINTER
    dq .createDPB           ;AH = 53H, RESERVED INTERNAL, TRANSLATE A BPB TO DPB
    dq .getVerifySetting    ;AH = 54H, DISK MANAGEMENT
    dq .createPSP           ;AH = 55H, RESERVED INTERNAL, CREATE A PSP
    dq .renameFile          ;AH = 56H, FILE OPERATION       HANDLE
    dq .getSetFileDateTime  ;AH = 57H, FILE OPERATION       HANDLE
    dq .getsetMallocStrat   ;AH = 58H, MEMORY MANAGEMENT
    dq .getExtendedError    ;AH = 59H, MISC. SYS. FUNCTION
    dq .createUniqueFile    ;AH = 5AH, FILE OPERATION       HANDLE
    dq .createNewFile       ;AH = 5BH, FILE OPERATION       HANDLE
    dq .lockUnlockFile      ;AH = 5CH, RECORD OPERATION     HANDLE
    dq .getCritErrorInfo    ;AH = 5DH, RESERVED INTERNAL, GET CRIT. ERROR DATA
    dq .networkServices     ;AH = 5EH, RESERVED NETWORK FUNCTION
    dq .networkRedirection  ;AH = 5FH, RESERVED NETWORK FUNCTION
    dq .trueName            ;AH = 60H, RESERVED INTERNAL, GET TRUE NAME
    dq .return              ;AH = 61H, RESERVED
    dq .getPSPaddr          ;AH = 62H, PROCESS MANAGEMENT
    dq .return              ;AH = 63H, RESERVED
    dq .setDriverLookahead  ;AH = 64H, RESERVED INTERNAL, DRIVER LOOKAHEAD
    dq .getExtLocalInfo     ;AH = 65H, MISC. SYS. FUNCTION
    dq .getsetGlobalCP      ;AH = 66H, MISC. SYS. FUNCTION
    dq .setHandleCount      ;AH = 67H, FILE OPERAITON       F/H
    dq .commitFile          ;AH = 68H, FILE OPERATION       HANDLE
    dq .getsetDiskSerial    ;AH = 69H, RESERVED INTERNAL, GET/SET DISK SER. NUM
dispatchTableL  equ $ - .dispatchTable 

terminateHandler:   ;Int 42h
ctrlCHandler:       ;Int 43h
critErrorHandler:   ;Int 44h
;User Stack in usage here, must be swapped to before this is called
;Entered with:  
;               AH = Critical Error Bitfield
;               Bit 7 = 0 - Disk Error, Bit 7 = 1 - Char Device Error
;               Bit 6 - Reserved
;               Bit 5 = 0 - IGNORE not allowed, Bit 5 = 1 - IGNORE allowed
;               Bit 4 = 0 - RETRY not allowed, Bit 4 = 1 - RETRY allowed
;               Bit 3 = 0 - FAIL not allowed, Bit 3 = 1 - FAIL allowed
;               Bits [2-1] = Affected Disk Error
;                     0 0   DOS area
;                     0 1   FAT area
;                     1 0   Directory area
;                     1 1   Data area
;               Bit 0 = 0 - Read Operation, Bit 0 = 1 - Write Operation
;               AL  = Failing drive number if AH[7] = 0
;               DIL = Error code for errorMsg
;               RSI = EA of Device Header for which device the error occured
;Return:
;               AL = 0 - Ignore the Error       (Ignore)
;                  = 1 - Retry the Operation    (Retry)
;                  = 2 - Terminate the Program  (Abort)
;                  = 3 - Fail the DOS call      (Fail)
    push rbx
    push rcx
    push rdx
    push rdi
    push rsi
    cld         ;Make String ops go forward

    mov bx, ax  ;Save ah in bh and al in bl (if needed)
    lea rdx, qword [.crlf]
    mov ah, 09h ;Print String
    int 41h     ;Call DOS to print CRLF part of message

    and edi, 00FFh   ;Zero the upper bytes of DI just in case
    mov ecx, 0Ch
    cmp edi, ecx  ;Check if the error number is erroniously above Gen Error
    cmova edi, ecx  ;If it is, move Gen Error into edi
    movzx rdi, di
    mov rdx, rdi    ;Copy error code
    shl rdi, 4  ;Multiply by 16
    shl rdx, 1  ;Multiply by 2
    add rdi, rdx    ;Add the resultant multiplications
    lea rdx, qword [.errorMsgTable]
    lea rdx, qword [rdx+rdi]   ;Load EA to rdx
    mov ah, 09h ;Print String
    int 41h     ;Call DOS to print first part of message

    lea rdx, qword [.readmsg]
    lea rdi, qword [.writemsg]
    test bh, 1  ;Bit 0 is set if write operation
    cmovnz rdx, rdi ;Move the correct r/w part of the message to rdx
    mov ah, 09h ;Print String
    int 41h     ;Call DOS to print error reading/writing portion

    test bh, 80h    ;Test bit 7 for char/Disk assertation
    jnz .charError
;Disk error continues here
    lea rdx, qword [.drive] ;Drive message
    mov ah, 09h
    int 41h
    mov dl, bl  ;Get zero based drive number into dl
    add dl, "A" ;Add ASCII code
    mov ah, 02h ;Print char in dl
    int 41h
.userInput:
    lea rdx, qword [.crlf]  ;Print new line
    mov ah, 09h
    int 41h
;Abort, Retry, Ignore, Fail is word order
;Last message gets a ?, otherwise a comma followed by a 20h (space)
.userAbort:
;Abort is always an option
    lea rdx, qword [.abortmsg]
    mov ah, 09h
    int 41h ;Call DOS to prompt user for ABORT option
.userRetry:
    test bh, 10h  ;Bit 4 is retry bit
    jz .userIgnore    ;If clear, dont print message
    lea rdx, qword [.betweenMsg]
    mov ah, 09h
    int 41h
    lea rdx, qword [.retrymsg]
    mov ah, 09h
    int 41h
.userIgnore:
    test bh, 20h    ;Bit 5 is ignore bit
    jz .userFail
    lea rdx, qword [.betweenMsg]
    mov ah, 09h
    int 41h
    lea rdx, qword [.ignoremsg]
    mov ah, 09h
    int 41h
.userFail:
    test bh, 08h    ;Bit 3 is Fail bit
    jz .userMsgEnd
    lea rdx, qword [.betweenMsg]
    mov ah, 09h
    int 41h
    lea rdx, qword [.failmsg]
    mov ah, 09h
    int 41h
.userMsgEnd:
    lea rdx, qword [.endMsg]
    mov ah, 09h
    int 41h
;Get user input now 
    xor ecx, ecx  ;4 Possible Responses
    lea rdi, qword [.responses] ;Go to start of string
    mov ah, 01h ;STDIN without Console Echo
    int 41h ;Get char in al
    cmp al, "a" ;Chack if lowercase
    jb .uip1    ;If the value is below, ignore subtraction
    sub al, "a"-"A"  ;Turn the char into uppercase
.uip1:
    scasb   ;Compare char to list, offset gives return code
    je .validInput  ;If they are equal, ecx has return code
    inc ecx
    cmp ecx, 4
    jne .uip1
    jmp .userInput ;If valid char not found, keep waiting 
.validInput:
    mov al, cl  ;Move the offset into .responses into al
;Now check if the input is permitted
    cmp al, 2   ;Check if abort, abort always permitted
    je .cehExit
    test al, al ;Check if 0 => Ignore
    je .viIgnore
    cmp al, 1   ;Check if 1 => Retry
    je .viRetry
.viFail:    ;Fallthrough for fail (al = 3)
    test bh, 8  ;Bit 3 is Fail bit
    jz .userInput  ;If bit 3 is zero, prompt and get input again
    jmp short .cehExit
.viIgnore:
    test bh, 20h    ;Bit 5 is Ignore bit
    jz .userInput
    jmp short .cehExit
.viRetry:
    test bh, 10h    ;Bit 4 is Retry bit
    jz .userInput
.cehExit:
    pop rsi
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    iretq
.charError:
    mov ecx, 8  ;8 chars in device name
    add rsi, drvHdr.drvNam  ;Get the address of the Drive name
.ce1:
    lodsb   ;Get a string char into al and inc rsi
    mov dl, al  ;Move char into dl
    mov ah, 02h
    int 41h ;Print char
    loop .ce1   ;Keep looping until all 8 char device chars have been printed
    jmp .userInput

.errorMsgTable: ;Each table entry is 18 chars long
            db "Write Protect $   "       ;Error 0
            db "Unknown Unit $    "       ;Error 1
            db "Not Ready $       "       ;Error 2
            db "Unknown Command $ "       ;Error 3
            db "Data $            "       ;Error 4
            db "Bad Request $     "       ;Error 5
            db "Seek $            "       ;Error 6
            db "Unknown Media $   "       ;Error 7
            db "Sector Not Found $"       ;Error 8
            db "Out Of Paper $    "       ;Error 9
            db "Write Fault $     "       ;Error A
            db "Read Fault $      "       ;Error B
            db "General Failure $ "       ;Error C

.drive      db "drive $"
.readmsg    db "error reading $"
.writemsg   db "error writing $"
.crlf       db 0Ah, 0Dh, "$"
.abortmsg   db "Abort$" 
.ignoremsg  db "Ignore$"
.retrymsg   db "Retry$"
.failmsg    db "Fail$"
.betweenMsg db ", $"
.endMsg     db "? $"
.responses  db "IRAF"   ;Abort Retry Ignore Fail
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address
;ecx = Number of sectors to read (max 255 for now)
;rdx = Start LBA to read from
    movzx rax, al   ;Zero extend DOS drive number 
    mov al, byte [msdDriver.msdBIOSmap + rax] ;Get translated BIOS num into al
    xchg rax, rcx
    xchg rcx, rdx
    mov ah, 82h
    int 33h
    iretq
absDiskWrite:       ;Int 46h
    movzx rax, al   ;Zero extend DOS drive number 
    mov al, byte [msdDriver.msdBIOSmap + rax] ;Get translated BIOS num into al
    xchg rax, rcx
    xchg rcx, rdx
    mov ah, 83h
    int 33h
    iretq
terminateResident:  ;Int 47h
inDosHandler:       ;Int 48h
;Called when DOS idle
    iretq
fastOutput:         ;Int 49h
;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
passCommand:        ;Int 4Eh, hooked by COMMAND.COM
    iretq
multiplex:          ;Int 4Fh, kept as iretq for now
    iretq
;-----------------------------------:
;          Driver routines          :
;-----------------------------------:
drivers:
conHdr:
    dq auxHdr
    dw 0813h
    dq commonStrat
    dq conDriver
    db "CON     "
auxHdr:
    dq prnHdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "AUX     "
prnHdr:
    dq clkHdr
    dw 0A040h
    dq commonStrat
    dq lpt1Hdr
    db "PRN     "
clkHdr:
    dq msdHdr
    dw 08008h
    dq commonStrat
    dq clkDriver
    db "CLOCK$  "
msdHdr:
    dq com1Hdr
    dw 00800h   ;Once Generic IO implemented, change to 00840h
    dq commonStrat
    dq msdDriver
    db 0,0,0,0,0,0,0,0
com1Hdr:
    dq com2Hdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "COM1    "
com2Hdr:
    dq com3Hdr
    dw 08000h
    dq commonStrat
    dq com2Intr
    db "COM2    "
com3Hdr:
    dq com4Hdr
    dw 08000h
    dq commonStrat
    dq com3Intr
    db "COM3    "
com4Hdr:
    dq lpt1Hdr
    dw 08000h
    dq commonStrat
    dq com4Intr
    db "COM4    "
lpt1Hdr:
    dq lpt2Hdr
    dw 0A040h
    dq commonStrat
    dq lptDriver
    db "LPT1    "
lpt2Hdr:
    dq lpt3Hdr
    dw 0A040h
    dq commonStrat
    dq lptDriver
    db "LPT2    "
lpt3Hdr:
    dq -1
    dw 0A040h
    dq commonStrat
    dq lptDriver
    dq "LPT3    "

commonStrat:
;DOS calls this function with rbx=Ptr to request header
    mov qword [reqHdrPtr], rbx
    ret
reqHdrPtr  dq 0    ;Where the default device drivers store the ReqPtr

nulStrat:
    mov word [rbx + drvReqHdr.status], 0100h    ;Set done bit directly
nulIntr:
    ret

conDriver:
    push rax
    push rbx
    mov rbx, qword [reqHdrPtr]
    mov al, byte [rbx + drvReqHdr.cmdcde]
    test al, al
    jz conInit
    cmp al, 4
    jz conRead
    cmp al, 5
    jz conNondestructiveRead
    cmp al, 6
    jz conExit
    cmp al, 7
    jz conFlushInputBuffers
    cmp al, 8
    jz conWrite
    cmp al, 9
    jz conWrite
;All other cases fall through here
conExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rbx
    pop rax
    ret
conInit:    ;Function 0
    push rdx
    ;Flush keyboard buffer
.ci0:
    mov ah, 01      ;Get buffer status
    int 36h
    jz .ci1      ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short .ci0
.ci1:
    mov eax, 0500h  ;Set page zero as the default page
    int 30h
    mov ah, 02h
    xor edx, edx    ;Set screen cursor to top right corner
    mov bh, dl      ;Set cursor for page 0
    int 30h
    mov bh, 07h     ;Grey/Black attribs
    mov eax, 0600h  ;Clear whole screen
    int 30h
    pop rdx
    jmp short conExit
conIORead:
    mov word [rbx + drvReqHdr.status], 8003h    ;Error, unknown command!
    jmp short conExit
conRead:    ;Function 4
    push rdi
    push rcx
    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cr1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
    xor eax, eax
    int 36h
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short .cr1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rdi
    jmp short conExit
conNondestructiveRead:  ;Function 5
    mov ah, 01h     ;Get key if exists
    int 36h
    jz .cnr           ;If zero clear => no key, go forwards
    ;Keystroke available
    mov byte [rbx + nonDestInNoWaitReqPkt.retbyt], al   ;Move char in al
    jmp short conExit
.cnr: ;No keystroke available
    mov word [rbx + nonDestInNoWaitReqPkt.status], 0300h   ;Set busy bit
    jmp short conExit
conFlushInputBuffers:   ;Function 7
    mov ah, 01      ;Get buffer status
    int 36h
    jz conExit      ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short conFlushInputBuffers
conWrite:   ;Function 8 and 9
    push rsi
    push rcx
    mov rsi, qword [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    int 49h ;Fast print char
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rsi
    jmp conExit

clkDriver:

comDriver:
com1Intr:
    mov byte [comDevice], 0
    jmp short comIntr
com2Intr:
    mov byte [comDevice], 1
    jmp short comIntr
com3Intr:
    mov byte [comDevice], 2
    jmp short comIntr
com4Intr:
    mov byte [comDevice], 3
comIntr:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    mov rbx, qword [reqHdrPtr]
    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 4
    jz comRead
    cmp al, 5
    jz comNondestructiveRead
    cmp al, 8
    jz comWrite
    cmp al, 9
    jz comWrite
;All other cases fall through here
comExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
comRead:
    push rdi
    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cr1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
    mov eax, 02h    ;Recieve 
    mov dx, word [comDevice]    ;Get transacting com device
    int 34h ;Recieve Char
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short .cr1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rdi
    jmp short comExit
comNondestructiveRead:
    mov word [rbx + nonDestInNoWaitReqPkt.status], 0200h    ;Set busy bit 
    jmp short comExit
comWrite:
 mov rsi, qword [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    mov ah, 01h ;Move function number into ah
    mov dx, word [comDevice]
    int 34h ;Transmit char
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    jmp short comExit
comDevice   db 0

lptDriver:    ;Drivers for LPT 1, 2, 3
    push rdi
    mov rdi, qword [reqHdrPtr]
    mov word [rdi + drvReqHdr.status], 0100h    ;Done bit set
    pop rdi
    ret

msdDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    mov rbx, qword [reqHdrPtr]  ;Get the ptr to the req header in rbx
    mov al, byte [rbx + drvReqHdr.cmdcde]   ;Get command code in al
    cmp al, 24  ;Check cmd num is valid
    ja .msdError
    test al, al
    jz .msdInit
    cmp al, 01
    jz .msdMedChk
    cmp al, 02
    jz .msdBuildBPB
    cmp al, 03
    jz .msdIOCTLRead
    cmp al, 04
    jz .msdRead
    cmp al, 08
    jz .msdWrite
    cmp al, 09
    jz .msdWriteVerify
    cmp al, 12
    jz .msdIOCTLWrite
    cmp al, 13
    jz .msdDevOpen
    cmp al, 14
    jz .msdDevClose
    cmp al, 15
    jz .msdRemovableMedia
    cmp al, 19
    jz .msdGenericIOCTL
    cmp al, 23
    jz .msdGetLogicalDev
    cmp al, 24
    jz .msdSetLogicalDev
.msdError:
.msdDriverExit:
    or word [rbx + drvReqHdr.status], 0100h ;Set done bit
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.msdInit:            ;Function 0
    int 31h ;Get number of Int 33h devices in r8b
    movzx r8, r8b   ;Keeps real count
    mov eax, r8d
    cmp al, 1
    ja .mi1
    inc al ;Make it two
.mi1:
    mov edx, 5
    cmp eax, edx
    cmova eax, edx  ;If num of drives is greater than 5, consider only first 5
    mov byte [msdHdr + drvHdr.drvNam], al ;Save num of drvs in drvr hdr
    mov byte [rbx + initReqPkt.numunt], al ;And in req packet
    add byte [numMSDdrv], r8b ;Add the true number of devices to total
    xor ebp, ebp    ;Use bpl as device counter, cmp to r8b
    lea rdi, qword [.msdBPBblks]
    push rbx
.mi2:
    mov edx, ebp
    lea rbx, qword [driverDataPtr]  ;Get effective address of scratch space
    xor ecx, ecx    ;Sector 0
    mov eax, 8201h       ;Read 1 sector
    int 33h
    jc .msdInitError

    lea rsi, qword [driverDataPtr]  ;Point to start of data
    mov ecx, bpbEx_size/8
    rep movsq   ;Move the BPB data into the right block

    inc ebp
    cmp rbp, r8 ;Have we written the BPB for all physical drives?
    jne .mi2  ;No? Go again

    lea rdi, qword [.msdBPBTbl]  ;Point to start of table
    lea rdx, qword [.msdBPBblks]
.mi3:
    mov qword [rdi], rdx   ;Move the block entry ptr to rdi
    add rdx, bpbEx_size      ;Make rdx point to the next block entry
    dec ebp
    jnz .mi3  ;If not zero yet, go again

    pop rbx
    lea rdx, qword [.msdBPBTbl]  ;Get far pointer 
    mov qword [rbx + initReqPkt.optptr], rdx  ;Save ptr to array
    lea rdx, qword [driverDataPtr]
    mov qword [rbx + initReqPkt.endptr], rdx    ;Save free space ptr
    jmp .msdDriverExit
.msdInitError:
    pop rbx
    jmp .msdDriverExit
.msdMedChk:          ;Function 1
;Once the BIOS function is implmented that reads the changeline, use that!
;For BIOSes that dont support the changeline, the following procedure will 
; suffice.
    movzx rax, byte [rbx + mediaCheckReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]    ;Translate unitnum to BIOS num
    test dl, 80h    ;If it is a fixed disk, no change!
    jnz .mmcNoChange
;Now we test Media Descriptor
    mov dl, byte [rbx + mediaCheckReqPkt.medesc]    ;Media descriptor
    mov rdi, qword [.msdBPBTbl + 8*rax]
    mov rdi, qword [rdi]    ;Dereference rdi
    cmp byte [rdi + bpb32.media], dl    ;Compare media descriptor bytes
    je .mmcUnsure
.mmcChange: ;Fail safe, always assume the device has changed
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    mov qword [rbx + mediaCheckReqPkt.desptr], .msdDefLabel ;Temp, ret def label
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    jmp .msdDriverExit
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    jmp .msdDriverExit

.msdBuildBPB:        ;Function 2
    mov rsi, rbx
    movzx rax, byte [rsi + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer
    xor ecx, ecx    ;Read Sector 0
    mov eax, 8201h  ;LBA Read 1 sector
    int 33h
    jc .mbbpbError
    xchg rbx, rsi    ;Transf Buf(rbx) <-> ReqHdr(rsi)
    movzx rax, byte [rbx + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov rdi, qword [.msdBPBTbl + 8*rax] ;Get pointer to pointer to buffer
    mov rdi, qword [rdi] ;Dereference to get pointer to buffer 
    mov qword [rbx + bpbBuildReqPkt.bpbptr], rdi ;rdi -> final bpb resting place
    mov ecx, bpbEx_size/8
    rep movsq   ;Move the BPB data into the right space
    jmp .msdDriverExit
.mbbpbError:
.msdIOCTLRead:       ;Function 3, returns done
    jmp .msdDriverExit
.msdRead:            ;Function 4
    mov rbp, rbx
    mov ah, 82h ;LBA Read Sectors
    call .msdBlkIOCommon
    mov rbx, rbp
    jmp .msdDriverExit
.msdWrite:           ;Function 8
    mov rbp, rbx
    mov ah, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    mov rbx, rbp
    jmp .msdDriverExit
.msdWriteVerify:     ;Function 9, writes sectors then verifies them
    mov rbp, rbx
    mov ah, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    mov ah, 84h ;LBA Verify Sectors
    call .msdBlkIOCommon
    mov rbx, rbp
    jmp .msdDriverExit
.msdIOCTLWrite:      ;Function 12, returns done
    jmp .msdDriverExit
.msdDevOpen:         ;Function 13
    movzx rax, byte [rbx + openReqPkt.unitnm]
    inc byte [.msdHdlCnt + rax]  ;Inc handle cnt for given unit
    jmp .msdDriverExit
.msdDevClose:        ;Function 14
    movzx rax, byte [rbx + closeReqPkt.unitnm]
    dec byte [.msdHdlCnt + rax]  ;Dec handle cnt for given unit
    jmp .msdDriverExit
.msdRemovableMedia:  ;Function 15
    movzx rax, byte [rbx + remMediaReqPkt.unitnm]
    mov al, byte [.msdBIOSmap + rax]    ;Get BIOS number
    test al, 80h
    jz .msdDriverExit   ;If removable, busy bit is clear
    mov word [rbx + remMediaReqPkt.status], 20h ;Set Busy bit
    jmp .msdDriverExit
.msdGenericIOCTL:    ;Function 19
    jmp .msdDriverExit
.msdGetLogicalDev:   ;Function 23
    mov al, byte [.msdCurDev]
    mov byte [rbx + getDevReqPkt.unitnm], al
    jmp .msdDriverExit
.msdSetLogicalDev:   ;Function 24
    mov al, byte [rbx + getDevReqPkt.unitnm]
    mov byte [.msdCurDev], al
    jmp .msdDriverExit

.msdBlkIOCommon:  ;Does block IO
;Called with rbp containing old rbx value and ah with function number
;Error handled by caller
    movzx rax, byte [rbp + ioReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req
    mov rcx, qword [rbp + ioReqPkt.strtsc]  ;Get start sector
    mov al, byte [rbp + ioReqPkt.tfrlen]    ;Get number of sectors, max 255
    mov rbx, qword [rbp + ioReqPkt.bufptr]  ;Get Memory Buffer
    int 33h
    ret

.msdDefLabel db "NO NAME ",0 ;Default volume label
;LASTDRIVE default is 5
.msdCurDev   db 0  ;Dev to be used by the driver saved here! (usually 1-1)
; Except when single drive in use, in which case Drive A and B refer to device 0
.msdBIOSmap  db 5 dup (0)    ;Translates DOS drive number to BIOS number
.msdHdlCnt   db 5 dup (0)    ;Keeps a count of open handles to drive N
.msdBPBTbl   dq 5 dup (0)    ;BPB pointer table to be returned
.msdBPBblks  db 5*bpbEx_size dup (0) ;Max 5 bpb records of exFAT bpb size

driverDataPtr: