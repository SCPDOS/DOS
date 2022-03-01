[map all scpdos.map]
[DEFAULT REL]
BITS 64
%include "driverStruc.inc"
%include "fatStruc.inc"
%include "dosStruc.inc"
%include "dosSeg.asm"

Segment .text align=1
; We arrive here with the following values in the registers.
; rbx =  LBA of first Logical Block after SCP/BIOS
; dx  = Int 33h boot device number
; fs  = userbase pointer (pointer to first usable block of RAM)
tempPSP:    ;Here to allow the loader to use Int 41h once it is loaded high
    dw 0AA55h           ;Initial signature
    db (100h-2) dup (90h)   ;Duplicate NOPs for the PSP
    mov byte fs:[bootDrive], dl ;Save the boot drive in memory
    lea rdx, qword [tempPSP]    ;Get the address of the tempPSP
    mov qword fs:[currentPSP], rdx
;DOS allows for non-PARA aligned PSPs but DOS aligns all programs on PARA bndry
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
conInit:    ;Rather than keeping this resident... do it here
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

;Fill in the default file table entries
    ;lea rbx, qword [rbp + firstSftHeader]
    ;mov qword [rbx + sfth.qNextSFTPtr], -1  ;Last sfth in chain
    ;mov word [rbx + sfth.wNumFiles], 5      ;5 default files
    ;mov qword fs:[sftHeadPtr], rbx  ;Save ptr to this sft header in SysVars

    ;lea rbx, qword [rbp + firstSft]
    ;mov word [rbx + sft.wNumHandles], 0 ;Nothing pointing to this file yet
    ;mov word [rbx + sft.w]


;Test Error Case
    ;mov ah, 00110000b
    ;mov al, 00h
    ;mov edi, 0Ch
    ;int 44h

    lea rdx, qword [startmsg]   ;Get the absolute address of message
    mov ah, 09h
    int 41h

    mov rsi, fs:[nulDevHdr]
    mov eax, 0C501h ;Connect debugger
    int 35h
l1:
    mov ah, 01h  ;Write with echo
    int 41h
    cmp al, 0
    je l2
    jmp short l1
l2:
    mov ah, 07h
    int 41h
    cmp al, 42h
    jne l1
l3:
    mov word fs:[CLOCKrecrd + clkStruc.dateWord], 0
    lea rbx, qword [rbp + charReqHdr] ;Get the address of this request block
    lea rax, qword [rbp + CLOCKrecrd]
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read the time
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 06
    call qword [rbp + clkHdr + drvHdr.strPtr]
    call qword [rbp + clkHdr + drvHdr.intPtr]

    mov ah, 03h
    xor bh, bh
    int 30h
    xor dl, dl  ;0 column
    mov ah, 02h
    int 30h

    lea rbx, qword [rbp + CLOCKrecrd]
    movzx eax, byte [rbx + clkStruc.hours]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, ":"
    int 30h
    movzx eax, byte [rbx + clkStruc.minutes]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, ":"
    int 30h
    movzx eax, byte [rbx + clkStruc.seconds]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, "."
    int 30h
    movzx eax, byte [rbx + clkStruc.hseconds]
    call .clkHexToBCD
    jmp l1
.clkHexToBCD:
;Converts a Hex byte into two BCD digits
;Takes input in each nybble of al
    push rbx
    ;xchg bx, bx
    mov rbx, 0Ah  ;Divide by 10
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .chtb0
    add dl, 'A'-'0'-10
.chtb0:
    mov cl, dl    ;Save remainder byte
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .chtb1
    add dl, 'A'-'0'-10
.chtb1:
    mov ch, dl    ;Save remainder byte

    mov al, ch    ;Get most sig digit into al
    mov ah, 0Eh
    int 30h
    mov al, cl    ;Get least sig digit into al
    mov ah, 0Eh
    int 30h
    pop rbx
    ret

adjustDrvHdr:
;Input: rsi = Effective address of driver in DOS segment
;       rbp = Ptr to the start of the DOS segment
;Output: rsi = EA of next header in DOS segment
    add qword [rsi + drvHdr.nxtPtr], rbp    ;Adjust address
    add qword [rsi + drvHdr.strPtr], rbp
    add qword [rsi + drvHdr.intPtr], rbp
    add rsi, drvHdr_size
    ret
startmsg db "Starting SCP/DOS...",0Ah,0Dh,"$"
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
    jb .fsbegin   ;If below skip these checks
    je .ctrlBreakCheck
    cmp ah, 64h
    je .setDriverLookahead  ;Reserved, but avoids usual Int 41h spiel
    ja .fsbegin   ;If above, do usual Int41 entry
    cmp ah, 51h
    je .getCurrProcessID    ;This an below are exactly the same
    cmp ah, 62h
    je .getPSPaddr          ;Calls the above function
    cmp ah, 50h
    je .setCurrProcessID
.fsbegin:
    pushDOS ;Push the usual prologue registers
    mov rax, qword [oldRSP]
    mov qword [oldoldRSP], rax
    inc byte [inDOS]    ;Increment in DOS flag
    mov qword [oldRSP], rsp
;Here, we want to save oldRSP in the callers PSP
    cmp byte [inDOS], 1 ;Check how many times we are in DOS
    jne .fsb1   ;If this is first entry, save rsp in callers PSP
    mov rax, qword [currentPSP] ;Get current PSP address
    mov qword [rax + psp.rspPtr], rsp    ;Save rsp on callers stack
.fsb1:
    pop rax     ;Get old rax back
    push rax    ;and push it back onto the stack
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
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, .stdinReadEchoBuffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
    cmp byte [.stdinReadEchoBuffer], 00h
    jz .stdireexit
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, .stdinReadEchoBuffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
.stdireexit:
    mov al, byte [.stdinReadEchoBuffer]
    ret
.stdinReadEchoBuffer    db 0
.stdoutWrite:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    mov byte [.stdoutWriteBuffer], dl
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, .stdoutWriteBuffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
    ret
.stdoutWriteBuffer db 0
.stdauxRead:        ;ah = 03h
.stdauxWrite:       ;ah = 04h
.stdprnWrite:       ;ah = 05h
.directCONIO:       ;ah = 06h
.waitDirectInNoEcho:;ah = 07h
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, .function7buffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
    mov al, byte [.function7buffer]
    ret
.function7buffer    db 0
.waitStdinNoEcho:   ;ah = 08h
    ret
.printString:       ;ah = 09h
    xor ecx, ecx    ;Clear char counter
    mov eax, "$"    ;Terminating char
    mov rdi, rdx    ;Set up for scasb
.ps0:   ;Search for $ to get count of chars
    scasb
    je .ps1
    inc ecx
    jmp short .ps0
.ps1:   ;Use handle 
    lea rbx, charReqHdr ;Get the address of this request block
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
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
.setCurrProcessID:  ;ah = 50h, set current process ID (Set current PSP)
.getCurrProcessID:  ;ah = 51h, get current process ID (Get current PSP)
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
    dq nulStrat
    dq nulIntr
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
    dq nulStrat
    dq nulIntr
    db "LPT1    "
lpt2Hdr:
    dq lpt3Hdr
    dw 0A040h
    dq nulStrat
    dq nulIntr
    db "LPT2    "
lpt3Hdr:
    dq -1
    dw 0A040h
    dq nulStrat
    dq nulIntr
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
    mov al, 03h ;Unknown Command
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    ja .conWriteErrorCode ;If yes, error!

    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 4
    jz .conRead
    cmp al, 5
    jz .conNondestructiveRead
    cmp al, 6
    jz .conInputStatus
    cmp al, 7
    jz .conFlushInputBuffers
    cmp al, 8
    jz .conWrite
    cmp al, 9
    jz .conWrite
    cmp al, 0Ah
    jz .conOutputStatus
    jmp short .conExit  ;All other valid functions return done
.conWriteErrorCode:     ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.conExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rbx
    pop rax
    ret
.conRead:    ;Function 4
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .conWriteErrorCode

    push rdi
    push rcx
    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cre1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
    cmp byte [.conBuf], 0   ;Does the buffer contain a zero?
    jnz .cre3   ;No, get the buffer value
    xor eax, eax
    int 36h
.cre11:
    stosb
    test al, al ;Was the ascii code 0?
    jnz .cre12  ;No, skip storing scancode
    mov byte [.conBuf], ah  ;Save scancode
.cre12:
    inc ecx ;Inc chars stored in buffer
    jmp short .cre1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rdi
    jmp short .conExit
.cre3:
    mov al, byte [.conBuf]  ;Get the buffer value
    mov byte [.conBuf], 0   ;Reset the buffer value
    jmp short .cre11

.conNondestructiveRead:  ;Function 5
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], nonDestInNoWaitReqPkt_size
    jne .conWriteErrorCode
    cmp byte [.conBuf], 0
    jnz .cnr2
    mov ah, 01h     ;Get key if exists
    int 36h
    jz .cnr1        ;If zero clear => no key, go forwards
    ;Keystroke available
.cnr0:
    mov byte [rbx + nonDestInNoWaitReqPkt.retbyt], al   ;Move char in al
    jmp .conExit
.cnr1: ;No keystroke available
    mov word [rbx + nonDestInNoWaitReqPkt.status], 0200h   ;Set busy bit
    jmp .conExit
.cnr2:
    mov al, byte [.conBuf]  ;Copy scancode but dont reset it
    jmp short .cnr0   ;Keystroke is available clearly

.conInputStatus:         ;Function 6
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .conWriteErrorCode
    jmp .conExit ;Exit, device ready

.conFlushInputBuffers:   ;Function 7
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .conWriteErrorCode
    mov byte [.conBuf], 0   ;Clear buffer
.cfib0:
    mov ah, 01      ;Get buffer status
    int 36h
    jz .conExit     ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short .cfib0

.conWrite:   ;Function 8 and 9
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .conWriteErrorCode

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
    jmp .conExit
.conOutputStatus:   ;Function 0Ah
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .conWriteErrorCode
    jmp .conExit

.conBuf db 0    ;Single byte buffer
clkDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp
    mov rbx, qword [reqHdrPtr]
    mov al, 03h ;Unknown Command
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    ja .clkWriteErrorCode ;If yes, error!

    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 04h
    jz .clkRead
    cmp al, 06h
    jz .clkInputStatus
    cmp al, 07h
    jz .clkFlushInputBuffers
    cmp al, 08h
    jz .clkWrite
    cmp al, 09h
    jz .clkWrite
    jmp short .clkExit  ;All other valid functions return done immediately!
.clkNotFunctioning:
    mov al, 02h ;Device not ready error
.clkWriteErrorCode:
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.clkExit:
    or word [rbx + drvReqHdr.status], 0100h ;Merge done bit
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

.clkRead:           ;Function 4
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .clkWriteErrorCode

    mov rsi, rbx    ;Save rbx temporarily in rsi
    mov rbp, qword [rbx + ioReqPkt.bufptr]    ;Save the clock struc ptr in rbp
    mov ax, word [.clkDate] ;Get the clock date
    mov word [rbp + clkStruc.dateWord], ax
    xor ah, ah
    int 3Ah         ;Read the system timer
    test al, al     ;Check to see if midnight has passed?
    jz .clkr1       ;Nope, now just time 
    xor ah, ah
    ;This works as al should keep count of the # of days passed since last read
    add word [rbp + clkStruc.dateWord], ax
    add word [.clkDate], ax ;Add to internal date counter too
.clkr1:
    mov byte [rbp + clkStruc.hours], cl   ;Save hours
    movzx edx, dx
    mov ebx, edx  ;Save the minutes/seconds/hseconds count
    mov eax, edx
    xor edx, edx
    mov eax, ebx
    mov ecx, 1092   
    div ecx
    mov byte [rbp + clkStruc.minutes], al
    mov eax, edx    ;Get remainder in eax
    lea eax, dword [eax + 4*eax]    ;Multiply by 5
    xor edx, edx
    mov ecx, 91 ;5*18.2
    div ecx
    mov byte [rbp + clkStruc.seconds], al
    mov eax, edx    ;Get remainder in eax
    ;lea eax, dword [eax + 4*eax]
    ;add eax, edx    ;Essentially multiply by 6
    mov byte [rbp + clkStruc.hseconds], al
    mov rbx, rsi    ;Return the packet pointer back to rbx
    jmp .clkExit

.clkInputStatus:    ;Function 6
;Always return ready
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .clkWriteErrorCode
    jmp .clkExit
.clkFlushInputBuffers:  ;Function 7
;Always return done immediately
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], flushReqPkt_size
    jne .clkWriteErrorCode
    jmp .clkExit

.clkWrite:          ;Functions 8 and 9
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .clkWriteErrorCode

    mov rsi, rbx    ;Save rbx temporarily in rsi
    mov rbp, qword [rbx + ioReqPkt.bufptr]    ;Save the clock struc ptr in rbp
    mov ax, word [rbp + clkStruc.dateWord]    ;Get date word
    mov word [.clkDate], ax ;Save date internally

    xor ebx, ebx    ;Clear temporary lo count register
    movzx eax, byte [rbp + clkStruc.hseconds]
    mov cl, 5
    div cl          ;Divide al by 5
    xor ah, ah      ;Remove the remainder
    add ebx, eax    ;Add the hseconds to final value
;Using the decimal part of this formula for the low count
;LoCount = (Minutes * 1092.38) + (Seconds * 18.21) + (Hundreths * .182)
    mov al, byte [rbp + clkStruc.seconds]
    mov ecx, 18
    mul ecx  
    add ebx, eax

    xor edx, edx
    movzx eax, byte [rbp + clkStruc.minutes]
    mov ecx, 1092
    mul ecx
    add ebx, eax
    mov edx, ebx    ;edx now has low count
    movzx ecx, byte [rbp + clkStruc.hours]
    mov ah, 01h     ;Set the system time
    int 3Ah

    mov rbx, rsi
    jmp .clkExit

.clkBCDtoHex:
;Converts a BCD value to a Hex byte
;Takes input in al, returns in al (zero-ed upper seven bytes)
    push rcx
    movzx eax, al   ;Zero extend
    mov ecx, eax    ;Save al in ecx
    and eax, 0Fh    ;Get lower nybble
    and ecx, 0F0h   ;Get upper nybble
    shr ecx, 4      ;Shift upper nybble value down
.cbth0:
    add eax, 10
    loop .cbth0
    pop rcx
    ret

.clkHexToBCD:
;Converts a Hex byte into two BCD digits
;Takes input in al, returns in al (zero-ed upper seven bytes)
    push rcx
    movzx eax, al   ;Zero extend
    xor ecx, ecx
.chtb0:
    cmp eax, 10
    jb .chtb1
    sub eax, 10
    inc ecx
    jmp short .chtb0
.chtb1:
    shl ecx, 4  ;Move to upper nybble
    or al, cl   ;Move upper nybble into al upper nybble
    pop rcx
    ret
.clkDate    dw 0    ;Number of days since 01/01/1980
;When counting the number of days, first compute the number of years since
; 1980 and your year. 
;Then, using the table below, find the number of leap years between 1980
; and (YourYear - 1). 
;Then do (YourYear - 1980) * 365 + numberOfLeapYears to get the number of 
; days since 01/01/1980 and 01/01/YourYear.
;Use the months table to get the number of days in a normal month as leap 
; years are added using the previous comment.
;Finally check if the date is after 28th Feb. If it is, check if your year is 
; a leap year using the table. If it is, add an extra day.
.clkLeapYears:
    db 00, 04, 08, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 
    db 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96
.clkMonths:
    db 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
;COM Driver headers and main interrupt strat
com1Intr:
    mov byte [comIntr.comDevice], 0
    jmp short comIntr
com2Intr:
    mov byte [comIntr.comDevice], 1
    jmp short comIntr
com3Intr:
    mov byte [comIntr.comDevice], 2
    jmp short comIntr
com4Intr:
    mov byte [comIntr.comDevice], 3
comIntr:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    mov rbx, qword [reqHdrPtr]
    mov al, 03h ;Unknown Command
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    ja .comWriteErrorCode ;If yes, error!

    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 4   ;Read Character(s)
    jz .comRead
    cmp al, 5   ;Non-destructive read, acts like fast read 1 char if available
    jz .comNondestructiveRead   
    cmp al, 6   ;Read Input Status, always return with Busy bit = 0
    jz .comReadInputStatus
    cmp al, 7   ;Flush read buffers, return done
    jz .comFlushInputBuffers
    cmp al, 8
    jz .comWrite
    cmp al, 9
    jz .comWrite
    cmp al, 0Ah
    jz .comOutputStatus ;Return Clear to send bit inverted for busy bit
    jmp short .comExit  ;All other valid functions should return done
.comErrorNoCount:
    mov al, 02h ;Unknown device
    jmp short .comWriteErrorCode
.comReadError:
    mov edx, 0Bh
.comWriteError:
    mov edx, 0Ah
.comError:
    mov dword [rbx + ioReqPkt.tfrlen], ecx ;Store actual transferred chars
    mov ecx, 02h    ;Unknown device
    cmp al, 0FEh    ;Invalid COM port
    cmove edx, ecx  ;Only move unknown device error code if invalid COM port
    mov al, dl      ;Move dl to al to store error code
.comWriteErrorCode:    ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.comExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

.comRead:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .comWriteErrorCode

    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cr1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
.cr11:  ;Blocking wait, could be an infinite loop. Imitate basic DOS driver
    mov eax, 02h    ;Recieve 
    mov dl, byte [.comDevice]    ;Get transacting com device
    cbw     ;Zero extend to upper byte
    int 34h ;Recieve Char
    jc .comError
    cmp ah, 80h ;Did a "timeout" occur? If so, keep waiting
    je .cr11
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short .cr1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    jmp short .comExit

.comReadInputStatus:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .comWriteErrorCode
    mov word [rbx + statusReqPkt.status], 0 ;Chars ready to read status
    jmp short .comExit

.comNondestructiveRead:
;Acts like a "read one character if there is one" function
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], nonDestInNoWaitReqPkt_size
    jne .comWriteErrorCode
.cndr1:
    mov eax, 02h    ;Recieve 
    mov dl, byte [.comDevice]    ;Get transacting com device
    cbw     ;Zero extend to upper byte
    int 34h ;Recieve Char
    jc .comErrorNoCount ;Dont save a char transfer number
    cmp ah, 80h ;Did a "timeout" occur? If so, return with busy = 1
    je .cndr2
    mov byte [rbx + nonDestInNoWaitReqPkt.retbyt], al   ;Get next char
    jmp short .comExit
.cndr2:
    mov word [rbx + nonDestInNoWaitReqPkt.status], 200h ;Busy bit set
    jmp short .comExit

.comFlushInputBuffers:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], flushReqPkt_size
    jne .comWriteErrorCode
.cfib0:
    mov dl, byte [.comDevice]
    cbw
    mov eax, 02h    ;Recieve
    int 34h
    jc .comErrorNoCount
    cmp ah, 80h ;Keep looping until ah = 80h (no more chars in buffer)
    jne .cfib0
    jmp .comExit

.comWrite:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .comWriteErrorCode

    mov rsi, qword [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    mov ah, 01h ;Move function number into ah
    mov dl, byte [.comDevice]
    cbw     ;Zero extend to upper byte
    int 34h ;Transmit char
    jc .comError
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    jmp .comExit

.comOutputStatus:
;Read MODEM status
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .comWriteErrorCode

    mov dl, byte [.comDevice]
    cbw     ;Zero extend to upper byte
    mov ah, 03h     ;Get status
    int 34h
    jc .comErrorNoCount
    and eax, 10h ;Isolate bit 4 of al, clear to set, and clear all other bits
    shl eax, 5   ;Shift it up to bit 9 (busy bit in status word) 
    not eax      ;Bitwise inversion
    and eax, 200h   ;Isolate bit 9
    mov word [rbx + rbx + drvReqHdr.status], ax  ;Add the busy bit
    jmp .comExit
.comDevice   db 0

msdDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8
    mov rbx, qword [reqHdrPtr]  ;Get the ptr to the req header in rbx
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    mov al, 03h
    ja .msdWriteErrorCode ;If yes, error!
    mov al, 01h ;Unknown Unit Error
    cmp byte [rbx + drvReqHdr.unitnm], 05h  ;Unit greater than 5 is invalid
    ja .msdWriteErrorCode ;If yes, error!
    mov al, byte [rbx + drvReqHdr.cmdcde]   ;Get command code in al
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
    jmp short .msdDriverExit    ;All other valid functions exit done
.msdIOError:  ;In Read and Write errors, rbp points to the dev struc
    mov rbx, rbp
    movzx eax, al   ;Number of IO-ed sectors in last request
    add esi, eax    ;esi Keeps sector count across transfers
    mov dword [rbx + ioReqPkt.tfrlen], esi ;Save number of IO-ed sectors
;Now fall through to general error
.msdGenDiskError:
    mov ah, 01h
    xor dl, dl  ;Work around bug that fails request if dl > 7Fh
    int 33h ;Read status of last operation
    cmp ah, 06h ;Mock Seek response (device not present)
    mov al, 02h ;Give device not ready error (sensibly I think)
    je .msdWriteErrorCode 
    mov al, 0Ch ;Preliminary General Error Faults
    cmp ah, -1  ;Sense operation failed
    je .msdWriteErrorCode 
    cmp ah, 20h ;Gen. ctrlr. failure. Consider new error code to halt system.
    je .msdWriteErrorCode
;Device Not Ready
    mov al, 02h  ;Device not ready code
    cmp r8b, al  ;SCSI Not ready commands start with 2
    je .msdWriteErrorCode
    shr r8, 8       ;Remove Sense Key
    movzx ecx, r8w  ;Get ASC and ASCQ in cl and ch bzw.
;Write Protected
    xor al, al
    cmp cx, 0027h   ;Write protected error
    je .msdWriteErrorCode
;CRC Error
    mov al, 04h     ;CRC error code
    cmp cx, 0308h   ;LU comms CRC error (UDMA/32)
    je .msdWriteErrorCode
    cmp cx, 0010h   ;ID CRC or ECC error
    je .msdWriteErrorCode
    cmp cx, 0147h   ;Data phase CRC error detected
    je .msdWriteErrorCode
;Seek Error
    mov al, 06h     ;Seek error code
    cmp cl, 02h     ;No Seek Complete
    je .msdWriteErrorCode
;Unknown Hardware Media (Shouldn't happen with Flash Drives)
;This error should only be called if BPB not recognised for Flash Drives
    mov al, 07h
    cmp cl, 30h   ;All issues with media returns unknown media
    je .msdWriteErrorCode
;Sector Not Found
    mov al, 08h     ;Sector not found code
    cmp cl, 21h     ;Illegal Request - Invalid LBA
    je .msdWriteErrorCode
;Write faults
    mov al, 0Ah     ;Write fault
    cmp cl, 0Ch     ;Write Error ASC code
    je .msdWriteErrorCode
;Read faults
    mov al, 0Bh     ;Read fault
    cmp cl, 11h     ;Read error
    je .msdWriteErrorCode
;General Errors
    mov al, 0Ch     ;Everything else is general error
.msdWriteErrorCode:    ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.msdDriverExit:
    or word [rbx + drvReqHdr.status], 0100h ;Set done bit
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.msdInit:            ;Function 0
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], initReqPkt_size
    jne .msdWriteErrorCode

    push r9
    int 31h ;Get number of Int 33h devices in r8b
    pop r9
    mov byte [numMSDdrv], r8b   ;Save number of physical int 33h devs
    mov r8, rbx ;Save the req block ptr in r8
    xor edx, edx  ;Start from device zero
    mov byte [lastdrvNum], dl   ;Zero this field, max 5
.mi0:   ;Now check each device for partitions
    cmp byte [lastdrvNum], 5
    je .msdExit ;IF we are at 5 now, we exit
    mov ah, 82h ;LBA read
    mov al, 1   ;1 sector
    mov ecx, 0  ;Read sector 0
    lea rbx, msdTempBuffer  ;Get address of this space
    int 33h
    jc .msdInitError
;Now we verify if this is a BPB. Removable devices can't be partitioned (yet)
;1) Check byte 0 for EBh (short jmp) and byte 2 for a 90h (nop).
    mov al, byte [rbx]
    mov ah, byte [rbx + 2]
    cmp ax, 090EBh
    jne .mimbr
;Valid BPB found! Copy to internal table and inc lastdrive
    mov rsi, rbx
    mov eax, bpbEx_size
    mov ecx, edx    ;Temporarily save dl in ecx
    mul edx
    mov edx, ecx
    lea rdi, qword [.msdBPBblks + eax]
    mov ecx, bpbEx_size
    mov rax, rdi    ;Save the entry address in rax
    rep movsb   ;Copy the bpb into the bpb table
    lea rdi, qword [.msdBPBTbl + 8*edx]
    mov qword [rdi], rax
    inc byte [lastdrvNum]
    inc dl
    cmp dl, byte [numMSDdrv] ;Once these are equal, we have processed last dev
    jne .mi0
.msdExit:
    mov rbx, r8
    jmp .msdDriverExit
.mimbr:
;Goto next device without incrementing LASTDRIVE
    inc dl
    cmp dl, byte [numMSDdrv] ;Once these are equal, we have processed last dev
    jne .mi0
    jmp short .msdExit
.msdInitError:
    mov rbx, r8
    jmp .msdGenDiskError
.msdMedChk:          ;Function 1
;Once the BIOS function is implmented that reads the changeline, use that!
;For BIOSes that dont support the changeline, the following procedure will 
; suffice.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], mediaCheckReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + mediaCheckReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]    ;Translate unitnum to BIOS num
    test dl, 80h    ;If it is a fixed disk, no change!
    jnz .mmcNoChange
;Now we do a BIOS changeline check. If it returns 80h or 86h then check med desc
    mov ah, 16h 
    int 33h
    jc .msdGenDiskError
    cmp ah, 80h
    je .mmcNoChangeLine
    cmp ah, 86h
    je .mmcNoChangeLine
    test ah, ah ;No change?
    jz .mmcNoChange
    test ah, 1  ;Neither 80h or 86h have bit 0 set
    jnz .mmcChange
;If nothing, fall through and test manually, should never happen though
.mmcNoChangeLine:
;Now we test Media Descriptor
    mov dl, byte [rbx + mediaCheckReqPkt.medesc]    ;Media descriptor
    mov rdi, qword [.msdBPBTbl + 8*rax]
    mov rdi, qword [rdi]    ;Dereference rdi
    cmp byte [rdi + bpb32.media], dl    ;Compare media descriptor bytes
    je .mmcUnsure
.mmcChange:
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    lea rax, qword [.msdDefLabel]           ;Temp, ret def label
    mov qword [rbx + mediaCheckReqPkt.desptr], rax 
    jmp .msdDriverExit
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    jmp .msdDriverExit
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    jmp .msdDriverExit

.msdBuildBPB:        ;Function 2
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], bpbBuildReqPkt_size
    jne .msdWriteErrorCode

    mov rsi, rbx
    movzx rax, byte [rsi + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer
    xor ecx, ecx    ;Read Sector 0
    mov eax, 8201h  ;LBA Read 1 sector
    int 33h
    jc .msdGenDiskError
;Check Media Descriptor, must be F0h or F8h-FFh or unknown media
    cmp byte [rbx + bpb.media], 0F0h    ;3.5" FDD standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0F8h    ;FDD/Large Media Standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0F9h    ;5.25" & 720K 3.5" Media Standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0FCh    ;Very Obsolete Media Standards
    mov al, 07h ;Unknown media error code
    jb .msdWriteErrorCode
.mbbpb0:
    xchg rbx, rsi    ;Transf Buf(rbx) <-> ReqHdr(rsi)
    movzx rax, byte [rbx + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov rdi, qword [.msdBPBTbl + 8*rax] ;Get pointer to pointer to buffer
    mov rdi, qword [rdi] ;Dereference to get pointer to buffer 
    mov qword [rbx + bpbBuildReqPkt.bpbptr], rdi ;rdi -> final bpb resting place
    mov ecx, bpbEx_size/8
    rep movsq   ;Move the BPB data into the right space
    jmp .msdDriverExit
.msdIOCTLRead:       ;Function 3, returns done
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdRead:            ;Function 4
;Will read one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set sector read counter to zero
.msdr0:
    mov dh, 82h ;LBA Read Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdr0
    mov rbx, rbp
    jmp .msdDriverExit
.msdWrite:           ;Function 8
;Will write one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set counter to zero
.msdw0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdw0
    mov rbx, rbp
    jmp .msdDriverExit
.msdWriteVerify:     ;Function 9, writes sectors then verifies them
;Will write one sector at a time and then verify it.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set counter to zero
.msdwv0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    mov dh, 84h ;LBA Verify Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdwv0
    mov rbx, rbp
    jmp .msdDriverExit
.msdIOCTLWrite:      ;Function 12, returns done
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdDevOpen:         ;Function 13
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], openReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + openReqPkt.unitnm]
    inc byte [.msdHdlCnt + rax]  ;Inc handle cnt for given unit
    jmp .msdDriverExit
.msdDevClose:        ;Function 14
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], closeReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + closeReqPkt.unitnm]
    dec byte [.msdHdlCnt + rax]  ;Dec handle cnt for given unit
    jmp .msdDriverExit
.msdRemovableMedia:  ;Function 15
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], remMediaReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + remMediaReqPkt.unitnm]
    mov al, byte [.msdBIOSmap + rax]    ;Get BIOS number
    test al, 80h
    jz .msdDriverExit   ;If removable, busy bit is clear
    mov word [rbx + remMediaReqPkt.status], 0200h ;Set Busy bit
    jmp .msdDriverExit
.msdGenericIOCTL:    ;Function 19
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioctlReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdGetLogicalDev:   ;Function 23
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], getDevReqPkt_size
    jne .msdWriteErrorCode

    mov al, byte [.msdCurDev]
    mov byte [rbx + getDevReqPkt.unitnm], al
    jmp .msdDriverExit
.msdSetLogicalDev:   ;Function 24
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], setDevReqPkt_size
    jne .msdWriteErrorCode

    mov al, byte [rbx + getDevReqPkt.unitnm]
    mov byte [.msdCurDev], al
    jmp .msdDriverExit

.msdBlkIOCommon:  ;Does block IO
;Called with rbp containing old rbx value and ah with function number
;Error handled by caller
;Sector count handled by caller
;Called with dh = BIOS function number
    movzx rax, byte [rbp + ioReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req in dl
    mov rcx, qword [rbp + ioReqPkt.strtsc]  ;Get start sector
    mov rbx, qword [rbp + ioReqPkt.bufptr]  ;Get Memory Buffer
    mov ah, dh
    mov al, 01h ;Do one sector at a time 
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