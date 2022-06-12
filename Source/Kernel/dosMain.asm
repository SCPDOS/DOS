;-----------------------------------:
;        Main Kernel dispatch       :
;            and routines           :
;-----------------------------------:
functionDispatch:   ;Int 41h Main function dispatcher
;ah = Function number, all other registers have various meanings
 %if DEBUG
    ;Entry function
    debugEnterM
    lea rbp, .l0000
    call debPrintNullString
    call debPrintFunctionName
    jmp short .l0001
.l0000 db 0Ah,0Dh,"Entering ",0
.l0001:    
    debugExitM
    %endif
    cli ;Halt external interrupts
    cld ;Ensure all string ops occur in the right direction
    cmp ah, kDispTblL/2    ;Number of functions
    jae .fdExitBad  ;If equal or above, exit error
    ;Cherry pick functions
    cmp ah, 33h ;CTRL+BREAK check
    jb .fsbegin   ;If below skip these checks
    je ctrlBreakCheck
    cmp ah, 64h
    je setDriverLookahead  ;Reserved, but avoids usual Int 41h spiel
    ja .fsbegin   ;If above, do usual Int41 entry
    cmp ah, 51h
    je getCurrProcessID    ;This and below are exactly the same
    cmp ah, 62h
    je getPSPaddr          ;Calls the above function
    cmp ah, 50h
    je setCurrProcessID
.fsbegin:
    call dosPushRegs ;Push the usual prologue registers
    mov rax, qword [oldRSP]
    mov qword [oldoldRSP], rax
    inc byte [inDOS]    ;Increment in DOS flag
    mov qword [oldRSP], rsp
;Network related variable resetting
    mov byte [dosInvoke], 0 ;Invoked Locally
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
    shl ebx, 1      ;Multiply the function number by 2 for offset into table
    push rax        ;Push rax onto the stack
    lea rax, kDispTbl
    add rbx, rax    ;Add dispatch table offset into rbx
    movzx rbx, word [rbx]    ;Get the address from the dispatch table
    add rbx, rax    ;Add the table base (since it is the base addr for table)
    pop rax

    test ah, ah     ;Simple Terminate function?
    jz .fddiskOp
    cmp ah, 59h     ;Extended Error report?
    je .fdGoToFunction  ;Bypass code that clears the error report
    cmp ah, 0Ch     ;Are we a char function?
    ja .fddiskOp
;Char operations here
    test byte [critErrFlag], 1  ;Are we in critical error?
    jnz .fdGoToFunction         ;If we are, stay on Critical Error Stack
    lea rsp, AuxStakTop        ;Otherwise, switch to IO stack
    jmp short .fdGoToFunction
.fddiskOp:
    ;Disk operations go here
    ;Save rax on entry to a disk funciton
    mov qword [oldRAX], rax
    ;Clear up error info
    mov byte [errorLocus], 1    ;Reset to generic, unknown locus
    mov byte [critErrFlag], 0   ;Clear the Critical Error Flag
    mov byte [errorDrv], -1     ;Set the drive which caused the error to none

    push rax
    mov ah, 82h ;Cancel all critical section!
    int 4ah ;DOS critical section semphore handler (default, iretq)
    pop rax

    mov byte [int48Flag], 0     ;Turn off the ability to trigger Int 48h
    lea rsp, DiskStakTop        ;Swap the stack to the Disk Transfer Stack
    test byte [breakFlag], -1   ;Test if set
    jz .fdGoToFunction
; HANDLE CTRL+BREAK HERE!
    push rax
    call checkBreakOnCon    ; Check for a ^C in the CON buffer
    pop rax
.fdGoToFunction:
    xchg rbx, qword [oldRBX]    ;Put the call addr in oldRBX and get oldRBX back
    ;Potentially point rbp to caller reg frame for easy access of registers 
    ;
    ;IF YOU USE RAX AND DONT NEED A RETURN VALUE IN AL, 
    ;ENSURE YOU READ AL FROM THE STACK FRAME BEFORE RETURNING TO PRESERVE AL!!!
    ;
    %if DEBUG && REGS
    ;Print stack if necessary function
    debugEnterM
    call debPrintDOSStack
    debugExitM
    %endif
    call qword [oldRBX]     ;Call the desired function, rax contains ret code
    %if DEBUG
    ;Entry function
    debugEnterM
    lea rbp, .l0002
    call debPrintNullString
    jmp short .l0003
.l0002 db "Exiting Int 41h",0Ah,0Dh,0
.l0003:    
    debugExitM
    %endif
    %if DEBUG && REGS
    ;Exit function
    debugEnterM
    call debPrintDOSStack
    debugExitM
    %endif
.fdExit:
    cli     ;Redisable interrupts
    dec byte [inDOS]            ;Decrement the inDOS count
    mov rsp, qword [oldRSP]     ;Point rsp to old stack
    mov byte [rsp], al   ;Put the ret code into its pos on the register frame
    mov rax, qword [oldoldRSP]
    mov qword [oldRSP], rax
    call dosPopRegs  ;Pop the frame
    iretq
.fdExitBad:
    xor al, al
defaultIretq:
    iretq
dosPopRegs:
    pop qword [dosReturn]   ;Put return here resetting RSP
    pop rax
    pop rbx
    pop rcx
    pop rdx
    pop rsi
    pop rdi
    pop rbp
    pop r8
    pop r9
    jmp qword [dosReturn]
dosPushRegs:
    pop qword [dosReturn]   ;Put return here resetting RSP
    push r9
    push r8
    push rbp
    push rdi
    push rsi
    push rdx
    push rcx
    push rbx
    push rax
    jmp qword [dosReturn]
dosCrit1Enter:
    ret     ;Needs to be patched with 50h (PUSH RAX)
    mov eax, 8001h
    int 4ah
    pop rax
    ret
dosCrit1Exit:
    ret
    mov eax, 8101h
    int 4ah
    pop rax
    ret
dosCrit2Enter:
    ret
    mov eax, 8002h
    int 4ah
    pop rax
    ret
dosCrit2Exit:
    ret
    mov eax, 8102h
    int 4ah
    pop rax
    ret
;========================================:
;      Reentrant Kernel Functions        :
;========================================:
ctrlBreakCheck:    ;ah = 33h
    test al, al
    jnz .cbcget  ;Get the state or other functions
    mov dl, byte [breakFlag]    ;Get the state
    iretq
.cbcget:
    cmp al, 02h
    ja .cbcBad
    jz .cbcxchg ;Function 2
    push rdx
    and dl, 1   ;Get only the bottom bit
    mov byte [breakFlag], dl    ;Set the state
    pop rdx
    iretq
.cbcxchg:
    and dl, 1
    xchg byte [breakFlag], dl
    iretq
.cbcBad:
    mov al, -1
    iretq


setCurrProcessID:  ;ah = 50h, set current process ID (Set current PSP)
    mov qword [currentPSP], rbx ;Set the pointer
    iretq

getCurrProcessID:  ;ah = 51h, get current process ID (Get current PSP)
    mov rdx, qword [currentPSP]
    iretq

getPSPaddr:        ;ah = 62h, gives PSP addr/Process ID
    mov rdx, qword [currentPSP]
    iretq

setDriverLookahead:;ah = 64h, reserved
    iretq

;========================================:
;            Kernel Functions            :
;========================================:
diskReset:         ;ah = 0Dh
;Flush all dirty buffers to disk
    mov rbp, qword [bufHeadPtr]
.drCheckBuffer:
    test byte [rbp + bufferHdr.bufferFlags], dirtyBuffer
    jz .drGotoNextBuffer
.drFlushBuffer:
    call flushBuffer    ;Called with rbp = buffer header
    jc .drError
.drGotoNextBuffer:
    mov rbp, qword [rbp + bufferHdr.nextBufPtr]
    cmp rbp, -1     ;If rbp points to -1, exit
    jne .drCheckBuffer
    ret
.drError:
;Abort/Retry/Ignore
;Abort returns to DOS, 
;Retry retries the write on the buffer, 
;Ignore marks the buffer as clean and proceeds as normal
    mov al, byte [rbp + bufferHdr.bufferFlags]
    and al, 0Fh ;Clear the upper nybble
    mov ah, 31h ;Disk Error, Ignore,Retry and Write operation
    cmp al, dosBuffer
    je .drErrorMain
    add ah, 2
    cmp al, fatBuffer
    je .drErrorMain
    add ah, 2
    cmp al, dirBuffer
    add ah, 2
.drErrorMain:
    mov al, byte [rbp + bufferHdr.driveNumber]
    mov rsi, qword [rbp + bufferHdr.driveDPBPtr]
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr]
    mov di, word [diskReqHdr + drvReqHdr.status]    ;Disk error occured!
    and di, 0FFh    ;Only bottom byte
    mov word [errorExCde], di     ;Save driver error code
    add word [errorExCde], drvErrShft    ;Add offset to driver error codes
    mov byte [errorDrv], al     ;Save the drive on which the error occured
    mov byte [errorLocus], eLocDsk   ;Error in Block Device Request code
    mov byte [errorClass], eClsMedia ;Media error (bad BPB or other) code
    mov byte [errorAction], eActRet   ;Retry request code
    call criticalDOSError       ;Critical error handler
    test al, al ;Ignore the troublesome buffer and mark it as free
    jz .drIgnore
    cmp al, 1   ;Retry flushing the buffer
    je .drFlushBuffer
    int 43h     ;Abort and fail both abort through int 43h
.drIgnore:
    mov byte [rbp + bufferHdr.driveNumber], -1  ;Mark buffer as free
    jmp .drGotoNextBuffer

selectDisk:        ;ah = 0Eh
;Called with dl = drive number, 0 = A, 1 = B etc...
    mov al, dl
    inc al  ;Convert to 1-based number to avoid 0 meaning current drive
    call setDrive  ;Must make sure provided drive is valid
    jc .skipSettingCurrent  ;Join and network drives cant be current drive!
    mov byte [currentDrv], al   ;Set drive as current
.skipSettingCurrent:
    movzx eax, byte [lastdrvNum]   ;Return lastdrive as "errorcode"
    ret

getCurrentDisk:    ;ah = 19h, get current default drive
    mov al, byte [currentDrv]
    ret

FATinfoDefault:    ;ah = 1Bh
    xor dl, dl
FATinfoDevice:     ;ah = 1Ch
;Return in:
;   al = Number of sectors per cluster
;   edx = Number of clusters
;   cx =  Size of a clsuter
    test dl, dl
    jnz .fidSkipdefault
    mov dl, byte [currentDrv]   ;Get current drive code, 0 = A, 1 = B etc...
    inc dl
.fidSkipdefault:
    dec dl ;Decrement the drive letter since 0 = Default, 1 = A etc...
;Walk the dpb chain manually
    mov al, dl  ;Move drive number into al
    call walkDPBchain    ;Get in rsi the dpb pointer for drive in al
    jnc .fidDPBFound
;Else, we at an error.
;Simply return with CY set and error code in al with extended error info
    call getUserRegs
    or qword [rsi + callerFrame.flags], 1   ;Set the CY flag
    mov eax, errBadDrv          ;Invalid drive error
    mov word [errorExCde], errBadDrv     
    mov byte [errorLocus], eLocUnk    ;Not appropriate
    mov byte [errorClass], eClsNotFnd    ;Drive not found
    mov byte [errorAction], eActRetUsr   ;Retry after user intervention
    ret
.fidDPBFound:
    mov al, byte [rsi + dpb.bMaxSectorInCluster]
    inc al  ;Since bMaxSectorInCluster is one less than the number of sec/clus
    mov edx, dword [rsi + dpb.dClusterCount]
    mov cl, byte [rsi + dpb.bBytesPerSectorShift]
    mov ebx, 1
    shl ebx, cl
    mov ecx, ebx    ;Save the value in ecx
    lea rbx, qword [rsi + dpb.bMediaDescriptor]
    call getUserRegs
    mov qword [rsi + callerFrame.rdx], rdx
    mov word [rsi + callerFrame.rcx], cx
    mov qword [rsi + callerFrame.rbx], rbx
    ret

setIntVector:      ;ah = 25h
;Called with:
;   rdx = Pointer to interrupt handler
;   al = Interrupt number
    mov ebp, eax ;al has interrupt number which we need to save
    and ebp, 0FFh   ;Zero everything but the bottom byte
;First call to get default BIOS segement selector and attribute word
    mov bl, al  ;Set interrupt number 
    mov eax, 0F007h ;Get the descriptor
    int 35h
    call getUserRegs
    mov rbx, qword [rsi + callerFrame.rdx]  ;Pointer passed in rdx
    mov esi, eax    ;Move segment selector info to esi
    mov ecx, ebp    ;Get the interrupt number into cl
;dx preserves the attribute word
    mov eax, 0F008h ;Set descriptor
    int 35h
    call getUserRegs
    mov al, byte [rsi + callerFrame.rax]    ;Preserve low byte of rax
    ret
createNewPSP:      ;ah = 26h
    ret
setResetVerify:    ;ah = 2Eh, turns ALL writes to write + verify
    mov byte [verifyFlag], al
    and byte [verifyFlag], 1       ;Only save the bottom bit
    ret
getDOSversion:     ;ah = 30h
    call getUserRegs
    xor ah, ah ;Continue the mainline PC-DOS identification line
    mov byte [rsi + callerFrame.rbx + 1], ah    ;Clear bh 
    mov ax, word [dosMajor] ;Major and minor version in al,ah resp.
    mov word [rsi + callerFrame.rax], ax    ;Save ax
    ret

;AH = 1Fh/32h - GET (current) DISK DPB
getCurrentDPBptr:  ;ah = 1Fh, simply falls in Int 41h\ah=32h with dl=0
    xor dl, dl
getDeviceDPBptr:   ;ah = 32h
;On entry: dl = Drive number
;On exit: rbx = DPB pointer
    test dl, dl
    jnz .gddpskipdefault
    mov dl, byte [currentDrv]   ;Get current drive code, 0 = A, 1 = B etc...
    inc dl
.gddpskipdefault:
    ;Decrement the drive letter since 0 = Default, 1 = A etc...
    dec dl
    mov al, dl
    call getCDS ;Get in rsi the dpb pointer for drive dl
    jc .bad
    mov rdi, qword [workingCDS]  ;Get pointer to current CDS in rdi
    test rdi, cdsNetDrive ;Is dev a network drv (since they have no DPB)?
    jnz .bad
    call dosCrit1Enter  ;Enter class 1 critical section
    call getDiskDPB   ;See if the Disk structures are still ok 
    call dosCrit1Exit   ;Exit class 1 critical section
    jc .bad
    call getUserRegs
    mov [rsi + callerFrame.rbx], rbp    ;RBP has DPB pointer
    xor al, al
    ret
.bad:
    mov al, -1
    ret

getInDOSflagPtr:   ;ah = 34h
    lea rdx, inDOS
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rdx  ;save ptr in rbx
    ret
getIntVector:      ;ah = 35h
;Called with:
;   al = Interrupt Number
;Returns:
;   rbx = Pointer to interrupt handler
    mov bl, al  ;Get the interrupt vector number into bl
    mov eax, 0F007h
    int 35h
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rbx  ;Save pointer in rbx
    mov al, byte [rsi + callerFrame.rax]    ;Get the low byte in al
    ret

getDiskFreeSpace:  ;ah = 36h
    test dl, dl
    jnz .gdfsSkipdefault
    mov dl, byte [currentDrv]   ;Get current drive code, 0 = A, 1 = B etc...
    inc dl
.gdfsSkipdefault:
    dec dl ;Decrement the drive letter since 0 = Default, 1 = A etc...
    mov al, dl
    call walkDPBchain ;Get in rsi the dpb pointer for drive al
    jnc .gdfsDPBFound
;Else, we at an error.
;Simply return with CY set and error code in al with extended error info
    mov word [errorExCde], errBadDrv     ;Invalid drive error
    mov byte [errorLocus], eLocDsk    ;Not appropriate
    mov byte [errorClass], eClsNotFnd    ;Drive not found
    mov byte [errorAction], eActRetUsr   ;Retry after user intervention
    call getUserRegs
    mov word [rsi + callerFrame.rax], -1    ;Set ax=FFFFh
    or qword [rsi + callerFrame.flags], 1   ;Set CF=CY
    ret
.gdfsDPBFound:
    mov al, byte [rsi + dpb.bMaxSectorInCluster]
    inc al  ;Since bMaxSectorInCluster is one less than the number of sec/clus
    mov edx, dword [rsi + dpb.dClusterCount]
    mov cl, byte [rsi + dpb.bBytesPerSectorShift]
    mov ebx, 1
    shl ebx, cl
    mov ecx, ebx    ;Save the value in ecx
    mov ebx, dword [rsi + dpb.dNumberOfFreeClusters]    ;Ger # free clusters
    call getUserRegs
    mov qword [rsi + callerFrame.rdx], rdx
    mov word [rsi + callerFrame.rcx], cx
    mov qword [rsi + callerFrame.rbx], rbx
    ret

getRetCodeChild:   ;ah = 4Dh, WAIT, get ret code of subprocess
    xor eax, eax
    xchg ax, word [errorLevel]
    call getUserRegs
    mov word [rsi + callerFrame.rax], ax
    ret

getSysVarsPtr:     ;ah = 52h
    lea rdx, sysVarsPtr
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rdx
    ret


;AH = 53h - CREATE DPB
createDPB:         ;generates a DPB from a given BPB
;Only translates the data that can be garnered from a BPB to the DPB
;This is done so that the other fields of the DPB can be recycled
;Input: rsi = ptr to the BPB
;       rbp = ptr to the DPB
;Leave accessed flag alone! A brand new DPB should have -1 as accessed flag!
;bMediaDescriptor
    mov al, byte [rsi + bpb.media]
    mov byte [rbp + dpb.bMediaDescriptor], al
;dFirstFreeCluster
    mov dword [rbp + dpb.dFirstFreeCluster], 0  ;Start searching from start
;dNumberOfFreeClusters
    mov dword [rbp + dpb.dNumberOfFreeClusters], -1 ;Unknown
;bBytesPerSectorShift
    mov ax, word [rsi + bpb.bytsPerSec]
    mov cl, 7   ;Start with 128 byte sectors (not supported, min 512)
    shr ax, cl  ;Shift down
.cd0:
    shr ax, 1
    jz .cd1
    inc cl
    jmp short .cd0
.cd1:
    mov byte [rbp + dpb.bBytesPerSectorShift], cl
;bMaxSectorInCluster
    mov al, byte [rsi + bpb.secPerClus]
    dec al  ;Subtract one to get the max number of the last sector in a cluster
    mov byte [rbp + dpb.bMaxSectorInCluster], al
;bSectorsPerClusterShift
    inc al
    xor cl, cl
.cd2:
    shr al, 1
    jz .cd3
    inc cl
    jmp short .cd2
.cd3:
    mov byte [rbp + dpb.bSectorsPerClusterShift], cl
;wFAToffset, number of reserved sectors in partition
    mov ax, word [rsi + bpb.revdSecCnt]
    mov word [rbp + dpb.wFAToffset], ax
;bNumberOfFATs
    mov al, byte [rsi + bpb.numFATs]
    mov byte [rbp + dpb.bNumberOfFATs], al
;wNumberRootDirSectors
    movzx eax, word [rsi + bpb.rootEntCnt] ;Must be 0 on FAT 32
    shl eax, 5  ;Multiply by 32
    movzx ecx, word [rsi + bpb.bytsPerSec]
    dec ecx
    add eax, ecx
    xor edx, edx    ;Clear for divide
    div ecx ;Divide 0:eax by ecx, (e)ax has number of clusters
    mov word [rbp + dpb.wNumberRootDirSectors], ax  ;0 for FAT32
;dFATlength, get the FAT length
    movzx eax, word [rsi + bpb.FATsz16]
    mov ebx, dword [rsi + bpb32.FATsz32]
    test eax, eax   ;If FATsz16 = 0, then use FATsz32
    cmovz eax, ebx  ;Only move 32bit value if sz16 was 0
    mov dword [rbp + dpb.dFATlength], eax
;Complex cases below...
;dClusterHeapOffset, start sector of the data area
    movzx eax, word [rsi + bpb.FATsz16]
    mov ebx, dword [rsi + bpb32.FATsz32]
    test eax, eax
    cmovz eax, ebx
;eax = FATsz
    movzx ebx, word [rsi + bpb.totSec16]
    mov ecx, dword [rsi + bpb.totSec32]
    test ebx, ebx
    cmovz ebx, ecx 
;ebx = TotSec
    mov cl, byte [rsi + bpb.numFATs]
    xor edx, edx    ;Use edx = NumFATs * FATsz temporarily
.cd4:
    add edx, eax
    dec cl
    jnz .cd4
    mov eax, edx    ;Store product in eax
    movzx edx, word [rsi + bpb.revdSecCnt]  ;Get reserved sectors in volume
    add eax, edx
    movzx edx, word [rbp + dpb.wNumberRootDirSectors]
    add eax, edx    ;This adds nothing if FAT32
    ;eax = BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors
    ;aka eax = Start sector of the data area in volume
    mov dword [rbp + dpb.dClusterHeapOffset], eax
;dClusterCount
    sub ebx, eax    ;ebx = Number of sectors in the data area
    mov eax, ebx    ;Move number of sectors in data area into eax
    xor edx, edx
    mov ebx, 1
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shl ebx, cl ;Get sectors per cluster
    div ebx ;Data area sector / sectors per cluster = cluster count
    inc eax ;Maximum valid cluster value is eax + 1
    mov dword [rbp + dpb.dClusterCount], eax    ;eax = Cluster count
;dFirstUnitOfRootDir
    cmp eax, fat16MaxClustCnt  ;If above, its FAT32
    mov eax, dword [rsi + bpb32.RootClus]   ;Just save this if FAT32
    ja .cd5
    ;Else, we need to find the first sector of the root directory
    ;Get the start sector of data area in volume 
    ; and sub the number of sectors in the root directory
    mov eax, dword [rbp + dpb.dClusterHeapOffset]
    movzx ebx, word [rbp + dpb.wNumberRootDirSectors]
    sub eax, ebx    ;eax now has start sector of root dir
.cd5:
    mov dword [rbp + dpb.dFirstUnitOfRootDir], eax
;Exit epilogue
    mov rbx, qword [oldRSP]
    mov al, byte [rbx + callerFrame.rax]        ;Return original al value 
    %if DEBUG && DPBINFO
    ;Print DPB 
    debugEnterM
    push rbp
    lea rbp, .l0000
    call debPrintNullString
    pop rbp
    call debDPBBPBptr
    call debMakeDebuggerRespond
    jmp short .l0001
.l0000 db "Constructed DPB from given device BPB",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    ret

getVerifySetting:  ;ah = 54h
    mov al, byte [verifyFlag]   ;al is the return value in this case
    ret
createPSP:         ;ah = 55h, creates a PSP for a program
    ret
getExtendedError:  ;ah = 59h
    call getUserRegs
    mov ax, word [errorExCde]
    mov ch, byte [errorLocus]
    mov bh, byte [errorClass]
    mov bl, byte [errorAction]
    mov word [rsi + callerFrame.rax], ax
    mov word [rsi + callerFrame.rbx], bx
    mov byte [rsi + callerFrame.rcx + 1], ch
    ret
getCritErrorInfo:  ;ah = 5Dh
networkServices:   ;ah = 5Eh, do nothing
networkRedirection:;ah = 5Fh, do nothing
    ret
getsetDiskSerial:  ;ah = 69h, get/set disk serial number
return:
    ret
