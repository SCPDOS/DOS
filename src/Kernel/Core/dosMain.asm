;-----------------------------------:
;        Main Kernel dispatch       :
;            and routines           :
;-----------------------------------:
functionDispatch:   ;Int 21h Main function dispatcher
;ah = Function number, all other registers have various meanings
%if DEBUG && HEADERS
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
;Cherry pick quick functions
    cmp ah, 33h
    je cbcQuick
    jb .fsbegin
    cmp ah, 64h
    je sdlQuick  
    ja .fsbegin             ;If above, do usual Int21 entry
    cmp ah, 51h
    je gcpspQuick           ;This and below are exactly the same
    cmp ah, 62h
    je gcpspQuick           ;Calls the above function
    cmp ah, 50h
    je scpspQuick
.fsbegin:
    call dosPushRegs ;Push the usual prologue registers
    mov qword [oldRBX], rbx ;Need to do this as I might switch stacks later
    mov word [machineNum], 0    ;Set the machine number for the request to us!
    mov rax, qword [oldRSP]
    mov qword [oldoldRSP], rax
    inc byte [inDOS]    ;Increment in DOS flag
    mov qword [oldRSP], rsp
;Network related variable resetting
    mov byte [dosInvoke], 0 ;Invoked Locally
;Here, we want to save oldRSP in the callers PSP
    mov rax, qword [currentPSP] ;Get current PSP address
    mov qword [rax + psp.rspPtr], rsp    ;Save rsp on callers stack
    pop rax     ;Get old rax back
    push rax    ;and push it back onto the stack
.charFun0CEP:
.serverEP:
    lea rsp, critStakTop
    sti         ;Reenable interrupts

    xor ebx, ebx    ;Zero rbx for later and bl for now
    mov byte [vConDrvSwp], bl   ;Clear the conDrvSwp (use default CON driver)
    mov byte [int28Flag], 1 ;Make it ok to trigger Int 28h
    mov byte [Int24Fail], bl    ;Clear the Int24 returned fail flag
    mov byte [dirFlag], bl  ;Default to look for dir
    mov qword [currentSFT], rbx ;Reset the current SFT field

    push rax        ;Save rax to use temporarily as table base 
    mov bl, ah      ;Move the function number bl (rbx = 0)
    shl ebx, 1      ;Multiply the function number by 2 for offset into table
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
;Default delchar UNLESS a function changes it. Placed here since delete/rename
; is a disk op. If a critical error occurs midway through a delete and the I24h
; handler needs to call DOS for char funcs, we dont want to reset this value as 
; the delete/rename operation might have set it to 0 (we dont do this yet).
    mov byte [delChar], 0E5h
;Similar for volIdFlag. Find file et al are disk ops. 
    mov byte [volIdFlag], 0    ;Force bit clear (else, forces volid search)

    push rax
    mov ah, 82h ;Cancel all critical section!
    int 2ah ;DOS critical section semphore handler (default, iretq)
    pop rax

    mov byte [int28Flag], 0     ;Turn off the ability to trigger Int 28h
    lea rsp, DiskStakTop        ;Swap the stack to the Disk Transfer Stack
    test byte [breakFlag], -1   ;Test if set
    jz .fdGoToFunction
; HANDLE CTRL+BREAK HERE!
    push rax
    call checkBreak   ; Check for a ^C
    pop rax
.fdGoToFunction:
    xchg rbx, qword [oldRBX]    ;Put the call addr in oldRBX and get oldRBX back
%if DEBUG && REGS
    ;Print stack if necessary function
    debugEnterM
    call debPrintDOSStack
    debugExitM
%endif
    call qword [oldRBX]     ;Call the desired function, rax contains ret code
%if DEBUG && HEADERS
    ;Entry function
    debugEnterM
    lea rbp, .l0002
    call debPrintNullString
    jmp short .l0003
.l0002 db "Exiting Int 21h",0Ah,0Dh,0
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
    return     ;Needs to be patched with 50h (PUSH RAX)
    mov eax, 8001h
    int 2ah
    pop rax
    return
dosCrit1Exit:
    return
    mov eax, 8101h
    int 2ah
    pop rax
    return
dosCrit2Enter:
    return
    mov eax, 8002h
    int 2ah
    pop rax
    return
dosCrit2Exit:
    return
    mov eax, 8102h
    int 2ah
    pop rax
    return

;FCB specific returns
fcbGoodExit:
;Preserve the carry flag on entry to DOS
    xor al, al ;Set return code al = 0
    return
fcbErrExit:
;Preserve the carry flag on entry to DOS
;Input: eax = Extended error code to store in SDA 
    call xLatError
    mov al, -1  ;Set return code al = -1
    stc
    return

;Handle and General DOS Returns
;All good exits destroy AT LEAST ax 
extGoodExit2:
;Good exit with an extended return code in eax
    call getUserRegs
    mov dword [rsi + callerFrame.rax], eax    ;Store eax
    jmp short extGoodExit.extGoodCommon
extGoodExit:
;Good exit
;Return code in ax
    call getUserRegs
    mov word [rsi + callerFrame.rax], ax    ;Store ax
.extGoodCommon:
    and byte [rsi + callerFrame.flags], ~1    ;Clear error flag
    clc
    return
extErrExit:
;The extended error exit from DOS
;Jumped to with AL=Extended error code
;Can be called too.
;Input: al = Extended error code
;If relevant (i.e. when called or jumped to from deep in DOS)
;   Returns with: eax = xLat Error
;                 rsi = callerFrame
    movzx eax, al   ;0 rax except for al with error code
    call xLatError
    call checkFail
.noXlat:
    call getUserRegs
    mov word [rsi + callerFrame.rax], ax
    or byte [rsi + callerFrame.flags], 1    ;Set error flag
    stc ;Set carry flag for if this function is called deep inside DOS
    return
xLatError:
;Translates the error code given in ax and sets error code in the var
; Input: ax = Extended Error Code
; Output: ax = Potentially translated Error Code
;         [errorExCde] = Original Error Code
;Preserves all registers except ax
;AH is always returned as 0
    push rbx
    push rcx
    push rsi
    mov word [errorExCde], ax
    mov ebx, eax    ;Save error code to ebx
    lea rsi, errXlatTbl ;Get translation table pointer in rsi
.readEntry:
    lodsw   ;Get the first word of the first table entry and rsi += 2
    cmp al, -1
    je .skipXlat
    cmp al, byte [oldRAX + 1]   ;Cmp to DOS function number (that is in ah)
    je .found
    ;Here, entry not found, goto next entry
    movzx eax, ah   ;Zero extend ah to rax to get number of bytes to skip
    add rsi, rax    ;Goto next entry
    jmp short .readEntry
.found:
    ;Here the table entry is found, now we search for if the error needs xlatng
    movzx ecx, ah   ;Get the number of bytes to check left into ecx
.mainSearch:
    lodsb   ;Get one byte into al
    cmp bl, al  ;Check against the error code
    je .skipXlat    ;If the error code is found, we can skip xlat
    dec ecx ;Avoid loop for the zoomies
    jnz .mainSearch ;Whilst ecx is not zero, keep searching
    ;Here only if ecx is zero, i.e present error code needs translating
    movzx ebx, al ;Move the xLat error code into ebx
.skipXlat:
    mov eax, ebx    ;Return the error code back to eax
    pop rsi
    pop rcx
    pop rbx
    return
setErrorVars:   ;Int 2Fh, AX=1222h
;Looks up the error code in the variable and sets the other error vars
;Called with the lookup table in rsi
;All regs preserved
    push rax
    push rbx
    push rcx
    push rsi
    movzx ebx, word [errorExCde]
    mov ecx, 8  ;Use to shift eax efficiently
.readEntry:
    lodsd   ;Read the dword table entry into eax
    cmp eax, -1  ;If the dword is -1, simply exit
    je .exit    
    cmp al, bl  ;Compare the error codes
    jne .readEntry ;Keep reading entries until we find the one we need
    shr eax, cl
    cmp al, -1
    je .skipClass
    mov byte [errorClass], al
.skipClass:
    shr eax, cl
    cmp al, -1
    je .skipAct
    mov byte [errorAction], al
.skipAct:
    shr eax, cl
    cmp al, -1
    je .exit
    mov byte [errorLocus], al
.exit:
    pop rsi
    pop rcx
    pop rbx
    pop rax
    return
checkFail:
;Checks if the error was dealt with by the user with a Fail on a Int 24h
; and swaps the var error code if so
    test byte [Int24Fail], -1   ;If zero, skip!
    jz .skipFail
    mov word [errorExCde], errFI24  ;Set error to "Fail on Int 24h"
.skipFail:
    push rsi
    lea rsi, extErrTbl
    call setErrorVars
    pop rsi
    return
;========================================:
;      Reentrant Kernel Functions        :
;========================================:
;ah = 33h, Control Break related functions + some undocumented stuff
cbcQuick:
;The entry point for this function if entered through a normal DOS call
    call ctrlBreakCheck
    iretq
cbcServer:
;The entry point for this function if entered through a Server Call (5D00h)
    call ctrlBreakCheck
    call getUserRegs
;Calls which dont modify the regs, preserve them themselves so this so 
; replacing them back on the stack is a small price to pay
    mov qword [rsi + callerFrame.rbx], rbx
    mov qword [rsi + callerFrame.rdx], rdx
;DO NOT WRITE AN ERROR CODE AS THIS WOULD OVERWRITE AN ERROR CODE 
; THAT MAY BE DEPENDED ON. RETURN WITH THE VALUE IN AL AS RETCODE.
    return 
ctrlBreakCheck:
;Can handle subfunctions:
; al = 0: Get state for the break flag in dl
; al = 1: Set the state of the break flag to dl
; al = 2: Exchange the value of the break flag with dl
; al = 3: Error, returns al = -1
; al = 4: Error, returns al = -1
;Undocumented subfunctions:
; al = 5: Get the boot drive in dl
; al = 6: Get the true DOS version number in bx with subversion flags in dl
    cmp al, 6
    je .trueVer
    cmp al, 5
    je .getBtDrv    ;Peel off btdrv req.
    test al, al
    jnz .cbcget     ;Get the state or other functions
    mov dl, byte [breakFlag]    ;Get the state
    return
.cbcget:
    cmp al, 02h
    ja .cbcBad
    jz .cbcxchg ;Function 2
    push rdx
    and dl, 1   ;Get only the bottom bit
    mov byte [breakFlag], dl    ;Set the state
    pop rdx
    return
.cbcxchg:
    and dl, 1
    xchg byte [breakFlag], dl
    return
.cbcBad:
    mov al, -1
    return
.getBtDrv:
;Undocumented.
;Might be unreliable so dont document yet.
;Return 1 based boot drive in dl
    mov dl, byte [bootDrive]    ;Get the 0 based bootDrive number
    inc dl  ;Return a 1 based drive number
    return
.trueVer:
;Undocumented.
;bx returns true DOS number.
;dl has "revision" number in bits 0-2. 
;dh has various flags. All reserved for future use.
    mov bx, dosVerMac
    mov dx, (dosVerFlags << 8) | dosRev
    return

;ah = 50h, set current PSP
scpspQuick:
    call setCurrPSP
    iretq
scpspServer:
setCurrPSP:
    mov qword [currentPSP], rbx ;Set the pointer
    return

;ah = 51h/62h, gives PSP addr/Process ID
gcpspQuick:
    call getCurrPSP
    iretq
gcpspServer:
    call getCurrPSP
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rbx
    return
getCurrPSP:     
    mov rbx, qword [currentPSP]
    return

;ah = 64h, set lookahead flag to al (-1 is on, 0 is off)
sdlQuick:
    call setDriverLookahead
    iretq
sdlServer:
setDriverLookahead:
    mov byte [lookahead], al    
    iretq

;========================================:
;            Kernel Functions            :
;========================================:
diskReset:         ;ah = 0Dh
;Flush all dirty buffers to disk
    call dosCrit1Enter
    mov al, -1  ;Mark all drives as flushable
    call flushAllBuffersForDrive  
    ;Now we free all buffers and set their referenced bit
    mov rdi, qword [bufHeadPtr]
.drBufferLp:
    cmp rdi, -1
    je .drExit
    mov word [rdi + bufferHdr.driveNumber], freeBuffer | (refBuffer << 8)
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    jmp short .drBufferLp
.drExit:
    call dosCrit1Exit
    mov eax, 1120h  ;Redirector flush all 
    int 2fh
    return

selectDisk:        ;ah = 0Eh
;Called with dl = drive number, 0 = A, 1 = B etc...
    mov al, dl
    inc al  ;Convert to 1-based number to avoid 0 meaning current drive
    call getCDSNotJoin  ;Must make sure provided drive is valid
    jc .skipSettingCurrent  ;Join and network drives cant be current drive!
    mov byte [currentDrv], al   ;Set drive as current
.skipSettingCurrent:
    movzx eax, byte [lastdrvNum]   ;Return lastdrive as "errorcode"
    return

getCurrentDisk:    ;ah = 19h, get current default drive
    mov al, byte [currentDrv]
    return

FATinfoDefault:    ;ah = 1Bh
    xor dl, dl
FATinfoDevice:     ;ah = 1Ch
;Input: dl = 1 based drive number
;Output:
;al = sectors per cluster (allocation unit), or FFh if invalid drive
;cx = bytes per sector
;edx = total number of clusters
;rbx = Ptr to media ID byte
    mov al, dl  ;Move drive number into al
    call getCDS    ;Get in workingCDS the cds pointer for drive in al
    jnc .fidCDSFound
    mov eax, errBadDrv          ;Invalid drive error
    jmp short .error 
.fidCDSFound:
    call getDiskData
    jc .error
;Now we have that:
;al = sectors per cluster
;ah = media ID byte
;ebx = total clusters
;cx = bytes per sector
;edx = number of available clusters
    call getUserRegs
    mov byte [mediaByte], ah    ;Store media ID byte
    xor ah, ah
    mov dword [rsi + callerFrame.rdx], ebx  ;Store total clusters
    mov word [rsi + callerFrame.rcx], cx    ;Store bytes per sector
    lea rbx, mediaByte
    mov qword [rsi + callerFrame.rbx], rbx  ;Store pointer to mediaByte
    and byte [rsi + callerFrame.flags], ~1  ;Clear CF
.badExit:
    mov word [rsi + callerFrame.rax], ax    ;Store sectors per cluster
    return
.error:
    call extErrExit ;Set rsi to point to callerFrame
    mov ax, -1
    jmp short .badExit

setIntVector:      ;ah = 25h
;Called with:
;   rdx = Pointer to interrupt handler
;   al = Interrupt number
    cli ;Halt interrupts
    sidt [dosIdtPtr]    ;Get the current IDT base pointer
    movzx eax, al
    shl rax, 4h     ;Multiply IDT entry number by 16 (Size of IDT entry)
    add rax, qword [dosIdtPtr.base]    
    mov word [rax], dx  ;Get low word into offset 15...0
    shr rdx, 10h    ;Bring next word low
    mov word [rax + 6], dx  ;Get low word into offset 31...16
    shr rdx, 10h    ;Bring last dword low
    mov dword [rax + 8], edx
    sti
    return

setResetVerify:    ;ah = 2Eh, turns ALL writes to write + verify
    mov byte [verifyFlag], al
    and byte [verifyFlag], 1       ;Only save the bottom bit
    return
getDOSversion:     ;ah = 30h
    call getUserRegs
    xor ah, ah ;Continue the mainline PC-DOS identification line
    mov byte [rsi + callerFrame.rbx + 1], ah    ;Clear bh 
    mov ax, word [dosVersion] ;Major and minor version in al,ah resp.
    mov word [rsi + callerFrame.rax], ax    ;Save ax
    return

setDOSversion:  ;Int 2Fh, AX=122Fh - Set DOS verstion to report
;Input: dx = Version number. Value of 0 means true value.
    test dx, dx
    jnz .newVal
    mov word [dosVersion], dosVerMac    ;Reset the value
    return
.newVal:
    mov word [dosVersion], dx    ;Store dx in the value to report.
    return

;AH = 1Fh/32h - GET (current) DISK DPB
getCurrentDPBptr:  ;ah = 1Fh, simply falls in Int 21h\ah=32h with dl=0
    xor dl, dl
getDeviceDPBptr:   ;ah = 32h
;On entry: dl = Drive number 1-based drive number (0=Default)
;On exit: rbx = DPB pointer
    mov al, dl
    call getCDS
    jc .bad
    mov rdi, qword [workingCDS]  ;Get pointer to current CDS in rdi
    test word [rdi + cds.wFlags], cdsRedirDrive ;Is dev a redir drv?
    jnz .bad    ;Redirector Drives have no DPBs!
    ;Here ONLY if accessing critical Disk data structures
    call dosCrit1Enter  ;Enter class 1 critical section
    call getDiskDPB   ;See if the Disk structures are still ok 
    call dosCrit1Exit   ;Exit class 1 critical section
    jc .bad
    call getUserRegs
    mov [rsi + callerFrame.rbx], rbp    ;RBP has DPB pointer
    xor al, al
    return
.bad:
    mov al, -1
    return

getInDOSflagPtr:   ;ah = 34h
    lea rdx, inDOS
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rdx  ;save ptr in rbx
    return

getIntVector:      ;ah = 35h
;Called with:
;   al = Interrupt Number
;Returns:
;   rbx = Pointer to interrupt handler
    call muxGetIntVector    ;Get int vector in rbx, all other regs preserved
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rbx  ;Save pointer in rbx
    return

getDiskFreeSpace:  ;ah = 36h
;Input: Drive number in dl (0 = Current)
;Output:    ax = sectors per cluster
;           ebx = number of free clusters
;           cx = bytes per sector
;           edx = total clusters on drive
    mov al, dl
    call getCDS ;Get CDS pointer in workingCDS var for given drive
    jnc .gdfsCDSFound   ;Exit if unable to find/make a CDS for drive
;Else, we at an error.
;Simply return with CY set and error code in al with extended error info
    mov eax, errBadDrv
    call extErrExit ;Call, don't jump, to allow us to set ax to -1
    ;extErrExit sets rsi to caller regs
    mov word [rsi + callerFrame.rax], -1    ;Set ax=FFFFh
    return
.gdfsCDSFound:
    call getDiskData
    jc .error
;Now we have that:
;al = sectors per cluster
;ah = media ID byte
;ebx = total clusters
;cx = bytes per sector
;edx = number of available clusters
    call getUserRegs
    xor ah, ah  ;Don't need media byte, zero extend
    mov dword [rsi + callerFrame.rdx], ebx  ;Store total clusters
    mov word [rsi + callerFrame.rcx], cx    ;Store bytes per sector
    mov dword [rsi + callerFrame.rbx], edx  ;Store # of Free clusters
    and byte [rsi + callerFrame.flags], ~1  ;Clear CF
.badExit:
    mov word [rsi + callerFrame.rax], ax    ;Store sectors per cluster
    return
.error:
    call extErrExit ;Sets rsi to point to callerFrame
    mov ax, -1
    jmp short .badExit


getRetCodeChild:   ;ah = 4Dh, WAIT, get ret code of subprocess
    xor eax, eax
    xchg ax, word [errorLevel]
    jmp extGoodExit

getSysVarsPtr:     ;ah = 52h
    lea rdx, sysVarsPtr
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rdx
    return


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
;dNextFreeClst
    mov dword [rbp + dpb.dNextFreeClst], -1  ;Start is default (clust 2)
;dFreeClustCnt
    mov dword [rbp + dpb.dFreeClustCnt], -1 ;Start with unknown
;wBytesPerSector
    movzx eax, word [rsi + bpb.bytsPerSec]
    mov word [rbp + dpb.wBytesPerSector], ax
;bMaxSectorInCluster
    mov al, byte [rsi + bpb.secPerClus]
    dec al  ;Subtract one to get the max number of the last sector in a cluster
    mov byte [rbp + dpb.bMaxSectorInCluster], al
;bSecPerClustShift
    inc al
    xor ecx, ecx
.cd2:
    shr al, 1
    jz .cd3
    inc ecx
    jmp short .cd2
.cd3:
    mov byte [rbp + dpb.bSecPerClustShift], cl
;wFAToffset, number of reserved sectors in partition
    mov ax, word [rsi + bpb.revdSecCnt]
    mov word [rbp + dpb.wFAToffset], ax
;bNumberOfFATs
    mov al, byte [rsi + bpb.numFATs]
    mov byte [rbp + dpb.bNumberOfFATs], al
;wNumberRootDirEntries
;Gets the number of 32 byte entries in the root directory
;Only valid for FAT 12/16
    movzx eax, word [rsi + bpb.rootEntCnt] ;Must be 0 on FAT 32
    mov word [rbp + dpb.wNumberRootDirEntries], ax  ;0 for FAT32
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
    xor edx, edx    ;Use edx = NumFATs * FATsz temporarily
    movzx ecx, byte [rsi + bpb.numFATs]
    jecxz .cd41
.cd4:
    add edx, eax
    dec ecx
    jnz .cd4
.cd41:
    mov eax, edx    ;Store product in eax
    movzx edx, word [rsi + bpb.revdSecCnt]  ;Get reserved sectors in volume
    add eax, edx
    ;Multiply by 32 and divide by bytes per sector to get number of sectors
    movzx edx, word [rbp + dpb.wNumberRootDirEntries]
    shl edx, 5  ;Bytes in the root directory
    movzx ecx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    push rax    ;Save the current accumulated number of sectors on stack
    mov eax, edx
    xor edx, edx
    div ecx
    mov edx, eax
    pop rax
    add eax, edx    ;This adds nothing if FAT32
    ;eax = BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors
    ;aka eax = Start sector of the data area in volume
    mov dword [rbp + dpb.dClusterHeapOffset], eax
;dClusterCount
    sub ebx, eax    ;ebx = Number of sectors in the data area
    mov eax, ebx    ;Move number of sectors in data area into eax
    xor edx, edx
    mov ebx, 1
    movzx ecx, byte [rbp + dpb.bSecPerClustShift]
    shl ebx, cl ;Get sectors per cluster
    div ebx ;Data area sector / sectors per cluster = cluster count
    inc eax ;Maximum valid cluster address is cluster count + 1
    mov dword [rbp + dpb.dMaxClusterAddr], eax    ;eax = Max cluster address
;dFirstUnitOfRootDir
    cmp eax, fat16MaxClustCnt  ;If above, its FAT32
    mov eax, dword [rsi + bpb32.RootClus]   ;Just save this if FAT32
    ja .cd5
    ;Else, we need to find the first sector of the root directory
    ;Add the number of reserved sectors to the number of FATs*FATsz
    movzx eax, word [rbp + dpb.wFAToffset]  ;Get reserved count
    movzx ecx, byte [rbp + dpb.bNumberOfFATs]
    jecxz .cd5
.cd51:
    add eax, dword [rbp + dpb.dFATlength]
    dec ecx
    jnz .cd51
.cd5:
    mov dword [rbp + dpb.dFirstUnitOfRootDir], eax
    mov byte [rbp + dpb.bAccessFlag], -1    ;Denote not yet accessed
    call readFSInfoSector   ;If FAT32, updates free cluster data with FSinfo
;Exit epilogue
.exit:
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
    ;call debMakeDebuggerRespond
    jmp short .l0001
.l0000 db "Constructed DPB from given device BPB",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    return

getVerifySetting:  ;ah = 54h
    mov al, byte [verifyFlag]   ;al is the return value in this case
    return

getExtendedError:  ;ah = 59h
    call getUserRegs
    mov ax, word [errorExCde]
    mov ch, byte [errorLocus]
    mov bh, byte [errorClass]
    mov bl, byte [errorAction]
    mov rdi, qword [errorVolLbl]
    mov word [rsi + callerFrame.rax], ax
    mov word [rsi + callerFrame.rbx], bx
    mov byte [rsi + callerFrame.rcx + 1], ch
    mov qword [rsi + callerFrame.rdi], rdi
noOp:
    return

systemServices: ;ah = 61h
;al = 0 -> Get Environment pointer in rdx
;   Output: rdx -> Environment Pointer. May be a null pointer. Caller checks!
;al = 1 -> Get Command Line Arguments Pointer in rdx
;   Output: rdx -> Pointer to whatever was passed as a CR terminated 
;                   command line.
;al = 2 -> Get ptr to ASCIIZ name for program in rdx. Might not be FQ.
;   Output: CF=NC: rdx -> Filename 
;           CF=CY: eax = Error code (errAccDen) if no filename ptr.
;                   The error case should only happen for special
;                   programs that are launched without an environment
;                   as DOS has nowhere to put the filename string.
;al > 2: Returns CF=CY and eax = Error code (errInvFnc).
    cmp al, 1
    je .getCmdLineArgs
    cmp al, 2
    jbe .getEnvPtr
    mov byte [errorLocus], eLocUnk  
    mov eax, errInvFnc  ;Error, with invalid function number error
.exitBad:
    jmp extErrExit
.getCmdLineArgs:
    mov rdx, qword [currentPSP]
    lea rdx, qword [rdx + psp.cmdLineArgPtr]   ;Get the cmdargs pointer
    jmp short .gepExitOk
.getEnvPtr:
;Gets the environment pointer in rdx
    mov rdx, qword [currentPSP]
    mov rdx, qword [rdx + psp.envPtr]   ;Get the environment pointer
    test al, al     ;Was al=0?
    jz .gepExitOk   ;Exit if al = 0 since we have the pointer we need!
    test rdx, rdx   ;Check if the env pointer is ok to use
    jz .gepFail
    cmp rdx, -1
    je .gepFail
;Here we search for the double 00 and then check if it is 0001 and
; pass the ptr to the word after.
    push rcx
    xor ecx, ecx
    mov ecx, 7FFFh  ;Max environment size
.gep0:
    cmp word [rdx], 0   ;Zero word?
    je short .gep1
    inc rdx         ;Go to the next byte
    dec ecx
    jnz short .gep0
.gep00:
;Failure here if we haven't hit the double null by the end of 32Kb
    pop rcx
.gepFail:
    xor edx, edx        ;Turn it into null pointer
    mov eax, errAccDen  ;Set error code here
    jmp short .exitBad  ;Return setting CF=CY and errAccDen (no pointer)
.gep1:
    add rdx, 2  ;Skip the double null
    cmp word [rdx], 1   ;Check if one more string in environment
    jne .gep00
    add rdx, 2  ;Skip the 0001 word.
    pop rcx
.gepExitOk:
    call getUserRegs
    mov qword [rsi + callerFrame.rdx], rdx
    jmp extGoodExit

getsetDiskSerial:  ;ah = 69h, get/set disk serial number
;Wraps the generic disk IO call to get/set the disk serial number and
; associated information.
;Input:
;   al = 0: Get disk serial number
;   al = 1: Set disk serial number
;   ebx = 1-based drive number
;   rdx -> Pointer to an ID parameter block
    movzx eax, al
    mov ecx, 0866h  ;Get Disk Serial Packet major/minor codes
    test eax, eax
    jz .doIoctl
    sub ecx, 20h    ;Turn Get major/minor codes into a Set
    cmp eax, 1      ;Is this a set?
    je .doIoctl
    mov eax, errInvFnc  ;Else, return error, invalid function!
    jmp extErrExit
.doIoctl:
    mov eax, 0Dh    ;Do block dev Generic IOCTL call
    call ioctrl     ;Sets up the return state internally
    return          ; so just return normally