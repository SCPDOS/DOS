;DOS utility functions (Will be made available through Int 4F ah=12xx eventually)

;Basic Drive related Utilities
;Any function which takes args in rax (or any subpart of it), has that 
; argument provided on the stack when called from Int 4Fh interface (when 
; that gets set up)


setDPBAsWorking:
;Gets dpb in rbp and saves to curDrvDPB (working dpb)
    mov qword [workingDPB], rbp
    ret

getUserRegs:   ;Int 4Fh AX=1218h
;Returns ptr to user regs in rsi
    mov rsi, qword [oldRSP]
    ret

walkCDSarray:     ;Int 4Fh AX=1217h
    ;Gets the CDS for the current drive in al
    ;Input: al = Drive number, 0 = A ...
    ;Output: CF=NC => rsi = Pointer to CDS for drive in al (and workingCDS var)
    ;        CF=CY => al not valid
    cmp al, byte [lastdrvNum]
    jb .ctn
    stc 
    ret
.ctn:
    push rax
    push rdx
    movzx eax, al
    mov rsi, cds_size
    xor edx, edx
    mul esi ;Get the multiples of CDS's to skip
    lea rax, qword [cdsHeadPtr] ;Get the first CDS
    add rsi, rax    ;Add the CDS array to the offset into it
    mov qword [workingCDS], rsi  ;Save in data area
    pop rdx
    pop rax
    clc
    ret

walkDPBchain:
;Called with al = 0 based drive number
;Returns in rsi a pointer to the DPB or if CF=CY, invalid drive number
    mov rsi, qword [sftHeadPtr]  ;Get variable pointing to first DPB
.walk:
    cmp rsi, -1
    je .exitBad
    cmp byte [rsi + dpb.bDriveNumber], al
    je .exit    ;Drive found
    mov rsi, qword [rsi + dpb.qNextDPBPtr]  ;Go to next drive 
    jmp short .walk
.exitBad:
    stc
.exit:
    %if DEBUG
    ;Print DPB 
    debugEnterM
    mov r8, rbp ;Save dpb pointer
    lea rbp, .l0000
    call debPrintNullString
    mov rbp, r8
    call debDPBptr
    jmp short .l0001
.l0000 db "Internal call to find DPB",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    ret

getCDS:
;Gets the device DPB and saves it in the DOS variable
;Input: al = 1 based drive number
;Sets workingCDS var with the CDS for the device. 
;   If device on a network, sets CF (currently error)
    test al, al
    jnz .skip
    mov al, byte [currentDrv]   ;Get current drive
    inc al
.skip:
    dec al  ;Convert to 0 based (0=A: ...)
    push rsi
    mov byte [errorLocus], eLocDsk  ;Set the locus
    test byte [dosInvoke], -1   ;If non-zero, invalid
    jz .physDrive
    ;Invalid invokation (21/5D00 invokation not yet supported)
    ;If returned with CF=CY, consider it an error for now
    ;Eventually, here we will build a fresh DPB for the network drive
    jmp short .exitBad1
.physDrive:
    call walkCDSarray ;Get CDS pointer in RSI and in curCDSPtr
    jc .exitBad
    test word [rsi + cds.wFlags], cdsPhysDrive
    jnz .exitOk ;Exit with flag cleared
    ;Else Return to unknown error locus
.exitBad:
    mov byte [errorLocus], eLocUnk
.exitBad1:
    stc
.exitOk:
    pop rsi
    ret
    
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;           DOS KERNEL FUNCTIONS, accessible through Int 41h
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
    test rdi, 8000h ;Is dev a virtual, network, drv (they have no DPB)?
    jnz .bad
    call dosCrit1Enter  ;Enter class 1 critical section
    call getDiskDPB   ;See if the Disk structures are still ok 
    call dosCrit1Exit   ;Exit class 1 critical section
    jc .bad
    call getUserRegs
    mov [rsi + callerFrame.rbx], rbp    ;RBP has DPB pointer
    ret
.bad:
    mov al, -1
    ret

;AH = 53h - CREATE DPB
createDPB:         ;generates a DPB from a given BPB
;Only translates the data that can be garnered from a BPB to the DPB
;This is done so that the other fields of the DPB can be recycled
;Input: rsi = ptr to the BPB
;       rbp = ptr to the DPB
;bMediaDescriptor
    mov al, byte [rsi + bpb.media]
    mov byte [rbp + dpb.bMediaDescriptor], al
;bAccessFlag
    mov byte [rbp + dpb.bAccessFlag], -1    ;Not accessed
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