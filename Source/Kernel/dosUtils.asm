;DOS utility functions (Will be made available through Int 4F ah=12xx eventually)

;Utilities
getUserRegsInRSI:
;Returns ptr to user regs in rsi
    mov rsi, [oldRSP]
    ret

getCDS:
    ;Gets the CDS for the current drive in rax
    ;Input: rax = Drive number, 0 = A ...
    ;Output: rbx = Pointer to CDS for drive in rax
    push rax
    push rcx
    push rdx
    lea rbx, qword [cdsHeadPtr] ;Point to cds array
    mov rcx, cds_size   
    xor edx, edx
    mul ecx 
    add rbx, rax    ;Move rbx to the right offset in the array
    pop rdx
    pop rcx
    pop rax
    ret

;DOS KERNEL FUNCTIONS
;AH = 1Fh/32h - GET (current) DISK DPB
getCurrentDPBptr:  ;ah = 1Fh, simply falls in Int 41h\ah=32h with dl=0
    xor dl, dl
getDeviceDPBptr:   ;ah = 32h
;On entry: dl = Drive number
;On exit: rbx = DPB pointer
    test dl, dl
    jnz .gddpskipdefault
    mov dl, byte [currentDrv]   ;Get current drive code, 0 = A, 1 = B etc...
    jmp short .gddpcommon
.gddpskipdefault:
    ;Decrement the drive letter since 0 = Default, 1 = A etc...
    dec dl
.gddpcommon:
    call findDPB ;Get in rbp the dpb pointer for drive dl
    test al, al
    jz .gddpMediaCheck
;Put in here error info
    mov word [errorExCde], errBadDrv ;Invalid drive spec
    mov byte [errorLocus], eLocDsk    ;Block device driver
    mov byte [errorClass], eClsNotFnd    ;Drive not found
    mov byte [errorAction], eActRetUsr   ;Retry after intervention
    ret ;Return. al = -1
.gddpMediaCheck:
;Media Check Section
    mov byte [diskReqHdr + mediaCheckReqPkt.hdrlen], mediaCheckReqPkt_size
    mov byte [diskReqHdr + mediaCheckReqPkt.unitnm], dl
    mov byte [diskReqHdr + mediaCheckReqPkt.cmdcde], drvMEDCHK
    mov word [diskReqHdr + mediaCheckReqPkt.status], 0
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + mediaCheckReqPkt.medesc], al
    mov al, dl  ;Save device number in al
    mov rdx, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    mov qword [drvrPtr], rdx
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    call goDriver
    jc .gddpError
    mov dl, al
    cmp byte [diskReqHdr + mediaCheckReqPkt.medret], 1 ;Certified no change
    je .gddpretdbp
    cmp byte [diskReqHdr + mediaCheckReqPkt.medret], 0
    jne .gddpBuildBPB   ;This means Media changed declared
    call testDirtyBufferForDrive
    jc .gddpretdbp    ;If there is a dirty buffer for the drive, skip build bpb
.gddpBuildBPB:
;BPB Build Section, only here if need a new bpb, i.e. sure of a new device
    call findLRUBuffer  ;Get lru buffer pointer in rbx
    cmp dl, byte [rbx + bufferHdr.driveNumber]  ;Does buffer belong to old drv?
    je .gddpBuildBPBInvalidateBuffer    ;Yes, immediately invalidate data
    ;If no, flush the data to disk.
.gddpBuildBPBFlush:
    mov rsi, rbp    ;Save rbp as pointer to old dl drive dpb
    mov rbp, rbx    ;Get buffer header pointer in rbp
    call flushBuffer    ;Flush the buffer to disk, rbx preserved
    mov rbp, rsi    ;Return old drive dpb pointer to rbp
    jc .gddpErrorType2  ;rbx points to buffer header
.gddpBuildBPBInvalidateBuffer:
    ;Write new buffer header
    mov byte [rbx + bufferHdr.driveNumber], dl
    mov byte [rbx + bufferHdr.bufferFlags], dataBuffer
    mov qword [rbx + bufferHdr.bufferLBA], 0
    mov byte [rbx + bufferHdr.bufFATcopy], 1
    mov dword [rbx + bufferHdr.bufFATsize], 0
    mov qword [rbx + bufferHdr.driveDPBPtr], rbp
    lea rbx, qword [rbx + bufferHdr.dataarea]
    ;Build BPB request
    mov byte [diskReqHdr + bpbBuildReqPkt.hdrlen], bpbBuildReqPkt_size
    mov byte [diskReqHdr + bpbBuildReqPkt.unitnm], dl
    mov byte [diskReqHdr + bpbBuildReqPkt.cmdcde], drvBUILDBPB
    mov word [diskReqHdr + bpbBuildReqPkt.status], 0 
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + bpbBuildReqPkt.medesc], al
    mov qword [diskReqHdr + bpbBuildReqPkt.bufptr], rbx ;Put lru pointer in rbx

    mov rdx, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    mov qword [drvrPtr], rdx
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    call goDriver
    jc .gddpError
    mov rsi, qword [diskReqHdr + bpbBuildReqPkt.bpbptr]
    ;rbp points to dpb so we good to go
    ;Call int 41h ah=53h Build DPB without reentering Int 41h
    ;Since this function doesnt modify the caller stack, it is safe to do so
    call createDPB 
.gddpretdbp: 
    mov byte [rbp + dpb.bAccessFlag], -1    ;Clear access flag
    call getUserRegsInRSI
    mov qword [rsi + callerFrame.rbx], rbp  ;Here, all paths have rbp as dpbptr
    xor al, al  ;Set al = 0 to indicate rbx=dpb pointer
    ret
.gddpretdpbFail:
    call getUserRegsInRSI
    or qword [rsi + callerFrame.flags], 1   ;Set CF=CY
    mov word [errorExCde], errFI24 ;Fail on INT 44h error code
    ret
.gddpError:
;Abort, Retry, Ignore are the only acceptible responses
;Entered with rbp = dpb for failing drive
;             rdx = driver header that caused fault
    mov rsi, rdx    ;rdx points to driver header in both cases
    mov rbx, qword [oldRSP]
    mov al, byte [rbx + callerFrame.rdx]    ;Get low byte = dl = Drive number
    mov dl, al  ;Save in dl
    test al, al
    jnz .gddpE0
    mov al, byte [currentDrv]
    jmp short .gddpE1
.gddpE0:
    dec al
.gddpE1:
    mov ah, 36h ;Read operation, data area, abort/retry/ignore, disk error
    mov di, word [diskReqHdr + drvReqHdr.status]   ;Get low byte of status
    and di, 0FFh    ;Save lo byte only
    mov qword [rdiErrorPtr], rdi    ;Save this byte
    mov word [errorExCde], di     ;Save driver error code
    add word [errorExCde], drvErrShft    ;Add offset to driver error codes
    mov byte [errorDrv], al     ;Save the drive on which the error occured
    mov byte [errorLocus], eLocDsk    ;Error in Block Device Request code
    mov byte [errorClass], eClsMedia   ;Media error (bad BPB or other) code
    mov byte [errorAction], eActRet   ;Retry request code
    call criticalDOSError   ;Critical error handler
    test al, al
    jz .gddpretdbp  ;Ignore error, return, rbp has old dpb pointer
    cmp al, 1
    je getDeviceDPBptr ;Reenter the function, dl has drive code
    int 43h ;Else, restart DOS
.gddpErrorType2:
;Error flushing the old buffer
;   rbx = Buffer header for data transfer
    mov ah, 39h ;Write operation, abort/retry/ignore/fail, disk error
    cmp byte [rbx + bufferHdr.bufferFlags], dosBuffer
    je .gddpErrorType2main
    or ah, 2h   ;Set bit 1
    cmp byte [rbx + bufferHdr.bufferFlags], fatBuffer
    je .gddpErrorType2main
    mov ah, 3Dh ;Set bit 2, clear bit 1
    cmp byte [rbx + bufferHdr.bufferFlags], dirBuffer
    je .gddpErrorType2main
    or ah, 2h   ;Set bit 2 and 1
.gddpErrorType2main:    
    mov di, word [diskReqHdr + drvReqHdr.status]   ;Get low byte of status
    and di, 0FFh    ;Save lo byte only
    mov word [errorExCde], di     ;Save driver error code
    add word [errorExCde], drvErrShft   ;Add offset to driver error codes
    mov al, byte [rbx + bufferHdr.driveNumber]
    mov byte [errorDrv], al
    mov byte [errorLocus], eLocDsk    ;Error in Block Device Request code
    mov byte [errorClass], eClsMedia  ;Media error (bad disk write) code
    mov byte [errorAction], eActRet   ;Retry request code
    mov rsi, qword [rbx + bufferHdr.driveDPBPtr]
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Get device driver header in rsi
    call criticalDOSError   ;Critical error handler
    cmp byte [rbx + bufferHdr.bufferFlags], fatBuffer
    je .gddpErrorType2FatDir
    cmp byte [rbx + bufferHdr.bufferFlags], dirBuffer
    je .gddpErrorType2FatDir
    test al, al
    jz .gddpBuildBPBInvalidateBuffer ;Ignore error, invalidate data
    cmp al, 1
    je .gddpBuildBPBFlush   ;Retry flush, rbx has buffer pointer
    cmp al, 3
    je .gddpretdpbFail
    int 43h ;al = 2, means just abort
.gddpErrorType2FatDir:
    test al, al ;Ignore converted to fail
    jz .gddpretdpbFail
    cmp al, 1
    je .gddpBuildBPBFlush   ;Retry flush, rbx has buffer pointer
    cmp al, 3
    je .gddpretdpbFail
    int 43h ;al = 2, means just abort
;-------------------------------------------



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