;---------------------------------------------------:
;                   KERNEL FUNCTIONS                :
;---------------------------------------------------:
makeDIR:           ;ah = 39h
;For make, the path must exist but the final componant must not exist.
;Input: rdx = Pointer to ASCIIZ string
    mov rdi, rdx
    call strlen
    cmp ecx, 64
    jbe .okLength
.badPath:
    mov al, errPnf
    jmp extErrExit
.okLength:
    mov rsi, rdx
    call checkPathspecOK
    jc .badPath  ;Don't allow any malformed chars
.pathOk:
    call scanPathWC
    jc .badPath ;Dont allow wildcards
    ;Path is ok, now proceed
    lea rdi, buffer1    ;Build the full path here
    call getFilePath ;Get a Directory path in buffer1, hitting the disk
    ;If the path exists, exit error
    jnc extErrExit
    ;Now check if the reason for the error was that the last pathcomp was 0
    call checkFailingComp
    jnz extErrExit
    ;So all is well, the new subdirectories name is in fcbName
    ;The parent dir's directory entry is in the curDirCopy
    call testCDSNet ;Check if the working CDS is a NET CDS
    ;returns in rdi the working cds ptr
    jnc .notNet
    mov eax, 1103h
    int 4fh
    jc extErrExit
    jmp extGoodExit
.notNet:
    call dosCrit1Enter
    ;Current dpb ptr is already set
    ;Setup directory variables to now search for a free space in parent dir.
    ;First we make a dummy dir in curDirCopy
    mov rbp, qword [workingDPB]
    lea rsi, fcbName    ;Copy the dir name we searched for over
    lea rdi, curDirCopy
    movsq   ;Copy the name over
    movsd
    call readDateTimeRecord ;Update DOS internal Time/Date variables
    call getDirDTwords  ;Get current D/T words packed in eax
    mov dword [curDirCopy + fatDirEntry.crtTime], eax
    mov dword [curDirCopy + fatDirEntry.wrtTime], eax
    xor eax, eax
    mov dword [curDirCopy + fatDirEntry.fileSize], eax
    mov byte [curDirCopy + fatDirEntry.attribute], dirDirectory 
    mov eax, dword [dirClustPar]
.searchForDirSpace:
    mov dword [dirClustA], eax
    xor eax, eax    ;Reset the search to the start of the current directory
    mov word [dirSect], ax
    mov dword [dirEntry], eax
    call findFreeDiskDirEntry   ;rsi = ptr to a dir entry in a disk buffer
    jnc .dirEntryFnd
    cmp dword [dirClustPar], 0  ;If the parent = 0 => Root Dir Fat12/16
    je .bad ;Set CF and exit
    call growDirectory  ;Increase directory size by 1 cluster, writes to buffer
    jc .bad
    cmp eax, -1 ;Disk Full?
    je .bad
    ;Else eax = Newly allocated cluster
    jmp short .searchForDirSpace
.dirEntryFnd:
;rdi points to current directory copy
;rsi now points to offset in the buffer to write the entry to
;Convert rsi into a byte offset into the buffer and save the sector number
    mov rbx, qword [currBuff]
    mov rax, qword [rbx + bufferHdr.bufferLBA]
     
    mov qword [tempSect], rax   ;Save in temp sector variable
    add rbx, bufferHdr.dataarea ;Goto data area
    sub rsi, rbx    ;rsi now contains offset into buffer data area
    mov word [entry], si    ;Word is enough to store byte offset into sector
;Must now request a cluster and sanitise it
    call startNewChain  ;Get cluster number in eax
    jc .badExit
    call sanitiseCluster    ;Sanitise this cluster, preserve eax, writes to buf
    jc .badExit
   ;Save the cluster in the dummy dir pointed to by rdi
    mov word [curDirCopy + fatDirEntry.fstClusLo], ax
    shr eax, 10h    ;Get high word low
    mov word [curDirCopy + fatDirEntry.fstClusHi], ax
    mov rax, qword [tempSect]   ;Get the sector back
    call getBufForDirNoFile
    jc .badExit
    movzx eax, word [entry] ;Get byte offset into sector back
    lea rsi, curDirCopy    ;The dummy dir is the source now
    lea rdi, qword [rbx + bufferHdr.dataarea + rax] ;Point to dir entry directly
    mov ecx, 4
    rep movsq   ;Copy over the buffered directory
    call markBufferDirty ;We wrote to this buffer
    ;Now need to read in data sector and make two entries . and ..
    push rdi
    push rcx
    mov ecx, 11
    lea rdi, curDirCopy
    mov al, " "
    rep stosb
    pop rcx
    pop rdi
    mov rax, "."
    mov byte [curDirCopy], al
    movzx eax, word [curDirCopy + fatDirEntry.fstClusLo]
    movzx edx, word [curDirCopy + fatDirEntry.fstClusHi]
    mov byte [curDirCopy + fatDirEntry.attribute], dirDirectory 
    shl edx, 10h
    or eax, edx ;Add upper bits to eax cluster number
    call getStartSectorOfCluster    ;Get start sector in rax
    call getBufForDirNoFile
    jc .badExit
    ;rbx has buffer pointer now
    lea rsi, curDirCopy
    lea rdi, qword [rbx + bufferHdr.dataarea]
    mov ecx, 4  ;4 qwords to copy
    rep movsq
    ;Now create .. entry
    mov byte [curDirCopy + 1], "."  ;Store a second dot
    mov eax, dword [dirClustPar]    ;Get starting cluster of parent dir
    call getFATtype
    cmp ecx, 2
    jb .notFAT32
    cmp dword [rbp + dpb.dFirstUnitOfRootDir], eax  ;Is the parent root clust?
    jne .notFAT32
    xor eax, eax    ;Store 0 if it is to keep algorithms happy
.notFAT32:
    mov word [curDirCopy + fatDirEntry.fstClusLo], ax
    shr eax, 10h
    mov word [curDirCopy + fatDirEntry.fstClusHi], ax
    lea rsi, curDirCopy
    mov ecx, 4
    rep movsq
    call markBufferDirty ;We wrote to this buffer
    call writeThroughBuffers    ;Write the buffers to disk
    jc .badExit
.okExit:
    ;AND WE ARE DONE!
    call dosCrit1Exit
    xor eax, eax
    jmp extGoodExit
.bad:
    mov eax, errAccDen
.badExit:
    call cancelWriteThroughBuffers
    call dosCrit1Exit
    jmp extErrExit

removeDIR:         ;ah = 3Ah
    mov rdi, rdx
    call strlen
    cmp ecx, 64
    jbe .okLength
.badPath:
    mov al, errPnf
    jmp extErrExit
.okLength:
    mov rsi, rdx
    call checkPathspecOK
    jc .badPath  ;Don't allow any malformed chars
.pathOk:
    call scanPathWC
    jc .badPath ;Dont allow wildcards
    ;Path is ok, now proceed
    lea rdi, buffer1    ;Build the full path here
    call getDirPath ;Get a Directory path in buffer1, hitting the disk
    jc .badPath    ;Path Doesn't exist
    call testCDSNet ;Check if the working CDS is a NET CDS
    jnc .notNet
    mov eax, 1101h  ;RMDIR for net
    int 4fh
    jc extErrExit
    jmp extGoodExit
.notNet:
    call dosCrit1Enter
    mov rbp, qword [workingDPB]
    ;Now let use check that our directory is not the CDS currentdir
    mov rsi, qword [workingCDS]
    lea rdi, buffer1
    call strlen ;Get the length of the full qualified name in ecx
    mov word [pathLen], cx
    call strcmp ;Then compare rdi to CDS string
    jnz .notEqual
    mov eax, errDelCD   ;Cant delete whilst in current directory
    call dosCrit1Exit
    jmp extErrExit
.notEqual:
    mov rdi, rsi    ;rsi points to CDS
    ;If the given path length is one more than the backslash offset
    ; due to the terminating null, then the user is trying to delete the 
    ; root dir. Fail this.
    movzx ecx, word [rdi + cds.wBackslashOffset]
    inc ecx
    cmp cx, word [pathLen]
    je .accessDenied
    call getDiskDirectoryEntry  ;Setup tempSect and entries (byte offset)
    ;for the entry in the sector we are hoping to delete
    movzx eax, word [curDirCopy + fatDirEntry.fstClusHi]
    shl eax, 10h
    movzx ebx, word [curDirCopy + fatDirEntry.fstClusLo]
    or eax, ebx
    mov dword [dirClustPar], eax    ;Store the first cluster of subdir here
    call getStartSectorOfCluster  ;Check first sector of cluster is . and ..
    call getBufForDirNoFile
    jc .exitBad
    ;rbx points to buffer
    lea rsi, qword [rbx + bufferHdr.dataarea]
    mov rax, ".       "
    cmp qword [rsi], rax
    jne .accessDenied
    mov ah, "." ;Screw the partial stall
    cmp qword [rsi + fatDirEntry_size], rax  ;Cmp next entry to ..
    jne .accessDenied
    add rsi, fatDirEntry_size
    lea rdi, curDirCopy
    mov ecx, 4
    rep movsq   ;Copy the .. entry into the curDirCopy to find parent later
;Now we gotta walk every sector of this directory to see if it is empty.
; If not, we cannot proceed. Do an inclusive search for *.*
    lea rdi, fcbName
    mov al, "?"
    mov ecx, 11
    rep stosb   ;Store the pattern to search for
    xor al, al  ;Store a terminating zero
    stosb
    mov eax, dword [dirClustPar]    ;Get searched directory starting cluster
    push rax    ;Save on stack temporarily
    mov dword [dirClustA], eax
    xor eax, eax    ;Reset the search to the start of the directory
    mov word [dirSect], ax
    mov dword [dirEntry], 2 ;Start at the second directory entry (past . and ..)
    mov byte [searchAttr], dirInclusive ;Search for anything
    pop rax
    call getStartSectorOfCluster
    call getBufForDOS   ;Not quite a DOS buffer but we won't be making changes
    jc .exitBad
    call adjustDosDirBuffer    ;rbx has the buffer pointer for this dir sector
    add rsi, fatDirEntry_size*2 ;Start searching from the second entry in dir
    sub ecx, 2  ;Two fewer entries to search for in this sector
    mov byte [fileDirFlag], -1  ;Make sure we are searching for everythin
    call searchDir.rmdirEP
    jnc .accessDenied   ;If a file is found, access denied, we can't delete this
    ;Else, this is a empty dir, we can remove it
    ;tempSect has the sector of the entry and entries points to the offset
    mov rax, qword [tempSect]
    call getBufForDOS
    jc .exitBad
    call adjustDosDirBuffer
    movzx eax, word [entry]
    lea rsi, qword [rbx + bufferHdr.dataarea]
    add rsi, rax    
    mov al, byte [delChar]  ;Move the delchar in place
    mov byte [rsi], al  ;Store delchar there
    movzx eax, word [rsi + fatDirEntry.fstClusLo]
    movzx edx, word [rsi + fatDirEntry.fstClusHi]
    call markBufferDirty ;We wrote to this buffer
    shl edx, 10h
    or eax, edx
    ;Now remove the FAT chain
    call unlinkFAT
    jc .exitBad
    call writeThroughBuffers
    jc .exitBad
    call dosCrit1Exit
    xor eax, eax
    jmp extGoodExit
.accessDenied:
    mov eax, errAccDen
.exitBad:
    call cancelWriteThroughBuffers
    stc
    call dosCrit1Exit
    jmp extErrExit

setCurrentDIR:     ;ah = 3Bh, CHDIR
;Input: rdx = Pointer to ASCIIZ string
    mov rdi, rdx
    call strlen
    cmp ecx, 64
    jbe .okLength
.badPath:
    mov al, errPnf
    jmp extErrExit
.okLength:
    mov rsi, rdx
    call checkPathspecOK
    jc .badPath  ;Don't allow any malformed chars or wildcards
    call checkPathNet
    jz .badPath ;Or Net paths
    ;Path is ok, now proceed
    lea rdi, buffer1    ;Build the full path here
    call getDirPath ;Get a Directory path in buffer1, hitting the disk
    jc extErrExit   ;Exit with error code in eax
    ;The path must've been ok, so now copy the path into the CDS
    ;The copy of the directory entry has the start cluster of this dir file
    mov rsi, qword [workingCDS] ;Copy the CDS to the tmpCDS
    test word [rsi + cds.wFlags], cdsRedirDrive
    jnz .net    ;This is done by the redirector for redirector drives
    lea rdi, tmpCDS
    mov ecx, cds_size
    rep movsb
    ;If the path is longer than 67, call it an invalid path
    lea rdi, buffer1
    call strlen ;Get the length of this path
    cmp ecx, 67
    ja .badPath
    mov rsi, rdi    ;Move buffer source to rsi
    lea rdi, tmpCDS
    rep movsb   ;Copy the path over
    ;Now get the start cluster from the directory copy
    movzx edx, word [curDirCopy + fatDirEntry.fstClusLo]
    movzx eax, word [curDirCopy + fatDirEntry.fstClusHi]
    shl eax, 10h
    or eax, edx ;Add low bits to eax
    mov dword [tmpCDS + cds.dStartCluster], eax ;Store this value in cds
    lea rsi, tmpCDS
    mov rdi, qword [workingCDS]
    mov ecx, cds_size
    call dosCrit1Enter  ;Ensure no task interrupts our copy
    rep movsb
    call dosCrit1Exit
    xor eax, eax
    jmp extGoodExit    ;Exit with a smile on our faces
.net:
;SDA Vars are setup for this request
    mov eax, 1105h
    int 4fh
    jc extErrExit
    jmp extGoodExit
getCurrentDIR:     ;ah = 47h
;Input: rsi = Pointer to a 64 byte user memory area
;       dl = 1-based Drive Number (0 = Default) 
    call dosCrit1Enter
    mov al, dl  ;Move drive number into al
    call setDrive ;Set drive variables if it is valid and NOT join
    jnc .okDrive
.badExit:
    call dosCrit1Exit
    mov eax, errBadDrv
    jmp extErrExit
.okDrive:
    ;Now we update the DPB, to be accurate for swapped disks
    push rsi
    push rdi
    mov rdi, qword [workingCDS] ;Get CDS ptr in rdi
    call getDiskDPB
    pop rdi
    pop rsi
    jc extErrExit
    ;Here, work needs to be done to ensure that the path built is proper
    mov rdi, rsi    ;Save destination buffer in rdi
    mov rsi, qword [workingCDS]  ;Get pointer to current CDS in rsi
    xor eax, eax
    dec eax
    cmp dword [rsi + cds.dStartCluster], eax    ;StartCluster != -1 is all ok
    jne .writePathInBuffer
    inc eax
    mov dword [rsi + cds.dStartCluster], eax    ;Set to root dir
    ;Here we now add a terminating null at wBackslashOffset
    movzx eax, word [rsi + cds.wBackslashOffset]
    mov byte [rsi + rax + 1], 0 ;Store a zero just past the backslash
.writePathInBuffer:
    movzx eax, word [rsi + cds.wBackslashOffset]
    inc eax ;Go past the backslash
    add rsi, rax ;Add this many chars to rsi to point to first char to copy
    call strcpy
    call dosCrit1Exit
    mov eax, 0100h  ;RBIL -> MS software may rely on this value
    jmp extGoodExit ;Exit very satisfied with ourselves that it worked!

trueNameMultiplex:  ;Int 4Fh, AX=1221h
    push rax
    mov eax, 6000h
    int 41h
    pop rax
    return
    
trueName:          ;ah = 60h, get fully qualified name. 
    ;Called with a path in rsi and 128 byte buffer in rdi
    call checkPathspecOK    ;This preserves rsi
    jnc .pathspecOk ;If CF=NC this path is totally ok
    jz .pathspecOk  ;If the last part has wildcards, allow it too
.badPath:
    mov eax, errPnf
    jmp extErrExit
.pathspecOk:
    push rdi    ;Save the destination
    lea rdi, buffer1    ;Build the full path here
    call canonicaliseFileName
    pop rdi
    jc extErrExit
    lea rsi, buffer1
    call strcpy
    xor eax, eax
    jmp extGoodExit

;-----------------------------------
;    General Directory Routines    :
;-----------------------------------

findFreeDiskDirEntry:
;Find a space in the directory we are searching for a directory entry
;Accept first entry starting with a 0E5h or 00h
;We place delchar in the first byte of fcbName to indicate we are searching 
; for a empty dir entry and then call searchDir (but recall this must only
; be called for CDS's that are NOT net CDS's).
;Input: qword [workingDPB] = DPB of transacting device
;       Directory vars must be set up for the directory we are searching in
;Output: CF=CY => Error, eax has error code
;        CF=NC => Refer to getDiskDirectoryEntry
    ;breakpoint
    mov al, byte [delChar]
    mov byte [fcbName], al
    call searchDir  ;Return in rsi a pointer to the directory entry
    retc
;Free entry found, dir variables are set for the free entry. 
;Fall into the below to get a pointer to a disk buffer for this dir entry
getDiskDirectoryEntry:
;Gets a ptr to a disk directory entry using the directory variables.
;Input: dword [dirClustA], word [dirSect], dword [dirEntry]
;Output: CF=NC => rsi = Pointer to the start of the disk directory
;        qword [tempSect] = Disk sector number of directory
;        word [entry] = 32 byte offset into sector
;        CF=CY => Error, exit 
    push rbx
    mov eax, dword [dirClustA]  
    ;Skip cluster manipulation if the cluster number is 0 because these are 
    ; root directories of FAT12/16 drives. Instead add manual offset from dpb
    movzx ebx, word [dirSect]
    test eax, eax
    jz .skipCluster
    call getStartSectorOfCluster    ;Get sector number in rax
    jmp short .skipOldFat
.skipCluster:
    push rbp
    mov rbp, qword [workingDPB]
    movzx eax, word [dirSect]
    mov ebx, dword [rbp + dpb.dFirstUnitOfRootDir]
    ;If dirSect is greater than wNumberRootDirEntries, then ret fail
    cmp ax, word [rbp + dpb.wNumberRootDirEntries]
    pop rbp
    jb .skipOldFat
    stc
    return
.skipOldFat:
    add rax, rbx    ;Add sector offset to start sector of cluster
    mov qword [tempSect], rax   ;Save this sector number
    call getBufForDOS   ;Get buffer for DOS in rbx
    pop rbx
    retc
    push rbx
    mov rbx, qword [currBuff]
    call adjustDosDirBuffer ;Change buffer to Dir buffer
    ;Above function gets data buffer ptr in rsi
    movzx eax, word [dirSect]   ;Get the sector in which the offset lies
    movzx ebx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    mul ebx ;Multiply these two words so eax has number of bytes to
    ; the current sector
    shr eax, 5  ;Divide by 32 to get the number of dir entries we are skipping
    mov ebx, dword [dirEntry]   ;Get offset into dir file cluster
    sub ebx, eax    ;Now ebx has the dir entry offset in the current sector
    shl ebx, 5  ;Multiply by 32 to get byte offset
    mov word [entry], bx  ;Save 32 byte offset into sector
    add rsi, rbx    ;rsi now points to the entry
    pop rbx
    return


updateDirectoryEntryForFile:    
;Updates the directory entry for disk files
;Called with:
;   [workingDPB] = DPB pointer for the disk device
;   [currentSFT] = Current SFT pointer
    push rax
    push rbx
    push rdi
    push rbp

    call dosCrit1Enter
    mov rdi, qword [currentSFT]
    mov rbp, qword [workingDPB]
    test word [rdi + sft.wDeviceInfo], blokFileNoFlush | devCharDev
    jnz .exit   ;If it is a char dev or hasn't been written to yet, skip this
    test word [rdi + sft.wDeviceInfo], blokNoDTonClose
    jnz .skipDT
    ;Get date and time words and add them to the directory entry
    call readDateTimeRecord ;Update DOS internal Time/Date variables
    ;Build date and time words
    call getDirDTwords  ;Get date time words packed in eax
    ;Update SFT fields
    mov word [rdi + sft.wTime], ax
    shr eax, 16 ;Eject the time, get the date in eax
    mov word [rdi + sft.wDate], ax
    and word [rdi + sft.wDeviceInfo], ~blokFileNoFlush  ;We update DT, so flush
.skipDT:
;Before we read the dir sector in, if we never wrote to the disk
; we skip all of this
    test word [rdi + sft.wDeviceInfo], blokFileNoFlush
    jnz .exit ;If the file was never written to, don't bother updating DIR data
    mov rax, qword [rdi + sft.qDirSect] ;Get the directory sector for this file
    call getBufForDir  ;Returns buffer pointer in rbx
    jc .exitBad    ;If an error is to be returned from, we skip the rest of this
    ;Now we write the changes to the sector
    ;Mark sector as referenced and dirty! Ready to be flushed!
    lea rbp, qword [rbx + bufferHdr.dataarea]   ;Goto data area
    movzx ebx, byte [rdi + sft.bNumDirEnt] ;Get the directory entry into ebx
    shl ebx, 5  ;Multiply by 32 (directory entry is 32 bytes in size)
    add rbp, rbx    ;Move rbp to point to the directory entry
    mov eax, dword [rdi + sft.dFileSize]    ;Get the file size
    mov dword [rbp + fatDirEntry.fileSize], eax ;And update field
    movzx eax, word [rdi + sft.wTime]   ;Get the last write time
    mov word [rbp + fatDirEntry.wrtTime], ax    ;And update field
    movzx eax, word [rdi + sft.wDate]   ;Get the last write time
    mov word [rbp + fatDirEntry.wrtDate], ax    ;And update field
    mov word [rbp + fatDirEntry.lastAccDat], ax    ;And update final field
    mov eax, dword [rdi + sft.dStartClust]  ;Always update the start cluster
    mov word [rbp + fatDirEntry + fatDirEntry.fstClusLo], ax
    shr eax, 10h
    mov word [rbp + fatDirEntry + fatDirEntry.fstClusHi], ax
    ;Directory sector updated and marked to be flushed to disk!
    xor eax, eax
    call qword [updateDirShare]
    clc ;Clear CF as updateDirShare Defaults to CF=CY
    call markBufferDirty
    call writeThroughBuffers
    jc .exitBad
.exit:
    call dosCrit1Exit
    pop rbp
    pop rdi
    pop rbx
    pop rax
    return
.exitBad:
    call cancelWriteThroughBuffers
    pushfq  ;Save the state for if we come here from a fail
    and word [rdi + sft.wDeviceInfo], ~blokFileNoFlush
    popfq
    jmp short .exit
    
growDirectory:
;Input: dword [dirClustPar] must have the first cluster number of the directory
;Output: CF=NC => All ok, directory grew by 1 sector
;                 eax = New Cluster number
;               TWO NON CF ERROR CASES.
;               If eax = -1 => disk full!
;               If eax = 0 => Trying to grow FAT12/16 root dir. Bad.
;        CF=CY => Something went wrong. Rip. 
    push rbx
    push rcx
    mov eax, dword [dirClustPar]    ;Get first cluster for directory
    test eax, eax
    jz .exit
    call getLastClusterInChain  ;Get last cluster in chain in eax
    mov ebx, eax    ;Setup last cluster value in ebx
    mov ecx, 1  ;Allocate one more cluster
    call allocateClusters   ;ebx has last cluster value
    jc .exit
    mov eax, ebx    ;Walk this next cluster value to get new cluster value
    call readFAT
    jc .exit
    call sanitiseCluster    ;Preserves all regs, sanitises the cluster for use
    jc .exit
    clc
.exit:
    pop rcx
    pop rbx
    return   
sanitiseCluster:
;Sanitises the cluster in eax to all zeros
;Does not move file pointers
;Currently, is only called to sanitise subdirectory clusters
;Input: eax = Cluster number
;       qword [workingDPB] = DPB of drive whose cluster we are sanitising
;Output: If CF=NC => eax = Sanitised Cluster number
;        If CF=CY => Error, exit
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp
    mov rbp, qword [workingDPB]
    call getStartSectorOfCluster    ;Needs DPB in rbp
    movzx edx, byte [rbp + dpb.bMaxSectorInCluster] 
    inc edx ;Make it a count of sectors
.getSectorInCluster:
    call getBufForDataNoFile  ;Get a generic data buffer in rbx
    jc .exitBad
    lea rdi, qword [rbx + bufferHdr.dataarea]
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    xor eax, eax
    rep stosb   ;Store one sectorful of zeros
    call markBufferDirty ;We wrote to this buffer

    dec edx     ;One less sector in the cluster to sanitise!
    jz .exit    ;Jump if we done
    mov rax, qword [rbx + bufferHdr.bufferLBA] ;Get current sector number
    inc rax ;Goto next sector in cluster
    jmp short .getSectorInCluster
.exitBad:
    stc
.exit:
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return