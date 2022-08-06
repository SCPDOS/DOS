;---------------------------------------------------:
;                   KERNEL FUNCTIONS                :
;---------------------------------------------------:
makeDIR:           ;ah = 39h
;For make, the path must exist but the final componant must not exist.
;Input: rdx = Pointer to ASCIIZ string
    mov rdi, rdx
.okLength:
    mov rsi, rdx
    call checkPathspecOK
    jnc .pathOk  ;Don't allow any malformed chars
.badPath:
    mov al, errPnf
    jmp extErrExit
.pathOk:
    call scanPathWC
    jc .badPath ;Dont allow wildcards
    call checkPathNet
    jz .badPath ;or network paths
    ;Path is ok, now proceed
    lea rdi, buffer1    ;Build the full path here
    call getDirPath ;Get a Directory path in buffer1, hitting the disk
    ;If the path exists, exit error
    jnc extErrExit
    ;Now check if the reason for the error was that the last pathcomp was 0
    call checkFailingComp
    jnz extErrExit
    ;So all is well, the new subdirectories name is in fcbName
    ;The parent dir's directory entry is in the curDirCopy

removeDIR:         ;ah = 3Ah
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
    jc .badPath  ;Don't allow any malformed chars
    call scanPathWC
    jc .badPath ;Or wildcards
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
    mov al, dl  ;Move drive number into al
    call getCDS ;Get in rsi the dpb pointer for drive dl
    jc extErrExit
.okDrive:
    mov rdi, rsi    ;Save destination in rdi
    call dosCrit1Enter  ;Ensure no task interrupts our copy
    mov rsi, qword [workingCDS]  ;Get pointer to current CDS in rsi
    movzx eax, word [rsi + cds.wBackslashOffset]
    inc eax ;Go past the backslash
    add rsi, rax ;Add this many chars to rsi to point to first char to copy
    call strcpy
    call dosCrit1Exit
    mov eax, 0100h  ;RBIL -> MS software may rely on this value
    jmp extGoodExit ;Exit very satisfied with ourselves that it worked!

getSetFileDateTime:;ah = 57h
trueName:          ;ah = 60h, get fully qualified name. Int 4Fh, AX=1221h
    ;Called with a path in rsi and 128 byte buffer in rdi
    call checkPathspecOK    ;This preserves rsi
    jnc .pathspecOk ;If CF=NC this path is totally ok
    jz .pathspecOk  ;If the last char in the path is malformed allow it here
.badPath:
    mov eax, errPnf
    jmp extErrExit
.pathspecOk:
    push rdi    ;Save the destination
    lea rdi, buffer1    ;Build the full path here
    call canonicaliseFileName
    mov byte [rdi], 0   ;Store a terminating zero if necessary
    pop rdi
    jc extErrExit
    lea rsi, buffer1
    call strcpy
    xor eax, eax
    jmp extGoodExit

;-----------------------------------
;    General Directory Routines    :
;-----------------------------------

getDiskDirectoryEntry:
;Gets a ptr to a disk directory entry using the directory variables.
;Input: dword [dirClustA], word [dirSect], dword [dirEntry]
;Output: CF=NC => rsi = Pointer to the start of the disk directory
;        CF=CY => Error, exit 
    push rbx
    mov eax, dword [dirClustA]  
    ;Skip cluster manipulation if the cluster number is 0 because these are 
    ; root directories of FAT12/16 drives.
    movzx ebx, word [dirSect]
    test eax, eax
    jz .skipCluster
    call getStartSectorOfCluster    ;Get sector number in rax
.skipCluster:
    add rax, rbx    ;Add sector offset to start sector of cluster
    call getBufForDOS   ;Get buffer for DOS in rbx
    jc .exit
    call adjustDosDirBuffer ;Change buffer to Dir buffer
    ;Above function moves buffer ptr to rsi
    movzx eax, word [dirSect]   ;Get the sector in which the offset lies
    movzx ebx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    mul ebx ;Multiply these two words so eax has number of bytes to
    ; the current sector
    shr eax, 5  ;Divide by 32 to get the number of dir entries we are skipping
    mov ebx, dword [dirEntry]
    sub ebx, eax    ;Now ebx has the dir entry offset in the current sector
    shl ebx, 5  ;Multiply by 32 to get byte offset
    add rsi, rbx    ;rsi now points to the entry
.exit:
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

    mov rdi, qword [currentSFT]
    mov rbp, qword [workingDPB]
    test word [rdi + sft.wDeviceInfo], blokNoDTonClose
    jnz .skipDT
    ;Get date and time words and add them to the directory entry
    call readDateTimeRecord ;Update DOS internal Time/Date variables
    jc .exit  ;If we fail to get time/date, fail the request
    ;Build date and time words
    call getDirDTwords  ;Get date time words packed in eax
    ;Update SFT fields
    mov word [rdi + sft.wTime], ax
    shr eax, 16 ;Eject the time, get the date in eax
    mov word [rdi + sft.wDate], ax
    or word [rdi + sft.wDeviceInfo], blokFileToFlush  ;We update DT, so flush
.skipDT:
;Before we read the dir sector in, if we never wrote to the disk
; we skip all of this
    test word [rdi + sft.wDeviceInfo], blokFileToFlush
    jz .exit ;If the file was never written to, don't bother updating DIR data
    mov rax, qword [rdi + sft.qDirSect] ;Get the directory sector for this file
    call getBufForDir  ;Returns buffer pointer in rbx
    jc .exit    ;If an error is to be returned from, we skip the rest of this
    ;Now we write the changes to the sector
    mov rbp, rbx    ;Move disk buffer header into rbp
    ;Mark sector as referenced and dirty! Ready to be flushed!
    or byte [rbp + bufferHdr.bufferFlags], dirtyBuffer 
    lea rbp, qword [rbp + bufferHdr.dataarea]   ;Goto data area
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
    ;Directory sector updated and marked to be flushed to disk!
    ;Now mark that the file has sectors not yet flushed to disk
    call setBufferReferenced
    pushfq  ;Save the state for if we come here from a fail
    or word [rdi + sft.wDeviceInfo], blokFileToFlush
    popfq
.exit:
    pop rbp
    pop rdi
    pop rbx
    pop rax
    return