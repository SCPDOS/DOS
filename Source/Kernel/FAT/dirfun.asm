;---------------------------------------------------:
;                   KERNEL FUNCTIONS                :
;---------------------------------------------------:
makeDIR:           ;ah = 39h
removeDIR:         ;ah = 3Ah
setCurrentDIR:     ;ah = 3Bh, set dir for current drive (or drive in path)
getCurrentDIR:     ;ah = 47h
getSetFileDateTime:;ah = 57h
trueName:          ;ah = 60h, get fully qualified name
    return

;-----------------------------------
;    General Directory Routines    :
;-----------------------------------

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

    pushfq  ;Save the state for if we come here from a fail
    or word [rdi + sft.wDeviceInfo], blokFileToFlush
    popfq
.exit:
    pop rbp
    pop rdi
    pop rbx
    pop rax
    return