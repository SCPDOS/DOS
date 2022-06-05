;-----------------------------------:
;    File Handle Kernel routines    :
;-----------------------------------:

createFileHdl:     ;ah = 3Ch, handle function
    ret
openFileHdl:       ;ah = 3Dh, handle function
    ret
closeFileHdl:      ;ah = 3Eh, handle function
    ret
readFileHdl:       ;ah = 3Fh, handle function
    mov byte [rwFlag], 0    ;Read
    jmp short rwFileHndleCommon
writeFileHdl:      ;ah = 40h, handle function
    mov byte [rwFlag], 1    ;Write
rwFileHndleCommon:
;bx has file handle, ecx has number of bytes to read
    mov word [currentHdl], bx
    call getSFTPtr  ;Get SFT ptr in var
    jnc .rwfhc0
    ret ;If carry is set and error code in al, exit!
.rwfhc0:

    ret

deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
movFileReadPtr:    ;ah = 42h, handle function, LSEEK
changeFileModeHdl: ;ah = 43h, handle function, CHMOD
ioctrl:            ;ah = 44h, handle function
duplicateHandle:   ;ah = 45h, handle function
forceDuplicateHdl: ;ah = 46h, handle function
findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
renameFile:        ;ah = 56h
createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
createNewFile:     ;ah = 5Bh
lockUnlockFile:    ;ah = 5Ch
setHandleCount:    ;ah = 67h
commitFile:        ;ah = 68h, flushes buffers for handle to disk 
    ret
;-----------------------------------:
;        File Handle routines       :
;-----------------------------------:
setFileAccessVariables:
;This will set up the file access variables and currentSFT 
; for the SFT pointer in rsi
;Only used if the SFT is pointing and reading/writing to/from a 
; hardfile (not device)
;Sets up the variables for the SFT AS IT IS WHEN THE FUNCTION IS INVOKED
;Input: rsi = SFT to setup for
;Output: Variables initialised:
;   currentSFT, workingDPB
;   currClust, currClustA, clustFact, currSect, currSectA, currByte
;   currByteA
    push rax
    push rcx
    push rdx
    push rsi
    push rbp
;Set current SFT
    call setCurrentSFT  ;Set rsi the current SFT ptr
    xor eax, eax
;Get Disk Relative (absolute) cluster
    mov eax, dword [rsi + sft.dAbsClusr]
    mov dword [currClustA], eax 
;Get File Relative Cluster
    mov eax, dword [rsi + sft.dRelClust]
    mov dword [currClust], eax
;Set working DPB
    mov rbp, qword [rsi + sft.qPtr] ;Get DPB ptr
    call setWorkingDPB
;Get Number of Sectors per Cluster
    xor eax, eax
    inc eax
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shl eax, cl ;Get number of Sectors Per Cluster
    mov byte [clustFact], al 
;Get Current Byte in File we are pointing to relative to the start of the file
    mov ecx, dword [rsi + sft.dCurntOff]
    mov dword [currByteA], ecx
;___
; | DO THE FOLLOWING TOGETHER
; | Get Cluster Relative Sector being pointed to
; | Get Current Byte in File we are pointing to relative to the sector
;_|_
    mov cl, byte [rbp + dpb.bBytesPerSectorShift]
    add cl, byte [rbp + dpb.bSectorsPerClusterShift]
    ;Get in cl bytes per Cluster shift
    mov eax, dword [currClust]  ;Get current file cluster number
    shl eax, cl ;Get number of bytes to the current File Relative cluster
    mov ecx, dword [currByteA]
    sub ecx, eax    ;Get the difference
    ;ecx now has the offset in bytes into the current cluster
    movzx rax, byte [clustFact] ;Get number of sectors per cluster into al
    movzx ecx, byte [rbp + dpb.bBytesPerSectorShift]
    shl eax, cl ;Get bytes per cluster in eax
    ;eax now has the number of bytes in a cluster
    xchg eax, ecx   ;Swap em
    xor edx, edx
    div ecx ;Offset into cluster (bytes)/bytes in sector (bytes)
    ;edx has the offset into the current sector in bytes (remainder)
    ;eax has the number of sectors into the cluster in sectors (quotient)
    mov word [currByte], dx ;Save sector offset
    mov byte [currSect], al ;Save cluster relative sector number
;Get Disk Relative (absolute) Sector being pointed to
    mov eax, [currClustA]   ;Get current absolute cluster
    call getStartSectorOfCluster    ;rbp points to dpb and eax has cluster num
    ;rax has starting disk sector of cluster
    movzx rcx, byte [currSect]  ;Get cluster relative sector offset
    add rax, rcx    
    mov qword [currSectA], rax  ;Save the current disk relative sector number
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret

setCurrentSFT:
;Set the pointer in rsi as current SFT 
    mov qword [currentSFT], rsi
    ret
getCurrentSFT:
;Get the current SFT pointer in rsi
    mov rsi, qword [currentSFT]
    ret

getSFTPtr:
;Gets the SFT pointer for a given file handle from the calling application
;On entry:
;   bx = File handle from JFT for calling application
;On exit: CF=NC, SFT found and placed in var
;         CF=CY, SFT not found, abort!
    push rax
    push rbx
    push rsi
    push rdi
    cmp bx, word [maxHndls]  ;current max number of file handles
    jnb .gspFail
    mov rsi, qword [currentPSP]
    movzx rbx, bx
    lea rbx, qword [rsi + psp.jobFileTbl + rbx] 
    mov [currentJFT], rbx   ;Save a pointer to the JFT entry
    mov bl, byte [rbx]   ;Use jft entry to get sft num
    cmp bl, -1  ;Non-existant SFT reference?
    je .gspFail
    xor eax, eax
    mov rdi, qword [sftHeadPtr]
.gsp0:
    add ax, word [rdi + sfth.wNumFiles]
    cmp al, bl  ;Check if the file header block contains the entry
    jbe .gsp1   ;IF bl is below or equal to al then it does
    cmp rdi, -1 ;End of list
    je .gspFail   ;If we have a number greater than the last entry, fail
    mov rdi, qword [rdi + sfth.qNextSFTPtr] ;Walk the chain
    jmp short .gsp0 ;Search again
.gsp1: 
    ;Now point to the right entry
    sub al, bl  ;Subtract the number from the total so far to get offset
    movzx eax, al
    add rdi, sfth_size  ;Point to first file in table
    test al, al ;Check if rdi points to the first file in this block
    jz .gsp12   ;Skip walking down the sft blocks
.gsp11:
    add rdi, sft_size
    dec al
    jnz .gsp11  ;Keep adding one until al is zero
.gsp12:
    mov qword [currentSFT], rdi ;Save pointer in variable
    clc
.gspExit:
    pop rdi
    pop rsi
    pop rbx
    pop rax
    ret
.gspFail:
    mov al, errBadHdl
    stc
    jmp short .gspExit

copySFTtoSDA:
;Called with rsi pointing to SFT structure
;Prepares the scratch SFT in SDA for use
    lea rdi, scratchSFT
    mov rsi, qword [currentSFT]   ;Get current SFT
    jmp short copySScommon
copySDAtoSFT:
    lea rsi, scratchSFT
    mov rdi, qword [currentSFT]   ;Get current SFT
copySScommon:
    push rcx
    mov ecx, sft_size
    rep movsb   ;Copy
    pop rcx
    ret

getSectorInCluster:
;Gets the sector in cluster from