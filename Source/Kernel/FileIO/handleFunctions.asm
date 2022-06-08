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
    call getSFTPtr  ;Get SFT ptr in var in rdi and var
    jnc .rwfhc0
    ret ;If carry is set and error code in al, exit!
.rwfhc0:

    ret

deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
lseekHdl:          ;ah = 42h, handle function, LSEEK
;New pointer passed in edx! ecx will be DOCUMENTED as having to be 0
    call getSFTPtr
    jnc .sftValid
    ;Error code and exit
    ;al (eax) has error code for bad file handle
    mov word [errorExCde], ax
.exitBad:
    mov byte [errorLocus], eLocUnk  ;Unknown Locus
    mov byte [errorAction], eActUsr ;Reinput data
    mov byte [errorClass], eClsNotFnd
    call getUserRegs    ;Get user regs in rsi
    or byte [rsi + callerFrame.flags], 1    ;Set CF
    ret
.sftValid:
    cmp al, 3
    jb .validFunction
    ;Error code and exit
    mov ax, errInvFnc
    jmp short .exitBad
.validFunction:
    cmp al, 1
    ja .seekend
    jb .seekset
;Here we are at seekcur, seek from current (signed)
    add edx, dword [rdi + sft.dCurntOff]    ;Get offset from current
.seekset:
;Seek from the start (unsigned)
    mov dword [rdi + sft.dCurntOff], edx ;Store the new offset
    call getUserRegs    ;Get user regs in rsi
    mov dword [rsi + callerFrame.rdx], edx
    xor al, al  ;Return OK!
    ret
.seekend:
;Here we are at seekend, seek from end (signed)
    add edx, dword [rdi + sft.dFileSize]    ;Add to file size
    jmp short .seekset
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
;Uses the file pointer value in the given SFT and the given SFT for
; computations.
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
; | Get Disk Relative (absolute) Sector being pointed to
;_|_
    call setSectorVars
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
;On exit: CF=NC, SFT found and placed in var and rdi
;         CF=CY, SFT not found, abort! al=BadHdl errorcode
    push rax
    push rbx
    push rsi
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
    pop rsi
    pop rbx
    pop rax
    ret
.gspFail:
    mov eax, errBadHdl  ;al, zero rest of it
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

updateCurrentSFT:
;Updates the Current SFT fields before returning from a file handle operation
    push rsi
    push rax
    mov rsi, qword [currentSFT]
    mov eax, dword [currByteA]
    mov dword [rsi + sft.dCurntOff], eax
    mov eax, dword [currClustA]
    mov dword [rsi + sft.dAbsClusr], eax
    mov eax, dword [currClust]
    mov dword [rsi + sft.dRelClust], eax
    pop rax
    pop rsi
    ret

setClusterVars:
;Uses the number given in eax as the file pointer, to compute
; sft fields
;Works on the SFT pointer provided in rsi
;Input: rsi = SFT entry pointer
;Output: rsi = SFT cluster fields updated IF CF=NC
;       CF=CY => Fail request with Int 44h
    push rax
    push rbx
    push rcx
    push rdx
    push rbp
;Use variables instead of SFT fields in case the disk fails
    mov dword [currByteA], eax
    xor ecx, ecx
    xor edx, edx
    mov rbp, qword [rsi + sft.qPtr] ;Get DPB pointer
    mov cl, byte [rbp + dpb.bBytesPerSectorShift]
    add cl, byte [rbp + dpb.bSectorsPerClusterShift]
    ;Get in cl bytes per Cluster shift
    mov edx, 1
    shl edx, cl ;Get number of bytes in a cluster in edx
    mov ecx, edx    ;Move the number of bytes in a cluster to ecx
    xor edx, edx
    mov ebx, eax    ;Save byte pointer in ebx
    div ecx
    ;eax = Quotient => Relative cluster number
    ;edx = Remainder => Byte offset into cluster
    mov dword [currClust], eax    ;Save relative cluster 
;Now walk the FAT relative cluster number of times
    mov ecx, eax
    mov eax, dword [rsi + sft.dStartClust]
    mov qword [workingDPB], rbp ;Make the dpb working for walkFAT
.fatWalk:
    cmp eax, -1
    je .getFreeSector
    call walkFAT
    jc .diskFail
    dec ecx
    jnz .fatWalk
;eax has absolute cluster number now, set SFT fields
    mov dword [rsi + sft.dAbsClusr], eax
    mov eax, dword [currClust]
    mov dword [rsi + sft.dRelClust], eax
    mov eax, dword [currByteA]
    mov dword [rsi + sft.dCurntOff], eax
    clc
.exit:
    pop rbp
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.diskFail:
;FAT read failed, error
    stc
    jmp short .exit
.getFreeSector:
;Simply need to get the first free sector and add it to the file allocation
