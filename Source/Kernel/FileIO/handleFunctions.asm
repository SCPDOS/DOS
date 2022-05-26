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
    mov bl, byte [rsi + psp.jobFileTbl + rbx]   ;Use jft entry to get sft num
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

