loadExecChild:     ;ah = 4Bh, EXEC
;Input: rdx = Ptr to the ASCIIZ string for the file to load. Must include ext.
;       rbx = Ptr to the parameter block used for loading
;       al = Subfunction:
;            00h = Load Program and transfer control to it
;            01h = Load Program but do not transfer ctrl to it
;            03h = Load overlay (no PSP) -> Blk copy file from filesystem 
;      Reserved for future expansion:
;            04h = Load in background execution mode (PE only)
;            If bit 7 of the subfunction byte is set, we load the corresponding
;               function but in 16-bit mode. 
;
;If the loaded file is not a PE it is assumed to be a COM/RFS file. 
; If not EXE, we read the filename extension. If it is RFS, we assign maximum 
; memory. If it is COM, we assign only 64Kb to the application.

;Start by setting up a stack frame of local vars to keep track of vars in call
    push rbp
    mov rbp, rsp
    sub rsp, execFrame_size   ;Make the space pointing at rbp
    cmp al, 3
    jbe .validSubfunction
.badSubFunction:
    mov eax, errInvFnc
    mov byte [errorLocus], eLocUnk
.badExit:
    mov rsp, rbp
    pop rbp
    jmp extErrExit

.validSubfunction:
    cmp al, 2
    je .badSubFunction
    ;Save registers for each function call
    mov qword [rbp - execFrame.pParam], rbx
    mov qword [rbp - execFrame.pProgname], rdx
    movzx eax, al
    mov qword [rbp - execFrame.bSubFunc], rax   ;clear alignment and progHdl
    mov rdi, rdx
    call strlen ;Get string length in cx
    mov word [rbp - execFrame.wNameLen], cx   ;Get the string length  
    ;Now open the file we wanna yeet to
    xor eax, eax    ;al = 0 => Normal program attributes to search for
    push rbp    ;Preserve local frame ptr
    call openFileHdl
    pop rbp
    jc .badExit ;Exit preserving error code in al
    ;Now ax has the file handle
    mov word [rbp - execFrame.wProgHdl], ax
    movzx ebx, ax   ;Move file handle into bx
    call derefSFTPtr    ;And deref it into rdi
    movzx edx, word [rdi + sft.wDeviceInfo] ;Get device word
    test edx, devCharDev
    jz .validDiskFile    ;We cannot have a char device
    mov al, errFnf
    jmp .cleanAndFail
.validDiskFile:
;For now we only support COM files.
    pop rbp
    return
.cleanAndFail:
;Close the open file and any open resources and fail
    call .clearArenaOwner   ;Enters level 1 critical section
    call dosCrit1Exit
    movzx ebx, word [rbp - execFrame.wProgHdl]
    push rax    ;Save error code
    push rbp
    call closeFileHdl
    pop rax
    pop rbp
    jmp .badExit

.readDataFromHdl:
;Input: bx = File Handle
;       ecx = Number of bytes to transfer
;       rdx = Ptr to the buffer to use
    call .clearArenaOwner   ;Entering critical section!
    movzx ebx, word [rbp - execFrame.wProgHdl]
    push rbp
    call readFileHdl
    pop rbp
    call .setPSPArenaOwner  ;Exiting critical section!
    return
.clearArenaOwner:
    push rbx
    xor ebx, ebx    ;Make owner null, ok to trash flags here
    call dosCrit1Enter
    call .setProgOrEnvArenaOwnerToRBX
    pop rbx
    return
.setPSPArenaOwner:
;Sets the current PSP as the arena owner
    push rbx
    mov rbx, qword [currentPSP]
    call .setProgOrEnvArenaOwnerToRBX
    call dosCrit1Exit
    pop rbx
    return
.setProgOrEnvArenaOwnerToRBX:
;Input: rbx = Owner ID  (Start of PSP address)
    pushfq
    push rax
    ;Only one of the two below addresses may be non zero at any one time!
    ;This is because they are set up at separate points in the routine!
    mov rax, qword [rbp + execFrame.pProgBase]
    call .writeArenaHeaderOwner
    mov rax, qword [rbp + execFrame.pEnvBase]
    call .writeArenaHeaderOwner
    pop rax
    popfq
    return
.writeArenaHeaderOwner:
;Input: rax = Ptr to arena (NOT HEADER)
;       rbx = Owner ID
    test rax, rax   ;Don't write if arena header null
    retz
    sub rax, mcb.program    ;Go to start of arena header
    mov qword [rax + 1], rbx
    return
