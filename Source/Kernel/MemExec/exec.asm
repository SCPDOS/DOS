

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

;If AL = 0 :
;            Load Child Program and Xfr control to it
; rbx ------>   |-------------------------------|
;               |   Ptr to Environment Block    |
;               |                               |
;               | Can be 0 => Get pointer to    |
;               | parent Environment Block      |
;               |-------------------------------|
;               |   Ptr to Command Line to be   |
;               |   placed at PSP + 80h         |
;               |-------------------------------|
;               |   Ptr to first default FCB    |
;               |      to copy to PSP.fcb1      |
;               |                               |
;               | Should be used to parse the   |
;               |  first two command line args  |
;               |-------------------------------|
;               |   Ptr to second default FCB   |
;               |      to copy to PSP.fcb2      |
;               |-------------------------------|
;
;If AL = 1 :
;           Load Child Program BUT DO NOT Xfr control to it
; rbx ------>   |-------------------------------|
;               |         AS IN AL=00H          |
;               |-------------------------------|
;               |     Initial Value of RSP      |
;               |       on program launch       |
;               |                               |
;               |     This is returned to the   |
;               |         caller by DOS         |
;               |-------------------------------|
;               |     Initial Value of RIP      |
;               |       on program launch       |
;               |                               |
;               |     This is returned to the   |
;               |         caller by DOS         |
;               |-------------------------------|
;
;                      !!Important Note!!
;   The block pointed to by rbx must reserve the space for 
;        those last two qwords on a AL=1 call to EXEC.
;
;If AL = 3 :
;                      Load program overlay
; rbx ------>   |-------------------------------|
;               |  Pointer to the byte in the   |
;               |  prog space to start writing  |
;               |        the overlay at         |
;               |-------------------------------|
;               | DWORD offset from the base of |
;               |  the program to the location  |
;               |  the overlay is being loaded  |
;               |              in               |
;               |  (Called a Relocation Factor) |
;               |     Only FOR EXE Overlays     |
;               |        for CODE fixups        |
;               |  For COM, Current PSP + 100h  |
;               |      assumed to suffice       |
;               |-------------------------------|
;
; All three will setup both COM and EXE files for whatever purpose. 
;           AL = 3 DOES NOT BUILD THE PROGRAM A PSP.
;
;Start by setting up a stack frame of local vars to keep track of vars in call
    push rbp
    mov rbp, rsp
    sub rsp, execFrame_size   ;Make the space pointing at rbp
    cmp al, execOverlay
    jbe .validSubfunction
.badSubFunction:
    mov eax, errInvFnc
    mov byte [errorLocus], eLocUnk
.badExit:
    mov rsp, rbp
    pop rbp
    jmp extErrExit

.validSubfunction:
    cmp al, execInvld
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
    xor eax, eax
    mov qword [rbp - execFrame.pEnvBase], rax   ;Clear pEnv and pProg Bases
    mov qword [rbp - execFrame.pProgBase], rax
    cmp byte [rbp - execFrame.bSubFunc], execOverlay
    je .loadProgram ;If overlay, skip making an environment block
    mov rdi, qword [rbp - execFrame.pParam] ;Get params ptr in rdi
    mov rax, qword [rdi + execProg.pEnv]
    test rax, rax   ;Is this 0? (i.e. inherit parent env ptr)
    jnz .copyEnvironmentBlock
    mov rsi, qword [currentPSP] ;Get current PSP address in rsi
    mov rax, qword [rsi + psp.envPtr]   ;Get the environment ptr
    mov qword [rbp - execFrame.pEnvBase], rax   ;Store the parent ptr
    test rax, rax   ;Was the parent pointer 0? If so, make a new block
    jnz .loadProgram
.copyEnvironmentBlock:
    mov rdi, rax    ;Point rdi to the source of the environment
    ;Get the length of the environment
    mov ecx, 7fffh  ;Arbitrary 32kb DOS limit, consider increasing to 64Kb
    xor eax, eax
    mov rbx, rdi    ;Use rbx as the base ptr of the scan
.envVerifyLp:
    rep scasb   ;Scan for a terminating word of nulls
    jnz .invalidEnvironmentError
    jecxz .invalidEnvironmentError  ;Error if no space for a second null
    dec ecx
    scasb   ;Check if we have a second byte of 00 (i.e. end of environment)
    jnz .envVerifyLp

    sub rdi, rbx ;Get offset into block, gives a result less than 7FFFh
    push rdi     ;Save the length of the environment block
    add edi, 11h    ;Add 11 to round up when converting to paragraphs
    movzx ebx, word [rbp - execFrame.wNameLen]  ;Get name length
    add edi, ebx    ;edi has number of bytes to allocate for environment blk
    mov ebx, edi
    shr ebx, 4  ;Turn bytes needed into paragrapsh
    call allocateMemory
    pop rcx ;Pop the length of the environment block into rcx
    jnc .copyEnvironment
    ;Fall thru if not enuff memory
.insufficientMemory:
    mov eax, errNoMem
    jmp .cleanAndFail
.invalidEnvironmentError:   ;DO NOT MOVE THIS TO USE JECXZ ABOVE
    mov eax, errBadEnv
    jmp .cleanAndFail

.copyEnvironment:
    ;rax has the ptr to allocated memory block
    ;rcx has the number of chars to copy from the source env block
    mov rdi, rax    ;This is the destination of the copy
    mov qword [rbp - execFrame.pEnvBase], rax   ;Save the env block in frame
    mov rsi, qword [rbp - execFrame.pParam]
    mov rsi, qword [rsi + execProg.pEnv]    ;Get in rsi the src of the env
    rep movsb   ;Copy from rsi to rdi
    mov eax, 1  ;One additional string and a second null char!
    stosw       ;Away you go!
    mov rsi, qword [rbp - execFrame.pProgname]  ;Get ASCIIZ string for filespec
    movzx ecx, word [rbp - execFrame.wNameLen]
    rep movsb   ;Move the bytes to rdi
;Done with the environment... more or less
.loadProgram:
    mov ecx, imageDosHdr_size   ;Read the DOS header for the exe file
    movzx ebx, word [rbp - execFrame.wProgHdl]  ;Get file handle
    lea rdx, exeHdrSpace    ;Read into the SDA area
    call .readDataFromHdl
    jc .badFmtErr
    test eax, eax   ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, imageDosHdr_size
    jb .loadCom

    cmp word [rdx + imageDosHdr.e_magic], dosMagicSignature
    je .proceedEXE
    cmp word [rdx + imageDosHdr.e_magic], dosMagicSignature2
    jne .loadCom    ;If not equal to ZM or MZ, must be a COM file
.proceedEXE:
    ;Now we need to read e_lfanew
    push rdx    ;Save exeHdrSpace addr on stack
    mov edx, dword [rdx + imageDosHdr.e_lfanew]
    mov dword [rbp - execFrame.dCOFFhdr], edx   ;Save this for later
    xor ecx, ecx    ;Officially, need to set ecx to 0
    xor al, al  ;Set file pointer from start of file
    ;handle in bx is preserved 
    call lseekHdl   ;Move to that position in the file

    pop rdx ;Get exeHdrSpace address back
    mov ecx, imageFileHeader_size
    call .readDataFromHdl       ;Now read the COFF header
    test eax, eax   ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, imageFileHeader_size
    jb .badFmtErr
    ;So now rdx points to the imageFileHeader
    cmp word [rdx + imageFileHeader.wMachineType], imageFileMachineAMD64
    jne .badFmtErr
    ;Now we save wNumberOfSections and wSizeOfOptionalHdr
    movzx eax, word [rdx + imageFileHeader.wNumberOfSections]
    mov word [rbp - execFrame.wNumSecns], ax
    movzx eax, word [rdx + imageFileHeader.wSizeOfOptionalHdr]
    mov word [rbp - execFrame.wSzOptHdr], ax    ;0 only for object files
    

.loadCom:
    pop rbp
    return

.badFmtErr:
    mov eax, errBadFmt  ;Fall thru with bad resource format error
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
