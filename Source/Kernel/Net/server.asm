;Network functions here

dosServer:  ;ah = 5Dh
;Dispatches the server function in al. Many of these are share hooks.
;Input: For all functions except al = 06h, 07h, 08h, 09h
;   rdx = Pointer to DPL
    cmp al, 07h
    jb .skip
    cmp al, 09h
    jbe .skip2
.skip:
    mov rsi, rdx
    mov rbx, qword [rsi + dpl.compID]   ;Low word only used
    mov word [machineNum], bx
    mov rbx, qword [rsi + dpl.procID]
    mov qword [serverPSP], rbx
.skip2:
    lea rbx, noOp   ;Push the ok function return address
    push rbx
    mov rbx, qword [serverDispTblPtr]   ;Get the qword
    push rbx    ;Push the table ptr on the stack
    push rax    ;Push al onto stack as a qword
    call serverFunctionSelect
    mov eax, eLocUnk
    mov byte [errorLocus], al
    jmp extErrExit

serverDispatch: ;AX=5D00h
;Input: rsi points to the DOS parameter list
    pop rax ;Pop additional return ptr off the stack
    push rsi    ;Put dpl ptr in rdi
    pop rdi
    call getUserRegs
    xchg rdi, rsi   ;Put ptr to caller frame in rdi and return dpl ptr into rsi
    push rsi
    mov ecx, 6
    rep movsq   ;Copy over first 6 registers from dpl to caller frame
    add rdi, 8  ;Skip rbp on the caller stack
    movsq   ;Transfer r8...
    movsq   ;... and r9
    pop rsi
    mov rax, qword [rsi + callerFrame.rax]
    mov rbx, qword [rsi + callerFrame.rbx]
    mov rcx, qword [rsi + callerFrame.rcx]
    mov rdx, qword [rsi + callerFrame.rdx]
    mov rdi, qword [rsi + callerFrame.rdi]
    mov r8, qword [rsi + callerFrame.r8]
    mov r9, qword [rsi + callerFrame.r9]
    mov rsi, qword [rsi + callerFrame.rsi]
    mov qword [oldRBX], rbx
    mov byte [dosInvoke], -1    ;Mark Server invoke
    jmp functionDispatch.serverEP   ;Enter the server EP

commitAllFilesForProcess:   ;AX=5D01h
;Will commit all the files for the current Process as indicated by the DPL
;A bad procID (otherwise known as a PSP) may otherwise crash the call.
;Thus we check the first two bytes of the current PSP to be CD 40h
;If so, we proceed, otherwise, fail with AccDen
    mov rbx, qword [currentPSP] ;Get the current PSP (setup from DPL)
    cmp word [rbx], 40CDh
    je .validTask
    mov eax, errAccDen
    jmp extErrExit
.validTask:
    xor ebx, ebx    ;Start from file 0
    call dosCrit1Enter
.mainLoop:
    ;Now loop through the JFT of the current task committing the files
    push rbx
    call derefSFTPtr    ;Get in rdi the pointer for this SFT entry
    jc .exit
    cmp word [rdi + sft.wNumHandles], 0   ;Is this an unopened file?
    je .gotoNextFile
    cmp word [rdi + sft.wNumHandles], -1  ;Is this in the process of something?
    je .gotoNextFile
    test word [rdi + sft.wDeviceInfo], devRedirDev  ;Dont commit redirs
    jnz .gotoNextFile
    mov qword [currentSFT], rdi ;Set this as the current SFT
    call commitMain ;Will reenter critical section but thats ok
    ;If it succeeded, great, cool also if not, keep going!
.gotoNextFile:
    pop rbx
    inc ebx
    jmp short .mainLoop
.exit:
    call dosCrit1Exit
    pop rbx ;Align stack
    jmp extGoodExit

closeFilesByName:           ;AX=5D02h
    call qword [closeNameShare]
.shareExit: ;Use this symbol if we need a decision to be made
.shareExitBad:  ;Use this symbol if we want to exit Error
    jc extErrExit
.shareExitGood:
    jmp extGoodExit

closeFilesByComputer:       ;AX=5D03h
    call qword [closeCompShare]
    jmp short closeFilesByName.shareExit

closeFilesByProcess:        ;AX=5D04h
    call qword [closeTaskShare]
    jmp short closeFilesByName.shareExit

getOpenFileListEntry:       ;AX=5D05h
    call qword [openFileListShare]  ;Must zero extend all results to 8 bytes
    jc closeFilesByName.shareExitBad
    call getUserRegs
    mov qword [rsi + callerFrame.rbx], rbx  ;Network machine number (0-ext)
    mov qword [rsi + callerFrame.rdi], rdi  ;Ptr to file name
.shareExit:    ;rcx must be zero extended however to use this
    mov qword [rsi + callerFrame.rcx], rcx  ;Lock count
.shareExit2:
    jmp short closeFilesByName.shareExitGood

getSDAData:                 ;AX=5D06h
;Returns:
;   rsi -> nonreentrant data area (includes all three DOS stacks)
;   (critical error flag is first byte) (see #01687)
;   rcx = size in bytes of area which must be swapped while in DOS
;   rdx = size in bytes of area which must always be swapped
    lea rdi, sda
    mov rcx, sdaLen
    mov rdx, sdaMSLen
    call getUserRegs
    mov qword [rsi + callerFrame.rsi], rdi
    mov qword [rsi + callerFrame.rdx], rdx
    jmp short getOpenFileListEntry.shareExit   ;Stores rcx and returns ok

printerRedir:               ;AX=5D07/8/9h
    push rax
    mov eax, 1125h  ;Redir Printer Mode setup
    int 4Fh
    pop rbx
    jc short closeFilesByName.shareExitBad
    jmp short  closeFilesByName.shareExitGood

setExtendedErrorInfo:       ;AX=5D0Ah
;Input:
;   rsi -> DPL
    mov eax, dword [rsi + dpl.rax]  ;Get ax (extended error code)
    mov word [errorExCde], ax
    mov rax, qword [rsi + dpl.rdi]  ;Get rdi as a full ptr
    mov qword [xInt44RDI], rax
    mov eax, dword [rsi + dpl.rbx]  ;Get bx (error action and class)
    mov word [errorAction], ax  ;Store action and class together
    mov eax, dword [rsi + dpl.rcx]  ;Get ch (error locus)
    mov byte [errorLocus], ah
    return  ;Return to the function ptr on the stack (the return function)

setExtendedErrorMulti:   ;Int 4Fh, AX=1222h
;Input: rsi -> Four byte records. Value of -1 means dont change.
    push rax
    lodsb
    cmp al, -1
    je .skipExCde
    xor ah, ah
    mov word [errorExCde], ax
.skipExCde:
    lodsb
    cmp al, -1
    je .skipClass
    mov byte [errorClass], al
.skipClass:
    lodsb
    cmp al, -1
    je .skipAct
    mov byte [errorAction], al
.skipAct:
    lodsb
    cmp al, -1
    je .exit
    mov byte [errorLocus], al
.exit:
    pop rax
    return

serverFunctionSelect:
;First parameter is al ONLY, subfunction number (BP + 2*8)
;Second parameter is table address, (BP + 3*8)
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, qword [rbp + 3*8]  ;Get table ptr
    movzx ebx, byte [rbx]   ;Get the table length
    cmp bl, byte [rbp + 2*8]    ;Is subfunction number less than bl?
    jnb .argumentTooBig
    movzx ebx, byte [rbp + 2*8] ;Get subfunction into ebx zeroextended
    shl ebx, 2  ;Convert to word offset
    inc ebx ;Go past the initial byte of the table
    movzx ebx, word [rbx]   ;Get the offset of the function from the tbl head
    add rbx, qword [rbp + 3*8]  ;Add the table base address to the offset
    mov qword [rbp + 2*8], rbx  ;Store this address as the return address
    pop rbx
    pop rbp
    add rsp, 3*8    ;Go past old return and old subfunction value
    return
.argumentTooBig:
    pop rbx
    pop rbp
    ret 3*8 ;Clear stack of all argument bytes



netServices:   ;ah = 5Eh, do nothing
    return
netRedir:;ah = 5Fh, redirector needs to be installed
;Exception: We pick off ah=07 (ENABLE DRIVE) and ah=08 (DISABLE DRIVE)
    cmp ah, 07h
    je .driveAction
    cmp ah, 08h
    je .driveAction
    ;Else, use redirector to process request
    push rax
    mov eax, 111eh  ;Do redirection redirector function
    int 4Fh
    pop rbx
.badExit:
    jc extErrExit
.goodExit:
    jmp extGoodExit
.driveAction:
;dl must have valid 0-based drive number
    xchg al, dl ;Get function number in dl and drive number in al
    call getCDSforDrive
    jc short .badExit
    ;rsi points to CDS
    sub dl, 7
    jz .enable
    and word [rsi + cds.wFlags], ~cdsValidDrive ;Clear bit
    jmp short .goodExit
.enable:
    or word [rsi + cds.wFlags], cdsValidDrive   ;Set bit
    jmp short .goodExit