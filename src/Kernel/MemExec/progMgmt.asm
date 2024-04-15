;EXEC and all default terminates are here

;========================
;   Interrupt handlers
;========================
terminateProcess:   ;Int 20h
    xor eax, eax    ;Prepare for AH=00h call
    jmp functionDispatch    ;Dispatch 21h/AH=00h (which jumps to 21h/AX=4C00h)
terminateRes:       ;Int 27h
;Input: edx = offset of last byte in program to remain resident plus 1
    add edx, 0Fh    ;Round up number of bytes to next paragraph
    shr edx, 4      ;Divide by 16 to get number of paragraphs
    mov eax, 3100h  ;Setup a call to TSR 21h/AH=31h
    jmp functionDispatch    ;Dispatch 21h/AH=31h Terminate and Stay Resident
;========================
;    Int 21h functions
;========================
createPSP:         ;ah = 55h, creates a PSP for a program
;Input:
;rdx = Pointer to new PSP in memory.
;   Will be rounded up to next paragraph if not paragraph aligned.
;   Officially document that this MUST be paragraph aligned.
;rsi = alloc size for new psp block
;
;----------------!!!! HANDLE COPY CAVEAT !!!!----------------
; Note, only the first 20 handles will be copied 
; from wherever the JFT is into the PSP JFT of the new task. 
; If any of these handles are non-inheritable or closed, then 
; they will be copied as -1 (if closed) or set to -1 during 
; the inheritence check.
;----------------!!!! HANDLE COPY CAVEAT !!!!----------------
    mov byte [pspCopyFlg], -1   ;We are making a child process
    mov r8, qword [currentPSP]
    or esi, esi ;Zero upper dword of rsi
    push rsi    ;esi is passed to us for PSP allocsize
    jmp short copyPSP.pspCommon
copyPSP:      ;ah = 26h
;Input:
;rdx = Pointer to new PSP in memory.
;   Will be rounded up to next paragraph if not paragraph aligned.
;   Officially document that this MUST be paragraph aligned.
    mov r8, qword [currentPSP]
    mov ebx, dword [r8 + psp.allocSize]    ;Get alloc size (zero upper dword)
    push rbx    ;Save this value for PSP allocsize
.pspCommon:
    add rdx, 0Fh    ;If we need to round up, this will do it
    shr rdx, 4  ;Now eliminate the bottom nybble
    shl rdx, 4  ;And pull out a fresh zero with inc nybble 1 IF nybble 0 != 0
;r8 is current PSP, now copy psp to rdx
;Preserve rdx and r8 until the end
    mov rsi, r8
    mov rdi, rdx
    mov ecx, psp_size/8 ;psp must be 100h
    rep movsq   ;Copy the psp over zoom zoom qword boom
    mov qword [rdx + psp.parentPtr], 0 ;Set the current parent to 0 by default
    test byte [pspCopyFlg], -1
    jz .copy
    ;Now reset the parent psp data
    mov qword [rdx + psp.prevPSP], -1  ;Share pointer, leave as -1 for now
    mov qword [rdx + psp.parentPtr], r8 ;Replace the parent with the currnt
    ;Now reset the copied jobFileTable
    lea rdi, qword [rdx + psp.jobFileTbl]
    xor ecx, ecx
    mov ecx, dfltJFTsize  ;Store dfltJFTsize free handles in new child PSP
    mov word [rdx + psp.jftSize], cx ;Set the size of JFT in new PSP to dflt 20
    mov al, -1
    rep stosb   ;Store 20 many -1's indicating 20 free handles
    ;Here we now proceed to copy all inheritable hdls and nullify other hdls
    ;lea rsi, qword [r8 + psp.jobFileTbl]    ;Source
    xor ebx, ebx    ;Get the pointer to jft[0] of source JFT
    call getJFTPtr  ;Get JFT pointer to parent process JFT in rdi
    mov rsi, rdi    ;Store it in rsi
    lea rdi, qword [rdx + psp.jobFileTbl]   ;Get the new processes' JFT ptr
    movzx ecx, word [rdx + psp.jftSize]   ;Copy over first dfltJFTsize handles only
.xfrJFT:
    jecxz .copy
    dec ecx
    lodsb   ;Get the SFTndx in al
    movzx ebx, al
    push rbx
    call getSFTndxInheritable ; ZF=ZE => Inheritable
    pop rax
    jnz .badJFT
    stosb   ;Else store the SFTndx at that position... 
    call incrementOpenCount ;and increment the open count for the SFT
    jmp short .xfrJFT
.badJFT:
    inc rdi ;If not inheritable, skip this position and get the next SFTNdx
    jmp short .xfrJFT 
.copy:
    mov byte [pspCopyFlg], 0    ;Reset flag
    pop rax ;Pop the allocsize back into rax
    mov dword [rdx + psp.allocSize], eax    ;Store allocsize
    ;Now we copy the Interrupt addresses from the IDT to the PSP
    lea rdi, qword [rdx + psp.oldInt22h]
    mov al, 22h
    call muxGetIntVector    ;Get vector in rbx
    mov rax, rbx    ;Move vector number to rax
    stosq   ;Move rdi to next entry and store
    mov al, 23h
    call muxGetIntVector    ;Get vector in rbx
    mov rax, rbx    ;Move vector number to rax
    stosq   ;Move rdi to next entry and store
    mov al, 24h
    call muxGetIntVector    ;Get vector in rbx
    mov rax, rbx    ;Move vector number to rax
    stosq   ;Move rdi to next entry and store
    ;Now we add the additional useful bits... just in case they are damaged
    mov word [rdx + psp.return], 020CDh  ;Int 20h
    mov word [rdx + psp.unixEntry], 021CDh  
    mov byte [rdx + psp.unixEntry + 2], 0C3h ;Return
    return

terminateStayRes:  ;ah = 31h
;Input: al  = Error code
;       edx = Number of paragraphs to keep resident
    mov byte [exitType], 3  ;TSR exit signature!
    ;Minimum number of paragraphs to shrink to is 6 (As per DOS 3.3 - c.f. RBIL)
    cmp edx, 6
    jae .aboveMinimum
    mov edx, 6  ;Min number of paragraphs
.aboveMinimum:
;Now we setup a call to Realloc 
;Setup regs with: 
;   r8 = address of the block to be realloc'ed
;   ebx = How many paras this block should contain after realloc.
    mov r8, qword [currentPSP]  ;Get current PSP, one para before should be MCB
    mov ebx, edx
    push rax    ;Preserve errorlevel across call
    push rbx    ;Preserve new number of paragraphs across call
    call reallocMemory
    pop rbx
    pop rax
    jc terminateClean.altEP ;If an error, return w/o editing psp seg. size
    mov dword [r8 + psp.allocSize], ebx   ;Store the new number of paragraphs
    ;al has the error code (errorlevel), exitType is set to 3
    jmp short terminateClean.altEP    ;Terminate as normal

simpleTerminate:   ;ah = 00h
    xor eax, eax    ;Just fall through as normal
terminateClean:    ;ah = 4Ch, EXIT
;For now, adjust error level in var
    xor ah, ah  ;Eliminate the 4Ch
    xchg ah, byte [exitType]    ;Set type to zero
    test byte [ctrlCExit], -1   ;Is ^C flag set?
    jz .storeELvl   ;Jump if we are here due to normal exit
    xchg ah, byte [ctrlCExit]   ;Zero the flag
    mov byte [exitType], 1   ;Set the return type to 1 => Ctrl-C exit
.altEP: ;EP for Abort and TSR. exitType must be set beforehand
    mov ah, byte [exitType] ;Get the exitType
.storeELvl:
    mov word [errorLevel], ax   ;Store word
;rbx points to current PSP
;Use setIntVector. Takes in al the interrupt number and rdx = ptr to routine
    mov rbx, qword [currentPSP]
    mov rdx, qword [rbx + psp.oldInt24h]
    mov al, 24h
    call setIntVector
    mov rdx, qword [rbx + psp.oldInt23h]
    mov al, 23h
    call setIntVector
    mov rdx, qword [rbx + psp.oldInt22h]
    mov al, 22h
    call setIntVector

    mov ah, 82h ;Cancel all critical sections 0-7
    int 2Ah
    mov byte [Int24Trans], -1   ;Aborts now get translated temporarily
    mov eax, 1122h              ;Net redir, Process Termination Hook
    mov r8, qword [currentPSP]  ;Use r8 instead of DS
    int 2Fh

    mov rdi, qword [currentPSP] ;Get the current psp
    mov rdx, rdi    ;Save in rdx
    mov rbx, qword [rdi + psp.parentPtr]
    cmp rbx, rdi    ;Check if the application is it's own parent
    je .ownParent   ;No resource freeing if it is its own parent!
    cmp byte [exitType], 3  ;TSR exit?
    je .freeOk   ;Skip resource freeing if so as TSR exit resizes memory alloc.

    cmp byte [exitType], 2  ;Abort type exit?
    jne .skipAbortNetClose  ;Skip the following
    mov eax, 111Dh  ; Close all remote files for process on Abort!
    int 2Fh
.skipAbortNetClose:
    call qword [closeTaskShare] ;Close all shared files for this task
    call qword [unloadDLLHook]  ;Now free exported function for this task
;Now close file handles
    mov rdi, qword [currentPSP]
    movzx ecx, word [rdi + psp.jftSize] ;Number of entries in current JFT
    xor ebx, ebx    ;Start from handle 0
.hdlLp:
    push rbx
    push rcx
    call closeFileHdl
    pop rcx
    pop rbx
    inc ebx ;Goto next handle to close
    cmp ebx, ecx
    jne .hdlLp   ;Keep looping for all entries in the JFT 
;Now free MCB's owned by task
    mov rbx, qword [currentPSP] ;Get back the current psp
    ;Now we must walk the MCB chain and find all paragraphs
    ; with the currentPSP signature and free them.
    mov rsi, qword [mcbChainPtr]    ;Get the anchor MCB
.wlkMcb:  ;And walk the chain
;First verify the address in rsi is a valid mcb
    mov cl, byte [rsi + mcb.marker] ;Get the marker char into cl
    cmp cl, mcbMarkCtn
    je .checkToFree
    cmp cl, mcbMarkEnd
    jne .freeOk  ;Something wrong so stop freeing
.checkToFree:
    cmp qword [rsi + mcb.owner], rbx ;Is this valid block owned by current PSP?
    jne .noFree
    lea r8, qword [rsi + mcb.program] ;Move pointer to block in r8
    push rbx
    push rcx
    push rsi
    call freeMemory ;Free this memory block
    pop rsi
    pop rcx
    pop rbx
    ;If an error occured, the internal vars will be set.
.noFree:
    cmp cl, mcbMarkEnd  ;Are we at the end of the MCB chain?
    je .freeOk          ;Skip if we are
    mov eax, dword [rsi + mcb.blockSize]
    shl rax, 4  ;Multiply by 4 to get bytes from paragraphs
    lea rsi, qword [rsi + mcb.program + rax]    ;Goto next mcb block
    jmp short .wlkMcb
.freeOk:
    call qword [terminateTask]  ;Registers task terminating, no retval
    mov rax, qword [rbx + psp.parentPtr]    ;Get the parent PSP pointer
    mov qword [currentPSP], rax ;and set it to be the current PSP
.ownParent:
    mov al, -1  ;Flush all drive buffers
    call dosCrit1Enter
    call flushAllBuffersForDrive
    call dosCrit1Exit
    cli
    mov byte [Int24Trans], 0    
    mov byte [inDOS], 0     ;Exiting DOS now
    mov byte [errorDrv], -1 ;Reset error drive
    mov rbx, qword [currentPSP]
    mov rsp, qword [rbx + psp.rspPtr]   ;Point rsp to the rsp on entry to DOS call
    ;Dont touch the previous stack pointer thats left on the stack, only
    ; the ret ptr and the flags
    mov al, 22h
    call muxGetIntVector    ;Get return vector in rbx
    mov qword [rsp + callerFrame.rip], rbx
    mov qword [rsp + callerFrame.flags], 0202h  ;Mimic safely DOS's ret flags
    call dosPopRegs  ;Pop the stack frame pointed to by rsp
    iretq   ;rsp ends up on the stack of the last entry into DOS
    ;Caveat: If a task which is its own parent CTRL+C's or "aborts and
    ; accesses DOS during Int 24h", then rsp on return to the Int22h vector 
    ; is invalid as it points to rsp within DOS. 