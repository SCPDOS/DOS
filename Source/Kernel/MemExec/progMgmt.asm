;EXEC and all default terminates are here

;========================
;   Interrupt handlers
;========================
terminateProcess:   ;Int 40h
    xor eax, eax    ;Prepare for AH=00h call
    jmp functionDispatch    ;Dispatch 41h/AH=00h (which jumps to 41h/AX=4C00h)
terminateRes:       ;Int 47h
;Input: edx = offset of last byte in program to remain resident plus 1
    add edx, 0Fh    ;Round up number of bytes to next paragraph
    shr edx, 4      ;Divide by 16 to get number of paragraphs
    mov eax, 3100h  ;Setup a call to TSR 41h/AH=31h
    jmp functionDispatch    ;Dispatch 41h/AH=31h Terminate and Stay Resident
;========================
;    Int 21h functions
;========================
createPSP:         ;ah = 55h, creates a PSP for a program
;Input:
;rdx = Pointer to new PSP in memory.
;   Will be rounded up to next paragraph if not paragraph aligned.
;   Officially document that this MUST be paragraph aligned.
;rsi = alloc size for new psp block
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
    movzx ecx, word [maxHndls]  ;Get the max handles in JFT
    mov al, -1
    rep stosb   ;Store maxHndls many -1's 
    ;Here we now proceed to copy all inheritable hdls and nullify other hdls
    lea rsi, qword [r8 + psp.jobFileTbl]    ;Source
    lea rdi, qword [rdx + psp.jobFileTbl]
    movzx ecx, word [maxHndls]  ;Get the max handles in JFT
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
    lea rdi, qword [rdx + psp.oldInt42h]
    mov al, 42h
    call muxGetIntVector    ;Get vector in rbx
    mov rax, rbx    ;Move vector number to rax
    stosq   ;Move rdi to next entry and store
    mov al, 43h
    call muxGetIntVector    ;Get vector in rbx
    mov rax, rbx    ;Move vector number to rax
    stosq   ;Move rdi to next entry and store
    mov al, 44h
    call muxGetIntVector    ;Get vector in rbx
    mov rax, rbx    ;Move vector number to rax
    stosq   ;Move rdi to next entry and store
    ;Now we add the additional useful bits... just in case they are damaged
    mov word [rdx + psp.return], 040CDh  ;Int 40h
    mov word [rdx + psp.unixEntry], 041CDh  
    mov byte [rdx + psp.unixEntry + 2], 0CBh ;Return
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
;Here we must:
;0) Build errorlevel and adjust variables accordingly
;1) Call Network Termination hook.
;2) Check if the program is it's own parent. If so, return.
;3) Swap the console back to the original driver if it is swapped.
;3.5) If we are exiting due to TSR, jump to 5
;4) Free all file handles associated to the current process.
;       Note this means, reducing the open counts and setting PSP entries to -1
;5) Free all memory blocks that have the signature of current PSP
;6) Set current PSP to parent PSP
;7) Restore Int 42h, 43h, 44h handlers from the PSP to the IDT
;8) Set rsp of parent proc upon entry to DOS to our rsp
;9) Set Int 42h to be the RIP value on the now oldRSP stack
;10) Exit all critical sections.
;
; Step 0
;For now, just adjust error level in var
    xor ah, ah  ;Eliminate the 4Ch
    xchg ah, byte [exitType]    ;Set type to zero
    test byte [ctrlCExit], -1   ;Is ^C flag set?
    jz .storeELvl   ;Jump if we are here due to normal exit
    mov byte [exitType], 1   ;Set the return type to 1 => Ctrl-C exit
.altEP: ;EP for Abort and TSR. exitType must be set beforehand
    mov ah, byte [exitType] ;Get the exitType
.storeELvl:
    mov word [errorLevel], ax   ;Store word
    
; Step 1 Tell network a process is terminating
    mov eax, 1122h  ;Net redir, Process Termination Hook
    mov r8, qword [currentPSP]  ;Use r8 instead of DS
    int 4Fh

; Step 2
.step1:
    mov rdi, qword [currentPSP] ;Get the current psp
    mov rdx, rdi    ;Save in rdx
    mov rbx, qword [rdi + psp.parentPtr]
    cmp rbx, rdi    ;Check if the application is it's own parent
    ;rete            ;If it is, simply return (al has errorLevel)
    je .exit
; Step 3
    call vConRetDriver  ;Always reset the driver flag
; Step 3.5
    cmp byte [exitType], 3  ;TSR exit?
    je .step6   ;Skip resource freeing if so as TSR exit resizes memory alloc.
; Step 4
    cmp byte [exitType], 2  ;Abort type exit?
    jne .skipAbortNetClose  ;Skip the following
    mov eax, 111Dh  ; Close all remote files for process on Abort!
    int 4Fh
.skipAbortNetClose:
    call qword [closeTaskShare] ;Close all shared files for this task
    add rdi, psp.jobFileTbl ;Move rdi to point to the start of the JFT
    mov rsi, rdi    ;Point rsi to jft too
    movzx ecx, word [maxHndls] ;Number of entries in JFT
.s4lp:
    lodsb   ;Inc rsi, get the SFT number in al
    cmp al, -1  ;End of open JFT entries?
    je .step5
    movzx ebx, al   ;Move the file handle number into ebx
    ;Replace with a call to close the handle eventually
    push rdi
    call derefSFTPtr    ;Ret in rdi the SFT pointer
    jc .badHdl  ;If a bad handle, skip the decrementing of the handle count
    push qword [currentSFT]
    call setCurrentSFT  ;Set rdi to currentSFT
    call closeMain  ;Close all files opened by this program. Decrement ref ONLY
    ;closeMain also flushes all sectors associated to the file
    ;Ignore errors, simply keep closing files
    pop qword [currentSFT]
.badHdl:
    pop rdi
    mov al, -1
    stosb   ;Store a -1 in it's place (not strictly necessary)
    dec ecx ;Zoom Zoom \mu-op!
    jnz .s4lp   ;Keep looping for all entries in the JFT 
;Step 5
.step5:
    mov rbx, qword [currentPSP] ;Get back the current psp
    ;Now we must walk the MCB chain and find all paragraphs
    ; with the currentPSP signature and free them.
    mov rsi, qword [mcbChainPtr]    ;Get the anchor MCB
.s5lp:  ;And walk the chain
;First verify the address in rsi is a valid mcb
    mov cl, byte [rsi + mcb.marker] ;Get the marker char into cl
    cmp cl, mcbMarkCtn
    je .checkToFree
    cmp cl, mcbMarkEnd
    jne .step6  ;Something wrong so stop freeing
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
    je .step6   ;Skip if we are
    mov eax, dword [rsi + mcb.blockSize]
    shl rax, 4  ;Multiply by 4 to get bytes from paragraphs
    lea rsi, qword [rsi + mcb.program + rax]    ;Goto next mcb block
    jmp short .s5lp
;Step 6
.step6:
    mov rax, qword [rbx + psp.parentPtr]    ;Get the parent PSP pointer
    mov qword [currentPSP], rax ;and set it to be the current PSP
;Step 7
    ;rbx points to current PSP, the old parent task
    ;Use setIntVector. Takes in al the interrupt number and rdx = ptr to routine
    mov rdx, qword [rbx + psp.oldInt44h]
    mov al, 44h
    call setIntVector
    mov rdx, qword [rbx + psp.oldInt43h]
    mov al, 43h
    call setIntVector
    mov rdx, qword [rbx + psp.oldInt42h]
    mov al, 42h
    push rdx
    call setIntVector
    pop rdx
;Step 8
.exit:
    mov ah, 82h ;Cancel all critical sections 0-7
    int 4ah

    cli
    mov rbx, qword [currentPSP]
    mov rdx, qword [rbx + psp.oldInt42h]
    ;Make the parent register frame the current one
    ;Make RSP point to user stack from parent entry to exec
    mov rsp, qword [rbx + psp.rspPtr]

    mov qword [rsp + callerFrame.rip], rdx  ;Store return address vector here
    mov qword [rsp + callerFrame.flags], 0F202h ;Mimic DOS's return flags

    mov byte [Int44Trans], 0    ;Clear this flag
    mov byte [inDOS], 0 ;Exiting DOS now
    mov byte [errorDrv], -1 ;Reset
    call dosPopRegs  ;Pop the stack frame pointed to by rsp
    
    iretq   ;and return to address that was in rdx