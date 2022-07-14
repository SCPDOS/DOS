;EXEC and all default terminates are here

;========================
;   Interrupt handlers
;========================
terminateProcess:   ;Int 40h
    xor eax, eax
    jmp functionDispatch    ;Dispatch 41h/AH=00h (which jumps to 41h/AX=4C00h)
terminateRes:       ;Int 47h
    iretq
;========================
;    Int 21h functions
;========================
terminateStayRes:  ;ah = 31h
    ret
loadExecChild:     ;ah = 4Bh, EXEC
    ret
simpleTerminate:   ;ah = 00h
    mov eax, 4C00h  ;Fall thru
terminateClean:    ;ah = 4Ch, EXIT
;Here we must:
;0) Build errorlevel and adjust variables accordingly
;1) Swap the console back to the original driver if it is swapped.
;2) Check if the program is it's own parent. If so, return.
;3) Free all file handles associated to the current process.
;       Note this means, reducing the open counts and setting PSP entries to -1
;4) Free all memory blocks that have the signature of current PSP
;5) Set current PSP to parent PSP
;6) Restore Int 42h, 43h, 44h handlers from the PSP to the IDT
;7) Call Network Termination hook.
;8) Set old old rsp as old rsp
;9) Set Int 42h to be the RIP value on the now oldRSP stack
;10) Return!
;
; Step 0
;For now, just adjust error level in var
    mov byte [errorLevel + 1], 0
.abortEP:
    mov byte [errorLevel], al   ;Store in lower byte
    mov al, 2
    test byte [ctrlCExit], al
    jz .step1   ;If ctrlCExit is zero, leave upper byte
    mov byte [errorLevel + 1], al   ;Else change to CtrlC exit type
; Step 1
.step1:
    call vConRetDriver  ;Always reset the driver flag
; Step 2
    mov rdi, qword [currentPSP] ;Get the current psp
    mov rbx, qword [rdi + psp.parentPtr]
    cmp rbx, rdi    ;Check if the application is it's own parent
    rete            ;If it is, simply return (al has errorLevel)
;Step 3
    add rdi, psp.jobFileTbl ;Move rdi to point to the start of the JFT
    mov rsi, rdi    ;Point rsi to jft too
    movzx ecx, word [maxHndls] ;Number of entries in JFT
.s3lp:
    lodsb   ;Inc rsi, get the SFT number in al
    cmp al, -1  ;End of open JFT entries?
    je .step4
    movzx ebx, al   ;Move the file handle number into ebx
    push rdi
    call derefSFTPtr    ;Ret in rdi the SFT pointer
    jc .badHdl  ;If a bad handle, skip the decrementing of the handle count
    dec word [rdi + sft.wNumHandles]    ;Decrement the count
.badHdl:
    pop rdi
    mov al, -1
    stosb   ;Store a -1 in it's place (not strictly necessary)
    dec ecx ;Zoom Zoom \mu-op!
    jnz .s3lp   ;Keep looping for all entries in the JFT 
;Step 4
.step4:
    mov rbx, qword [currentPSP] ;Get back the current psp
    ;Now we must walk the MCB chain and find all paragraphs
    ; with the currentPSP signature and free them.
    mov rsi, qword [mcbChainPtr]    ;Get the anchor MCB
.s4lp:  ;And walk the chain
;First verify the address in rsi is a valid mcb
    mov cl, byte [rsi + mcb.marker] ;Get the marker char into cl
    cmp cl, mcbMarkCtn
    je .checkToFree
    cmp cl, mcbMarkEnd
    jne .step5  ;Something wrong so stop freeing
.checkToFree:
    cmp qword [rsi + mcb.owner], rbx ;Is this valid block owned by current PSP?
    jne .noFree
    mov r8, rsi ;Move pointer to r8
    call freeMemory ;Free this memory block
    ;If an error occured, the internal vars will be set.
.noFree:
    cmp cl, mcbMarkEnd  ;Are we at the end of the MCB chain?
    je .step5   ;Skip if we are
    mov eax, dword [rsi + mcb.blockSize]
    shl rax, 4  ;Multiply by 4 to get bytes from paragraphs
    add rsi, rax    ;Goto next mcb block
    jmp short .s4lp
;Step 5
.step5:
    mov rax, qword [rbx + psp.parentPtr]    ;Get the parent PSP pointer
    mov qword [currentPSP], rax
;Step 6
    ;rbx points to current PSP
    ;Use setIntVector. Takes in al the interrupt number and rdx = ptr to routine
    mov rdx, qword [rbx + psp.oldInt44h]
    mov al, 44h
    call setIntVector
    mov rdx, qword [rbx + psp.oldInt43h]
    mov al, 43h
    call setIntVector
    mov rdx, qword [rbx + psp.oldInt42h]
    mov al, 42h
    call setIntVector
;Step 7
    mov eax, 1122h  ;Network Termination Hook
    int 4fh ;Beep it out
;Step 8
    mov rax, qword [oldoldRSP]  ;Make the parent register frame the current one
    mov qword [oldRSP], rax
;Step 9
    mov rbp, qword [oldRSP] ;Get pointer to parent stack register frame in rbp
    mov qword [rbp + callerFrame.rip], rdx  ;Store return address vector here
;Step 10
    xor al, al    ;Set al to 0
    return