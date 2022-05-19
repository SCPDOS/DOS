;Memory related functions

;-----------------------------------:
;  Memory related Kernel routines   :
;-----------------------------------:
allocateMemory:    ;ah = 48h
    ;Maintain 2 pointers, best and last
    ; Also maintain a variable, ecx for the size of the allocation
    ;All sizes in paragraphs
freeMemory:        ;ah = 49h
;Input: r8 = address of the block to be returned (MCB + 1 para)
;Output: CF=CY => al = error code, CH=NC, nothing
;Always skip the first block as this is the anchor for DOS
    sub r8, mcb.program ;Point r8 to the MCB for the returned block
    xor ecx, ecx
    mov rsi, qword [mcbChainPtr]    ;Get MCB chain ptr to start walking
    mov rdi, rsi
    mov ecx, dword [rsi + mcb.blockSize]
    shl rcx, 4  ;Turn to bytes
    add rcx, mcb.program
    add rsi, rcx    ;Go to next block
.mainLoop:
    xor ecx, ecx
    cmp byte [rsi + mcb.marker], mcbMarkCtn
    je .valid
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    jne memSysHalt
.valid:
    cmp r8, rsi
    je .blockFound
    ;Not valid, check if last block in chain
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    je .blockNotFound
    mov ecx, dword [rsi + mcb.blockSize]
    shl rcx, 4  ;Turn to bytes
    add rcx, mcb.program    ;Go past the arena mcb
    add rsi, rcx    ;Go to next block
    jmp short .mainLoop
.blockFound:
    ;If hole, error.
    ;Else, set free, check if previous block is free, then check if next is free
    cmp qword [rsi + mcb.owner], mcbOwnerHole
    je .blockHole
    mov qword [rsi + mcb.owner], mcbOwnerFree
    cmp qword [rdi + mcb.owner], mcbOwnerFree   ;Is the previous block free?
    jne .blockFoundCheckFollowing   ;No, check if block following is free
    ;It is, let it absorb this space
    xor ecx, ecx
    mov ecx, dword [rsi + mcb.blockSize]
    add ecx, (mcb.program >> 4) ;Add 1 for the mcb itself
    add dword [rdi + mcb.blockSize], ecx    ;Add to previous entry
    mov rsi, rdi    ;Now point rsi to this block
.blockFoundCheckFollowing:
    ;First check if we are the last block in chain
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    je .blockFoundExit  ;If yes, exit!
    mov rdi, rsi    ;Now point rdi to current block
    mov ecx, dword [rsi + mcb.blockSize]
    shl rcx, 4  ;Turn to bytes
    add rcx, mcb.program
    add rsi, rcx    ;Go to next block
    cmp qword [rsi + mcb.owner], mcbOwnerFree
    jne .blockFoundExit ;If not free, exit
    ;If free, absorb into block pointed to by rdi
    xor ecx, ecx
    mov ecx, dword [rsi + mcb.blockSize]
    add ecx, (mcb.program >> 4) ;Add 1 for the mcb itself
    add dword [rdi + mcb.blockSize], ecx    ;Add to previous entry
.blockFoundExit:
    call verifyIntegrityOfMCBChain  ;Ensure MCB chain is still ok!
    mov rbx, qword [oldRSP]
    and byte [rbx + callerFrame.flags], 0FEh    ;Clear Carry flag
    ret
.blockNotFound:
    ;Set CF and error code
    mov byte [errorClass], eClsNotFnd   ;Block not found 
    jmp short .blockError
.blockHole:
;Cannot free a hole! Fail!
    mov byte [errorClass], eClsLocked   ;Cant free a hole
.blockError:
    mov byte [errorDrv], -1 ;No drive
    mov byte [errorLocus], eLocMem  ;Memory locus
    mov word [errorExCde], errMemAddr   ;Invalid mem addr
    mov byte [errorAction], eActUsr ;Retry with different value
    mov eax, errMemAddr
    mov rbx, qword [oldRSP]
    mov word [rbx + callerFrame.rax], ax    ;Save this word on stack
    or byte [rbx + callerFrame.flags], 1    ;Set Carry flag on
    ret
reallocMemory:     ;ah = 4Ah
;Input: r8 = address of the block to be realloc'ed (MCB + 1 para)
;       ebx = How many paras this block should contain after realloc. 
;               If ebx = 0, jump to free memory
;Needs to be at least 2 paragraphs free for growth or shrink to work
    test ebx, ebx
    jz freeMemory   ;If resize to 0, equivalent to free!
    cmp ebx, 1      ;Can't resize to only 1 paragraph
    je .notEnuffMem
    sub r8, mcb.program ;Return pointer to MCB for arena
    xor ecx, ecx
    mov rsi, r8     ;Get segment pointer in rsi
    cmp byte [rsi + mcb.marker], mcbMarkCtn
    je .ctn
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    jne .badAddrGiven
.ctn:
    ;This MCB is valid
    ;Check if Growth or Shrink
    mov rdi, rsi    ;Point rdi to same block
    xor ecx, ecx
    mov ecx, dword [rsi + mcb.blockSize]
    cmp ebx, ecx    ;If ebx is bigger than ecx, we have growth
    ja .growth
    je .exit    ;If they are equal, do nothing!
;We can always shrink
    sub ecx, ebx    ;In ecx save num. paras in new arena
    sub ecx, (mcb.program >> 4) ;Reserve space for new MCB 
    mov dword [rsi + mcb.blockSize], ebx ;Save new num paras in old MCB
    and ebx, -1 ;Zero the upper bytes of qword
    shl rbx, 4
    add rsi, rbx    ;Move rsi to point to new mcb
    mov al, byte [rdi + mcb.marker] ;Get old marker
    mov byte [rdi + mcb.marker], mcbMarkCtn
    mov byte [rsi + mcb.marker], al ;Place old marker in new mcb
    mov qword [rsi + mcb.owner], mcbOwnerFree
    mov dword [rsi + mcb.blockSize], ecx    ;Save new arena size
    cmp al, mcbMarkEnd  ;If the new block is at the end, exit
    je .exit
;Else, now see if the block following is also free and absorb it
    mov rdi, rsi    ;Point rdi to new mcb for new arena
    xor ecx, ecx
    mov ecx, dword [rsi + mcb.blockSize]    ;Get block size in paras
    shl rcx, 4  ;Convert to bytes
    add rsi, rcx    ;Goto next arena
    cmp byte [rsi + mcb.marker], mcbMarkCtn
    je .shrinkAbsorb
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    jne memSysHalt
.shrinkAbsorb:
    cmp qword [rsi + mcb.owner], mcbOwnerFree  ;Is this free?
    jne .exit
    ;It is free, absorb it
    mov ecx, dword [rsi + mcb.blockSize] ;Get the absorb arena size
    add dword [rdi + mcb.blockSize], ecx ;Add it to the new arena size
    xor ecx, ecx
    ;Clear absorbed MCB
    mov qword [rsi], rcx
    mov qword [rsi + 8], rcx
    jmp .exit
.growth:
;Check if we are the last block in chain. IF yes, not enuff mem err
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    je .notEnuffMem
;Now check the following space is free. If not, not enuff mem err
    xor ecx, ecx
    mov ecx, dword [rsi + mcb.blockSize]
    shl rcx, 4
    mov rdi, rsi    
    add rsi, rcx
    cmp byte [rsi + mcb.marker], mcbMarkCtn
    je .growthOK
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    jne memSysHalt
.growthOK:
    ;rdi points to block we are growing
    cmp qword [rsi + mcb.owner], mcbOwnerFree
    jne .notEnuffMem    ;Not enough memory to grow if space owned
    ;rsi must own all memory up to the next owned arena
    mov al, byte [rsi + mcb.marker] ;Get the next block's marker
    xor ecx, ecx
    xor edx, edx
    mov ecx, dword [rsi + mcb.blockSize]    ;Get new block size 
    add ecx, (mcb.program >> 4) ;Add the new block mcb size
    mov edx, dword [rdi + mcb.blockSize]    ;Get original block size
    add edx, ecx    ;Add two blocks together, save in rdx, preserve ecx
    cmp ebx, edx    ;If ebx > edx, fail
    cmova ebx, edx  ;Move max block size in ebx if ebx is bigger
    ja .notEnuffMem1
    sub edx, ebx    ;Remove ebx amount from edx
    cmp edx, 1
    je .notEnuffMem2
    mov dword [rdi + mcb.blockSize], ebx    ;Resize OG block
    mov rsi, rdi    ;Point rsi back to original block
    shl rdx, 4      ;Convert to bytes
    add rsi, rdx    ;Move rsi to next block header position
    shr rdx, 4      ;Convert back to paragraphs
    mov byte [rsi + mcb.marker], al ;This ensures if a new mcb is created,
    ; it has the same marker as the old one, and if it was completely absorbed,
    ; then the absorbing MCB has the marker of the absorbed MCB
    test edx, edx   ;If this is zero, then we skip the building of the new mcb
    jz .exit
    sub edx, (mcb.program >> 4) ;Make space for MCB header
    mov dword [rsi + mcb.blockSize], edx
    mov qword [rsi + mcb.owner], mcbOwnerFree
.exit:
    call verifyIntegrityOfMCBChain
    mov rbx, qword [oldRSP]
    and byte [rbx + callerFrame.flags], 0FEh    ;Clear Carry flag
    ret
.notEnuffMem2:
    dec ebx ;Max allocation must be 1 less than what it currently is
    jmp short .notEnuffMem1
.notEnuffMem:
    xor ebx, ebx    ;No space to grow
.notEnuffMem1:      ;Here with max block size in rbx
    mov eax, errNoMem   ;Not enough memory
    mov rdx, qword [oldRSP]
    mov dword [rdx + callerFrame.rbx], ebx  ;Save max realloc size for block
    jmp short .bad
.badAddrGiven:
    mov rdx, qword [oldRSP]
    mov eax, errMemAddr   ;Bad address given
.bad:
    mov byte [errorDrv], -1 ;No drive
    mov byte [errorLocus], eLocMem  ;Memory locus
    mov word [errorExCde], ax   ;Error code
    mov byte [errorAction], eActUsr ;Retry with different value
    mov word [rdx + callerFrame.rax], ax    ;Save this word on stack
    or byte [rdx + callerFrame.flags], 1    ;Set Carry flag on
    ret
getsetMallocStrat: ;ah = 58h
    test al, al
    jz .get
    cmp al, 2
    jae .bad
;Set here
    mov rbx, qword [oldRSP]
    mov ax, word [rbx + callerFrame.rbx]    ;Loword in rbx has alloc strat
    mov byte [allocStrat], al   ;Only save low word
    and byte [rbx + callerFrame.flags], 0FEh    ;Clear Carry flag
    ret
.get:
    mov rbx, qword [oldRSP]
    xor eax, eax
    mov al, byte [allocStrat]
    mov word [rbx + callerFrame.rax], ax    ;Store word
    and byte [rbx + callerFrame.flags], 0FEh    ;Clear Carry flag
    ret
.bad:
    mov byte [errorDrv], -1 ;No drive
    mov byte [errorLocus], eLocMem  ;Memory locus
    mov word [errorExCde], errInvFnc   ;Invalid function number addr
    mov byte [errorAction], eActUsr ;Retry with different value
    mov eax, errInvFnc
    mov rbx, qword [oldRSP]
    mov word [rbx + callerFrame.rax], ax    ;Save this word on stack
    or byte [rbx + callerFrame.flags], 1    ;Set Carry flag on
    ret
;-----------------------------------:
;      Memory related routines      :
;-----------------------------------:
verifyIntegrityOfMCBChain:
    mov rbx, qword [mcbChainPtr]    ;Get the head of the chain
.ok:
    cmp byte [rbx + mcb.marker], mcbMarkCtn
    je .ok1
    cmp byte [rbx + mcb.marker], mcbMarkEnd    ;End of the chain?
    jne memSysHalt    ;It was not M or Z, fail violently
.exit:
    ret ;We have reached the end of the chain, return all good!
.ok1:
    xor eax, eax
    mov eax, dword [rbx + mcb.blockSize]    ;Add the block size
    shl rax, 4  ;Convert from paragraphs to bytes
    add rbx, mcb.program    ;The block starts at the program
    add rbx, rax
    jmp short .ok
memSysHalt:
;Only arrive here if the integrity of the system is not verified
;Lock the system
    lea rbx, .sysHltString
    mov ah, 09h
    int 41h
    ;Only halt IRQ's in production!
    %if !DEBUG
    cli ;Halt interrupts
    mov al, 0FFh    ;Mask IRQ lines 
    out 0A1h, al
    out 021h, al
    %endif
    hlt             ;Halt the system
    jmp short $ - 3 ;Go back far enough to capture the hlt
.sysHltString db "Memory allocation error",0Dh,0Ah,
              db "Cannot load COMMAND, system halted$"