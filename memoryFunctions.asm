;Memory related functions

;-----------------------------------:
;  Memory related Kernel routines   :
;-----------------------------------:
allocateMemory:    ;ah = 48h
freeMemory:        ;ah = 49h
reallocMemory:     ;ah = 4Ah
getsetMallocStrat: ;ah = 58h
    ret
;-----------------------------------:
;      Memory related routines      :
;-----------------------------------:
verifyIntegrityOfMCBChain:
    mov rbx, qword [mcbChainPtr]    ;Get the head of the chain
.ok:
    cmp byte [rbx + mcb.marker], 'M'
    je .ok1
    cmp byte [rbx + mcb.marker], "Z"    ;End of the chain?
    jne .sysHalt    ;It was not M or Z, fail violently
.exit:
    ret ;We have reached the end of the chain, return all good!
.ok1:
    mov eax, dword [rbx + mcb.blockSize]    ;Add the block size
    add rbx, rax
    jmp short .ok
.sysHalt:
;Only arrive here if the integrity of the system is not verified
;Lock the system
    lea rbx, .sysHltString
    mov ah, 09h
    int 41h
    cli ;Halt interrupts
    mov al, 0FFh    ;Mask IRQ lines 
    out 0A1h, al
    out 021h, al
    hlt             ;Halt the system
    jmp short $ - 3 ;Go back far enough to capture the hlt
.sysHltString db "Memory allocation error",0Dh,0Ah,
              db "Cannot load COMMAND, system halted$"