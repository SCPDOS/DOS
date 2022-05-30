;DOS utility functions (Will be made available through Int 4F ah=12xx eventually)

;Basic Drive related Utilities
;Any function which takes args in rax (or any subpart of it), has that 
; argument provided on the stack when called from Int 4Fh interface (when 
; that gets set up)

setDPBAsWorking:
;Gets dpb in rbp and saves to curDrvDPB (working dpb)
    mov qword [workingDPB], rbp
    ret

getUserRegs:   ;Int 4Fh AX=1218h
;Returns ptr to user regs in rsi
    mov rsi, qword [oldRSP]
    ret

walkCDSarray:     ;Int 4Fh AX=1217h
    ;Gets the CDS for the current drive in al
    ;Input: al = Drive number, 0 = A ...
    ;Output: CF=NC => rsi = Pointer to CDS for drive in al (and workingCDS var)
    ;        CF=CY => al not valid
    cmp al, byte [lastdrvNum]
    jb .ctn
    stc 
    ret
.ctn:
    push rax
    push rdx
    movzx eax, al
    mov rsi, cds_size
    xor edx, edx
    mul esi ;Get the multiples of CDS's to skip
    lea rax, qword [cdsHeadPtr] ;Get the first CDS
    add rsi, rax    ;Add the CDS array to the offset into it
    mov qword [workingCDS], rsi  ;Save in data area
    pop rdx
    pop rax
    clc
    ret

walkDPBchain:
;Called with al = 0 based drive number
;Returns in rsi a pointer to the DPB or if CF=CY, invalid drive number
    mov rsi, qword [sftHeadPtr]  ;Get variable pointing to first DPB
.walk:
    cmp rsi, -1
    je .exitBad
    cmp byte [rsi + dpb.bDriveNumber], al
    je .exit    ;Drive found
    mov rsi, qword [rsi + dpb.qNextDPBPtr]  ;Go to next drive 
    jmp short .walk
.exitBad:
    stc
.exit:
    %if DEBUG
    ;Print DPB 
    debugEnterM
    mov r8, rbp ;Save dpb pointer
    lea rbp, .l0000
    call debPrintNullString
    mov rbp, r8
    call debDPBptr
    jmp short .l0001
.l0000 db "Internal call to find DPB",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    ret

getCDS:
;Gets the device DPB and saves it in the DOS variable
;Input: al = 1 based drive number
;Sets workingCDS var with the CDS for the device. 
;   If device on a network, sets CF (currently error)
    test al, al
    jnz .skip
    mov al, byte [currentDrv]   ;Get current drive
    inc al
.skip:
    dec al  ;Convert to 0 based (0=A: ...)
    push rsi
    mov byte [errorLocus], eLocDsk  ;Set the locus
    test byte [dosInvoke], -1   ;If non-zero, invalid
    jz .physDrive
    ;Invalid invokation (21/5D00 invokation not yet supported)
    ;If returned with CF=CY, consider it an error for now
    ;Eventually, here we will build a fresh DPB for the network drive
    jmp short .exitBad1
.physDrive:
    call walkCDSarray ;Get CDS pointer in RSI and in curCDSPtr
    jc .exitBad
    test word [rsi + cds.wFlags], cdsPhysDrive
    jnz .exitOk ;Exit with flag cleared
    ;Else Return to unknown error locus
.exitBad:
    mov byte [errorLocus], eLocUnk
.exitBad1:
    stc
.exitOk:
    pop rsi
    ret