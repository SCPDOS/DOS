;DOS utility functions (Will be made available through Int 4F ah=12xx eventually)

;Basic Drive related Utilities
;Any function which takes args in rax (or any subpart of it), has that 
; argument provided on the stack when called from Int 4Fh interface (when 
; that gets set up)

setWorkingDPB:
;Gets dpb in rbp and saves to curDrvDPB (working dpb)
    mov qword [workingDPB], rbp
    ret

getUserRegs:   ;Int 4Fh AX=1218h
;Returns ptr to user regs in rsi
    mov rsi, qword [oldRSP]
    ret

getCDSforDrive:     ;Int 4Fh AX=1217h
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
    ret
setDrive:   ;Int 4Fh AX=1219h   
;Gets a drive CDS, sets it as working and checks it is a valid physical drive
;Input: al = 0-based drive number
;Output: al = 0-based drive number
;   CF=NC => Drive can be set as Current Drive (i.e. Not Network or Join)
;   CF=CY => 0-based drive number invalid OR CDS returned with Net or Join flags
;            set.
    call getCDS ;Setup working CDS DOS variable for this drive
    jc .exit    ;Carry the CF flag if not Physical
    push rsi
    mov rsi, qword [workingCDS] ;Get CDS
    test word [rsi + cds.wFlags], cdsJoinDrive  ;Check if Join
    pop rsi
    jz .exit
    stc
.exit:
    ret
getCDS:
;Gets the device DPB and saves it in the DOS variable
;This can be called to get CDS for network drives too!
;Input: al = 1 based drive number
;Sets workingCDS var with the CDS for the device. 
;   If device on a network, sets CF
;Returns al with 0-based drive number
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
    ;Eventually, here we will build a fresh DPB for the non-physical drive
    jmp short .exitBad1
.physDrive:
    call getCDSforDrive ;Get CDS pointer in RSI and in curCDSPtr
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