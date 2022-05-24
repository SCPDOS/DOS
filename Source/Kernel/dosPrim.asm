;Driver Primitives, functions for Disk IO and calling a device driver
; are placed here (Int 45h Int 46h and goDriver)

goDriver:   ;Executes the driver packet pointed to by rbx
;Called with rbx = Ptr to the request header for the driver call!
    push rsi
    mov rsi, qword [drvrPtr]    ;Get the driver pointer
    call qword [rsi + drvHdr.strPtr]  ;Passing rbx through here
    call qword [rsi + drvHdr.intPtr]
    test word [diskReqHdr + ioReqPkt.status], 8000h ;Clear carry flag and test
    jz .exit    ;Skip the setting in an error sitch
    stc 
.exit:
    pop rsi
    ret

absDiskWrite:       ;Int 46h
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    %if DEBUG
    ;Print DPB 
    debugEnterM
    lea rbp, .l0000
    call debPrintNullString
    jmp short .l0001
.l0000 db "Entering Int 46h",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    push rax
    push rbx
    push rdx
    push rbp
    mov ah, drvWRITE
    add ah, byte [verifyFlag]   ;Change to Write/Verify if set
    jmp short absDiskReadWriteCommon
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    %if DEBUG
    ;Print DPB 
    debugEnterM
    lea rbp, .l0000
    call debPrintNullString
    jmp short .l0001
.l0000 db "Entering Int 45h",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    push rax
    push rbx
    push rdx
    push rbp
    mov ah, drvREAD
absDiskReadWriteCommon:
;Entered with the appropriate function number in ah
    push rdx    ;Save start LBA
    push rax
    mov dl, al
    call findDPB   ;Get dpb ptr in rbp
    pop rax
    pop rdx

    mov byte [diskReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [diskReqHdr + ioReqPkt.unitnm], al
    mov byte [diskReqHdr + ioReqPkt.cmdcde], ah
    mov word [diskReqHdr + ioReqPkt.status], 0
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + ioReqPkt.medesc], al
    mov qword [diskReqHdr + ioReqPkt.bufptr], rbx
    mov qword [diskReqHdr + ioReqPkt.strtsc], rdx
    mov dword [diskReqHdr + ioReqPkt.tfrlen], ecx
    
    mov rdx, qword [rbp + dpb.qDriverHeaderPtr] ;Get driver pointer
    mov qword [drvrPtr], rdx    ;Store ptr in global variable

    lea rbx, diskReqHdr ;Get ReqHeader pointer in rbx
    call goDriver   ;If carry set, command failed
    pop rbp
    pop rdx
    pop rbx
    pop rax
    jc .absDiskError
    clc
    ret
.absDiskError:
    mov al, byte [diskReqHdr + ioReqPkt.status] ;Get low byte into al
    mov ah, 80h ;Attachment failure
    cmp al, 0Ch ;Gen error
    je .absExit
    mov ah, 40h ;Seek error
    cmp al, 06h
    je .absExit
    mov ah, 08h ;Bad CRC
    cmp al, 04h
    je .absExit
    mov ah, 04h ;Sector not found
    cmp al, 08h
    je .absExit
    xor ah, ah  ;Write Protect Violation
    test al, al
    je .absExit
    mov ah, 02h ;Other Error
.absExit:
    stc
    ret