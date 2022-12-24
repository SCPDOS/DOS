

;We start by checking that the version number is OK
;al has flag if the passed argument is ok
;r8 points to the PSP
start:
    jmp short .cVersion
.vNum:    db 1
.cVersion:
    push rax
    mov ah, 30h
    int 41h
    cmp al, byte [.vNum] ;Version 1
    jbe .okVersion
    pop rax
    lea rdx, badVerStr
.printExit:
    mov ah, 09h
    int 41h
    int 40h ;Exit to caller or DOS to print bad command interpreter line
.okVersion:
;Check the passed argument is ok (flag in al)
    pop rax
    cmp al, -1
    jnz .driveOk
.badDrive:
    lea rdx, badDrvLtr
    jmp short .printExit
.driveOk:
;Now save the old default drive, and set our drive to default.
;On exit this must be restored
    mov ah, 19h ;Get Current Default Drive in al
    int 41h
    mov byte [oldDrive], al
    add al, "A"
    mov byte [driveLetter], al  ;Store for error message

; Here we now hook ^C so that if the user calls ^C we restore DOS state
; (i.e. default drive and reactivate the drive if it is deactivated)
    lea rdx, breakRoutine
    mov eax, 2543h
    int 41h
;Now fetch the drive we are working on
    mov dl, byte [r8 + psp.fcb1] ;Get the fcb 0 based drvNum
    mov byte [fmtDrive], dl
    mov ah, 0Eh ;Select Drive
    int 41h
    jnc .driveSelected
    lea rdx, badRedir
    jmp badExit
.driveSelected:
;Now we check that the associate drive is not a network, subst or join.
; If it is, fail. Else, we deactivate
    mov ah, 52h
    int 41h ;Get in rbx a ptr to list of lists
    add rbx, 2Ah  ;Point rbx to cdsHeadPtr
    mov rsi, qword [rbx]    ;Get the ptr to the CDS array
    movzx ecx, byte [fmtDrive]
    jecxz .atCurrentCDS
.walkCDSArray:
    add rsi, cds_size
    dec ecx
    jnz .walkCDSArray
.atCurrentCDS: 
    mov qword [cdsPtr], rsi ;Save a ptr to the current CDS
    test word [rsi + cds.wFlags], cdsJoinDrive | cdsSubstDrive | cdsRedirDrive
    jnz .badDrive   ;Cannot format a Join/Subst/Redir drive

    call dosCrit1Enter
    and word [rsi + cds.wFlags], ~cdsValidDrive ;Clear the validDrive bit
    mov byte [fmtDrvInv], -1    ;Set flag to indicate we need to reactivate CDS
    call dosCrit1Exit
    ;CDS deactivated, now we can format disk.
    ;First attempt to ascertain if removable or not.
    xor ebx, ebx    ;Default Drive
    mov eax, 4408h  ;IOCTL, Get if removable or not
    int 41h
    jnc .gotRemStatus
    jmp badExitGenericString
.gotRemStatus:
    test al, al
    jnz .fixedDisk
    ;Now request IOCTL to give medium parameters
    mov ch, 08h ;Disk drive type IOCTL
    mov cl, 80h | 60h   ;Use undocumented LBA get parameters
    mov eax, 440Dh  ;Generic IOCTL 
    lea rdx, reqTable   ;Point to the table to fill in
    int 41h
    jc badExitNoString
    mov rax, qword [rdx + genioctlGetParamsTable.sectorSize]    ;Get sector size
    mov word [sectorSize], ax
    mov rax, qword [rdx + genioctlGetParamsTable.numSectors]    ;Get num sectors
    sub rax, 2 ;Always sub 2 to ensure no edge issues and round clusters down
    mov qword [numSectors], rax
    jmp short .selectFATtype
.fixedDisk:
    mov byte [remDev], -1   ;Set flag for fixed disk
    ;Read VBR for volume, request a buffer of 1000h bytes (max sector size 4k)
    mov ebx, 100h   ;Request 100 paragraphs
    mov eax, 4800h
    int 41h
    jc badExitGenericString
    mov qword [bufferArea], rax ;Use this as the buffer
    mov rbx, rax
    mov al, byte [fmtDrive] ;Get the format drive
    mov ecx, 1
    xor edx, edx    ;Read sector 0 of the volume
    call readSector

.selectFATtype:

;Utility functions below
readSector:
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    int 45h
    pop rax ;Pop old flags into rax
    return
writeSector:
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    int 46h
    pop rax ;Pop old flags into rax
    return
badExitGenericString:
    lea rdx, badGeneric
badExitNoString:
    jmp short badExit.noPrint
badExit:
;Jumped to with rdx = Error message or 0 if no message
    test rdx, rdx
    jz .noPrint
    mov ah, 09h
    int 41h
.noPrint:
    call reset
    mov eax, 4CFFh  ;Return with -1 as error code
    int 41h

reset:
    call reactivateCDS
    call resetDriveForExit
    return
resetDriveForExit:
    mov dl, byte [fmtDrive]
    mov ah, 0Eh ;Select Drive
    int 41h
    return
reactivateCDS:
    test byte [fmtDrvInv], -1   ;If no bits set, drive is active
    retz
    mov rsi, qword [cdsPtr]
    call dosCrit1Enter
    mov byte [fmtDrvInv], 0    ;Clear flag
    or word [rsi + cds.wFlags], cdsValidDrive
    call dosCrit1Exit
    return

dosCrit1Enter:
    push rax 
    mov eax, 8001h
    int 4ah
    pop rax
    return
dosCrit1Exit:
    push rax 
    mov eax, 8101h
    int 4ah
    pop rax
    return

breakRoutine:
;This subroutine is called by ^C
;Prompts the user for what they want to do.
    lea rdx, cancel
    mov ah, 09h
    int 41h
    mov ah, 01h ;Get a char
    int 41h
    cmp al, "y"
    je short .breakReturnExit
    cmp al, "Y"
    je short .breakReturnExit
    cmp al, "n"
    je short .breakReturnNoExit
    cmp al, "N"
    jmp short breakRoutine 
.breakReturnNoExit:
    cli
    add rsp, 8*3    ;Skip returning to DOS and just return to task
    iretq
.breakReturnExit:
;Set Default Drive back, reactivate the CDS if it is deactivated
    stc
    ret 8 