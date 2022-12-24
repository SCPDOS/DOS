

;We start by checking that the version number is OK
;al has flag if the passed argument is ok
;r8 points to the PSP
startFormat:
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
    jmp short selectFATtype
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
    jc badExitGenericString
    movzx eax, word [rbx + bpb.bytsPerSec]  ;Get sector size
    mov word [sectorSize], ax
    movzx ecx, word [rbx + bpb.totSec16]
    mov eax, dword [rbx + bpb.totSec32]
    cmp eax, ecx
    cmovb eax, ecx
    push rax
    push r8
    mov r8, qword [bufferArea]
    mov eax, 4900h  ;Free the block now
    int 41h
    pop r8
    pop rax
    mov qword [bufferArea], 0   ;Clear the ptr
selectFATtype:
;Arrive here with rax = Number of sectors in volume
    sub rax, 2 ;Always sub 2 to ensure no edge issues and round clusters down
    mov qword [numSectors], rax
    ;Now we select the FAT based on the size of the volume
    movzx ebx, word [sectorSize]    ;Get the sector size
    mul rbx ;Multiply rax with rbx
    ;rax has the number of bytes on the volume
    mov rbx, 1FFFFFFFE00h ;If our volume is above 2Tb in size, abort
    cmp rax, rbx
    jb .okSize
.badSize:
    lea rdx, badVolBig
    jmp badExit
.okSize:
    mov byte [fatType], 0   ;Start by saying it must be FAT12
    mov ecx, 4  ;4 entries in the fat16table without the first entry
    lea rsi, fat16ClusterTable
    cmp eax, dword [rsi]
    jbe .medFound   ;Here we need to build a custom BPB for this device. 
    inc byte [fatType]  ;Make now FAT 16
    add rsi, 5  ;Goto next entry    
.fat16Lp:
    cmp eax, dword [rsi]
    jbe .medFound
    add rsi, 5
    dec ecx
    jnz .fat16Lp
    inc byte [fatType]
    mov ecx, 4
.fat32Lp:
    cmp eax, dword [rsi]
    jbe .medFound
    dec ecx
    jnz .fat32Lp
    jmp short .badSize
.medFound:
;Called with rsi pointing to the table entry
    mov al, byte [rsi + 4]  ;Get the sector per cluster value in al
    mov byte [secPerClust], al
    cmp byte [fatType], 2
    je .fat32
    lea rdi, genericBPB12
    lea rsi, genericBPB16
    cmp byte [fatType], 1
    cmove rdi, rsi  ;Use FAT16 BPB if FAT16 volume
    mov ax, word [sectorSize]
    mov word [rdi + bpb.bytsPerSec], ax
    mov al, byte [secPerClust]
    mov byte [rdi + bpb.secPerClus], al
    mov ax, word [numSectors]
    mov word [rdi + bpb.totSec16], ax
    call computeFATSize
    mov word [rdi + bpb.FATsz16], ax
    mov al, byte [remDev]
    and al, 80h ;Save only bit 7
    mov byte [rdi + bpb.drvNum], al
    call getVolumeID
    mov dword [rdi + bpb.volID], eax
    mov byte [bpbSize], 62  ;62 bytes to copy
    jmp short .bpbReady
.fat32:
    lea rdi, genericBPB32
    mov ax, word [sectorSize]
    mov word [rdi + bpb32.bytsPerSec], ax
    mov al, byte [secPerClust]
    mov byte [rdi + bpb32.secPerClus], al
    mov eax, dword [numSectors]
    mov dword [rdi + bpb32.totSec32], eax
    call computeFATSize
    mov dword [rdi + bpb32.FATsz32], eax
    mov al, byte [remDev]
    and al, 80h ;Save only bit 7
    mov byte [rdi + bpb32.drvNum], al
    mov word [rdi + bpb32.extFlags], 0  ;FAT mirroring active
    ;Here we need to assign cluster 2 to be root dir. Later we
    ; check to see if we can actually use cluster 2. If yes, 
    ; we allocate it on the FAT. If not, we reassign the root 
    ; dir location
    mov dword [rdi + bpb32.RootClus], 2
    call getVolumeID
    mov dword [rdi + bpb32.volID], eax
    mov byte [bpbSize], 90  ;90 bytes to copy
.bpbReady:
;Now the BPB is ready, save the pointer and now setup bootsector for writing
    mov qword [bpbPointer], rdi
    movzx ebx, word [sectorSize] ;Get the sector size
    shr ebx, 4  ;Divide by 4 to get number of paragraphs
    mov eax, 4800h  ;Allocate
    int 41h
    jc badExitGenericString
    mov qword [bufferArea], rax ;Use this as the buffer
    cld ;Ensure string ops are done the right way
    mov rdi, rax
    xor eax, eax
    movzx ecx, word [sectorSize]    ;Get num of bytes and div by 8 for qwords
    shr ecx, 3
    push rdi
    rep stosq   ;Zero the data area
    pop rdi
    mov rsi, qword [bpbPointer]
    movzx ecx, byte [bpbSize]
    rep movsb


;Utility functions below
getVolumeID:
;Uses the time to set a volume ID
;Output: eax = VolumeID
    mov eax, 2C00h     ;Get Time in cx:dx
    int 41h
    movzx ebx, dx
    movzx eax, cx
    shl ebx, 10h
    or eax, ebx
    return
computeFATSize:
; ;Works on the genericBPB in memory. Applies the following algorithm
; RootDirSectors = ((BPB_RootEntCnt * 32) + (BPB_BytsPerSec – 1)) / BPB_BytsPerSec;
; TmpVal1 = DskSize – (BPB_ResvdSecCnt + RootDirSectors);
; TmpVal2 = (256 * BPB_SecPerClus) + BPB_NumFATs;
; If(FATType == FAT32)
;   TmpVal2 = TmpVal2 / 2;
; FATSz = (TMPVal1 + (TmpVal2 – 1)) / TmpVal2;
;Input:
;   rdi = Pointer to the head of the BPB we are using
;Returns: 
;   eax = Number of sectors per FAT needed. Low word only valid for FAT12/16
    push rbx
    push rcx
    push rdx
    push rdi
    
    movzx eax, word [rdi + bpb.rootEntCnt]
    shl eax, 5  ;Multiply by 32
    movzx ebx, word [rdi + bpb.bytsPerSec]
    dec ebx
    add eax, ebx
    inc ebx
    div ebx
    mov edx, eax    ;edx = RootDirSectors

    movzx eax, word [rdi + bpb.totSec16]
    mov ebx, dword [rdi + bpb.totSec32]
    test eax, eax   ;If totSec16 is 0, move totSec32 into eax
    cmovz eax, ebx
    movzx ebx, word [rdi + bpb.revdSecCnt]
    add ebx, edx    ;Add RootDirSectors
    sub eax, ebx
    mov ecx, eax    ;ecx = TmpVal1

    movzx eax, byte [rdi + bpb.secPerClus]
    shl eax, 8  ;multiply by 256
    movzx ebx, byte [rdi + bpb.numFATs]
    add ebx, eax    ;ebx = TmpVal2

    cmp byte [fatType], 2
    jne .notFat32
    shr ebx, 1  ;Divide by 2
.notFat32: 
    mov eax, ecx    ;TmpVal1
    dec ebx
    add eax, ebx    ;TmpVal1 + (TmpVal2 - 1)
    inc ebx
    div ebx ;Exit with eax = number of sectors needed per FAT
.exit:
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    return

readSector:
;Input:
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    int 45h
    pop rax ;Pop old flags into rax
    return
writeSector:
;Input:
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