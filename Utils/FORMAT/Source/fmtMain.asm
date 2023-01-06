

;We start by checking that the version number is OK
;al has flag if the passed argument is ok
;r8 points to the PSP
startFormat:
    jmp short .cVersion
.vNum:          db 1
.sectorSize:    dw 200h
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
; Here we now hook ^C so that if the user calls ^C we restore DOS state
; (i.e. default drive and reactivate the drive if it is deactivated)
    lea rdx, breakRoutine
    mov eax, 2543h
    int 41h
;Now fetch the drive we are working on
    mov dl, byte [r8 + psp.fcb1] ;Get the fcb 1 based drvNum
    dec dl  ;Turn it into a 0 based number
    mov byte [fmtDrive], dl
    mov byte [driveLetter], dl  ;Store for error message
.driveSelected:
;Now check that drive we want to fmt is not current drv
    mov eax, 1900h  ;Get current drive 
    int 41h
    cmp al, byte [fmtDrive]
    jne .notCurrentDrive
    lea rdx, currentFmt
    jmp badExit
.notCurrentDrive:
;Now we check that the associate drive is not a network, subst or join.
; If it is, fail. Else, we deactivate
    mov ah, 52h
    int 41h ;Get in rbx a ptr to list of lists
    add rbx, 22h    ;Point rbx to bufHeadPtr
    mov rbp, qword [rbx]    ;Get the ptr to the buffer header array
    mov qword [dosBuffPtr], rbp
    add rbx, 8h     ;Point rbx to cdsHeadPtr
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
    jnz .badRedir   ;Cannot format a Join/Subst/Redir drive
    ;Now attempt to ascertain if removable or not.
    movzx ebx, byte [fmtDrive]    ;0 based number
    inc ebx  ;Turn it into a 1 based number
    mov eax, 4408h  ;IOCTL, Get if removable or not
    int 41h
    jnc .gotRemStatus
.badRedir:
    lea rdx, badRedir
    jmp badExit
.gotRemStatus:
    test al, al
    jnz .fixedDisk
    ;Now request IOCTL to give medium parameters
    mov ch, 08h ;Disk drive type IOCTL
    mov cl, 80h | 60h   ;Use undocumented LBA get parameters
    mov eax, 440Dh  ;Generic IOCTL 
    lea rdx, reqTable   ;Point to the table to fill in, bl has drive number 
    int 41h
    jc badExitGenericString
    mov rax, qword [rdx + genioctlGetParamsTable.sectorSize]    ;Get sector size
    mov word [sectorSize], ax
    mov rax, qword [rdx + genioctlGetParamsTable.numSectors]    ;Get num sectors
    sub rax, 2 ;Sub 2 to ensure no edge issues and round clusters down
    mov byte [media], 0F0h
    mov dword [hiddSector], 0   ;Make sure we initialise this to 0
    jmp selectFATtype
.fixedDisk:
    mov byte [remDev], -1   ;Set flag for fixed disk
    ;Read VBR for volume, request a buffer of 1000h bytes (max sector size 4k)
    mov ebx, 100h   ;Request 100 paragraphs
    mov eax, 4800h
    int 41h
    jc badExitGenericString
    mov qword [bufferArea], rax ;Use this as the buffer
    mov ecx, 1
    xor edx, edx    ;Read sector 0 of the volume
    call dosCrit1Enter
    mov byte [inCrit], -1   ;Entered a critical section
    call readSector
    mov byte [inCrit], 0    ;Exited critical section
    call dosCrit1Exit
    jc badExitGenericString
    mov rbx, qword [bufferArea]
    movzx eax, word [rbx + bpb.bytsPerSec]  ;Get sector size
    mov word [sectorSize], ax
    mov eax, dword [rbx + bpb.hiddSec]  ;Get the number of hidden sectors
    mov dword [hiddSector], eax
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
    mov byte [media], 0F8h
selectFATtype:
;Arrive here with rax = Number of sectors in volume
    mov qword [numSectors], rax
    movzx edx, word [startFormat.sectorSize]  ;Get the start var
    cmp word [sectorSize], dx ;Only allow for sector size 200h for now
    lea rdx, badSecSize
    jne badExit
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
    xor ebx, ebx
    mov byte [fatType], 0   ;Start by saying it must be FAT12
    mov ecx, 4  ;4 entries in the fat16table without the first entry
    lea rsi, fat16ClusterTable
    cmp eax, dword [rsi]
    jbe .medFound   ;Here we need to build a custom BPB for this device. 
    inc byte [fatType]  ;Make now FAT 16
    add rsi, 5  ;Goto next entry    
.fat16Lp:
    mov ebx, dword [rsi]    ;Clears upper 32 bytes of ebx
    cmp rax, rbx
    jbe .medFound
    add rsi, 5
    dec ecx
    jnz .fat16Lp
    inc byte [fatType]
    mov ecx, 4
.fat32Lp:
    cmp rax, qword [rsi]
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
    mov al, byte [media]
    mov byte [rdi + bpb.media], al
    mov eax, dword [hiddSector]
    mov dword [rdi + bpb.hiddSec], eax
    mov ax, word [sectorSize]
    mov word [rdi + bpb.bytsPerSec], ax
    mov al, byte [secPerClust]
    mov byte [rdi + bpb.secPerClus], al
    mov ax, word [numSectors]
    mov word [rdi + bpb.totSec16], ax
    call computeFATSize
    mov word [rdi + bpb.FATsz16], ax
    movzx eax, ax
    mov dword [fatSize], eax
    mov al, byte [remDev]
    and al, 80h ;Save only bit 7
    mov byte [rdi + bpb.drvNum], al
    call getVolumeID
    mov dword [rdi + bpb.volID], eax
    mov byte [bpbSize], 62  ;62 bytes to copy
    jmp short .bpbReady
.fat32:
    lea rdi, genericBPB32
    mov al, byte [media]
    mov byte [rdi + bpb32.media], al
    mov eax, dword [hiddSector]
    mov dword [rdi + bpb.hiddSec], eax
    mov ax, word [sectorSize]
    mov word [rdi + bpb32.bytsPerSec], ax
    mov al, byte [secPerClust]
    mov byte [rdi + bpb32.secPerClus], al
    mov eax, dword [numSectors]
    mov dword [rdi + bpb32.totSec32], eax
    call computeFATSize
    mov dword [rdi + bpb32.FATsz32], eax
    mov dword [fatSize], eax
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
;The attached bootloader has a FAT12 BPB, skip it
    lea rsi, qword [bootloader + 62]    ;Go past the BPB of the bootloader
    movzx ecx, word [sectorSize]
    sub ecx, 62 ;That many fewer bytes
    rep movsb   ;Copy the bootsector over
    mov rbx, qword [bufferArea] ;rbx = Memory Buffer address to read from
    mov byte [rbx + 509], 0 ;Make the disk not bootable
    mov word [rbx + 510], 0AA55h
    mov ecx, 1      ;ecx = Number of sectors to write
    xor edx, edx    ;rdx = Start LBA to write to
    call dosCrit1Enter
    mov byte [inCrit], -1   ;Entering a DOS level 1 critical section
    call freeAllDriveBuffers    ;Now we begin formatting, free all buffers
    call writeSector
    jc badExitGenericString
createDPB:
    ;Here we now create a DPB for this BPB
    ;First we find the current DPB for this device
    mov rbp, qword [cdsPtr] ;Get CDS ptr
    mov rbp, qword [rbp + cds.qDPBPtr]  ;Get the CDS's DPB ptr
    mov rsi, qword [bpbPointer]
    mov eax, 5300h  ;Now we update the DPB with this new information
    int 41h 
createFAT:
    ;Now we create the FAT sectors.
    ;We write both copies one sector at a time interleaving them.
    mov esi, dword [fatSize]    ;Get the number of sectors to write, as counter
    mov rdi, qword [bufferArea] ;Get the buffer area
    mov rbx, rdi    ;Save the pointer in rbx
    xor eax, eax
    movzx ecx, word [sectorSize]
    shr ecx, 3  ;Divide by 8
    rep stosq   ;Store that many 0 qwords
    mov rdi, rbx    ;Return rdi back to the start of the sector
    call writeFATStartSig   ;Write the first two clusters in the map
    mov ecx, 1  ;ecx = Number of sectors to write
    mov rdi, qword [bpbPointer] ;Get the ptr to the BPB
    movzx edx, word [rdi + bpb.revdSecCnt]  ;Get the first sector past reserved
    push rdx
    push rsi
    call writeSector
    pop rsi
    pop rdx
    jc badExitGenericString
    mov eax, dword [fatSize]
    add edx, eax    ;Go to second fat copy
    mov ecx, 1
    push rax
    push rdx
    push rsi
    call writeSector
    pop rsi
    pop rdx
    pop rax
    jc badExitGenericString
    mov rdi, qword [bufferArea]
    mov qword [rdi], 0  ;Overwrite the FAT reserved cluster markers
    mov dword [rdi + 8], 0  ;Overwrite potential additional FAT32 data
    dec esi ;Decrement the number of fat sectors left to count
.fatFillLoop:
    sub edx, eax    ;Come back to the first FAT copy
    inc edx ;Goto next sector
    mov ecx, 1
    push rdx
    push rsi
    call writeSector
    pop rsi
    pop rdx
    jc badExitGenericString
    mov eax, dword [fatSize]
    add edx, eax    ;Go to second fat copy
    mov ecx, 1
    push rax
    push rdx
    push rsi
    call writeSector
    pop rsi
    pop rdx
    pop rax
    jc badExitGenericString
    dec esi
    jnz .fatFillLoop
    ;Fall through once done with FAT
rootDirectory:
    ;FAT12 and 16 are simple, FAT32 is a bit more complex
    cmp byte [fatType], 2
    je .fat32
    ;Here we compute the number of Root Dir sectors and sanitise them
    ;rdx should point to that sector now (since it works on FAT copy 2 last)
    mov rbx, qword [bpbPointer]
    movzx esi, word [rbx + bpb.rootEntCnt]  ;Get the number of 32 byte entries
    shl esi, 5  ;Convert into the number of bytes in root directory
    movzx eax, word [sectorSize]    ;Get the sector size
    xchg esi, eax
    push rdx
    xor edx, edx
    div esi ;Divide to get number of sectors in eax, preserve edx
    pop rdx ;Save edx
    mov esi, eax    ;Number of sectors in esi
    mov ecx, 1  ;Write one sector
.fatLoop:
    inc edx ;Go to next sector now
    push rcx
    push rdx
    push rsi
    call writeSector
    pop rsi
    pop rdx
    pop rcx
    jc badExitGenericString
    dec esi
    jnz .fatLoop
    jmp exitFormat
.fat32:
    ;Now we have to allocate one cluster to the root directory.
    ;Allocate cluster 2, as this is a quick and dirty program
    ; Later come back and make this proper with bad cluster tags etc
    ;We do this by writing our zero sectors to the cluster
    ;edx should have the sector number for the first sector of cluster 2 at 
    ; this point.
    mov rbx, qword [bpbPointer] ;Get the bpbPointer
    movzx esi, byte [rbx + bpb32.secPerClus]
    mov ecx, 1  ;Write one sector at a time
.fat32Loop:
    inc edx ;Go to next sector now
    push rcx
    push rdx
    push rsi
    call writeSector
    pop rsi
    pop rdx
    pop rcx
    jc badExitGenericString
    dec esi
    jnz .fat32Loop

exitFormat:
    mov byte [inCrit], 0    ;Out of the critical section now
    call dosCrit1Exit
    call freeMemoryBlock
    lea rdx, okFormat
    mov eax, 0900h
    int 41h
    mov eax, 4C00h  ;Return with 0 as error code
    int 41h


;Utility functions below
freeMemoryBlock:
    mov r8, qword [bufferArea]
    test r8, r8
    retz
    mov eax, 4900h
    int 41h
    return

freeAllDriveBuffers:
;Frees all buffers that belong to the drive being formatted
;Called once in the critical section
    push rax
    push rdi
    mov al, byte [fmtDrive]
    mov rdi, qword [dosBuffPtr]
.mainLp:
    cmp byte [rdi + bufferHdr.driveNumber], al
    jne .gotoNextBuffer
    mov word [rdi + bufferHdr.driveNumber], 0   ;Clears the flags too
.gotoNextBuffer:
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    cmp rdi, -1
    jne .mainLp
    pop rdi
    pop rax
    return

writeFATStartSig:
;Writes the first two cluster blocks with the necessary signature
;Input: rdi -> Start of the FAT sector
    push rax
    push rbx
    mov rbx, qword [bpbPointer]
    movsx eax, byte [rbx + bpb.media]   ;Get the media byte, sign extend
    cmp byte [fatType], 1
    je .fat16
    ja .fat32
;Fat 12 here
    and eax, 00FFFFFFh  ;Save only low three bytes
.fat16:
    mov dword [rdi], eax
    jmp short .exit
.fat32:
    mov dword [rdi], eax
    mov rax, -1
    mov qword [rdi + 4], rax    ;Write a qword to allocate cluster 2 too
.exit:
    pop rbx
    pop rax
    return
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
    xor edx, edx
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
    xor edx, edx
    div ebx ;Exit with eax = number of sectors needed per FAT
.exit:
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    return

readSector:
;Input:
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    mov al, byte [fmtDrive]     ; Always read from fmtDrive
    mov rbx, qword [bufferArea] ; Memory Buffer address to read from
    int 45h
    pop rax ;Pop old flags into rax
    return
writeSector:
;Input:
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    mov al, byte [fmtDrive]     ; Always write to fmtDrive
    mov rbx, qword [bufferArea] ; Memory Buffer address to read from
    int 46h
    pop rax ;Pop old flags into rax
    return
badExitGenericString:
    lea rdx, badGeneric
badExit:
;Jumped to with rdx = Error message or 0 if no message
    test byte [inCrit], -1
    jz .noCrit
    call dosCrit1Exit
.noCrit:
    test rdx, rdx
    jz .noPrint
    mov ah, 09h
    int 41h
.noPrint:
    call freeMemoryBlock    ;Free the memory block if it needs freeing
    mov eax, 4CFFh  ;Return with -1 as error code
    int 41h

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
.breakReturnExit:
    or byte [rsp + 8*2], 1  ;Set CF on the stack flags
    call dosCrit1Exit   ;Exit the critical section since we are quitting
.breakReturnNoExit:
    iretq   ;Redo the operation
