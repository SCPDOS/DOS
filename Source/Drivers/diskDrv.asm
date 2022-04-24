msdDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8
    mov rbx, qword [reqHdrPtr]  ;Get the ptr to the req header in rbx
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    mov al, 03h
    ja .msdWriteErrorCode ;If yes, error!
    mov al, 01h ;Unknown Unit Error
    cmp byte [rbx + drvReqHdr.unitnm], 05h  ;Unit greater than 5 is invalid
    ja .msdWriteErrorCode ;If yes, error!
    movzx eax, byte [rbx + drvReqHdr.cmdcde]   ;Get command code in al
    shl eax, 1  ;Multiply by 2 since each entry is a word in size
    lea rcx, .msdTable
    movzx eax, word [rcx + rax] ;Get distance from table base
    test eax, eax   ;Is the distance 0, i.e. function not implemented?
    jz .msdDriverExit ;Valid function number but not for MSD, exits with done!
    add rax, rcx    ;Else, add table address to the distance from the table
    jmp rax ;Goto the function

.msdIOError:  ;In Read and Write errors, rbp points to the dev struc
    mov rbx, rbp
    movzx eax, al   ;Number of IO-ed sectors in last request
    add esi, eax    ;esi Keeps sector count across transfers
    mov dword [rbx + ioReqPkt.tfrlen], esi ;Save number of IO-ed sectors
;Now fall through to general error
.msdGenDiskError:
    mov ah, 01h
    xor dl, dl  ;Work around bug that fails request if dl > 7Fh
    int 33h ;Read status of last operation
    cmp ah, 80h ;Timeout/Media Not Ready response (device not present)
    mov al, 02h ;Give device not ready error (sensibly I think)
    je .msdWriteErrorCode 
    mov al, 0Ch ;Preliminary General Error Faults
    cmp ah, -1  ;Sense operation failed
    je .msdWriteErrorCode 
    cmp ah, 20h ;Gen. ctrlr. failure. Consider new error code to halt system.
    je .msdWriteErrorCode
;Device Not Ready
    mov al, 02h  ;Device not ready code
    cmp r8b, al  ;SCSI Not ready commands start with 2
    je .msdWriteErrorCode
    shr r8, 8       ;Remove Sense Key
    movzx ecx, r8w  ;Get ASC and ASCQ in cl and ch bzw.
;Write Protected
    xor al, al
    cmp cx, 0027h   ;Write protected error
    je .msdWriteErrorCode
;CRC Error
    mov al, 04h     ;CRC error code
    cmp cx, 0308h   ;LU comms CRC error (UDMA/32)
    je .msdWriteErrorCode
    cmp cx, 0010h   ;ID CRC or ECC error
    je .msdWriteErrorCode
    cmp cx, 0147h   ;Data phase CRC error detected
    je .msdWriteErrorCode
;Seek Error
    mov al, 06h     ;Seek error code
    cmp cl, 02h     ;No Seek Complete
    je .msdWriteErrorCode
;Unknown Hardware Media (Shouldn't happen with Flash Drives)
;This error should only be called if BPB not recognised for Flash Drives
    mov al, 07h
    cmp cl, 30h   ;All issues with media returns unknown media
    je .msdWriteErrorCode
;Sector Not Found
    mov al, 08h     ;Sector not found code
    cmp cl, 21h     ;Illegal Request - Invalid LBA
    je .msdWriteErrorCode
;Write faults
    mov al, 0Ah     ;Write fault
    cmp cl, 0Ch     ;Write Error ASC code
    je .msdWriteErrorCode
;Read faults
    mov al, 0Bh     ;Read fault
    cmp cl, 11h     ;Read error
    je .msdWriteErrorCode
;General Errors
    mov al, 0Ch     ;Everything else is general error
.msdWriteErrorCode:    ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.msdDriverExit:
    or word [rbx + drvReqHdr.status], 0100h ;Set done bit
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.msdTable:
    dw .msdInit - .msdTable         ;Function 0
    dw .msdMedChk - .msdTable       ;Function 1
    dw .msdBuildBPB - .msdTable     ;Function 2
    dw .msdIOCTLRead - .msdTable    ;Function 3
    dw .msdRead - .msdTable         ;Function 4
    dw 0                            ;Function 5
    dw 0                            ;Function 6
    dw 0                            ;Function 7
    dw .msdWrite - .msdTable        ;Function 8
    dw .msdWriteVerify - .msdTable  ;Function 9
    dw 0                            ;Function 10
    dw 0                            ;Function 11
    dw .msdIOCTLWrite - .msdTable   ;Function 12
    dw .msdDevOpen - .msdTable      ;Function 13
    dw .msdDevClose - .msdTable     ;Function 14
    dw .msdRemovableMedia - .msdTable   ;Function 15
    dw 0                            ;Function 16
    dw 0                            ;Function 17
    dw 0                            ;Function 18
    dw .msdGenericIOCTL - .msdTable ;Function 19
    dw 0                            ;Function 20
    dw 0                            ;Function 21
    dw 0                            ;Function 22
    dw .msdGetLogicalDev - .msdTable    ;Function 23
    dw .msdSetLogicalDev - .msdTable    ;Function 24


.msdInit:            ;Function 0
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], initReqPkt_size
    jne .msdWriteErrorCode

    lea rbp, endptr
    mov qword [rbx + initReqPkt.endptr], rbp    ;Where the end is gonna be
    lea rbp, .msdBPBTbl
    mov qword [rbx + initReqPkt.optptr], rbp    ;Where bpb tbl is gonna be

    mov rbp, rbx ;Save the req block ptr in rbp
    xor edx, edx  ;Start from device zero
    mov byte [rbp + initReqPkt.numunt], dl   ;Zero this field, max 5
.mi0:   ;Now check each device for partitions
    cmp byte [rbp + initReqPkt.numunt], 5
    je .msdExit ;IF we are at 5 now, we exit
    mov ah, 82h ;LBA read
    mov al, 1   ;1 sector
    xor ecx, ecx  ;Read sector 0
    lea rbx, msdTempBuffer  ;Get address of this space
    int 33h
    jc .msdInitError
;Now we verify if this is a BPB. Removable devices can't be partitioned
;1) Check byte 0 for EBh (short jmp) and byte 2 for a 90h (nop).
    mov al, byte [rbx]
    mov ah, byte [rbx + 2]
    cmp ax, 090EBh
    jne .mimbr
;Valid BPB found! Copy to internal table and inc lastdrive
    mov rsi, rbx
    mov eax, bpbEx_size
    mov ecx, edx    ;Temporarily save dl in ecx
    mul edx
    mov edx, ecx
    lea rdi, .msdBPBblks
    add rdi, rax
    mov ecx, bpbEx_size
    mov rax, rdi    ;Save the entry address in rax
    rep movsb   ;Copy the bpb into the bpb table
    lea rdi, .msdBPBTbl
    lea rdi, qword [rdi + 8*rdx]
    mov qword [rdi], rax
    lea rdi, .msdBIOSmap
    add rdi, rdx    ;rdx contains a number, table is a list of bytes
    mov byte [rdi], dl
    inc byte [rbp + initReqPkt.numunt]
    inc dl
    cmp dl, byte [numRemMSD] ;Once these are equal, we have processed last dev
    jne .mi0
.msdExit:
;If one device only, copy its BPB pointer and drive number
;When HDD support implemented, this will check the number of remdevs not lastdrv
    cmp byte [rbp + initReqPkt.numunt], 1
    jne .msdexit1
;Here ONLY if one device found
    lea rsi, .msdBPBTbl
    lea rdi, qword [rsi + 8]    ;Point to next entry
    movsq   ;Copy pointer
    lea rsi, .msdBIOSmap
    lea rdi, qword [rsi + 1]
    movsb   ;Copy byte
    inc byte [rbp + initReqPkt.numunt]
.msdexit1:
    mov rbx, rbp
    jmp .msdDriverExit
.mimbr:
;Goto next device without incrementing LASTDRIVE
    inc dl
    mov al, byte [numRemMSD]
    cmp dl, al ;Once these are equ, we have processed last dev
    jne .mi0
    jmp short .msdExit
.msdInitError:
    mov rbx, rbp
    jmp .msdGenDiskError
.msdMedChk:          ;Function 1
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], mediaCheckReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + mediaCheckReqPkt.unitnm]
    lea rcx, .msdBIOSmap
    mov dl, byte [rcx + rax]    ;Translate unitnum to BIOS num
    test dl, 80h    ;If it is a fixed disk, no change!
    jnz .mmcNoChange
;Now we do a BIOS changeline check. If it returns 80h or 86h then check med desc
    mov ah, 16h 
    int 33h
    jc .msdGenDiskError
    cmp ah, 80h
    je .mmcNoChangeLine
    cmp ah, 86h
    je .mmcNoChangeLine
    test ah, ah ;No change?
    jz .mmcNoChange
    test ah, 1  ;Neither 80h or 86h have bit 0 set
    jnz .mmcChange
;If nothing, fall through and test manually, should never happen though
.mmcNoChangeLine:
;Now we test Media Descriptor
    mov dl, byte [rbx + mediaCheckReqPkt.medesc]    ;Media descriptor
    lea rdi, .msdBPBTbl
    mov rdi, qword [rdi + 8*rax]
    mov rdi, qword [rdi]    ;Dereference rdi
    cmp byte [rdi + bpb32.media], dl    ;Compare media descriptor bytes
    je .mmcUnsure
.mmcChange:
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    lea rax, .msdDefLabel          ;Temp, ret def label
    mov qword [rbx + mediaCheckReqPkt.desptr], rax 
    jmp .msdDriverExit
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    jmp .msdDriverExit
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    jmp .msdDriverExit

.msdBuildBPB:        ;Function 2
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], bpbBuildReqPkt_size
    jne .msdWriteErrorCode

    mov rsi, rbx
    movzx rax, byte [rsi + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    lea rcx, .msdBIOSmap
    mov dl, byte [rcx + rax]  ;Get translated BIOS number for req
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer
    xor ecx, ecx    ;Read Sector 0
    mov eax, 8201h  ;LBA Read 1 sector
    int 33h
    jc .msdGenDiskError
;Check Media Descriptor, must be F0h or F8h-FFh or unknown media
    cmp byte [rbx + bpb.media], 0F0h    ;3.5" FDD standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0F8h    ;FDD/Large Media Standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0F9h    ;5.25" & 720K 3.5" Media Standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0FCh    ;Very Obsolete Media Standards
    mov al, 07h ;Unknown media error code
    jb .msdWriteErrorCode
.mbbpb0:
    xchg rbx, rsi    ;Transf Buf(rbx) <-> ReqHdr(rsi)
    movzx rax, byte [rbx + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    lea rdi, .msdBPBTbl
    mov rdi, qword [rdi + 8*rax] ;Get pointer to pointer to buffer
    mov rdi, qword [rdi] ;Dereference to get pointer to buffer 
    mov qword [rbx + bpbBuildReqPkt.bpbptr], rdi ;rdi -> final bpb resting place
    mov ecx, bpbEx_size/8
    rep movsq   ;Move the BPB data into the right space
    jmp .msdDriverExit
.msdIOCTLRead:       ;Function 3, returns done
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdRead:            ;Function 4
;Will read one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set sector read counter to zero
.msdr0:
    mov dh, 82h ;LBA Read Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdr0
    mov rbx, rbp
    jmp .msdDriverExit
.msdWrite:           ;Function 8
;Will write one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set counter to zero
.msdw0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdw0
    mov rbx, rbp
    jmp .msdDriverExit
.msdWriteVerify:     ;Function 9, writes sectors then verifies them
;Will write one sector at a time and then verify it.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set counter to zero
.msdwv0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    mov dh, 84h ;LBA Verify Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdwv0
    mov rbx, rbp
    jmp .msdDriverExit
.msdIOCTLWrite:      ;Function 12, returns done
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdDevOpen:         ;Function 13
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], openReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + openReqPkt.unitnm]
    lea rcx, .msdHdlCnt
    inc byte [rcx + rax]  ;Inc handle cnt for given unit
    jmp .msdDriverExit
.msdDevClose:        ;Function 14
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], closeReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + closeReqPkt.unitnm]
    lea rcx, .msdHdlCnt
    dec byte [rcx + rax]  ;Dec handle cnt for given unit
    jmp .msdDriverExit
.msdRemovableMedia:  ;Function 15
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], remMediaReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + remMediaReqPkt.unitnm]
    lea rcx, .msdBIOSmap
    mov al, byte [rcx + rax]    ;Get BIOS number
    test al, 80h
    jz .msdDriverExit   ;If removable, busy bit is clear
    mov word [rbx + remMediaReqPkt.status], 0200h ;Set Busy bit
    jmp .msdDriverExit
.msdGenericIOCTL:    ;Function 19
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioctlReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdGetLogicalDev:   ;Function 23
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], getDevReqPkt_size
    jne .msdWriteErrorCode

    mov al, byte [.msdCurDev]
    mov byte [rbx + getDevReqPkt.unitnm], al
    jmp .msdDriverExit
.msdSetLogicalDev:   ;Function 24
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], setDevReqPkt_size
    jne .msdWriteErrorCode

    mov al, byte [rbx + getDevReqPkt.unitnm]
    mov byte [.msdCurDev], al
    jmp .msdDriverExit

.msdBlkIOCommon:  ;Does block IO
;Called with rbp containing old rbx value and ah with function number
;Error handled by caller
;Sector count handled by caller
;Called with dh = BIOS function number
    movzx rax, byte [rbp + ioReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req in dl
    mov rcx, qword [rbp + ioReqPkt.strtsc]  ;Get start sector
    mov rbx, qword [rbp + ioReqPkt.bufptr]  ;Get Memory Buffer
    mov ah, dh
    mov al, 01h ;Do one sector at a time 
    int 33h
    ret

.msdDefLabel db "NO NAME ",0 ;Default volume label
;LASTDRIVE default is 5
.msdCurDev   db 0  ;Dev to be used by the driver saved here! (usually 1-1)
; Except when single drive in use, in which case Drive A and B refer to device 0
.msdBIOSmap  db 5 dup (0FFh) ;Translates DOS drive number to BIOS number
.msdHdlCnt   db 5 dup (0)    ;Keeps a count of open handles to drive N
.msdBPBTbl   dq 5 dup (0)    ;BPB pointer table to be returned
.msdBPBblks  db 5*bpbEx_size dup (0) ;Max 5 bpb records of exFAT bpb size

endptr equ $