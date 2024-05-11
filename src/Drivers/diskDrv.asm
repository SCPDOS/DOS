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
    mov al, drvBadCmd
    ja .msdWriteEntryError ;If yes, error!
    mov al, drvBadUnit 
    cmp byte [rbx + drvReqHdr.unitnm], 05h  ;Unit greater than 5 is invalid
    ja .msdWriteEntryError ;If yes, error!
    lea rsi, .msdBPBTbl  ;Point to the BPB pointer table
    movzx eax, byte [rbx + drvReqHdr.unitnm]
    shl eax, 3  ;Multiply by 8 to get pointer to pointer to bpb
    mov rbp, qword [rsi + rax]    ;Get pointer to bpb in rbp
    movzx eax, byte [rbx + drvReqHdr.cmdcde]   ;Get command code in al
    shl eax, 1  ;Multiply by 2 since each entry is a word in size
    lea rcx, .msdTable
    movzx eax, word [rcx + rax] ;Get distance from table base
    test eax, eax   ;Is the distance 0, i.e. function not implemented?
    jz .msdDriverExit ;Valid function number but not for MSD, exits with done!
    add rax, rcx    ;Else, add table address to the distance from the table
    call rax ;Goto function, rbp = devBPBPtr, rbx = reqBlkPtr
.msdDriverExit:
    or word [rbx + drvReqHdr.status], drvDonStatus ;Set done bit
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.msdWriteEntryError:
;Used for errors which occur before a function!
    call .msdWriteErrorCode
    jmp short .msdDriverExit
.msdIOError:  ;In Read and Write errors, rdi points to the dev struc
    mov rbx, rdi
    mov dword [rbx + ioReqPkt.tfrlen], esi ;Save number of IO-ed sectors
;Now fall through to general error
.msdGenDiskError:   ;DISK DRIVER ERROR HANDLER
    mov rbx, qword [reqHdrPtr]
    mov ah, 01h
    int 33h ;Read status of last operation
    jc .msdGenErr
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
.msdGenErr:
    mov al, 0Ch     ;Everything else is general error
.msdWriteErrorCode:    ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
    ret ;Return to set done bit
.msdTable:
    dw .msdInitShim - .msdTable     ;Function 0
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
.msdInitShim:
    push rbx
    push r15
    call msdInit
    pop r15
    pop rbx
    mov word [.msdTable], 0 ;Now prevent init from firing again
    ret
;All functions have the request packet ptr in rbx and the bpb pointer in rbp
.msdMedChk:          ;Function 1
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], mediaCheckReqPkt_size
    jne .msdWriteErrorCode
    ;If the BPB makes no sense, claim it was changed, so we can rebuild BPB.
    test byte [rbp + bpb.secPerClus], -1
    jz .mmcChange   ;If the BPB weird, say that it was changed!
    ;Now set the volume ID appropriately so that if error, we have it ready
    push rax
    lea rax, qword [rbp + bpb.volID]    ;Get the volID from the BPB
    mov qword [rbx + mediaCheckReqPkt.desptr], rax 
    pop rax

    call .msdCheckDeviceType    ;Check and ensure that media type is "swapped"
    jnz .mmcChange  ;Always change if swapping between same phys volume!
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
    movzx rax, byte [rbx + mediaCheckReqPkt.unitnm]
    mov dl, byte [rbx + mediaCheckReqPkt.medesc]    ;Media descriptor
    cmp byte [rbp + bpb32.media], dl    ;Compare media descriptor bytes
    je .mmcUnsure
.mmcChange:
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    ;Check here if there were any open handles on the device when it was changed
    ret
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    ret
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    ret


.msdBuildBPB:        ;Function 2
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], bpbBuildReqPkt_size
    jne .msdWriteErrorCode

    mov rsi, rbx
    movzx rax, byte [rsi + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    lea rcx, .msdBIOSmap
    mov dl, byte [rcx + rax]  ;Get translated BIOS number for req
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer
    xor ecx, ecx    ;Read Sector 0...
    add ecx, dword [rbp + bpb32.hiddSec]    ;Of selected volume!
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
    mov rdi, rbp     ;Get pointer to buffer to overwrite
    mov ecx, bpbEx_size/8
    push rsi
    rep movsq   ;Move the BPB data into the right space
    pop rsi
    test byte [rbp + bpb.secPerClus], -1 ;Does this BPB makes sense?
    retnz ;If its not zero, we ok
    ;Else, we error.
    mov eax, drvNotReady
    jmp .msdWriteErrorCode
.msdIOCTLRead:       ;Function 3, returns done
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode
    ret
.msdRead:            ;Function 4
;Will read one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode
    call .msdIOSetVolLbl
;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
;    test byte [7c02h], 1
;    jnz .msdGenErr
;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
    call .msdCheckDeviceType

    mov rdi, rbx
    xor esi, esi  ;Set sector read counter to zero
.msdr0:
    mov dh, 82h ;LBA Read Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    movzx eax, word [rbp + bpb.bytsPerSec] 
    add qword [rdi + ioReqPkt.strtsc], rax  ;Add one sector
    add qword [rdi + ioReqPkt.bufptr], rax  ;Add one sector
    inc esi
    cmp esi, dword [rdi + ioReqPkt.tfrlen]
    jne .msdr0
    mov rbx, rdi
    ret
.msdWrite:           ;Function 8
;Will write one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode
    call .msdIOSetVolLbl
;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
;    test byte [7c02h], 2
;    jnz .msdGenErr
;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
    call .msdCheckDeviceType

    mov rdi, rbx
    xor esi, esi  ;Set sector read counter to zero
.msdw0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    movzx eax, word [rbp + bpb.bytsPerSec] 
    add qword [rdi + ioReqPkt.strtsc], rax  ;Add one sector
    add qword [rdi + ioReqPkt.bufptr], rax  ;Add one sector
    inc esi
    cmp esi, dword [rdi + ioReqPkt.tfrlen]
    jne .msdw0
    mov rbx, rdi
    ret
.msdWriteVerify:     ;Function 9, writes sectors then verifies them
;Will write one sector at a time and then verify it.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode
    call .msdIOSetVolLbl
;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
;    test byte [7c02h], 2
;    jnz .msdGenErr
;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
    call .msdCheckDeviceType

    mov rdi, rbx
    xor esi, esi  ;Set counter to zero
.msdwv0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    mov dh, 84h ;LBA Verify Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    movzx eax, word [rbp + bpb.bytsPerSec] 
    add qword [rdi + ioReqPkt.strtsc], rax  ;Add one sector
    add qword [rdi + ioReqPkt.bufptr], rax  ;Add one sector
    inc esi
    cmp esi, dword [rdi + ioReqPkt.tfrlen]
    jne .msdwv0
    mov rbx, rdi
    ret
.msdIOCTLWrite:      ;Function 12, returns done
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    ret
.msdDevOpen:         ;Function 13
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], openReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + openReqPkt.unitnm]
    lea rcx, .msdHdlCnt
    inc byte [rcx + rax]  ;Inc handle cnt for given unit
    ret
.msdDevClose:        ;Function 14
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], closeReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + closeReqPkt.unitnm]
    lea rcx, .msdHdlCnt
    dec byte [rcx + rax]  ;Dec handle cnt for given unit
    ret
.msdRemovableMedia:  ;Function 15
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], remMediaReqPkt_size
    jne .msdWriteErrorCode
    
    movzx rax, byte [rbx + remMediaReqPkt.unitnm]
    lea rcx, .msdBIOSmap
    movzx eax, byte [rcx + rax]    ;Get BIOS number
    and eax, 80h ;Isolate bit 7 (the fixed drive bit)
    shl eax, 2  ;Shift the removable bit (bit 7) to the busy bit (bit 9)
    mov word [rbx + remMediaReqPkt.status], ax  ;Busy set if fixed!
    ret
.msdGenericIOCTL:    ;Function 19
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], ioctlReqPkt_size
    jne .msdWriteErrorCode
;Need to spend some time to implement proper IOCTL with LBA instead of CHS.
;Implement two undoc functions 80h|42h (format) and 80h|60h (get LBA params)
    mov al, drvBadCmd
    movzx ecx, word [rbx + ioctlReqPkt.majfun]
    cmp ch, 08h    ;Disk Drive Major Code?
    jne .msdWriteErrorCode  ;If not, exit bad
    test cl, 80h    ;Extended function bit set?
    jz .msdWriteErrorCode
    and cl, 7Fh     ;Clear the upper bit
    cmp cl, 41h     
    je .msdGIOCTLWrite
    cmp cl, 42h
    je .msdGIOCTLFormat
    cmp cl, 60h
    jne .msdWriteErrorCode  ;Error if not this function with bad command
    ;Get params here
    movzx eax, byte [rbx + ioctlReqPkt.unitnm] ;Get the driver unit number
    lea rdx, .msdBIOSmap
    mov dl, byte [rdx + rax]    ;Get the BIOS number for the device
    mov ah, 88h ;Read LBA Device Parameters
    push rbx
    int 33h
    ;Returns:
    ;rbx = Sector size in bytes
    ;rcx = Last LBA block
    mov rax, rbx    ;Move sector size into rax
    pop rbx ;Get back the ioctlReqPktPtr
    jc .msdGenDiskError
;Get LBA Table:
;Offset 0:  Size of the table in bytes (24 bytes) (BYTE)
;Offset 1:  Reserved, 7 bytes
;Offset 8:  Sector size in bytes (DWORD)
;Offset 16: Number Of Sectors on Medium + 1 (QWORD)
    mov rdx, qword [rbx + ioctlReqPkt.ctlptr]   ;Get the req pkt ptr
    mov qword [rdx + genioctlGetParamsTable.size], 24
    mov qword [rdx + genioctlGetParamsTable.sectorSize], rax
    mov qword [rdx + genioctlGetParamsTable.numSectors], rcx
    return

.msdGIOCTLWrite:
;Write Table:
;Offset 0:  Size of the table in bytes (24 bytes) (BYTE)
;Offset 1:  Number of sectors to write (BYTE)
;Offset 2:  Reserved, 6 bytes
;Offset 8:  Sector to start format at (QWORD)
;Offset 16: Pointer to transfer buffer (QWORD)
    call .msdGIOCTLFormatWriteSetup
    mov rbx, qword [rdi + genioctlLBAwrite.xferBuffer]
    mov ah, 83h
.msdGIOCTLwfCommon:
    int 33h
    jc .msdGenDiskError
    mov rbx, rsi    ;Geturns rbx to point to the request pointer
    return 

.msdGIOCTLFormat:
;Format Table:
;Offset 0:  Size of the table in bytes (24 bytes) (BYTE)
;Offset 1:  Number of sectors to format (BYTE)
;Offset 2:  Reserved, 6 bytes
;Offset 8:  Sector to start format at (QWORD)
    call .msdGIOCTLFormatWriteSetup
    mov ah, 85h
    jmp short .msdGIOCTLwfCommon

.msdGIOCTLFormatWriteSetup:
;Sets the following:
;al = Number of sectors to write/format
;rcx = Sector to begin transfer at
;dl = BIOS Drive to do transfer on
;rsi = Driver Packet (usually set to rbx)
;rdi = Write/Format packet
    movzx eax, byte [rbx + ioctlReqPkt.unitnm] ;Get the driver unit number
    lea rdx, .msdBIOSmap
    mov dl, byte [rdx + rax]    ;Get the BIOS number for the device
    mov rsi, rbx
    mov rdi, qword [rsi + ioctlReqPkt.ctlptr]   ;Get the req pkt ptr
    mov al, byte [rdi + genioctlLBAformat.numSectors]
    mov rcx, qword [rdi + genioctlLBAformat.startSector]
    return

.msdGetLogicalDev:   ;Function 23
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], getDevReqPkt_size
    jne .msdWriteErrorCode

    mov al, byte [.msdCurDev]
    mov byte [rbx + getDevReqPkt.unitnm], al
    ret
.msdSetLogicalDev:   ;Function 24
    mov al, drvBadDrvReq
    cmp byte [rbx + drvReqHdr.hdrlen], setDevReqPkt_size
    jne .msdWriteErrorCode
.msdInternalSetUnitNumber:  ;Called to set the unit number from reqpkt
    mov al, byte [rbx + getDevReqPkt.unitnm]
    mov byte [.msdCurDev], al
    ret

.msdBlkIOCommon:  ;Does block IO
;Called with rdi containing old rbx value and ah with function number
;rbp points to bpb
;Error handled by caller
;Sector count handled by caller
;Called with dh = BIOS function number
    movzx rax, byte [rdi + ioReqPkt.unitnm]
    lea rcx, .msdBIOSmap
    mov dl, byte [rcx + rax]  ;Get translated BIOS number for req in dl
    xor ecx, ecx
    mov ecx, dword [rbp + bpb32.hiddSec]  ;Goto start of volume
    add rcx, qword [rdi + ioReqPkt.strtsc]  ;Get sector in volume
    mov rbx, qword [rdi + ioReqPkt.bufptr]  ;Get Memory Buffer
    mov ah, dh
    mov al, 01h ;Do one sector at a time 
    int 33h
    return

.msdCheckDeviceType:
;Checks a new device is being transacted on. Sets the internal var if so.
;If additionally in single drive mode, and a different drive (either A or B)
; is being transacted on, prompts the user. Else, 
;Returns ZF=NZ if media number changed!
;!!!WARNING!!! THIS USES THE CONSOLE BIOS!!! VIOLATES HARDWARE ABSTRACTION!!!!
    movzx eax, byte [rbx + drvReqHdr.unitnm]    ;Get the now unit number
    cmp al, byte [.msdCurDev]    ;Compare against the last transacted device
    rete    ;Exit if equal (ZF=ZE)
;If not equal, check new drive is not A or B
    cmp al, 2
    jae .msdCDTexitOk ;Exit by setting the new unit number, keep ZF=ZE
    ;Check if we are in single drive mode or not
    test byte [.msdSingleFlag], -1
    jz .msdCDTexitOk    ;If not in single drive mode, exit ok
    cmp al, byte [.msdSingleDrv]    ;Is this single drive the same as the old?
    je .msdCDTexitOk    ;Exit if so
    mov byte [.msdSingleDrv], al    ;Else, replace this number
    add al, "A" ;Convert to a letter
    mov byte [.msdStrikeLetter], al
    lea rsi, .msdStrike
    mov ecx, .msdStrikeL
.msdCDTprintMessage:
    lodsb   ;Get the char in al, inc rsi
    int 29h ;Print char in al
    dec ecx
    jnz .msdCDTprintMessage
    xor eax, eax
    int 36h ;Blocking wait at the keyboard for a keystroke
.msdCDTexit:
    call .msdCDTexitOk  ;Set unit number and Set ZF
    inc eax ;Clear ZF
    ret
.msdCDTexitOk:
    call .msdInternalSetUnitNumber  ;Set unit number internally
    xor eax, eax
    ret
.msdIOSetVolLbl:
;Sets the volume label on requests to read, write, write/verify. Medchk does its own
;Input: rbx -> io request packet
;       rbp -> BPB to get volume ID from
;Output: Pointer placed in io request packet
    push rax
    lea rax, qword [rbp + bpb.volID]    ;Get the volID from the BPB
    mov qword [rbx + ioReqPkt.desptr], rax 
    pop rax
    ret
.msdStrike db 0Dh,0Ah,"Insert for drive "
.msdStrikeLetter db "A: and strike",0Dh,0Ah,"any key when ready",0Dh,0Ah,0Ah
.msdStrikeL equ $ - .msdStrike

.msdDefLabel db "NO NAME ",0 ;Default volume label
;LASTDRIVE default is 5
;This driver can only handle a maximum of 5 drives. Any more and 
; more MSD drivers must be loaded from CONFIG.SYS
.msdSingleFlag  db 0    ;Single removable drive only
.msdSingleDrv   db 0    ;Keeps track of the last single drive used. 
.msdCurDev   db 0  ;Dev to be used by the driver saved here! (usually 1-1)
; Except when single drive in use, in which case Drive A and B refer to device 0
.msdBIOSmap  db -1, -1, -1, -1, -1 ;Translates DOS drive number to BIOS number
.msdHdlCnt   db 5 dup (0)    ;Keeps a count of open handles to drive N
.msdBPBTbl   dq 5 dup (0)    ;BPB pointer table to be returned
.msdBPBblks  db 5*bpbEx_size dup (0) ;Max 5 bpb records of exFAT bpb size
.dfltBPB     defaultBPB ;If no removable devices, A and B point here
endptr equ $