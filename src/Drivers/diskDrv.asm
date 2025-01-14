
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
    movzx esi, byte [rbx + devReqHdr.cmdcde]    ;Get the command code
    cmp esi, drvMAXCMD                  ;Command code bigger than max?
    ja .writeEntryError                 ;If yes, error!
    lea rbp, .fnTbl
    lea rdi, qword [rbp + 4*esi]    ;Ptr to table entry
    movzx esi, word [rdi]   ;Get the offset from table into esi
    test esi, esi           ;If the offset is 0, exit!
    jz .exit
    movzx ecx, byte [rbx + devReqHdr.hdrLen]       ;Get packet length
    cmp cx, word [rdi + 2]          ;Cmp packet lengths
    jne .writeEntryError
    add rsi, rbp    ;Add the two to get the pointer!
    movzx eax, byte [rbx + devReqHdr.unitnm]    ;Get the unit to setup
    call .setupDrive    ;Returns rbp -> Table entry
    mov rbx, rdx    ;Get the req packet into rbx
;Goto function! rbp -> Table entry, eax = Drive number. rbx -> Reqpkt
    call rsi 
.exit:
    mov rbx, qword [reqHdrPtr]  ;Get back the req header ptr
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

.fnTbl:
;Each table entry is 4 bytes to make searching easier. Low word is offset
; to function, high word is packet size for check
    dw .initShim - .fnTbl        ;Function 0
    dw initReqPkt_size
    dw .medChk - .fnTbl          ;Function 1
    dw mediaCheckReqPkt_size
    dw .buildBPB - .fnTbl        ;Function 2
    dw bpbBuildReqPkt_size
    dw .IOCTLRead - .fnTbl       ;Function 3
    dw ioReqPkt_size
    dw .read - .fnTbl            ;Function 4
    dw ioReqPkt_size
    dw 0                            ;Function 5
    dw 0
    dw 0                            ;Function 6
    dw 0
    dw 0                            ;Function 7
    dw 0
    dw .write - .fnTbl              ;Function 8
    dw ioReqPkt_size
    dw .write - .fnTbl              ;Function 9
    dw ioReqPkt_size
    dw 0                            ;Function 10
    dw 0
    dw 0                            ;Function 11
    dw 0
    dw .IOCTLWrite - .fnTbl      ;Function 12
    dw ioReqPkt_size
    dw .devOpen - .fnTbl         ;Function 13
    dw openReqPkt_size
    dw .devClose - .fnTbl        ;Function 14
    dw closeReqPkt_size
    dw .remMed - .fnTbl          ;Function 15
    dw remMediaReqPkt_size
    dw 0                            ;Function 16
    dw 0
    dw 0                            ;Function 17
    dw 0
    dw 0                            ;Function 18
    dw 0
    dw .IOCTL - .fnTbl           ;Function 19
    dw ioctlReqPkt_size
    dw 0                            ;Function 20
    dw 0
    dw 0                            ;Function 21
    dw 0
    dw 0                            ;Function 22
    dw 0
    dw .getLogicalDev - .fnTbl   ;Function 23
    dw getDevReqPkt_size
    dw .setLogicalDev - .fnTbl   ;Function 24
    dw setDevReqPkt_size
.errBadCmd:
    mov eax, drvBadCmd
    jmp short .writeEntryError
.errBadPkt:
    mov eax, drvBadDrvReq
.writeEntryError:
;Used for errors in the driver entry
    call .errorExit
    jmp short .exit

;DISK DRIVER ERROR HANDLER. Errors from within the functions come here!
.errorXlat:
    mov rbx, qword [reqHdrPtr]
.ioError:   ;Jumped to from the blkIO processor with rbx -> reqHdr already
    mov eax, 0100h
    int 33h ;Read status of last operation
    jc .genErrExit
    cmp ah, 80h ;Timeout/Media Not Ready response (device not present)
    mov al, 02h ;Give device not ready error (sensibly I think)
    je .errorExit 
    mov al, 0Ch ;Preliminary General Error Faults
    cmp ah, -1  ;Sense operation failed
    je .errorExit 
    cmp ah, 20h ;Gen. ctrlr. failure. Consider new error code to halt system.
    je .errorExit
;Device Not Ready
    mov al, 02h  ;Device not ready code
    cmp r8b, al  ;SCSI Not ready commands start with 2
    je .errorExit
    shr r8, 8       ;Remove Sense Key
    movzx ecx, r8w  ;Get ASC and ASCQ in cl and ch bzw.
;Write Protected
    xor al, al
    cmp cx, 0027h   ;Write protected error
    je .errorExit
;CRC Error
    mov al, 04h     ;CRC error code
    cmp cx, 0308h   ;LU comms CRC error (UDMA/32)
    je .errorExit
    cmp cx, 0010h   ;ID CRC or ECC error
    je .errorExit
    cmp cx, 0147h   ;Data phase CRC error detected
    je .errorExit
;Seek Error
    mov al, 06h     ;Seek error code
    cmp cl, 02h     ;No Seek Complete
    je .errorExit
;Unknown Hardware Media (Shouldn't happen with Flash Drives)
;This error should only be called if BPB not recognised for Flash Drives
    mov al, 07h
    cmp cl, 30h   ;All issues with media returns unknown media
    je .errorExit
;Sector Not Found
    mov al, 08h     ;Sector not found code
    cmp cl, 21h     ;Illegal Request - Invalid LBA
    je .errorExit
;Write faults
    mov al, 0Ah     ;Write fault
    cmp cl, 0Ch     ;Write Error ASC code
    je .errorExit
;Read faults
    mov al, 0Bh     ;Read fault
    cmp cl, 11h     ;Read error
    je .errorExit
;General Errors
.genErrExit:
    mov al, 0Ch     ;Everything else is general error
.errorExit:     ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov rbx, qword [reqHdrPtr]
    mov word [rbx + drvReqHdr.status], ax
    return      ;Return to set done bit

.initShim:
    push rbx
    push r15
    call msdInit
    pop r15
    pop rbx
    mov word [.fnTbl], 0 ;Now prevent init from firing again
    return
;All functions have the request packet ptr in rbx and the bpb pointer in rbp
.medChk:          ;Function 1
;Start by setting the volume ID appropriately so that if error
; we have it ready
    push rax
    lea rax, qword [rbp + msdTblEntry.volID]    ;Get the volID from the BPB
    mov qword [rbx + mediaCheckReqPkt.desptr], rax 
    pop rax

    call .checkDevType    ;Check and ensure that media type is "swapped"
    test word [rbp + msdTableEntry.wDevFlgs], devFixed
    jnz .mmcNoChange
    mov dl, byte [rbp + msdTableEntry.bBIOSNum]
;Now we do a BIOS changeline check. If it returns 80h or 86h then check med desc
    mov ah, 16h 
    int 33h
    jc .errorXlat
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
    cmp byte [rbp + msdTblEntry.media], dl    ;Compare media descriptor bytes
    je .mmcUnsure
.mmcChange:
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    return
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    return
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    return

.buildBPB:        ;Function 2
;Only build BPB for removable devices and "non-locked" devices.
;Start by setting the pointer to the BPB in the reqpkt as this is 
; the table entry bpb which we will be returning.
    mov rsi, qword [rbp + msdTblEntry.bpb]  ;Get BPB ptr in tbl entry
    mov qword [rbx + bpbBuildReqPkt.bpbptr], rsi    ;Store it!
    test word [rbp + msdTblEntry.wDevFlgs], devFixed | devLockBPB
    retnz 
    mov rsi, rbx    ;Move req ptr to rsi
    mov edi, 5      ;Retry 5 times
.bbpblp:
    movzx edx, byte [rbp + msdTblEntry.bBIOSNum]
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer
    xor ecx, ecx    ;Read Sector 0...
    add ecx, dword [rbp + msdTblEntry.hiddSec]      ;Of selected volume!
    mov eax, 8201h  ;LBA Read 1 sector
    int 33h
    jnc .bbpbOk
    dec edi         ;Dec the counter
    jz .errorXlat   ;If we are out of counts, sorry buddy :(
    mov eax, 0100h  ;Now read status of last error
    int 33h
    jmp short .bbpblp    ;And try again
.bbpbOk:
;------------------------------------------------------
;At this point: 
;   rsi -> Driver Request Packet
;   rbx -> New BPB that was been read in
;   rbp -> msdblEntry for this drive
;------------------------------------------------------
;Check we if we have a valid bootsector.
    cmp byte [rbx + bpb.jmpBoot], 069h  ;Direct jump has no NOP
    je .checkMedDesc
    cmp byte [rbx + bpb.jmpBoot], 0E9h  ;Short jump has no NOP
    je .checkMedDesc
    cmp byte [rbx + bpb.jmpBoot + 2], 090h  ;NOP
    jne .oldDisk
    cmp byte [rbx + bpb.jmpBoot], 0EBh      ;JMP SHORT
    je .checkMedDesc
.oldDisk:
;--------------------------------------------------------------------
; Temp: Old disks are not considered. For now we need a DOS 3.3 or 
; greater BPB and a 0F0h or 0F8h media byte in the BPB. Thus we fall 
; through into the unk med error below.
;--------------------------------------------------------------------
;In this case we assume that this sector was a FAT sector of a 
; Floppy. Read the first word and drop the high nybble.
    ;mov ax, word [rbx]
    ;and ax, 0FFFh
    ;cmp ah, 0Fh
    ;jne .bbpbErr   
    ;call .checkBPB
    ;jc .bbpbErr
;Here the msdTblEntry entry has already been filled in with a BPB for the
; media byte found. We set the tbl entry to FAT12 immediately.
;.plopBpb:
;    lea rbx, qword [rbp + msdTblEntry.bpb]
;    mov qword [rsi + bpbBuildReqPkt.bpbptr], rbx
;    return
.bbpbErr:
;Bad media bytes go here. Means the media is unknown.
    mov al, drvBadMed       ;Default to unknown media error code
    jmp .errorExit
.checkMedDesc:
    mov al, byte [rbx + bpb.media]
    call .checkBPB
    jc .bbpbErr
.mbbpb0:
;Update the msdTblEntry with info from the BPB.
;rbx points to the disk BPB. May be bad so we need to ensure the values 
; are ok before updating the msdTbl entry. 
    mov rsi, rbx
    lea rdi, qword [rbp + msdTblEntry.bpb]
    call .getFATType    ;Type is given in ecx
    test edx, bpbFat32  ;Was this a FAT32?
    mov eax, bpbL
    mov ecx, bpb32L
    cmovz ecx, eax
    rep movsb
    



.getFATType:
;Computes FAT type. Returns bpb flag in edx
    movzx ecx, word [rbx + bpb.bytsPerSec]
    mov eax, ecx
    dec eax
    movzx edx, word [rbx + bpb.rootEntCnt]
    shl edx, 5  ;Multiply by 32 (dir entry size)
    add eax, edx
    xor edx, edx
    div ecx     ;eax = Root Dir sectors
    push rax    ;Save Root Dir sectors on the stack
    movzx eax, word [rbx + bpb.FATsz16]
    mov edx, dword [rbx + bpb32.FATsz32]
    test eax, eax
    cmovz eax, edx
    movzx ecx, byte [rbx + bpb.numFATs]
    mul ecx         ;eax = BPB_NumFATs * FATSz
    pop rcx         ;Get RootDirSectors into ecx
    movzx edx, word [rbx + bpb.revdSecCnt]
    add ecx, eax    ;ecx = (BPB_NumFATs * FATSz) + RootDirSectors
    add ecx, edx    ;ecx = (BPB_ResvdSecCnt + ecx)
    movzx eax, word [rbx + bpb.FATsz16]
    mov edx, dword [rbx + bpb32.FATsz32]
    test eax, eax
    cmovz eax, edx  ;eax = Totsec
    sub eax, ecx    ;Datasec [eax] = eax - ecx
    movzx ecx, byte [rbx + bpb.secPerClus]
    xor edx, edx
    div ecx         ;eax = CountofClusters = DataSec / BPB_SecPerClus;
    mov edx, bpbFAT12
    cmp eax, fat12MaxClustCnt
    retb
    shl edx, 1  ;Move bit into FAT32 position
    cmp eax, fat16MaxClustCnt
    retae   ;If above or equal, its in FAT32
    shl edx, 1  ;Else move into FAT16 position
    return

.checkBPB:
    cmp al, 0F0h    ;3.5" FDD standard layout (1.44Mb) [REMDEV]
    rete
    cmp al, 0F8h    ;Fixed disk standard
    rete
;--------------------------------------------------------------------
; For now, we do not have true FDD support at the BIOS level so we 
; don't care about media byte based work. To be implemented once the 
; driver is fixed.
;--------------------------------------------------------------------
    ;cmp al, 0F9h    ;5.25" and 720K 3.5" standard
    ;rete
    ;ja .pickFake
;--------------------------------------------------------------------
    stc
    return
;.pickFake:
;For bytes 0FAh-0FFh build a fake BPB in the entry we already have.


.IOCTLRead:       ;Function 3, returns immediately
.IOCTLWrite:      ;Function 12, returns done
    return

.read:            ;Function 4
;Will read one sector at a time.
    call .ioSetVolLbl
    call .checkDevType
    mov rdi, rbx
    xor esi, esi  ;Set sector read counter to zero
.msdr0:
    mov dh, 82h ;LBA Read Sectors
    call .blkIO
    call .ioAdv
    jne .msdr0
    return

.write:           ;Function 8/9
;Will write and optionally verify one sector at a time.
    call .ioSetVolLbl
    call .checkDevType
    mov rdi, rbx
    xor esi, esi  ;Set sector read counter to zero
.msdw0:
    mov dh, 83h ;LBA Write Sectors
    call .blkIO
    cmp byte [rdi + ioReqPkt.cmdcde], drvWRITEVERIFY
    jne .msdw1
    mov dh, 84h ;LBA Verify Sectors
    call .blkIO
.msdw1:
    call .ioAdv
    jne .msdw0
    return

.ioAdv:
;Advances the buffers on successful IO. 
;If returns ZF=ZE, we have completed all the IO for the request.
    movzx eax, word [rbp + msdTblEntry.bytsPerSec] 
    add qword [rdi + ioReqPkt.strtsc], rax  ;Add one sector
    add qword [rdi + ioReqPkt.bufptr], rax  ;Add one sector
    inc esi
    cmp esi, dword [rdi + ioReqPkt.tfrlen]
    return

.blkIO:  ;Does block IO
;Error handled internally
;Sector count handled by caller
;Called with dh = BIOS function number, rdi -> ioReqPkt, rbp -> msdTblEntry
    push rsi    ;Save sector count
    mov esi, 5  ;Retry counter five times
.biolp:
    mov dl, byte [rbp + msdTblEntry.bBIOSNum]
    xor ecx, ecx
    mov ecx, dword [rbp + msdTblEntry.hiddSec]  ;Goto start of volume
    add rcx, qword [rdi + ioReqPkt.strtsc]  ;Get sector in volume
    mov rbx, qword [rdi + ioReqPkt.bufptr]  ;Get Memory Buffer
    mov ah, dh
    mov al, 01h ;Do one sector at a time 
    int 33h
    jc .bioError
    pop rsi
    return
.bioError:
    mov eax, 0100h
    int 33h ;Read status of last operation
    dec esi
    jnz .biolp
    pop rsi     ;Rebalance the stack
    pop rbx     ;Drop the return pointer to balance stack
    mov rbx, qword [reqHdrPtr]
    mov dword [rbx + ioReqPkt.tfrlen], esi ;Save number of IO-ed sectors
    jmp .ioError


.devOpen:         ;Function 13
    cmp word [rbp + msdTblEntry.wOpenCnt], -1
    je .genErrExit  ;Inc past -1 is gen fault!
    inc word [rbp + msdTblEntry.wOpenCnt]
    return
.devClose:        ;Function 14
    cmp word [rbp + msdTblEntry.wOpenCnt], 0
    je .genErrExit  ;Dec past zero is gen fault
    dec word [rbp + msdTblEntry.wOpenCnt]
    return
.remMed:  ;Function 15
    movzx eax, word [rbp + msdTblEntry.wDevFlgs]    ;Get flags
    and eax, 1  ;Save Bit 0 only
    shl eax, 9  ;Move bit to position 9
    mov word [rbx + remMediaReqPkt.status], ax  ;Busy set if fixed!
    return

.IOCTL:    ;Function 19
;Need to spend some time to implement proper IOCTL with LBA instead of CHS.
;Implement two undoc functions 80h|42h (format) and 80h|60h (get LBA params).
    movzx ecx, word [rbx + ioctlReqPkt.majfun]
    mov eax, drvBadCmd
    cmp ch, 08h    ;Disk Drive Major Code?
    jne .errorExit  ;If not, exit bad
    test cl, 80h    ;Extended function bit set?
    jz .errorExit
    and cl, 7Fh     ;Clear the upper bit
    cmp cl, 41h     
    je .gIOCTLwfCommon
    cmp cl, 42h
    je .gIOCTLFormat
    cmp cl, 60h
    jne .errorExit  ;Error if not this function with bad command
    ;Get params here
    movzx edx, byte [rbp + msdTblEntry.bBIOSNum]
    movzx eax, 8800h ;Read LBA Device Parameters
    push rbx
    int 33h
    ;Returns:
    ;rbx = Sector size in bytes
    ;rcx = Last LBA block
    mov rax, rbx    ;Move sector size into rax
    pop rbx ;Get back the ioctlReqPktPtr
    jc .errorXlat
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

.gIOCTLwfCommon:
;Write Table:
;Offset 0:  Size of the table in bytes (24 bytes) (BYTE)
;Offset 1:  Number of sectors to write (BYTE)
;Offset 2:  Reserved, 6 bytes
;Offset 8:  Sector to start format at (QWORD)
;Offset 16: Pointer to transfer buffer (QWORD)
    call .gIOCTLFormatWriteSetup
    mov rbx, qword [rdi + genioctlLBAwrite.xferBuffer]
    mov ah, 83h
.gIOCTLwfCommon:
    int 33h
    jc .errorXlat
    mov rbx, rsi    ;Geturns rbx to point to the request pointer
    return 

.gIOCTLFormat:
;Format Table:
;Offset 0:  Size of the table in bytes (24 bytes) (BYTE)
;Offset 1:  Number of sectors to format (BYTE)
;Offset 2:  Reserved, 6 bytes
;Offset 8:  Sector to start format at (QWORD)
    call .gIOCTLFormatWriteSetup
    mov ah, 85h
    jmp short .gIOCTLwfCommon

.gIOCTLFormatWriteSetup:
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

.getLogicalDev:   ;Function 23

    return

.setLogicalDev:   ;Function 24
    mov byte [.bCurDev], al
    return

.setupDrive:
;Finds the first entry in the linked list which is for this drive, and
; sets up internal vars according to it. 
;Input: eax = Zero based drive number
;Output: .pCurDev setup for us. rbp = Same value
    lea rbp, .msdTable
.sdChk:
    cmp byte [rbp + msdTblEntry.bDOSNum], al
    je .sdExit
    mov rbp, qword [rbp +  msdTblEntry.pLink]
    cmp rbp, -1
    jne .sdChk  ;Keep looping until end of table
    pop rax     ;Rebalance stack from the call
    mov al, drvBadMed
    jmp .errorExit
.sdExit:
    mov qword [.pCurDev], rbp
    return

.checkDevType:
;Checks if we need to display the swap MSD message and displays it if so.
;The device must already be setup in rbp (and var) for this to work.
;Input: rbx -> Request block. rbp -> msdTbl entry 
    test word [rbp + msdTblEntry.wDevFlgs], devFixed | devOwnMulti
    retnz   ;If fixed or already owns drv, don't allow swapping
    test word [rbp + msdTblEntry.wDevFlgs], devMulti
    retz    ;If only one drive owns this letter, exit
;Else, now we find the current owner of this drive letter :)
    mov al, byte [rbp + msdTblEntry.bBIOSNum]   ;Cmp by bios numbers
    mov rdi, qword [.msdTable]
.cdtlp:
    cmp rdi, -1
    je .cdtBadExit
    cmp rdi, rbp    ;Skip the current device pointer
    je .cdtNextEntry
    cmp byte [rdi + msdTblEntry.bBIOSNum], al   
    jne .cdtNextEntry   ;Skip entry if not for device in question.
    ;Now we check if this is the current owner of the device?
    test word [rdi + msdTableEntry.wDevFlgs], devOwnMulti
    jnz .cdtDevFnd
.cdtNextEntry:
    mov rdi, qword [rdi + msdTableEntry.pLink]
    jmp short .cdtLp
.cdtDevFnd:
;Now we swap owners. rdi (current owner) looses ownership, rbp (request
; device) gains ownership.
    and word [rdi + msdTableEntry.wDevFlgs], ~devOwnMulti   ;Clear rdi own
    or word [rbp + msdTableEntry.wDevFlgs], devOwnMulti     ;Set rbp to own
;If a set map request, don't prompt the message!
    cmp byte [rbx + devReqHdr.unitnm], drvSETDRVMAP
    je .cdtExit

;THIS BIT IS NOT MULTITASKING FRIENDLY...
    mov al, byte [rbx + devReqHdr.unitnm]
    add al, "A" ;Convert to a letter
    mov byte [.strikeMsgLetter], al
    lea rsi, .strikeMsg
    mov ecx, .strikeMsgL
.cdtPrint:
    lodsb   ;Get the char in al, inc rsi
    int 29h ;Print char in al
    dec ecx
    jnz .cdtPrint
    xor eax, eax
    int 36h ;Blocking wait at the keyboard for a keystroke
;THIS BIT IS NOT MULTITASKING FRIENDLY...

.cdtExit:
    clc ;Indicate goodness through CF
    return
.cdtBadExit:
    stc ;Indicate badness through CF
    return

.ioSetVolLbl:
;Sets the volume label on requests to read, write, write/verify. Medchk does its own
;Input: rbx -> io request packet
;       rbp -> msdTblEntry to get volume ID from
;Output: Pointer placed in io request packet
    push rax
    lea rax, qword [rbp + msdTblEntry.volID]    ;Get the volID from the BPB
    mov qword [rbx + ioReqPkt.desptr], rax 
    pop rax
    ret

.strikeMsg db 0Dh,0Ah,"Insert for drive "
.strikeMsgLetter db "A: and strike",0Dh,0Ah,"any key when ready",0Dh,0Ah,0Ah
.strikeMsgL equ $ - .strikeMsg

.fat12Str   db "FAT12   ",0
.fat16Str   db "FAT16   ",0
.fat32Str   db "FAT32   ",0
.defLbl     db "NO NAME ",0 ;Default volume label
.oemName    db "SCPDOSv1",0 ;Default OEM name

.pCurDev    dq 0    ;Pointer to the MSD table for the device we are accessing
.dfltBPB     defaultBPB                 ;If no remdev, A and B point here
.msdTable   db 5*msdTblEntry dup (0)    ;Core MSD data table 
