; This file contains four main routines:
; 1) Replacement Int 39h routine (for unhooking interrupts back).
; 2) Replacement Int 33h routine.
; 3) Int 33h replacement routine.
; 4) Driver itself
; 5) Int 2Fh Driver backdoor routine

i39Org  dq 0    ;Original BIOS Int 39h to replace on Int 39h
i33Org  dq 0    ;Original BIOS Int 33h to replace on Int 39h.
i2FNext dq 0    ;Previous Int 2Fh handler
i33Next dq 0    ;Current disk driver to call.

;Replacement Int 39h routine to replace interrupts these drivers hook.
dosInt39h:
;For now, we just replace int 33h and int 39h back and then jump to i39h
    mov eax, 2533h
    mov rdx, qword [i33Org]
    int 21h
    mov eax, 2539h
    mov rdx, qword [i39Org]
    int 21h
;And now do the actual warm reboot
    jmp qword [i39Org]

;Replacement Int 33h routine
dosInt33h:
;For now, we just call the original Int 33h with no additional processing.
;Will be used to implement DOS error handling on Int 33h calls
    test byte [.inInt], -1  ;Spin on this var until we have done Int 33h
    jnz dosInt33h
    mov byte [.inInt], -1   ;Set that we are about to enter Int 33h
    pop qword [.tmp]        ;Pop the original return address off the stack
    call qword [i33Next]    ;Call previous handler
    push qword [.tmp]       ;Put the return address on the stack
    mov byte [.inInt], 0    ;Exit, now permit any waiting tasks to enter 
    return                  ;And return to the caller :)
.inInt  db 0    ;Set if we are doing this
.tmp dq 0

;Int 33h replacement routine
i2fhSwap33h:
;Replaces the current int 33h handler and the int 39h replacement handler
;Input: ah = 13h
;       rdx -> New Int 33h handler.
;       rbx -> Value to replace back when system shutdown occurs.
;Output:
;       rdx -> Replaced Int 33h handler.
;       rbx -> Replaced original lowest level Int 33h handler.
    cmp ah, 13h
    jne msdDriver.i2fDriver ;Goto the driver backdoor if not this handler 
    xchg qword [i33Next], rdx
    xchg qword [i33Org], rbx
    iretq

; Actual driver here
msdDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8
    mov rbx, qword [reqPktPtr]  ;Get the ptr to the req header in rbx
    movzx esi, byte [rbx + drvReqPkt.cmdcde]    ;Get the command code
    cmp esi, drvMAXCMD                  ;Command code bigger than max?
    ja .errBadCmd                 ;If yes, error!
    lea rbp, .fnTbl
    lea rdi, qword [rbp + 4*rsi]    ;Ptr to table entry
    movzx esi, word [rdi]   ;Get the offset from table into esi
    test esi, esi           ;If the offset is 0, exit!
    jz .exit
    movzx ecx, byte [rbx + drvReqPkt.hdrlen]       ;Get packet length
    cmp cx, word [rdi + 2]          ;Cmp packet lengths
    jne .errBadPkt
    add rsi, rbp    ;Add the two to get the pointer!
    movzx eax, byte [rbx + drvReqPkt.unitnm]    ;Get the unit to setup
    call .setupDrive    ;Returns rbp -> Table entry
;Goto function! rbp -> Table entry, eax = Drive number. rbx -> Reqpkt
    call rsi 
.exit:
    mov rbx, qword [reqPktPtr]  ;Get back the req header ptr
    or word [rbx + drvReqPkt.status], drvDonStatus ;Set done bit
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

.errBadCmd:
    mov eax, drvBadCmd
    jmp short .writeEntryError
.errBadPkt:
    mov eax, drvBadDrvReq
.writeEntryError:
;Used for errors in the driver entry
    call .errorExit
    jmp short .exit


.fnTbl:
;Each table entry is 4 bytes to make searching easier. Low word is offset
; to function, high word is packet size for check
    ;dw .initShim - .fnTbl        ;Function 0
    dw msdInit - .fnTbl
    dw initReqPkt_size
    dw .medChk - .fnTbl          ;Function 1
    dw mediaCheckReqPkt_size
    dw .buildBPB - .fnTbl        ;Function 2
    dw bpbBuildReqPkt_size
    dw .IOCTLRead - .fnTbl       ;Function 3
    dw ioReqPkt_size
    dw .read - .fnTbl            ;Function 4
    dw ioReqPkt_size
    dw 0                         ;Function 5
    dw 0
    dw 0                         ;Function 6
    dw 0
    dw 0                         ;Function 7
    dw 0
    dw .write - .fnTbl           ;Function 8
    dw ioReqPkt_size
    dw .write - .fnTbl           ;Function 9
    dw ioReqPkt_size
    dw 0                         ;Function 10
    dw 0
    dw 0                         ;Function 11
    dw 0
    dw .IOCTLWrite - .fnTbl      ;Function 12
    dw ioReqPkt_size
    dw .devOpen - .fnTbl         ;Function 13
    dw openReqPkt_size
    dw .devClose - .fnTbl        ;Function 14
    dw closeReqPkt_size
    dw .remMed - .fnTbl          ;Function 15
    dw remMediaReqPkt_size
    dw 0                         ;Function 16
    dw 0
    dw 0                         ;Function 17
    dw 0
    dw 0                         ;Function 18
    dw 0
    dw .IOCTL - .fnTbl           ;Function 19
    dw ioctlReqPkt_size
    dw 0                         ;Function 20
    dw 0
    dw 0                         ;Function 21
    dw 0
    dw 0                         ;Function 22
    dw 0
    dw .getLogicalDev - .fnTbl   ;Function 23
    dw getDevReqPkt_size
    dw .setLogicalDev - .fnTbl   ;Function 24
    dw setDevReqPkt_size

;DISK DRIVER ERROR HANDLER. Errors from within the functions come here!
.errorXlat:
    mov rbx, qword [reqPktPtr]
    mov eax, 0100h
    call .callI33h ;Read status of last operation
    jc .genErrExit
.ioError:   ;Jumped to from the blkIO processor with rbx -> reqHdr already
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
    mov rbx, qword [reqPktPtr]
    mov word [rbx + drvReqPkt.status], ax
    return      ;Return to set done bit

;All functions have the request packet ptr in rbx and the bpb pointer in rbp
.medChk:          ;Function 1
    test word [rbp + drvBlk.wDevFlgs], devFmt
    jz .mcNoFormat
    and word [rbp + drvBlk.wDevFlgs], ~devFmt   ;Clear this bit
    mov byte [.bLastDsk], -1    ;Formatted so cannot rely on timer logic
    test word [rbp + drvBlk.wDevFlgs], devFixed ;If fixed, declare changed!
    jnz .mmcChange
;For remdevs we now determine if the media was changed. If so, exit!
    jmp short .mcRem
.mcNoFormat:
    test word [rbp + drvBlk.wDevFlgs], devFixed
    jnz .mmcNoChange
.mcRem:
    call .checkDevType
    test word [rbp + drvBlk.wDevFlgs], devChgLine
    jnz .mmcNoChangeLine
    mov dl, byte [rbp + drvBlk.bBIOSNum]
;Now we do a BIOS changeline check. We rely on it for drives with changeline.
;Start by setting the lastDsk to unknown since this only makes sense for
; disks without changeline support
    mov byte [.bLastDsk], -1   
    mov ah, 16h 
    call .callI33h
    jc .mmcChange
;    cmp ah, 80h
;    je .mmcNoChangeLine
;    cmp ah, 86h
;    je .mmcNoChangeLine
    test ah, ah ;No change?
    jz .mmcNoChange
    jmp short .mmcChange
    ;test ah, 1  ;Neither 80h or 86h have bit 0 set
    ;jnz .mmcChange
.mmcNoChangeLine:
; If last accessed medchecked disk was this one and the time on this 
;  disk was more than 2 seconds ago, return unknown, else return ok.
    mov al, byte [rbp + drvBlk.bDOSNum] ;Get this disk number for the check
    xchg byte [.bLastDsk], al ;Swap with the old disk number
    cmp byte [.bLastDsk], al    ;Are they equal? If not, unsure.
    jne .mmcUnsure
;Else, now we do the famous time check. 
    call .checkTime ;Sets CF if unsure. Else stays the same
    jc .mmcUnsure
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    return
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    return
.mmcChange:
;Always store the volume label if we have a volume change.
    mov byte [.bLastDsk], -1    ;Default to unknown disk if a change occured!
    lea rdi, qword [rbp + drvBlk.volLab]
    mov qword [rbx + mediaCheckReqPkt.desptr], rdi
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    return

.buildBPB:        ;Function 2
;Only build BPB for removable devices and "non-locked" devices.
;Start by setting the pointer to the BPB in the reqpkt as this is 
; the table entry bpb which we will be returning.
    test word [rbp + drvBlk.wDevFlgs], devFixed
    jnz .bbpbExit
;------------------------------------------------------
; Here for removable devices only!!
;------------------------------------------------------
    call .resetIds  ;Reset the drvBlk volume ids
    mov rsi, rbx    ;Move req ptr to rsi
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer 
    call .updateBpb       ;Fill the BPB entries in the drvBlk
    jc .bbpbError
    call .moveVolIds    ;Move the volume ID's into the drvBlk if they exist.
    jnc .bbpbExit
;Here we will search the root directory for the volume label only!
;The FS string has been setup and volume ID is set to 0.
;
;   TEMP: DO NOTHING. USE DEFAULT STRING IN THIS CASE 
;
.bbpbExit:
    mov rbx, qword [reqPktPtr]  ;Get the driver ptr
    movzx eax, byte [rbp + drvBlk.bMedDesc] ;Get the meddesc from the bpb
    mov byte [rbx + bpbBuildReqPkt.medesc], al
    add rbp, drvBlk.bpb ;Move the drvBlk ptr to the BPB itself.
    mov qword [rbx + bpbBuildReqPkt.bpbptr], rbp
    return
.bbpbError:
    cmp al, drvBadMed   ;In case of bad media, just present it.
    je .errorExit   
    jmp .errorXlat  ;Else, get error code and xlat it to DOS error.

.resetIds:
;We reset the volume id string and label to the default for the 
; volume before the reset!
    push rax
    push rbx
    push rcx
    push rsi
    push rdi

;1) Clear volume Id
    mov dword [rbp + drvBlk.volId], 0
;2) Reset the volume label to default
    lea rsi, .defLbl
    lea rdi, qword [rbp + drvBlk.volLab]
    mov ecx, 12
    rep movsb
;3) Reset the FAT string
;Since fat32 indicator is in the middle, compare against it.
;If dskOff is set instead of FAT16, then FAT16 works as a default value :)
    lea rsi, .fat32Str
    lea rax, .fat12Str
    lea rbx, .fat16Str
    test byte [rbp + drvBlk.bBpbType], bpbFat32
    cmova rsi, rbx
    cmovb rsi, rax
    lea rdi, qword [rbp + drvBlk.filSysType]
    mov ecx, 9
    rep movsb

    pop rdi
    pop rsi
    pop rcx
    pop rbx
    pop rax
    return

.updateBpb:
;------------------------------------------------------
;Updates the BPB fields in drvBlk for the BPB on disk
; or failing, for the BPB indicated by the media byte.
;Never called on Fixed devs in normal operation.
;------------------------------------------------------
;Entered with: 
;   rbx -> Buffer to read bootsector into
;   rbp -> drvBlk for this drive
;------------------------------------------------------
    test word [rbp + drvBlk.wDevFlgs], devFixed | devLockBPB
    retnz  
    call .bbpbReadBS
    retc
;Check we if we have a valid bootsector.
    cmp byte [rbx], 069h   ;Direct jump has no NOP
    je .newDisk
    cmp byte [rbx], 0E9h    ;Short jump has no NOP
    je .newDisk
    cmp byte [rbx + 2], 090h  ;NOP
    jne .oldDisk
    cmp byte [rbx], 0EBh      ;JMP SHORT
    je .newDisk
.oldDisk:
    ;call .bbpbReadFAT   ;Read the FAT sector now instead
    ;retc
    ;mov ax, word [rbx]
    ;and ax, 0FFFh
    ;cmp ah, 0Fh     ;High byte must be 0Fh at this point.
    ;jne .bbpbErr
    ;call .bbpbCheckMedByt   ;Checks media byte to be valid
    ;jnz .bbpbErr
    ;cmp al, 0F0h    ;0F0h and 0F8h are not acceptable here as they need BPB
    ;je .bbpbErr
    ;cmp al, 0F8h
    ;je .bbpbErr
.bbpbErr:
;Bad media bytes go here. Means the media is unknown.
    mov al, drvBadMed       ;Default to unknown media error code
    stc
    return
.newDisk:
    add rbx, 11 ;Now point rbx to the BPB itself
    mov al, byte [rbx + bpb.media]
    call .bbpbCheckMedByt
;Update the drvBlk with info from the BPB.
;rbx points to the disk BPB. May be bad so we need to ensure the values 
; are ok before updating the msdTbl entry. 
    mov rsi, rbx    ;Source from the BPB in disk buffer
    lea rdi, qword [rbp + drvBlk.bpb]
    call .bbpbGetFAT    ;Fat type is given in edx
    mov byte [rbp + drvBlk.bBpbType], dl    ;Save the FAT type
;Get the correct length to correctly position rsi over the extended bs struct
; if it is present
    mov eax, bpb_size
    mov ecx, bpb32_size ;Now copy the BPB over!
    cmp dl, bpbFat32
    cmovne ecx, eax     ;If not FAT32, replace move count
    rep movsb   
    clc     ;Ensure if we return here, we return with CF happy :)
    return

.moveVolIds:
;Now check the BPB for a extBs. If it is present, we copy the information.
;Input: rsi -> End of the BPB in sector. rbx -> BPB in sector. rbp -> drvBlk
;Output: CF=CY: No volume label in sector found.
;        CF=NC: Volume Label in sector found and copied.
    cmp byte [rsi + extBs.bootSig], extBsSig
    jne .mviNoSig
;Else, now we copy the volume information from the extended bs info block
    mov eax, dword [rsi + extBs.volId]
    mov dword [rbp + drvBlk.volId], eax
    add rsi, extBs.volLab
    lea rdi, qword [rbp + drvBlk.volLab]
    mov ecx, 11 ;Copy the volume label
    rep movsb   
    ;rsi now points to the filSysType field in the extBs.
    ;Move rdi to the filSysType field in the drvBlk.
    lea rdi, qword [rbp + drvBlk.filSysType]
    mov ecx, 8  ;Now copy the 8 char string over too
    rep movsb   
;Clear the devswap bit now as we have a good BPB for this drive
    and word [rbp + drvBlk.wDevFlgs], ~devSwap
    clc
    return
.mviNoSig:
    stc
    return

.bbpbGetFAT:
;Computes FAT type. Returns bpb flag in edx. rbx -> BPB itself
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
    movzx eax, word [rbx + bpb.totSec16]
    mov edx, dword [rbx + bpb32.totSec32]
    test eax, eax
    cmovz eax, edx  ;eax = Totsec
    sub eax, ecx    ;Datasec [eax] = eax - ecx
    movzx ecx, byte [rbx + bpb.secPerClus]
    xor edx, edx
    div ecx         ;eax = CountofClusters = DataSec / BPB_SecPerClus;
    mov edx, bpbFat12
    cmp eax, fat12MaxClustCnt
    retb
    shl edx, 1  ;Move bit into FAT32 position
    cmp eax, fat16MaxClustCnt
    retnb   ;If above or equal, its in FAT32
    shl edx, 1  ;Else move into FAT16 position
    return

.bbpbCheckMedByt:
;Checks the media byte is of a valid type. Refuse media bytes we don't
; recognise as this is a sign of an unhealthy volume.
;Accept values 0FFh - 0F8h and 0F0h.
;Values 0FAh, 0F8h and 0F0h NEED to come from BPB. If found from FAT, then 
; do not accept the volume!
;Input: al = Media byte. 
;Ouput: ZF=NZ: Bad media byte. ZF=ZE: Ok media byte!
    cmp al, 0F0h
    rete
    cmp al, 0F8h
    retb
    cmp al, al  ;Set ZF
    return

.bbpbReadFAT:
;Reads the first FAT sector of media we are playing with.
;Input: rbx -> Buffer we are xacting on
    xor ecx, ecx
    inc ecx         ;Read Sector 1...
    jmp short .bbpbReadEp
.bbpbReadBS:
;Reads the bootsector of media we are playing with.
;Input: rbx -> Buffer we are xacting on
    xor ecx, ecx    ;Read Sector 0...
.bbpbReadEp:
    mov edi, 5      ;Retry 5 times
.bbpbrbsLp:
    movzx edx, byte [rbp + drvBlk.bBIOSNum]
    add ecx, dword [rbp + drvBlk.dHiddSec]      ;Of selected volume!
    mov eax, 8201h  ;LBA Read 1 sector
    call .callI33h
    jnc .bioExit    ;Exit via the IO exit
    dec edi         ;Dec the counter
    jz .bbpbrbsErr   ;If we are out of counts, sorry buddy :(
    mov eax, 0100h  ;Now read status of last error
    call .callI33h
    jmp short .bbpbrbsLp    ;And try again
.bbpbrbsErr:
    stc
    return

.IOCTLRead:       ;Function 3, returns immediately
.IOCTLWrite:      ;Function 12, returns done
    return

.read:            ;Function 4
;Will read one sector at a time.
    call .ioSetVolLbl
    call .checkDevType
    mov rdi, rbx
    call .bioSetupRegs
    retz
.msdr0:
    mov ah, 82h ;LBA Read Sectors
    call .blkIO
    call .ioAdv
    jne .msdr0
    return

.write:           ;Function 8/9
;Will write and optionally verify one sector at a time.
    call .ioSetVolLbl
    call .checkDevType
    mov rdi, rbx
    call .bioSetupRegs
    retz
.msdw0:
    mov ah, 83h ;LBA Write Sectors
    call .blkIO
    cmp byte [rdi + ioReqPkt.cmdcde], drvWRITEVERIFY
    jne .msdw1
    mov ah, 84h ;LBA Verify Sectors
    call .blkIO
.msdw1:
    call .ioAdv
    jne .msdw0
    return

.ioAdv:
;Advances the buffers on successful IO. 
;If returns ZF=ZE, we have completed all the IO for the request.
;Input: 
;       rbx -> Where we just IO'ed to
;       rcx = LBA sector we just xfred
;       dl  = BIOS drive number
;       rdi -> ioReqPkt
;       rbp -> drvBlk
;       esi = Number of sectors to xfr
;Output:
;       rbx -> Where to IO next sector to/from
;       rcx = LBA of next sector to xfer
;       dl  = BIOS drive number
;       rdi -> ioReqPkt
;       rbp -> devBlk
;       esi = Sectors left to xfr.
;       ZF=ZE if esi is 0. Else ZF=NZ.
    inc rcx     ;Goto next sector
    movzx eax, word [rbp + drvBlk.wBpS] 
    add rbx, rax  ;Advance the buffer pointer by 1 sector
    dec esi     ;Once this hits 0, we stop the xfr
    return

.bioSetupRegs:
;Sets up sector to read and buffer ptr in blkIO.
;If returns ZF=ZE then xfr 0 sectors, exit immediately
;Output: rdi -> ioReqPkt
;        rbp -> devBlk
;        rbx -> Transfer buffer
;        rcx = Sector to transfer
;        esi = Number of sectors to transfer
;        ZF=ZE if esi is 0. Else ZF=NZ.
    mov ecx, dword [rbp + drvBlk.dHiddSec]  ;Goto start of volume
    add rcx, qword [rdi + ioReqPkt.strtsc]  ;Get sector in volume
    mov rbx, qword [rdi + ioReqPkt.bufptr]  ;Get Memory Buffer
    mov dl, byte [rbp + drvBlk.bBIOSNum]    ;Get BIOS drive number
    mov esi, dword [rdi + ioReqPkt.tfrlen]  ;Get the tfrlen into esi
    test esi, esi                           ;If this is 0, avoid IO
    return

.blkIO:  ;Does block IO
;Error handled internally and return to 
;Sector count handled by caller.
;All registers marked as input registers must be preserved across the call
; except ah and rdi
;Input: ah = BIOS function number, 
;       rdi -> ioReqPkt 
;       rbp -> drvBlk
;       rbx -> Transfer buffer
;       rcx = LBA sector to transfer
;       dl  = BIOS drive number
;       esi = Sectors left to xfr!
    test word [rbp + drvBlk.wDevFlgs], devUnFmt
    jnz .bioufmted
    push rsi    ;Save sector count
    mov esi, 5  ;Retry counter five times
.biolp:
    call .bioSanity ;Sanity check ecx here
    mov al, 01h ;Do one sector at a time 
    call .callI33h
    jc .bioError
    cmp al, 1   ;Did we read one sector?
    jne .bioError
    pop rsi ;Rebalance stack
.bioExit:
    mov al, byte [rbp + drvBlk.bDOSNum]
    mov byte [.bLastDsk], al    ;Last DOS disk accessed
    test word [rbp + drvBlk.wDevFlgs], devFixed
    retnz
;Ensure we set the time of the operation w/o modifying the registers.
;Routine trashes ecx and edx so save!
    push rcx
    push rdx
    call .setTime   ;Set the current time and clear state for successful IO
    pop rdx
    pop rcx
    return
.bioError:
    ;xor eax, eax    ;Reset disk: CRASHES BOCHS
    push rdx    ;Preserve drive number. All other regs preserved
    mov eax, 0100h
    call .callI33h ;Read status of last operation
    pop rdx     ;Get back drive number.
    dec esi
    jnz .biolp
    pop rsi     ;Rebalance the stack
    pop rbx     ;Drop the return pointer to balance stack
    jmp .ioError
.bioSanity:
;Input: ecx = Sector we will transact on. rbp -> DrvBlk
;Output: CF=NC, sector ok to xact on
;        CF=CY, doesnt return, fails the call
    push rax
    push rbx
    movzx eax, word [rbp + drvBlk.wTotSec16]
    mov ebx, dword [rbp + drvBlk.dTotSec32] 
    test eax, eax
    cmovz eax, ebx  ;The 32 bit count is valid only if 16 bit count is 0
    cmp ecx, eax    ;This will set CF iff ecx < eax. If eax >= ecx CF=NC.
    pop rbx
    pop rax
    cmc             ;Flip CF from CY to NC if ok and NC to CY if not
    retnc
;Now pops the return from the sanity call and falls
    pop rax ;Pop from sanity call
.biobadsect:
    pop rax ;Pop from bio call
    mov eax, drvSecNotFnd
    jmp .errorExit
.bioufmted:
    pop rax ;Pop from bio call
    mov eax, drvBadMed
    jmp .errorExit

.devOpen:         ;Function 13
    cmp word [rbp + drvBlk.wOpenCnt], -1
    rete  ;Inc past -1 does nothing!
    inc word [rbp + drvBlk.wOpenCnt]
    return
.devClose:        ;Function 14
    cmp word [rbp + drvBlk.wOpenCnt], 0
    rete    ;Dec past zero does nothing
    dec word [rbp + drvBlk.wOpenCnt]
    return
.remMed:  ;Function 15
;Sets busy bit if fixed drive!
    test word [rbp + drvBlk.wDevFlgs], devFixed ;Is it fixed?
    retz
    mov word [rbx + remMediaReqPkt.status], drvBsyStatus
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
    je .gIOCTLWrite
    cmp cl, 42h
    je .gIOCTLFormat
    cmp cl, 60h
    jne .errorExit  ;Error if not this function with bad command
    ;Get params here
    movzx edx, byte [rbp + drvBlk.bBIOSNum]
    mov eax, 8800h ;Read LBA Device Parameters
    push rbx
    call .callI33h
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

.gIOCTLWrite:
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
    call .callI33h
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
    mov dl, byte [rbp + drvBlk.bBIOSNum]    ;Get BIOS number for device
    mov rsi, rbx
    mov rdi, qword [rsi + ioctlReqPkt.ctlptr]   ;Get the req pkt ptr
    mov al, byte [rdi + genioctlLBAformat.numSectors]
    mov rcx, qword [rdi + genioctlLBAformat.startSector]
    return

.getLogicalDev:   ;Function 23
;Returns 0 if device not multi. Else 1 based number of current drive
; owner of the BIOS device is returned in getDevReqPkt.unitnm
    xor eax, eax
    test word [rbp + drvBlk.wDevFlgs], devMulti
    jz .gldExit
    movzx eax, byte [rbp + drvBlk.bBIOSNum] ;Now find owner of this BIOS drv
    lea rbp, .drvBlkTbl ;Start from head of table :)
.gldLp:
    cmp byte [rbp + drvBlk.bBIOSNum], al
    cmovne rbp, qword [rbp +  drvBlk.pLink] ;If not for BIOS drive, goto next
    jne .gldLp
    test word [rbp + drvBlk.wDevFlgs], devOwnDrv
    cmovz rbp, qword [rbp +  drvBlk.pLink]  ;If not owner goto next
    jz .gldLp 
    movzx eax, byte [rbp + drvBlk.bDOSNum]  ;Else get DOS number for owner
    inc eax ;Make it 1 based
.gldExit:
    mov byte [rbx + getDevReqPkt.unitnm], al    ;Return value in unitnum
    return

.setLogicalDev:   ;Function 24
    call .checkDevType  ;Set the unit as the owner of this BIOS drive!
    return

.setupDrive:
;Finds the DOS drive in the linked list which is for this drive, and
; sets up internal vars according to it. 
;Input: eax = Zero based DOS drive number. rbx -> Packet
;Output: .pCurDrv setup for us. rbp = Same value
    cmp byte [rbx + drvReqPkt.cmdcde], drvINIT
    rete
    lea rbp, .drvBlkTbl
.sdChk:
    cmp byte [rbp + drvBlk.bDOSNum], al
    je .sdExit
    mov rbp, qword [rbp +  drvBlk.pLink]
    cmp rbp, -1
    jne .sdChk  ;Keep looping until end of table
    mov al, drvBadMed
    jmp .errorExit  ;Return through this exit
.sdExit:
    mov qword [.pCurDrv], rbp
    return

.checkDevType:
;Checks if we need to display the swap drive message and displays it if so.
;The device must already be setup in rbp (and var) for this to work.
;Input: rbx -> Request block. rbp -> drvBlk entry 
    test word [rbp + drvBlk.wDevFlgs], devFixed | devOwnDrv
    retnz   ;If fixed or already owns drv, don't allow swapping
    test word [rbp + drvBlk.wDevFlgs], devMulti
    retz    ;If only one drive owns this letter, exit
;Else, now we find the current owner of this drive letter :)
    mov al, byte [rbp + drvBlk.bBIOSNum]   ;Cmp by bios numbers
.cdtSetEp:
    lea rdi, .drvBlkTbl  ;Point to the first drvBlk
.cdtLp:
    cmp rdi, -1
    je .cdtBadExit
    cmp rdi, rbp    ;Skip the current device pointer
    je .cdtNextEntry
    cmp byte [rdi + drvBlk.bBIOSNum], al   
    jne .cdtNextEntry   ;Skip entry if not for device in question.
    ;Now we check if this is the current owner of the device?
    test word [rdi + drvBlk.wDevFlgs], devOwnDrv
    jnz .cdtDevFnd
.cdtNextEntry:
    mov rdi, qword [rdi + drvBlk.pLink]
    jmp short .cdtLp
.cdtDevFnd:
;Now we swap owners. rdi (current owner) looses ownership, rbp (request
; device) gains ownership.
    and word [rdi + drvBlk.wDevFlgs], ~devOwnDrv   ;Clear rdi own
    or word [rbp + drvBlk.wDevFlgs], devOwnDrv     ;Set rbp to own
;If a set map request, don't prompt the message!
    cmp byte [rbx + drvReqPkt.cmdcde], drvSETDRVMAP
    rete    ;Return if equal (clears CF)

;THIS BIT IS NOT MULTITASKING FRIENDLY...
    mov al, byte [rbp + drvBlk.bDOSNum]
    add al, "A" ;Convert to a letter
    mov byte [.strikeMsgLetter], al
    lea rsi, .strikeMsg
    mov ecx, .strikeMsgL
.cdtPrint:
    lodsb   ;Get the char in al, inc rsi
    int 29h ;Print char in al
    dec ecx
    jnz .cdtPrint

    call .cdtCleanKeyb  ;Clean the buffer!
    call .cdtAwaitKeyb  ;Await until a char ready in a friendly way :)
;THIS BIT IS NOT MULTITASKING FRIENDLY...

    clc ;Indicate goodness through CF
    return
.cdtBadExit:
    pop rax
    mov eax, drvBadMed
    stc ;Indicate badness through CF
    jmp .errorExit

.cdtAwaitKeyb:
    mov eax, 0100h
    int 36h ;If return ZF=ZE, we have no char in the buffer. Loop until we do!
    jz .cdtAwaitKeyb
    xor eax, eax
    int 36h ;Now pull the char!
    return
.cdtCleanKeyb:
    mov eax, 0100h
    int 36h ;If return ZF=NZ we have a char in the buffer, pull it!
    retz    ;Else ZF=ZE, no char, ready to await the keypress.
    xor eax, eax    ;Pull the char in the buffer from buffer
    int 36h
    jmp short .cdtCleanKeyb

.ioSetVolLbl:
;Sets the volume label on requests to read, write, write/verify. Medchk does its own
;Input: rbx -> io request packet
;       rbp -> drvBlk to get volume ID from
;Output: Pointer placed in io request packet
    push rax
    lea rax, qword [rbp + drvBlk.volLab]    ;Get the volLbl from the BPB
    mov qword [rbx + ioReqPkt.desptr], rax 
    pop rax
    ret

.getTime:
;Gets the current time in a format ready to be used for disk access.
    xor eax, eax
    int 3Ah
    movzx edx, dx
    shl ecx, 16 ;Move the high word into place, fill low word with 0's
    or ecx, edx ;Store the current time count into ecx
    test al, al ;Are we rolling over? al tells us how many days...
    jz .stStore
    movzx eax, al
    push rcx    ;Save the current time count
    mov ecx, 1800B0h    ;A single day's worth of ticks at 55ms
    mul ecx
    pop rcx
    add ecx, eax        ;Add "al" worth of ticks at 55ms to ecx :)
.stStore:
    clc
    return

.setTime:
;Sets the current time to the disk drive and resets the access counter
    call .getTime
    mov dword [rbp + drvBlk.dAccTime], ecx  ;And store it
    mov byte [.bAccCnt], 0  ;And set the access count back to 0
    return

.checkTime:
;Does the time/access count check :)
;Returns: CF=CY if unknown, CF=NC if no change
    call .getTime   ;Returns in ecx the current time
    test ecx, ecx   ;If this is 0 for some reason, use the accesses count  
    jnz .ctOk
    inc byte [.bAccCnt]
    cmp byte [.bAccCnt], maxAcc ;If below, we say ok!
    jb .ctNoChange
    dec byte [.bAccCnt] ;Else drop the inc and say unsure
    jmp short .ctMaybeCh
.ctOk:
    mov edx, dword [rbp + drvBlk.dAccTime]  ;Get last disk access time
;ecx = time of current check, adjusted for day rollovers 
    sub ecx, edx    
    cmp ecx, 36 ;Is this leq 36? 36 ticks at 55ms is approx 2 seconds.
    jbe .ctNoChange
.ctMaybeCh:
    stc
    return
.ctNoChange:
    clc
    return

.callI33h:
;Wraps all i33 calls allowing me to preserve all that I need to preserve
; across these calls.
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp
    int 33h
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    return

.i2fDriver:
;Back door into the block driver :)
    cmp ah, 08h
    jne .i2fNotUs
    test al, al ;AL=00, Install check
    jz .i2fCheck
    cmp al, 01  ;AL=01, Add block device
    je .i2fAddTbl
    cmp al, 02  ;AL=02, Execute blk drv request
    je .i2fExec
    cmp al, 03  ;AL=03, Get tbl ptr
    je .i2fGivTbl
.i2fNotUs:
    jmp qword [i2FNext]
.i2fCheck:
    mov al, -1  ;Indicate installed!
    iretq
.i2fAddTbl:
;Adds a new entry to the drive chain and updates the multiownership bits
; as it does :)
;Input: rdi -> New drvBlk to link to table (can be multiple!)
;Destroy rax, rbx and rsi
    lea rsi, .drvBlkTbl
    movzx eax, byte [rdi + drvBlk.bBIOSNum]
.i2fATLp:
    cmp byte [rsi + drvBlk.bBIOSNum], al
    jne .i2fATNext
;Set that the two disks are multi owned. New cannot own the drive
; and make sure that the new drive has the accurate changeline bit set
    or word [rsi + drvBlk.wDevFlgs], devMulti       ;Both drives now multi!
    or word [rdi + drvBlk.wDevFlgs], devMulti
    and word [rdi + drvBlk.wDevFlgs], ~devOwnDrv    ;New cant own drv
;Changeline check.
    and word [rdi + drvBlk.wDevFlgs], ~devChgLine   ;Assume no changeline
    test word [rsi + drvBlk.wDevFlgs], devChgLine   ;Do we really have cline?
    jz .i2fATNext   ;Skip adding the bit if not
    or word [rdi + drvBlk.wDevFlgs], devChgLine     ;Add if we do 
.i2fATNext:
    cmp qword [rsi + drvBlk.pLink], -1  ;Check if we @ end of table
    cmovne rsi, qword [rsi + drvBlk.pLink] ;Walk if not
    jne .i2fATLp    ;And go again if not
    mov qword [rsi + drvBlk.pLink], rdi ;Else, link rdi onto the end
    mov qword [rdi + drvBlk.pLink], -1  ;And terminate list @ rdi now :)
    iretq
.i2fExec:
;We make a small change in that we clean up the flags from the stack
; as opposed to DOS which leaves them on the stack. Doing so is fine 
; as no useful information is ever passed in the flags from a driver
; so by doing so, any ported applications which do an additional pop
; from the stack to balance the stack will not be harmed by this.
    push rax
    mov eax, 8002h  ;Enter Driver critical section
    int 2Ah

    push rbx
    mov qword [reqPktPtr], rbx  ;Save the ptr in var since we own it now :)
    call msdDriver  ;And call the driver like from within DOS!
    pop rbx

    mov eax, 8102h  ;Exit Driver critical section
    int 2Ah
    pop rax
    iretq
.i2fGivTbl:
;Output: rdi -> drvBlkTbl
    lea rdi, .drvBlkTbl
    iretq

.strikeMsg db 0Dh,0Ah,"Insert for drive "
.strikeMsgLetter db "A: and strike",0Dh,0Ah,"any key when ready",0Dh,0Ah,0Ah
.strikeMsgL equ $ - .strikeMsg

.fat12Str   db "FAT12   ",0
.fat16Str   db "FAT16   ",0
.fat32Str   db "FAT32   ",0
.defLbl     db "NO NAME ",0 ;Default volume label

maxAcc  equ 5       ;Maximum accesses
.bAccCnt    db 0    ;Counter of 0 time difference media checks
.bLastDsk   db -1   ;Last disk to be checked for media check.
.pCurDrv    dq 0    ;Pointer to the drvBlk for the drv we are accessing

drvBlkTblL equ 26   ;Space for 26 drive letters!
.drvBlkTbl:
    db drvBlkTblL*drvBlk_size dup (0)
;    %rep drvBlkTblL
;        defaultDrv
;    %endrep