; This file contains five main routines:
; 1) Replacement Int 39h routine (for unhooking interrupts back).
; 2) Replacement Int 33h routine.
; 3) Int 33h replacement routine.
; 4) Driver itself
; 5) Int 2Fh Driver backdoor routine

i39Org  dq 0    ;Original BIOS Int 39h to replace on Int 39h
i33Org  dq 0    ;Original BIOS Int 33h to replace on Int 39h.
i2FNext dq 0    ;Previous Int 2Fh handler
i33Next dq 0    ;Current disk driver to call.


;DEBUG COMMON ROUTINES!
drvDbg equ 0
i33Dbg equ 0
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
dbgPrintString:
;Pass in rsi the string we wanna print and its length in ecx.
;Preserves all registers
    pushfq
    push rax
.lp:
    lodsb
    test al, al
    jz .exit
    call dbgPutch
    jmp short .lp
.exit:
    pop rax
    popfq
    return

dbgCrlf:
;Prints a CRLF
    push rax
    mov al, 0Ah 
    call dbgPutch
    mov al, 0Dh
    call dbgPutch
    pop rax
    return

dbgPutch:
;Input: al = ASCII char to print
    push rdx
    mov dx, 0E9h
    out dx, al
    pop rdx
    return

dbgPrintHexByte:
;Print the hex byte in al as a hex value
    pushfq
    push rdx
    push rbx
    push rax

    mov dl, al           ;save byte in dl
    and ax, 00F0h        ;Hi nybble
    and dx, 000Fh        ;Lo nybble
    shr ax, 4            ;shift one hex place value pos right
    call .wrchar
    mov ax, dx           ;mov lo nybble, to print
    call .wrchar

    pop rax
    pop rbx
    pop rdx
    popfq
    return
.wrchar:
    push rdx
    lea rbx, .debascii
    xlatb    ;point al to entry in ascii table, using al as offset into table
    mov dx, 0E9h
    out dx, al
    pop rdx
    return
.debascii   db "0123456789ABCDEF"
%endif

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
;--------------------------------------------------------------------------
;       This is a wrapper around the BIOS Int 33h handler.
;--------------------------------------------------------------------------
;This routine does the following:
;1) Checks if a format request is being made, in which case
;   it will ensure that the devSetDASD and devChgd bits are set for all 
;   drives for the BIOS drive in dl to ensure that the DOS driver treats
;   the DOS drives properly.
;
; For the most part (CHS is depreciated so we don't talk about it here):
;   AH = Function number (05h/85h is FORMAT)
;   AL = Number of sectors to xact.
;   RBX -> Points to a xfr buffer.
;   RCX = Start sector of xft.
;   DL = BIOS Drive number to xact on.
;--------------------------------------------------------------------------
;Enter the DOS driver critical section . When the MSD driver enters this it 
; already has the lock so this simply incs the count. If a process attempts 
; to bypass DOS and we are already processing a request it gets put on ice.
;--------------------------------------------------------------------------
%if drvDbg and i33Dbg
    call .dbgFun
%endif
;Start by clearing the CF on entry
    and byte [rsp + 2*8], ~1
;Enter the device critical section
    push rax
    mov eax, 8002h
    int 2ah
    pop rax
;Now put the retaddr in the var we own 
    pop qword [.tmp]
;Now check that we are not formatting. If we are, we need to set the bit on
; all DOS drives that use this BIOS drive that it has been formatted and 
; changed.
    push rax    ;Push the function number on stack
    and ah, 7Fh ;Clear the top bit (as both 05h and 85h are formats)
    cmp ah, 05h
    je .format
    cmp ah, 07h ;Undocumented SCSI format?
    jne .notFormat
.format:
;Here we register the format request!
    mov eax, devChgd | devSetDASD   ;Bits to set in flags
    call msdDriver.setBitsForAllDevs
.notFormat:
    pop rax     ;Get the function number from stack
;Call previous handler and exit irq in this call.
    mov byte [.drv], dl ;Save the drive we are acting on
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rax
    push rsi
    lea rsi, .i33dbgStr1
    call dbgPrintString
    mov al, ah
    call dbgPrintHexByte
    lea rsi, .i33dbgStr11
    call dbgPrintString
    mov al, dl
    call dbgPrintHexByte
    call dbgCrlf
    pop rsi
    pop rax
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    call qword [i33Next]    
    jnc .exitI33
;We enter here when an error occurs!
    test ah, ah ;Error 0? Magical error needs to be cleaned up
    jz .exitI33
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .i33dbgStr
    call dbgPrintString
    pop rsi
    push rax
    mov al, ah
    call dbgPrintHexByte
    pop rax
    call dbgCrlf
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmp ah, 06h         ;Did a swap occur?
    jne .exitI33Bad     ;All other errors get bubbled up
;Here we ensure that on all drives with this BIOS number, we 
; register that the swap occured :)
    push rax
    push rdx
    movzx edx, byte [.drv]
    mov eax, devChgd    ;Set the device changed bit on all devices
    call msdDriver.setBitsForAllDevs
    pop rdx
    pop rax
.exitI33Bad:
    stc
.exitI33:
;Replace the retaddr back on the stack
    push qword [.tmp]
;Exit the device critical section now
    push rax
    mov eax, 8102h
    int 2ah
    pop rax
;And finally go back to the caller :)
    return
%if drvDbg and i33Dbg
.dbgFun:
    cmp byte [0700h], -1
    retne
    cmp byte [.drv], 0
    retne
    pop rax
    inc byte [.dbgCnt]
    mov ah, 06h
    cmp byte [.dbgCnt], 1
    je .dbgIret
    mov ah, 80h
    or byte [rsp + 2*8h], 1 ;Set CF
.dbgIret:
    iretq
.dbgCnt db 0
%endif
;Local data for the main IRQ handler
.drv    db 0    ;Drive we are acting on
.tmp    dq 0
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
.i33dbgStr1     db "[BIOS] Entering BIOS function ",0
.i33dbgStr11    db "h on drive ",0
.i33dbgStr db "[BIOS] Int 33h Error detected: ",0
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    dw 0                         ;Function 3
    dw 0
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
    dw 0                         ;Function 12
    dw 0
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
;Jumped to with ah = BIOS error code as reported on function call
    lea rdi, .biosErrTbl
    mov al, ah
    mov ecx, errTblLen
    repne scasb
    jne .exNotFnd
    mov al, byte [rdi + errTblLen - 1] ;Get entry in DOS table now
    jmp short .errorExit
.exNotFnd:
;Come here if the BIOS supplied code was not mapped to anything.
; We now get the SCSI code. Only a few cases make sense so
; we try and decypher. Else, general fault.
    mov eax, 0100h
    call .callI33h
;Device Not Ready
    mov eax, drvNotReady  ;Device not ready code
    cmp r8b, al  ;SCSI Not ready commands start with 2
    je .errorExit
    shr r8, 8       ;Remove Sense Key
    movzx ecx, r8w  ;Get ASC and ASCQ in cl and ch bzw.
;Write Protected
    mov eax, drvWPErr
    cmp cx, 0027h   ;Write protected error
    je .errorExit
;CRC Error
    mov eax, drvCRCErr     ;CRC error code
    cmp cx, 0308h   ;LU comms CRC error (UDMA/32)
    je .errorExit
    cmp cx, 0010h   ;ID CRC or ECC error
    je .errorExit
    cmp cx, 0147h   ;Data phase CRC error detected
    je .errorExit
;Seek Error
    mov eax, drvBadSeek 
    cmp cl, 02h     ;No Seek Complete
    je .errorExit
;Unknown Hardware Media (Shouldn't happen with Flash Drives)
;This error should only be called if BPB not recognised for Flash Drives
    mov eax, drvBadMed
    cmp cl, 30h   ;All issues with media returns unknown media
    je .errorExit
;Sector Not Found
    mov eax, drvSecNotFnd     ;Sector not found code
    cmp cl, 21h     ;Illegal Request - Invalid LBA
    je .errorExit
;Write faults
    mov eax, drvWriteFault     ;Write fault
    cmp cl, 0Ch     ;Write Error ASC code
    je .errorExit
;Read faults
    mov eax, drvReadFault     ;Read fault
    cmp cl, 11h     ;Read error
    je .errorExit
;General Errors
.genErrExit:
    mov eax, drvGenFault     ;Everything else is general error
.errorExit:     ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov rbx, qword [reqPktPtr]
    mov word [rbx + drvReqPkt.status], ax
    return      ;Return to set done bit
;The xlat table is used for simple error codes.
;The more complex stuff requires a further callout to int 33h for the SCSI
; error code.
.biosErrTbl:
    db 04h  ;Sector not found
    db 06h  ;Media changed or removed
    db 10h  ;ECC/CRC error
    db 40h  ;Seek error
    db 80h  ;Timeout error
errTblLen equ $ - .biosErrTbl
.dosErrTbl:
    db drvSecNotFnd
    db drvBadDskChnge
    db drvCRCErr
    db drvBadSeek
    db drvNotReady

;All functions have the request packet ptr in rbx and the bpb pointer in rbp
.medChk:          ;Function 1
;Start by placing the label pointer in the slot in the event of a change!
    lea rdi, qword [rbp + drvBlk.volLab]
    mov qword [rbx + mediaCheckReqPkt.desptr], rdi
;Did this drive have its parameters swapped since the last time?
    test word [rbp + drvBlk.wDevFlgs], devNewParms
    jz .mcNoFormat
;Reset and apply the different logic now :)
    and word [rbp + drvBlk.wDevFlgs], ~devNewParms   ;Clear this bit
    mov byte [.bLastDsk], -1    ;Formatted so cannot rely on timer logic
    call .checkDevFixed ;If fixed, declare changed!
    jnz .mmcChange
;For remdevs we now determine if the media was changed. If so, exit!
    jmp short .mcRem
.mcNoFormat:
    call .checkDevFixed
    jnz .mmcNoChange
.mcRem:
    call .checkDevType
    test word [rbp + drvBlk.wDevFlgs], devChgLine
    jz .mmcNoChangeLine
    mov dl, byte [rbp + drvBlk.bBIOSNum]
;Now we do a BIOS changeline check. 
    call .checkMediaChange  ;If we know w
    jnz .mmcVolCheck
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .mcDbgMsg
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mov eax, 1600h 
    call .callI33h
    jnc .mmcNoChangeFnd
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .mcDbgMsg1
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.mmcVolCheck:
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .mcDbgMsg3
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;Here if are checking the volume for its ID.
    call .updateBpb ;If this fails, change!
    jc .mmcChange
    call .checkVolumeSame
    test eax, eax
    jz .mmcUnsure
    js .mmcChange   ;If the sign bit is set, eax = -1. Disk changed!
    jmp short .mmcNoChange
.mmcNoChangeFnd:
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .mcDbgMsg2
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;If the changeline says no change, check that we were the last disk to access
; If we were not, we do a time check do time/last access check. If more than
; two seconds, have passed, we do a volume check. Else, since the changeline
; said no change, we assume that no change!
    mov al, byte [.bLastDsk]
    cmp byte [rbp + drvBlk.bDOSNum], al
    je .mmcNoChange
    call .checkTime ;Returns CF=CY if unsure. If unsure, do BPB check
    jc .mmcVolCheck
    jmp short .mmcNoChange    ;Else, we say no change!
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
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    call .mmcDbgNoCh
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mov eax, 1
    jmp short .mmcExit
.mmcUnsure:
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    call .mmcDbgUnk
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xor eax, eax
    jmp short .mmcExit
.mmcChange:
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    call .mmcDbgChange
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mov eax, -1
    mov byte [.bLastDsk], al    ;Default to unknown disk if a change occured!
.mmcExit:
    mov rbx, qword [reqPktPtr]
    mov byte [rbx + mediaCheckReqPkt.medret], al
    return
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
.mmcDbgNoCh:
    lea rsi, .mmcNoStrD
    jmp short .mmcDbgCmn
.mmcNoStrD  db "_ not changed",0Ah,0Dh,0

.mmcDbgUnk:
    lea rsi, .mmcUnkStrD
    jmp short .mmcDbgCmn
.mmcUnkStrD db "_ unknown",0Ah,0Dh,0

.mmcDbgChange:
    lea rsi, .mmcChStrD
    jmp short .mmcDbgCmn
.mmcChStrD  db "_ changed",0Ah,0Dh,0

.mmcDbgCmn:
    push rsi
    lea rsi, .mmcMedChgStr
    call dbgPrintString
    pop rsi
    mov al, byte [rbp + drvBlk.bDOSNum]
    add al, "A"
    mov byte [rsi], al
    call dbgPrintString
    return
.mmcMedChgStr   db "[MEDCHECK] ",0
.mcDbgMsg db "[MEDCHECK] Doing BIOS medcheck",0Ah,0Dh,0
.mcDbgMsg1 db "[MEDCHECK] Reported change",0Ah,0Dh,0
.mcDbgMsg2 db "[MEDCHECK] No Change Reported",0Ah,0Dh,0
.mcDbgMsg3 db "[MEDCHECK] Doing volume check",0Ah,0Dh,0
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.buildBPB:        ;Function 2
;Only build BPB for removable devices and "non-locked" devices.
;Start by setting the pointer to the BPB in the reqpkt as this is 
; the table entry bpb which we will be returning.
    call .checkDevFixed
    jnz .bbpbExit
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .bbpbMsg
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;------------------------------------------------------
; Here for removable devices only!!
;------------------------------------------------------
    call .resetIds  ;Reset the drvBlk volume ids
    call .updateBpb ;Fill the BPB entries in the drvBlk
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    push rdi
    lea rsi, .bbpbMsg1
    lea rdi, .bbpbMsg11
    cmovc rsi, rdi
    call dbgPrintString
    pop rdi
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    jc .ioDoErr     ;Errors returned as if from block IO handler
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
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
.bbpbMsg1 db "[BUILDBPB] BPB Build Ok",0Ah,0Dh,0
.bbpbMsg11 db "[BUILDBPB] BPB Build Failed",0Ah,0Dh,0
.bbpbMsg db "[BUILDBPB] Building BPB",0Ah,0Dh,0
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
;   rbp -> drvBlk for this drive
;------------------------------------------------------
;Exited with:
;   If CF=NC:
;       rbx -> The start of the BPB
;       rsi -> End of the BPB. Points to the extSig
;               if present.
;   Else:
;       If ZF=NZ: 
;           eax = DOS error code (Invalid BPB detected)
;       Else:
;           eax = BIOS error code
;------------------------------------------------------
    test word [rbp + drvBlk.wDevFlgs], devFixed | devLockBpb
    retnz  
    call .bbpbReadBS    ;Sets up rbx to point to internal disk buffer
    retc    ;If an error occured, return ZF=ZE
;Check we if we have a valid bootsector.
    cmp byte [rbx + oemHeader.jump], 069h       ;Direct jump has no NOP
    je .ubpbCheckMedOk
    cmp byte [rbx + oemHeader.jump], 0E9h       ;Near jump has no NOP
    je .ubpbCheckMedOk
    cmp byte [rbx + oemHeader.jump], 0EBh       ;Short jump has a NOP
    jne .ubpbOldDisk
    cmp byte [rbx + oemHeader.jump + 2], 090h   ;NOP should be here
    jne .ubpbOldDisk
.ubpbCheckMedOk:
    mov al, byte [rbx + oemHeader_size + bpb.media] ;Get medbyte from BPB
    call .checkMedByt   ;Check if it is 0F0h or geq 0F8h
    jnz .ubpbOldDisk    ;If it is not, don't trust the BPB. Read the FAT sector
;Now we do the pre DOS 3.2 single sided check and kludge.
    test al, 1  ;Double sided bit set on media byte?
    jnz .ubpbNormalDisk ;If set, proceed as normal.
;Here if we have a "single-sided" formatted media.
;Check the OEM string. We filter out SCPDOS disks first.
    cmp dword [rbx + oemHeader.oemString], "SCPD"
    jne .ubpbNotSCP
    cmp word [rbx + oemHeader.oemString + 4], "OS"
    je .ubpbNormalDisk
.ubpbNotSCP:
;Now blind check version numbers for 16-bit DOS OEM strings.
;Search for DOS 2.0, 3.1 and 3.2. These versions have ID's such that 
; the version number starts at position 5.
;Usually, these BPBs have MSDOS or IBM<SPC><SPC> or OEM<SPC><SPC>
; as the OEM string. OEM stands for any three characters used by an
; OEM to identify disks they formatted, follows by "x.y" for a major
; and minor version number.
;Since we cannot enumerate all the OEM strings and can't guarantee
; that they have the bug, we will simply check the version number @
; position 5 in the string which seems to have been an unofficial 
; standard for placing the version number in the OEM string at the 
; time. 
;The dot in the check helps "guarantee" the number is a version number 
; we are possibly interested in.
    cmp word [rbx + oemHeader.oemString + 5], "3."  ;DOS 3?
    jb .ubpbAdjustSPC  ;If below, must be "2." or "1." which has bug. Adjust.
    cmp byte [rbx + oemHeader.oemString + 7], "2"   ;DOS 3.2?
    jae .ubpbNormalDisk ;If above or equal 3.2, no need for adjustment.
.ubpbAdjustSPC:
;Here we must be pre 16-bit DOS 3.2. Adjust the BPB in memory to have 
; a spc value of 1.
    mov byte [rbx + oemHeader_size + bpb.secPerClus], 1
    jmp .ubpbNormalDisk ;Now proceed as a normal disk
.checkMedByt:
;Checks the media byte is of a valid type. Refuse media bytes we don't
; recognise as this is a sign of an unhealthy volume.
;Accept values 0FFh - 0F8h and 0F0h.
;Input: al = Media byte. 
;Ouput: ZF=NZ: Bad media byte. ZF=ZE: Ok media byte!
    cmp al, 0F0h
    rete
    cmp al, 0F8h
    retb
    cmp al, al  ;Set ZF if greater than F8h
    return
.ubpbOldDisk:
;We accept media bytes 0F9h-0FFh now. 0F8h and 0F0h make no sense here.
    call .bbpbReadFAT   ;Read the FAT sector now instead
    retc    ;If an error occured, return ZF=ZE
    movzx eax, word [rbx]   ;Clear upper bytes
    and eax, 0FFFh
    cmp eax, 0FF9h    ;Cannot accept less than 0F9h as we dont know how to handle
    jb .ubpbErr
    jne .ubpbOldest ;If not equal to 0F9h, it must be a "normal" disk.
;The only way to check which 0F9h we have, is to read the FAT and try to 
; find the second FAT, as they are at different "known" sectors. 
;First we read sector 4. If we find the 12 bytes 0FFF9h then use the first entry
; in the 0F9h table. 
;Else, read sector 8. If we find the 12 bytes 0FFF9h there, then use the 
; second entry in the 0F9h table.
    lea rbx, .drvBpbTblF9
    call .upbpFindF9
    retc                ;If the sector read failed, exit with ZF=ZE!
    je .ubpbMoveBpb     ;If second FAT found, proceed with this bpb!
    add rbx, bpb_size   ;Else, goto next entry
    call .upbpFindF9    ;And try with this BPB
    retc                ;If the sector read failed, exit with ZF=ZE!
    je .ubpbMoveBpb     ;If second FAT found, proceed with this bpb!
;Else fall through as we don't know what 0F9h means here.
.ubpbErr:               
;Bad media bytes or BPB go here. Means the media is unknown.
    mov al, drvBadMed       ;Default to unknown media error code (07h)
    test eax, eax           ;Set ZF=NZ so we indicate a DOS error code!
    stc                     ;And set CF=CY to always return error!
    return
.upbpFindF9:
;Reads the supposed start of the second FAT sector to search for a FF9h 
;Call with rbx -> Table entry for this drive
;Returns:   CF=NC and ZF=ZE: Second boot sector found. Use the bpb in rbx.
;           CF=NC and ZF=NZ: Second boot sector not found. Goto next entry.
;           CF=CY: Disk read failed.
    movzx ecx, word [rbx + bpb.FATsz16] ;Get number of fat sectors in a FAT
    inc ecx     ;Add one for the reserved sector to get sector !
    push rbx
    call .bbpbReadEp
    movzx eax, word [rbx]   ;Read the first word
    pop rbx
    retc            ;If an error occured, return ZF=ZE
    and eax, 0FFFh  ;Scan off the upper nybble to get low meaningful 12 bytes
    cmp eax, 0FF9h  ;Is this FF9h as it should be?
    clc             ;Since eax can be a random word, force clear CF here.
    return
.ubpbOldest:
;Here we build a pretend BPB in the sector buffer pointed to by rbx.
;This will then be used to build the internal data structure. 
    lea rbx, .drvBpbTbl
    sub al, 0FAh    ;Get the offset into the bpb table
    mov ecx, bpb_size
    mul ecx         ;Get byte offset into the bpb table
    add rbx, rax    ;Point rbx to this bpb in the table
    jmp short .ubpbMoveBpb
.ubpbNormalDisk:
    add rbx, oemHeader_size ;Now point rbx to the BPB itself
;Update the drvBlk with info from the BPB.
;rbx points to the disk BPB. May be bad so we need to ensure the values 
; are ok before updating the msdTbl entry. 
.ubpbMoveBpb:
    mov rsi, rbx    ;Source from the BPB in disk buffer
    lea rdi, qword [rbp + drvBlk.bpb]
    call .getFATType    ;Fat type is given in edx
    jc .ubpbErr ;Only happens if crucial BPB fields are zero 
    mov byte [rbp + drvBlk.bBpbType], dl    ;Save the FAT type
;Get the correct length to correctly position rsi over the extended bs struct
; if it is present
    mov eax, bpb_size
    mov ecx, bpb32_size - 12    ;BPB32 minus reserved count
    cmp dl, bpbFat32
    cmovne ecx, eax     ;If not FAT32, replace move count
    rep movsb        ;Now copy the BPB over!
    clc     ;Ensure if we return here, we return with CF happy :)
    return

.moveVolIds:
;Now check the BPB for a extBs. If it is present, we copy the information.
;Input: rsi -> End of the BPB in sector. rbp -> drvBlk
;Output: CF=CY: No volume label in sector found.
;        CF=NC: Volume Label in sector found and copied.
    call .clearMediaChange   ;Start by clearing the changed bit if it was set
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
;Clear the change bit now
    clc
    return
.mviNoSig:
    stc
    return

.getFATType:
;Computes FAT type. Returns bpb flag in edx. rbx -> BPB itself
    movzx ecx, word [rbx + bpb.bytsPerSec]
    jrcxz .bbpbGFTErr
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
    test eax, eax   ;If either BPB_NumFATs or FATSz is 0, fail!
    pop rcx         ;Get RootDirSectors into ecx
    jz .bbpbGFTErr
    movzx edx, word [rbx + bpb.revdSecCnt]
    add ecx, eax    ;ecx = (BPB_NumFATs * FATSz) + RootDirSectors
    add ecx, edx    ;ecx = (BPB_ResvdSecCnt + ecx)
    movzx eax, word [rbx + bpb.totSec16]
    mov edx, dword [rbx + bpb32.totSec32]
    test eax, eax
    cmovz eax, edx  ;eax = Totsec
    sub eax, ecx    ;Datasec [eax] = eax - ecx
    movzx ecx, byte [rbx + bpb.secPerClus]
    jrcxz .bbpbGFTErr
    xor edx, edx
    div ecx         ;eax = CountofClusters = DataSec / BPB_SecPerClus;
    mov edx, bpbFat12
    cmp eax, fat12MaxClustCnt
    jb .bbpbGFTExit
    shl edx, 1  ;Move bit into FAT32 position
    cmp eax, fat16MaxClustCnt
    jnb .bbpbGFTExit   ;If above or equal, its in FAT32
    shl edx, 1  ;Else move into FAT16 position
.bbpbGFTExit:
    clc
    return
.bbpbGFTErr:
    stc
    return

.bbpbReadFAT:
;Reads the first FAT sector of media we are playing with.
    xor ecx, ecx
    inc ecx         ;Read Sector 1...
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .bbpbDbgReadFatStr
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    jmp short .bbpbReadEp
.bbpbReadBS:
;Reads the bootsector of media we are playing with.
    xor ecx, ecx    ;Read Sector 0...
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .bbpbDbgReadBSStr
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.bbpbReadEp:
    add ecx, dword [rbp + drvBlk.dHiddSec]
    movzx edx, byte [rbp + drvBlk.bBIOSNum]
    lea rbx, .inBuffer  ;Use the in sector buffer. Ensure ownership.
    mov esi, 5
.bbpbReadLp:
    mov eax, 8201h  ;LBA Read function (read 1 sector)
    call .callI33h
%if drvDbg
    jnc .bbpbDbgReadExit
%else
    jnc .bioExit
%endif
;Here if an error. AH has the BIOS error code. Return with
; ZF=ZE to indicate we are returning a BIOS code!
    call .bioReset  ;Reset the drive. WARNING: CRASHES BOCHS
    dec esi
    jnz .bbpbReadLp
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .bbpbDbgReadBadStr
    call dbgPrintString
    pop rsi
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    call .bioExit   ;Setup the time and the drive info for the access
    stc             ;Now set up the carry flag!
    return
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
.bbpbDbgReadExit:
    push rsi
    lea rsi, .bbpbDbgReadOkStr
    call dbgPrintString
    pop rsi
    jmp .bioExit

.bbpbDbgReadFatStr db "[DRIVER] Reading FAT Sector",0Ah,0Dh,0
.bbpbDbgReadBSStr db "[DRIVER] Reading Boot Sector",0Ah,0Dh,0
.bbpbDbgReadOkStr db "[DRIVER] Read OK",0Ah,0Dh,0
.bbpbDbgReadBadStr db "[DRIVER] Read Bad",0Ah,0Dh,0
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.read:              ;Function 4
;Will read one sector at a time.
    call .ioSetVolLbl
    call .checkDevType
    call .checkSwap 
    jc .ioDoErr
    mov rdi, rbx    ;Move ioreqpktptr to rdi
    call .bioSetupRegs
    retz
.msdr0:
    mov eax, 8200h  ;LBA Read function
    call .blkIO
    jc .ioDoErr
    call .ioAdv
    jne .msdr0
    return

.write:             ;Function 8/9
;Will write and optionally verify one sector at a time.
    call .ioSetVolLbl
    call .checkDevType
    call .checkSwap 
    jc .ioDoErr
    mov rdi, rbx    ;Move ioreqpktptr to rdi
    call .bioSetupRegs
    retz
.msdw0:
    mov eax, 8300h ;LBA Write Sectors
    call .blkIO
    jc .ioDoErr
    cmp byte [rdi + ioReqPkt.cmdcde], drvWRITEVERIFY
    jne .msdw1
    mov eax, 8400h ;LBA Verify Sectors
    call .blkIO
    jc .ioDoErr
.msdw1:
    call .ioAdv
    jne .msdw0
    return
.ioDoErr:
;Come here if after an error in block IO handler.
;If ZF=ZE, disk error occured, the error needs translation so do it.
;Else just return the error code in al
    jz .errorXlat
    jmp .errorExit

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
;       rbp -> drvBlk
;       esi = Sectors left to xfr.
;       ZF=ZE if esi is 0. Else ZF=NZ.
    push rax
    inc rcx     ;Goto next sector
    movzx eax, word [rbp + drvBlk.wBpS] 
    add rbx, rax  ;Advance the buffer pointer by 1 sector
    dec esi     ;Once this hits 0, we stop the xfr
    pop rax
    return

.bioSetupRegs:
;Sets up sector to read and buffer ptr for block IO call.
;If returns ZF=ZE then xfr 0 sectors, exit immediately
;Input: rdi -> ioReqPkt
;       rbp -> drvBlk
;Output: rdi -> ioReqPkt
;        rbp -> drvBlk
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

.blkIODirect:    ;Does block IO without sanity checking the sector number
;All registers as below!
    test word [rbp + drvBlk.wDevFlgs], devUnFmt
    jnz .bioUfmted
    jmp short .biocmn
.blkIO:  ;Does block IO for one sector
;Sector count handled by caller.
;All registers marked as input registers must be preserved across the call
; except ah
;Input: ah = BIOS function number
;       rdi -> ioReqPkt             (Normal Read/Write only)
;       rbp -> drvBlk
;       rbx -> Transfer buffer
;       rcx = LBA sector to transfer
;       dl  = BIOS drive number
;       esi = Sectors left to xfr!  (Normal Read/Write only)
;Output: CF=NC: esi number of sectors xferred.
;        CF=CY: An error ocured. 
;           ZF=NZ: Non-disk error, return the DOS error code in eax
;           ZF=ZE: Disk error, xlat BIOS error code in ah
    test word [rbp + drvBlk.wDevFlgs], devUnFmt
    jnz .bioUfmted
    call .bioSanity ;Sanity check ecx here
.biocmn:
    push rsi    ;Save sector count
    mov esi, 5  ;Retry counter five times
.biolp:
    mov al, 01h ;Do one sector 
    call .callI33h  ;Preserves all passed regs except eax
    jc .bioError
    cmp al, 1   ;Did we do one sector?
    jne .bioNoIO    ;No, try again without calling BIOS error handling
    pop rsi ;Rebalance stack
.bioExit:
    ;mov dl, byte [rbp + drvBlk.bDOSNum]
    mov byte [.bLastDsk], dl    ;Last DOS disk accessed
    call .checkDevFixed
    retnz
;Below routine saves the registers it trashes
    call .setTime   ;Set the current time and clear state for successful IO
    return
.bioNoIO:
    dec esi ;Decrement the retry counter
    jnz .biolp  ;and try again if we still have retries to do
    pop rsi ;Now rebalance the stack
;Here we return as if our request was successful but we set esi to zero
; to stop IO processing as the operation isn't reading/writing the sector
; for no erroring reason (should never actually happen as the sanity check 
; should handle this case but _just in case_ ).
    xor esi, esi
    return
.bioError:
;Jumped to with ah = BIOS error code
    call .checkSwapIO
    call .bioReset  ;Reset the drive. WARNING: CRASHES BOCHS. Does E9h check
    dec esi
    jnz .biolp
    pop rsi ;Pop the sector count off the stack
    ;ZF=ZE set now as esi counted down
    stc
    return
.bioReset:
;Resets the drive system for the drive in dl
    push rax
    in al, 0E9h
    cmp al, 0E9h
    je .bioResetSkip
    xor eax, eax    ;Do reset
    call .callI33h  ;Ignore any errors
.bioResetSkip:
    mov byte [.bLastDsk], -1    ;Reset the last disk accessed
    pop rax
    return
.bioSanity:
;Input: ecx = Sector we will transact on. rbp -> DrvBlk
;Output: CF=NC, sector ok to xact on
;        CF=CY, doesnt return, fails the call. ZF=ZE always (have DOS code)
;        If the sector fails check, we return from the top level with ZF=ZE
;           to indicate a DOS error code in eax.
    test eax, 0FFh  ;If the bottom byte is set, it is a IOCTL call.
    retnz           ;BIOS checks these for us as IOCTL bypasses partitions.
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
;Now pops the return from the sanity call and falls.
    pop rax ;Return from block IO with error code in eax below
    mov eax, drvSecNotFnd
.bioNoDiskErr:
    test eax, eax   ;Clear ZF
    stc
    return
.bioUfmted:
;Returns the DOS error code bad media to caller. 
;Returns CF=CY and ZF=ZE (with eax = DOS error code)
    mov eax, drvBadMed
    jmp short .bioNoDiskErr

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
    call .checkDevFixed ;Is it fixed?
    retz
    mov word [rbx + remMediaReqPkt.status], drvBsyStatus
    return

.IOCTL:    ;Function 19
;Implements LBA versions of the CHS functions by setting the high bit
; in the minor code.
    mov eax, drvBadCmd
    movzx ecx, word [rbx + ioctlReqPkt.majfun]  ;Get CH and CL in one read
    cmp ch, 08h     ;Disk Drive Major Code?
    jne .errorExit  ;If not, exit bad
;Disk Drive IOCTL here
    test cl, 18h    ;If either bits 3 or 4 set, fail the call
    jnz .errorExit
    test cl, 60h    ;One of these two bits MUST be set (bits 5 and 6)
    jz .errorExit
    movzx edx, cl
    and edx, ~0F8h   ;Clear bits 3-7 to get table offset
    lea rdi, .ioctlTbl
    push rdi
    lea rdi, qword [rdi + 4*rdx]
    test cl, 20h
    jz .ioctlNoRead
    add rdi, 2  ;If we are doing the read function, goto the next instruction
.ioctlNoRead:
    movzx edx, word [rdi]   ;Read the word offset
    pop rdi
    add rdi, rdx    ;Add the table base
    mov rdx, qword [rbx + ioctlReqPkt.ctlptr]
    test cl, 80h    ;Now set the flag for if CHS (ZF=ZE) or LBA (ZF=NZ)
    jmp rdi ;And enter the function with the ptr to the ctrl block in rdx
.ioctlTbl:
;Note: For the CHS IO functions, we translate the CHS requests into LBA
; requests and then recycle the LBA request code. We do this using the 
; track table. If the sector size of any table entry is neq the 
; device sector size, we fail the request. This is because these requests
; work on a track by track basis.
    dw .ioSetDevParams - .ioctlTbl  ;CL = 40h or CL = C0h, Write IOCTL
    dw .ioGetDevParams - .ioctlTbl  ;CL = 60h or CL = E0h, Read IOCTL
    dw .ioWrite - .ioctlTbl         ;CL = 41h or CL = C1h, Write IOCTL
    dw .ioRead - .ioctlTbl          ;CL = 61h or CL = E1h, Read IOCTL
    dw .ioFormat - .ioctlTbl        ;CL = 42h or CL = C2h, Write IOCTL
    dw .ioVerify - .ioctlTbl        ;CL = 62h or CL = E2h, Read IOCTL
    dw .iobadCmd - .ioctlTbl        ;CL = 43h or CL = C3h, error
    dw .iobadCmd - .ioctlTbl        ;CL = 63h or CL = E3h, error
    dw .iobadCmd - .ioctlTbl        ;CL = 44h or CL = C4h, error
    dw .iobadCmd - .ioctlTbl        ;CL = 64h or CL = E4h, error
    dw .iobadCmd - .ioctlTbl        ;CL = 45h or CL = C5h, error
    dw .iobadCmd - .ioctlTbl        ;CL = 65h or CL = E5h, error
    dw .ioSetIds - .ioctlTbl        ;CL = 46h or CL = C6h, Set vol Ids
    dw .ioGetIds - .ioctlTbl        ;CL = 66h or CL = E6h, Get vol Ids
    dw .ioSetAccessFlag - .ioctlTbl ;CL = 47h or CL = C7h, Write IOCTL
    dw .ioGetAccessFlag - .ioctlTbl ;CL = 67h or CL = E7h, Read IOCTL
.iobadCmd:
    mov eax, drvBadCmd
    jmp .errorExit
.iobadCmdLen:
    mov eax, drvBadDrvReq
    jmp .errorExit
.iobadMed:
    mov eax, drvBadMed
    jmp .errorExit
;---------------------------------------------------------------------------
;               Set Device parameters in CHS and LBA here
;---------------------------------------------------------------------------
.ioSetDevParams:
    pushfq
;If the parameters are swapped, set the flags and indicate the media 
; was swapped (even if not).
    or word [rbp + drvBlk.wDevFlgs], devNewParms | devChgd
    popfq
    jnz .lbaSetParams
;Here we set CHS params. 
;Before we trust the table, we check that indeed
; no sector index is past the max sector count and that if the caller
; claimed that all the sectors are of the same size, they really are.    
    lea rsi, qword [rdx + chsParamsBlock.TrackLayout]
    mov rdi, rsi    ;Save the pointer in rdi
    lodsw           ;Get the lead word and adv rsi by 2
    movzx ecx, ax   ;Get the lead table entry into ax
    cmp ecx, maxTrackTblSz
    jz .iosdpNoTrack   ;Don't touch the tracks bit in this case!
    ja .genErrExit
    cmp word [rsi], maxTrackTblSz   ;Check the first entry is ok!
    ja .genErrExit
.iosdpTblCheckLp:
    lodsd   ;Go to next entry
    dec ecx ;And remove from the count
    jz .iosdpTblCheckEnd
    cmp word [rsi], maxTrackTblSz   ;Ensure no entry above the max entry value
    ja .genErrExit
    test byte [rdx + chsParamsBlock.bSpecFuncs], specFuncSec ;All same size?
    jz .iosdpTblCheckLp ;If not, skip this check (always check 1=<i<=n)
    mov ax, word [rsi + 2]  ;Else get sector size
    cmp word [rsi - 2], ax  ;And compare with the previous sector size
    jne .genErrExit
    jmp short .iosdpTblCheckLp
.iosdpTblCheckEnd:
;Now we set the sector size bit if all tracks same size check was passed.
;If all sectors have the same size but the caller didn't specify this
; bit, we also don't specify this bit.
    and word [rbp + drvBlk.wDevFlgs], ~devSameSec    ;Clear bit first
    test byte [rdx + chsParamsBlock.bSpecFuncs], specFuncSec
    jz .iosdpNoSetTrackBit
;The only way we got here if the bit was set is that the check passed.
; Set the bit in the device block.
    or word [rbp + drvBlk.wDevFlgs], devSameSec
.iosdpNoSetTrackBit:
;Now we copy the table directly as sector numbers may be purposefully
; interleaved. NO SORTING!!
    mov rsi, rdi    ;Get back the track layout pointer 
    lea rdi, .ioTrackTbl    ;We overwrite our internal track table
    lodsw   ;Get the table length
    movzx ecx, ax   ;This many entries
    stosw
    rep movsd       ;Move the dword entries over
    test byte [rdx + chsParamsBlock.bSpecFuncs], specFuncTrk    ;Just tracks?
    retnz   ;Return if bit set!
.iosdpNoTrack:
;Now we update the rest of the disk metadata.
;Now copy the rest of the bytes and return
    movzx eax, word [rdx + chsParamsBlock.wDevFlgs]
    and eax, devFixed | devChgLine  ;Keep only these two bits
    and word [rbp + drvBlk.wDevFlgs], ~(devFixed | devChgLine)   ;Clear em
    or word [rbp + drvBlk.wDevFlgs], ax  ;Add those two bits as set
    movzx eax, byte [rdx + chsParamsBlock.bDevType]
    mov byte [rbp + drvBlk.bDevType], al
    or word [rbp + drvBlk.wDevFlgs], devSetDASD 
    movzx eax, word [rdx + chsParamsBlock.wNumCyl]
    mov word [rbp + drvBlk.wNumCyl], ax
;Finally, here we do the BPB magic.
;
;The logic here is that in a format operation, we first update the driver 
; devBlk bpb fields with the new format and set the devLockBpb flag. 
; This is so the bpb isn't updated from the disk when a disk operation is 
; enacted and that the parameters we passed in this call are what are
; reported to DOS whilst the bootsector hasn't been synchronised with 
; the disk.
;
;We then write the bootsector with the new bpb to disk, thus synchronising
; the new bpb parameter block with what is on disk.
;
;Then we call this again, this time, to clear the locking flag as the disk
; is now sychronised with the accurate bpb. We also here can get rid of 
; the old backup bpb and so the second call will replace the backup
; with whatever we pass it (ideally, the same bpb).
;
;This procedure protects us from accidentally making the drive unusable if
; we somehow fail to write the BPB by having updated the drvBlk. The 
; protection occurs by allowing us to restore the backup bpb as the devBlk 
; bpb if the disk synchronisation of the new parameters failed.
    test byte [rdx + chsParamsBlock.bSpecFuncs], specFuncBPB
    jnz .iosdpSetBPB    ;If bit set, lock the bpb
;Else we update the backup bpb and unlock the main bpb
    lea rdi, qword [rbp + drvBlk.sBkupBPB]
    mov ecx, bpb32_size
    and word [rbp + drvBlk.wDevFlgs], ~devLockBpb   ;Now allow update bpb 
    jmp short .iosdpCopy
.iosdpSetBPB:
    lea rdi, qword [rbp + drvBlk.bpb]   ;Default to the normal BPB in drvblk
    mov ecx, drvBlkBpb_size
    or word [rbp + drvBlk.wDevFlgs], devLockBpb ;Lock the BPB now
.iosdpCopy:
    lea rsi, qword [rdx + chsParamsBlock.deviceBPB]
    rep movsb
    return

.lbaSetParams:
;This only sets the sector size and number of sectors in drvBlk.bpb.
    ;Set start sector of partition
    mov ecx, dword [rdx + lbaParamsBlock.qStartSector]
    mov dword [rbp + drvBlk.dHiddSec], ecx
    ;Set sector size
    mov ecx, dword [rdx + lbaParamsBlock.qSectorSize]
    mov word [rbp + drvBlk.wBpS], cx
    ;Set same device flags as in CHS.
    movzx ecx, byte [rdx + lbaParamsBlock.wDevFlgs]
    and cx, devFixed | devChgLine
    or word [rbp + drvBlk.wDevFlgs], cx
    ;Set sector information.
    mov ecx, dword [rdx + lbaParamsBlock.qNumSectors]
    cmp ecx, 0FFFFh
    jna .lbaSetSmall
    mov dword [rbp + drvBlk.dTotSec32], ecx
    xor ecx, ecx
.lbaSetSmall:
    mov word [rbp + drvBlk.wTotSec16], cx
    return
;---------------------------------------------------------------------------
;               Get Device parameters in CHS and LBA here
;---------------------------------------------------------------------------
.ioGetDevParams:
    jnz .lbaGetParams
;Here we get CHS params. 
    lea rsi, qword [rbp + drvBlk.bpb]
    test byte [rdx + chsParamsBlock.bSpecFuncs], specFuncBPB
    jnz .iogdpBkup  ;If set, return the bpb data as is.
    call .updateBpb ;Else, gets the BPB from the disk.
    jc .ioDoErr ;Errors returned as if from block IO handler
    call .moveVolIds    ;Move the volume ID's into the drvBlk if they exist.
    lea rsi, qword [.inBuffer + 11]
.iogdpBkup:
;The caller block in memory must have a bpb32_size'ed space for the BPB
; even if it is a FAT16/12 drive. The caller has to assertain the 
; type of BPB it is based information in the common part of the BPB.
    lea rdi, qword [rdx + chsParamsBlock.deviceBPB]
    mov ecx, bpb32_size
    rep movsb
    mov eax, typeHard
    mov ecx, typeGenRem
    test byte [rbp + drvBlk.wDevFlgs], devFixed
    cmovz eax, ecx  ;eax is set to 7 if the dev is removable
    mov byte [rdx + chsParamsBlock.bDevType], al
    movzx eax, word [rbp + drvBlk.wDevFlgs]
    and eax, devFixed | devChgLine
    mov word [rdx + chsParamsBlock.wDevFlgs], ax
    mov byte [rdx + chsParamsBlock.bMedTyp], 0
    movzx eax, word [rbp + drvBlk.wNumCyl]
    mov word [rdx + chsParamsBlock.wNumCyl], ax
    return

.lbaGetParams:
;Gets more "updated" information on partitions.
;The data returned will always be the partition maximum! For remdevs
; this means up to the whole media size and for fixed disks we are still 
; restricted to the partition size.
    mov eax, drvBadDrvReq
    cmp byte [rdx + lbaParamsBlock.bSize], lbaParamsBlock_size
    jne .errorExit
    mov rdi, rdx    ;Store the params block ptr in rdi
    cmp byte [rdi + lbaParamsBlock.bSpecFuncs], 2   ;Check get phys call.
    ja .errorExit   ;If above 2, error with bad request!
    je .lgpbpbGetPhys
    test byte [rdi + lbaParamsBlock.bSpecFuncs], 1  ;Check if we update BPB.
    jnz .lgpbpbok
    push rdi    ;Push the param block onto the stack
    call .updateBpb
    jc .lgpbpbnotok ;Even if just bad BPB, keep changed bit on!
    call .moveVolIds    ;Move the volume ID's into the drvBlk if they exist.
    pop rdi     ;Pop param block
    jmp short .lgpbpbok
.lgpbpbnotok:
;If no valid BPB found, and the device removable, return BIOS params for the 
; whole device. We never come here for fixed disks as updateBpb always passes.
    pop rdi
    cmp al, drvBadMed   ;If remdev has bad media, get bios attribs.
    jne .errorExit
;We only fall here if on an unformatted removable disk.
;Unformatted means with an unrecognisable BPB.
.lgpbpbGetPhys:
    movzx edx, byte [rbp + drvBlk.bBIOSNum]
    mov eax, 8800h  ;Read LBA Device Parameters
    int 33h
    jc .errorXlat
    inc rcx         ;Turn into an absolute count of sectors
    xor edx, edx    ;0 Hidden sectors on remdevs/unformatted media
;eax = 0 since either not formatted or values may not be ok for CHS calls
    mov eax, edx     
    jmp short .lgpStor
.lgpbpbok:
    xor eax, eax
    mov ebx, eax
    inc ebx
;Here eax=0 and ebx=1.
    test byte [rbp + drvBlk.bNumFAT], -1    ;If 0 FATs, the FAT is invalid!
    cmovnz eax, ebx                         ;Set if we have a FAT
    mov edx, dword [rbp + drvBlk.dHiddSec]
    movzx ebx, word [rbp + drvBlk.wBpS]
    movzx ecx, word [rbp + drvBlk.wTotSec16]
    test ecx, ecx
    jnz .lgpStor
    mov ecx, dword [rbp + drvBlk.dTotSec32]
.lgpStor:
;Enter with:
;ax = 0 if unformatted, 1 if any FAT type (means can call CHS function)
;rbx = Sector size in bytes
;rcx = Last LBA block address + 1 (Count of sectors)
;rdx = Hidden sectors
    mov word [rdi + lbaParamsBlock.wFSType], ax
    mov qword [rdi + lbaParamsBlock.qSectorSize], rbx
    mov qword [rdi + lbaParamsBlock.qNumSectors], rcx
    mov qword [rdi + lbaParamsBlock.qStartSector], rdx
    movzx eax, word [rbp + drvBlk.wDevFlgs]
    and eax, devFixed | devChgLine
    mov word [rdi + lbaParamsBlock.wDevFlgs], ax
    return 
;---------------------------------------------------------------------------
;                    CHS IO requests are structured here
;---------------------------------------------------------------------------
.ioWrite:
    jnz .lbaWrite
;Here for CHS write tracks. 
    mov ebx, 8300h  ;Write sectors
.iochsRW:
    call .ioChsToLba
    call .ioChsSanity
    movzx eax, word [rdi + chsIOBlock.wStartSector]
    add ecx, eax    ;Add the zero based sector number to start of "track"
    movzx esi, word [rdi + chsIOBlock.wNumSectors]  ;How many sectors to IO on
.iochsCmn:
    mov eax, ebx    ;Move the function number to eax
    mov rbx, qword [rdi + chsIOBlock.pXferBuffer]   ;And get buffer ptr in rbx
    jmp .ioEp
.ioRead:
    jnz .lbaRead
;Here for CHS read tracks.
    mov ebx, 8200h  ;Read sectors
    jmp short .iochsRW
.ioFormat:
;DASD TEMP DASD TEMP DASD TEMP DASD TEMP DASD TEMP DASD TEMP DASD TEMP 
;
;We start by setting DASD parameters but for now we do nothing so just
; clear the flag.
    pushfq
    and word [rbp + drvBlk.wDevFlgs], ~devSetDASD
    popfq
;DASD TEMP DASD TEMP DASD TEMP DASD TEMP DASD TEMP DASD TEMP DASD TEMP 
    jnz .lbaFmt
;Here for CHS format track.
    mov ebx, 8500h  ;Format sectors
    test byte [rdx + chsFormatBlock.bSpecFuncs], 1  ;If this bit clear do format
    jz .iochsFmtCmn
;Else we should respond if the sector table is valid. Since our BIOS is crap
; we always return OK and let the format call fail. This would possibly be
; dangerous but its not since the BIOS CHS emulation is very meh.
    mov byte [rdx + chsFormatBlock.bSpecFuncs], 0   ;All ok!
    return
.iochsFmtCmn:
    call .ioChsToLba    ;Get the LBA of the first sector of the track in ecx
    movzx esi, word [rbp + drvBlk.wSecPerTrk]   ;Fmt/Verify this many sectors
    mov eax, ebx    ;Move the function number to eax
    jmp .ioEp
.ioVerify:
    jnz .lbaVerify
;Here for CHS verify track.
    mov ebx, 8400h
    jmp short .iochsFmtCmn

.ioChsSanity:
;Checks that the read/write will be on one track and makes sense.
;Input: ebx = BIOS function to call. Preserved.
;       ecx = LBA of start sector.
;       rdi -> chsIOBlock
;       rbp -> Drive block ptr
    movzx eax, word [rdi + chsIOBlock.wStartSector] ;Zero based
    inc eax         ;Make it 1 based for the comparison
    add ax, word [rdi + chsIOBlock.wNumSectors]
    cmp ax, word [rbp + drvBlk.wSecPerTrk]  ;Does this surpass num sect/trck?
    retna
.ioctlerr:
    pop rbx ;Pop the ret addr off the stack and tail to the bad media
    jmp .iobadMed
.ioChsToLba:
;Gets the first sector of the track selected by this call.
;Works with the values set in the bpb of drvBlk which isn't ideal...
;LBA = (( C x HPC ) + H ) x SPT + S - 1
;Input: ebx = BIOS Function to call. Preserved.
;Output: ecx = eax = LBA address for the first sector of the track
;        rdi -> chsFormatBlock or IOBlock
    mov rdi, rdx
    movzx eax, word [rbp + drvBlk.wNumHeads]
    movzx ecx, word [rdi + chsFormatBlock.wStartCyl]
    cmp cx, word [rbp + drvBlk.wNumCyl]
    jae .ioctlerr
    inc ecx ;Inc as we get it as a zero based number
    mul ecx
    movzx ecx, word [rdi + chsFormatBlock.wStartHead]
    cmp cx, word [rbp + drvBlk.wNumHeads]
    jae .ioctlerr 
    add eax, ecx
    movzx ecx, word [rbp + drvBlk.wSecPerTrk]
    mul ecx
    mov ecx, eax
    return
;---------------------------------------------------------------------------
;                    LBA IO requests are structured here
;---------------------------------------------------------------------------
.lbaVerify:
    mov eax, 8400h  ;Verify sectors
    jmp short .lbaFmtCmn
.lbaFmt:
    mov eax, 8500h  ;Format sectors
.lbaFmtCmn:
    cmp byte [rdx + lbaIOBlock.size], lbaFormatBlock_size
    jmp short .lbaCmn
.lbaRead:
    mov eax, 8200h  ;Read sectors
    jmp short .lbaRWCmn
.lbaWrite:
    mov eax, 8300h  ;Write sectors
.lbaRWCmn:
    mov rbx, qword [rdx + lbaIOBlock.xferBuffer]
    cmp byte [rdx + lbaIOBlock.size], lbaIOBlock_size
.lbaCmn:
    jne .iobadCmdLen
;Setup the vars for block IO
    mov rdi, rdx
    movzx esi, word [rdi + lbaFormatBlock.numSectors]
    mov ecx, dword [rdi + lbaFormatBlock.startSector]
    add ecx, dword [rbp + drvBlk.dHiddSec]  ;Point to sector in partition
.ioEp:
    mov dl, byte [rbp + drvBlk.bBIOSNum]    ;Get BIOS number for device
.ioLp:
    push rax        ;Always preserve the function number we are using
    call .blkIODirect
    pop rax
    jc .ioDoErr
    call .ioAdv
    jnz .ioLp
    return
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
;                           Misc GENIO routines 
;---------------------------------------------------------------------------
.ioSetIds:
.ioGetIds:
    jmp .iobadCmd
.ioSetAccessFlag:
;Sets/Clears the unformatted bit of a device block.
    test byte [rdx + accFlgBlk.bAccMode], -1
    jz .iosafNoAcc
;Enables access if the access mode is non-zero
    and word [rbp + drvBlk.wDevFlgs], ~devUnFmt
    return
.iosafNoAcc:
;Disables access if the access mode is zero 
    or word [rbp + drvBlk.wDevFlgs], devUnFmt
    return
.ioGetAccessFlag:
;Gets the state of the unformatted bit of a device block
    mov byte [rdx + accFlgBlk.bAccMode], 0  ;Init to clear, no access
    test word [rbp + drvBlk.wDevFlgs], devUnFmt
    retnz
    inc byte [rdx + accFlgBlk.bAccMode] ;If bit clear, set mode to access ok!
    return

.ioTrackTbl:
    dw maxTrackTblSz    ;Have a maximum of 63 sectors per track
;Each row is a pair of words:
;   dw Sector number, Sector size
    dw 1, 200h
    dw 2, 200h
    dw 3, 200h
    dw 4, 200h
    dw 5, 200h
    dw 6, 200h
    dw 7, 200h
    dw 8, 200h
    dw 9, 200h
    dw 10, 200h
    dw 11, 200h
    dw 12, 200h
    dw 13, 200h
    dw 14, 200h
    dw 15, 200h
    dw 16, 200h
    dw 17, 200h
    dw 18, 200h
    dw 19, 200h
    dw 20, 200h
    dw 21, 200h
    dw 22, 200h
    dw 23, 200h
    dw 24, 200h
    dw 25, 200h
    dw 26, 200h
    dw 27, 200h
    dw 28, 200h
    dw 29, 200h
    dw 30, 200h
    dw 31, 200h
    dw 32, 200h
    dw 33, 200h
    dw 34, 200h
    dw 35, 200h
    dw 36, 200h
    dw 37, 200h
    dw 38, 200h
    dw 39, 200h
    dw 40, 200h
    dw 41, 200h
    dw 42, 200h
    dw 43, 200h
    dw 44, 200h
    dw 45, 200h
    dw 46, 200h
    dw 47, 200h
    dw 48, 200h
    dw 49, 200h
    dw 50, 200h
    dw 51, 200h
    dw 52, 200h
    dw 53, 200h
    dw 54, 200h
    dw 55, 200h
    dw 56, 200h
    dw 57, 200h
    dw 58, 200h
    dw 59, 200h
    dw 60, 200h
    dw 61, 200h
    dw 62, 200h
    dw 63, 200h

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
;Output: rbp = Points to the drvBlk
    cmp byte [rbx + drvReqPkt.cmdcde], drvINIT
    rete
    lea rbp, .drvBlkTbl
.sdChk:
    cmp byte [rbp + drvBlk.bDOSNum], al
    rete
    mov rbp, qword [rbp + drvBlk.pLink]
    cmp rbp, -1
    jne .sdChk  ;Keep looping until end of table
    pop rax     ;Pop return address off the stack
    mov al, drvBadMed
    jmp .writeEntryError

.checkSwapIO:
;Checks if the reason for an error mid IO operation was
; really media being swapped.
    call .checkDevFixed ;If the dev is fixed, skip checking swap
    retnz
    cmp ah, 06h         ;ah = 06 is BIOS Drive changed error code 
    retne
;Here the BIOS is reporting that the media was swapped. Check the media
; to see if the drive really was swapped. 
;We start by checking the open count. If it is zero, we never report
; an illegal disk swap. This prevents this error from being thrown
; if no files are open.
    call .checkOpen ;If opcnt = 0, ignore this error.
    retz
;Here, if we determine that a media swap occured, we must report a 
; bad disk change. That means, unsure or no swap simply return the 
; error code. 
    movzx eax, byte [rbp + drvBlk.bMedDesc] ;Get original meddesc byte
    push rax                                ;and save it on the stack
    call .csiogetbpb    ;Now get new bpb
    jc .csiogetbpberr   ;If error in getting the BPB, bubble it up
    pop rax             ;Get back the FAT byte in al
    call .checkFATSame  ;Returns status in eax
    js .csioBadDskChg
.csioExit:
    mov ah, 06h         ;Maintain the BIOS error code here
    return
.csioBadDskChg:
;Restore the stack to return directly to DOS and not caller. 
;Place DOS error code into al
    pop rax
    pop rax
    mov eax, drvBadDskChnge
    jmp .errorExit
.csiogetbpberr:
;Return the error code from getbpb
;Drop the saved media byte from the stack
    add rsp, 8
    stc
    return      ;and return with rax = Error code from updatebpb
.csiogetbpb:
;Saves the IO registers for use across the updatebpb call and calls
; the get bpb function
    push rbx    
    push rcx
    push rdx
    push rsi
    push rdi
    call .updateBpb ;Update the BPB
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    return
    
.checkSwap:
;Checks if the media represented by drvBlk has been swapped when it 
; shouldn't've been swapped.
;Input: rbp -> drvBlk to check for
;Output: CF=NC: All ok.
;        CF=CY: Error in getting updated BPB. 
;               AL = BIOS error code
;   If an illegal disk swap is detected, then this call returns directly
;   and doesnt bubble up to the caller. 
    call .checkDevFixed ;If the dev is fixed, skip checking swap
    retnz
    call .checkOpen
    retz
    call .checkMediaChange
    retz
;Since the open count is non-zero and a media swap has been seen (since
; the flag was set) we update the BPB and check if the media is the 
; same as the previous media. 
    call .updateBpb     ;Update the BPB
    retc
    call .checkVolumeSame
    retns   ;If the sign bit is not set (i.e. unsure or no change) return ok
;Else, we now return a bad disk change!
    pop rax ;Pop original return address off the stack
    mov al, drvBadDskChnge  ;Driver error code
    jmp .errorExit  ;Place error code in packet and return

.checkFATSame:
;At this point, we are unsure of the media swap status. 
    cmp al, byte [rbp + drvBlk.bMedDesc]
    jne .cvsChange  ;If they are not equal, there mustve been a change
;The FAT media byte is the same so how about literally anything else?
.checkVolumeSame:
;If the volume has an extended BPB, we check the serial number. If they are 
; the same, we then say no change. Else, the volume has changed. 
;Else, we report unknown. 
;----------------------------------------------------------------------------
;TODO: In the outer else, replace with a read of the filesystem volume label
;      and implement in buildBPB a routine to read the volume label from the
;      root directory of whatever drive.
;----------------------------------------------------------------------------
    xor eax, eax    ;Set eax = 0, unsure
    cmp byte [rsi + extBs.bootSig], extBsSig
    retne
;Here if we have an extended boot signature. 
; Check the volume ids are equal
    push rax
    mov eax, dword [rsi + extBs.volId]
    cmp dword [rbp + drvBlk.volId], eax
    pop rax
    jne .cvsChange
.cvsNoChange:
    inc eax ;Make eax = 1, no change
    jmp .clearMediaChange    ;Exit tail calling through this function
.cvsChange:
    dec eax ;Make eax = -1, change
    mov byte [.bLastDsk], al    ;Ensure we do a media check next time
    return

.checkDevFixed:
;Input: rbp -> drvBlk to check if fixed media or not
;Output: ZF=ZE: Not fixed
;        ZF=NZ: Fixed
    test word [rbp + drvBlk.wDevFlgs], devFixed
    return

.checkMediaChange:
;Input: rbp -> drvBlk to check changed flag for
;Output: ZF=ZE: No change
;        ZF=NZ: Change
    test word [rbp + drvBlk.wDevFlgs], devChgd
    return

.clearMediaChange:
;Input: rbp -> drvBlk to clear the devChanged bit for
    and word [rbp + drvBlk.wDevFlgs], ~devChgd
;~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~
%if drvDbg
    push rsi
    lea rsi, .cmcstr
    call dbgPrintString
    pop rsi
    push rax
    mov al, byte [rbp + drvBlk.bDOSNum]
    add al, "A"
    call dbgPutch
    pop rax
    call dbgCrlf
    return
.cmcstr db "[DRIVER] Media Change bit cleared for drive ",0
%endif
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return

.checkOpen:
;Input: rbp -> drvBlk to check open count for
;Output: ZF=ZE: Open count is 0
;        ZF=NZ: Open count geq 0
    cmp word [rbp + drvBlk.wOpenCnt], 0
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

;Broadcast the disk-swap message for multitaskers to hook
; and issue message (and skip the "not multitasking friendly"
; section below)
    mov dh, byte [rdi + drvBlk.bDOSNum] ;Unit that has lost ownership
    mov dl, byte [rbp + drvBlk.bDOSNum] ;Unit that has gained ownership
    xor ecx, ecx
    mov eax, 4A00h
    int 2Fh         ;If either ecx or cx = -1, return
    movsx ecx, cx   ;Convert 16-bit responses to 32-bit
    inc ecx         ;If ecx = -1, we return
    retz

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
;Preserves all registers and edits .bAccCnt and .dAccTime for rbp -> drvBlk
    push rax
    push rcx
    push rdx
    call .getTime   ;Return in ecx the time. eax and edx trashed.
    mov dword [rbp + drvBlk.dAccTime], ecx  ;And store it
    mov byte [.bAccCnt], 0  ;And set the access count back to 0
    pop rdx
    pop rcx
    pop rax
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
; across these calls. Only allows returning values in ax.
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    int 33h
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    return

.setBitsForAllDevs:
;Sets the selected bits for all devices with a particular BIOS number.
;Input: ax = Bits to set in wDevFlgs
;       dl = BIOS drive number
    push rbp
    pushfq
    lea rbp, .drvBlkTbl
.sbfadLp:
    cmp byte [rbp + drvBlk.bBIOSNum], dl
    jne .sbfadNext
    or word [rbp + drvBlk.wDevFlgs], ax
.sbfadNext:
    mov rbp, qword [rbp + drvBlk.pLink]
    cmp rbp, -1
    jne .sbfadLp
.sbfadExit:
    popfq
    pop rbp
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

.strikeMsg db 0Dh,0Ah,"Insert disk for drive "
.strikeMsgLetter db "A: and strike",0Dh,0Ah,"any key when ready",0Dh,0Ah,0Ah
.strikeMsgL equ $ - .strikeMsg

.fat12Str   db "FAT12   ",0
.fat16Str   db "FAT16   ",0
.fat32Str   db "FAT32   ",0
.defLbl     db "NO NAME ",0 ;Default volume label

.bAccCnt    db 0    ;Counter of 0 time difference media checks
.bLastDsk   db -1   ;Last disk to be checked for media check/IO.

;Keep this @ 4096 for hotplugging a 4096 dev that needs 512 byte pseudo
; access. 
;Access to this buffer should be mediated through a critical section... 
; but this driver doesnt need to be reentrant yet.
.inBuffer   db 4096 dup (0)  

.drvBpbTbl:
;Table of BPBs from FAh-FFh
    istruc bpb
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 1    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 112  ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 640  ;Number of sectors on medium
        at .media,      db 0FAh ;Media descriptor byte
        at .FATsz16,    dw 1    ;Number of sectors per FAT
        at .secPerTrk,  dw 8    ;Number of sectors per "track"
        at .numHeads,   dw 1    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend
    istruc bpb
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 2    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 112  ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 1280 ;Number of sectors on medium
        at .media,      db 0FBh ;Media descriptor byte
        at .FATsz16,    dw 2    ;Number of sectors per FAT
        at .secPerTrk,  dw 8    ;Number of sectors per "track"
        at .numHeads,   dw 2    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend
    istruc bpb
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 1    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 64   ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 360  ;Number of sectors on medium
        at .media,      db 0FCh ;Media descriptor byte
        at .FATsz16,    dw 2    ;Number of sectors per FAT
        at .secPerTrk,  dw 9    ;Number of sectors per "track"
        at .numHeads,   dw 1    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend
    istruc bpb
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 2    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 112  ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 720  ;Number of sectors on medium
        at .media,      db 0FDh ;Media descriptor byte
        at .FATsz16,    dw 2    ;Number of sectors per FAT
        at .secPerTrk,  dw 9    ;Number of sectors per "track"
        at .numHeads,   dw 2    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend
    istruc bpb
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 1    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 64   ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 320  ;Number of sectors on medium
        at .media,      db 0FEh ;Media descriptor byte
        at .FATsz16,    dw 1    ;Number of sectors per FAT
        at .secPerTrk,  dw 8    ;Number of sectors per "track"
        at .numHeads,   dw 1    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend
    istruc bpb
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 2    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 112  ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 640  ;Number of sectors on medium
        at .media,      db 0FFh ;Media descriptor byte
        at .FATsz16,    dw 1    ;Number of sectors per FAT
        at .secPerTrk,  dw 8    ;Number of sectors per "track"
        at .numHeads,   dw 2    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend
.drvBpbTblF9:
;Contains the special F9 BPBs
    istruc bpb  ;720Kb 3.5" floppies
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 2    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 112  ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 1440 ;Number of sectors on medium
        at .media,      db 0F9h ;Media descriptor byte
        at .FATsz16,    dw 3    ;Number of sectors per FAT
        at .secPerTrk,  dw 9    ;Number of sectors per "track"
        at .numHeads,   dw 2    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend
    istruc bpb  ;1.2Mb 5.25" floppies
        at .bytsPerSec, dw 512  ;Bytes per sector
        at .secPerClus, db 1    ;Sectors per cluster
        at .revdSecCnt, dw 1    ;Number of reserved sectors, in volume
        at .numFATs,    db 2    ;Number of FATs on media
        at .rootEntCnt, dw 224  ;Number of 32 byte entries in Root directory
        at .totSec16,   dw 2400 ;Number of sectors on medium
        at .media,      db 0F9h ;Media descriptor byte
        at .FATsz16,    dw 7    ;Number of sectors per FAT
        at .secPerTrk,  dw 15   ;Number of sectors per "track"
        at .numHeads,   dw 2    ;Number of read "heads"
        at .hiddSec,    dd 0    ;Number of hidden sectors
        at .totSec32,   dd 0    ;32 bit count of sectors
    iend

.drvBlkTbl:
;All drives start with Sectors/Cluster as -1 to indicate not initialised.
;All drives start with Media Descripter as 0 (invalid type)
;All drives start with dAccTime at -1 to force "uncertain" read for remdevs
;All drives present 63 Cylinders (only valid as a field on fixed disks)
;All drives have as an alt BPB, a 1.44Mb 3.5" Floppy.
    %assign i 0
    %rep drvBlkTblL
    istruc drvBlk
        at .pLink,      dq -1   ;End of table marker
        at .bBIOSNum,   db i    ;DOS drive number
        at .bDOSNum,    db i    ;BIOS drives default to removable
;Do a FAT12/16 BPB in FAT32 format
        at .wBpS,       dw 200h
        at .bSpC,       db -1       
        at .wResC,      dw 0001h
        at .bNumFAT,    db 02h
        at .wRtCntNum,  dw 00E0h    
        at .wTotSec16,  dw 0B40h    
        at .bMedDesc,   db 00h    
        at .wFATsz16,   dw 0009h    
        at .wSecPerTrk, dw 0012h    
        at .wNumHeads,  dw 0002h    
        at .dHiddSec,   dd 0    
        at .dTotSec32,  dd 0     
;FAT 32 fields. All zeros
        at .FATsz32,    dd 0
        at .extFlags,   dw 0
        at .FSver,      dw 0
        at .RootClus,   dd 0
        at .FSinfo,     dw 0
        at .BkBootSec,  dw 0
;DrvBlk Flags
        at .bBpbType,   db bpbFat12
        at .wOpenCnt,   dw 0
        at .bDevType,   db typeGenRem   ;Init to generic removable device
        at .wDevFlgs,   dw 0
        at .wNumCyl,    dw 63   
        istruc bpb32
            at .bytsPerSec, dw 200h
            at .secPerClus, db 01h
            at .revdSecCnt, dw 0001h
            at .numFATs,    db 02h    
            at .rootEntCnt, dw 00E0h    
            at .totSec16,   dw 0B40h    
            at .media,      db 0F0h    
            at .FATsz16,    dw 0009h    
            at .secPerTrk,  dw 0012h    
            at .numHeads,   dw 0002h    
            at .hiddSec,    dd 0    
            at .totSec32,   dd 0     
            at .FATsz32,    dd 0
            at .extFlags,   dw 0
            at .FSver,      dw 0
            at .RootClus,   dd 0
            at .FSinfo,     dw 0
            at .BkBootSec,  dw 0
            at .reserved,   db 12 dup (0) 
        iend
        at .dAccTime,   dd -1 
        at .volLab,     db "NO NAME    ",0
        at .volId,      dd 0    ;Vol ID of 0
        at .filSysType, db "FAT12   ",0
    iend
        %assign i i+1
    %endrep