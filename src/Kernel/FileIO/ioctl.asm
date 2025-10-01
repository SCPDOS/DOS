ioctrl:            ;ah = 44h, handle function
;Possible subfunctions
;======================================================
;Handle Information Functions                         |
;al = 00h => Get Handle Information word in dx        x
;al = 01h => Set Handle Information word              x
;======================================================
;IO Control String Functions (optionally implemented) |
;al = 02h => Read from Char device                    x
;al = 03h => Write to Char device                     x
;al = 04h => Read from Block device                   x
;al = 05h => Write to Block device                    x
;======================================================
;IO Status Functions (optionally implemented)         |
;al = 06h => Get input status                         x
;al = 07h => Get output status                        x
;======================================================
;Device information (optionally implemented)          |  
;al = 08h => Is a block device removable?       
;al = 09h => Is a logical device local or remote?   
;al = 0Ah => Is a handle to a local or remote file?
;======================================================
;SHARE.EXE control                                    |
;al = 0Bh => Change sharing retry count               x  
;======================================================
;al = 0Ch => Generic IOCTL handle request (RESERVED) - Code page switching
;======================================================
;Generic IOCTL string (optionally implemented)
;al = 0Dh => Block device Generic IOCTL request    
;======================================================
;Disk Drive change drive (optionally implemented)
;al = 0Eh => Get logical device                 
;al = 0Fh => Set logical device  
;======================================================
;Query IOCTL capacity (optionally implemented)
;al = 10h => Query IOCTL Handle
;al = 11h => Quety IOCTL block Device
;======================================================
    cmp al, 17
    ja .invalidFunction
    test al, al
    jz .getDevWord
    cmp al, 1
    je .setDevWord
    sub al, 2
    cmp al, 3
    jbe .ioctlStringFunctions
    sub al, 4
    cmp al, 1
    jbe .ioStatusFunctions
    sub al, 2
    cmp al, 2
    jbe .devControl
    sub al, 3
    jz .setSharingCount
    dec al
    jz .genericCharDevIOCTL
    dec al
    jz .genericBlokDevIOCTL
    dec al
    jz .getDrvLogicalDevice
    dec al
    jz .setDrvLogicalDevice
    cmp al, 1
    jz .queryIOCTLHdl
    jmp .queryIOCTLDev
.invalidFunction:
    mov eax, errInvFnc
.ifExit:
    jmp extErrExit
.accessDeniedError:
    mov eax, errAccDen
    jmp short .ifExit
.badHandle:
    mov eax, errBadHdl
    jmp short .ifExit
.badData:
    mov eax, errInvDat
    jmp short .ifExit
;...Functions...
.getDevWord:
;Input: bx = File Handle
;Ouput: dl = Low byte of device info word
;       dh = If char dev, upper byte of driver attribute word
;            Else, upper byte of device info word
    call derefSFTPtr
    jc .badHandle
    mov dx, word [rdi + sft.wDeviceInfo]
    test dx, devCharDev
    jz .gdwExit
    mov rdi, qword [rdi + sft.qPtr]         ;Get driver pointer
    mov dh, byte [rdi + drvHdr.attrib + 1]  ;Get the header 
.gdwExit:
    xor al, al
    call getUserRegs
    mov word [rsi + callerFrame.rdx], dx
    jmp extGoodExit
.setDevWord:
;Input: bx = File Handle
;       dl = Low byte of Device information word (if char dev)
;       dh = Upper byte of dev info for disk devs (undocumented). 
;            Must be 0 for char devs.
    call derefSFTPtr
    jc .badHandle 
    test word [rdi + sft.wDeviceInfo], devCharDev
    jz .sdwDisk
    mov byte [errorLocus], eLocChr
    test dh, dh ;If dh is not 0 for a char dev, complain!
    jnz .badData
    or dl, devCharDev   ;Ensure we remain a char dev if we are one
    mov byte [rdi + sft.wDeviceInfo], dl    ;And store these bits here
    jmp extGoodExit
.sdwDisk:
;Set high bits here for disk devices only.
;Can only set/clear bit 1 for EOF and disk full special handling. 
; Fail if any other bits are set in dh.
    test dh, ~1
    jnz .badData
    mov byte [rdi + sft.wDeviceInfo + 1], dh
    jmp extGoodExit
.ioctlStringFunctions:
;al = 0 -> ReadCharDev
;al = 1 -> WriteCharDev
;al = 2 -> ReadDiskDev
;al = 3 -> WriteDiskDev
;Input: bx = File handle/drive number
;       ecx = Bytes to transfer
;       rdx = Ptr to string to transfer
    movzx esi, bl
    lea rbx, primReqPkt
    mov byte [errorLocus], eLocUnk
    mov byte [Int24bitfld], 0
;Get in rdi the ptr to the SFT for the handle we are looking at
;Setup the common ioReqPkt fields and the read/write 
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    mov qword [rbx + ioReqPkt.bufptr], rdx
    push rcx
    push rdx
    mov ecx, drvIOCTLREAD
    mov edx, drvIOCTLWRITE
    test al, 1  ;If set, this is a write operation
    cmovnz ecx, edx ;Move write command into ecx
    jnz .notWrite
    or byte [Int24bitfld], critWrite
.notWrite:
    pop rdx
    pop rcx
    mov byte [rbx + ioReqPkt.cmdcde], cl
    cmp al, 1
    ja .ioctlStringBlockDevice
    ;Here we are for char devices only
    xchg rbx, rsi    ;Save the driver ptr to rsi and the handle to ebx 
    call derefSFTPtr    ;Get the sft ptr in rdi
    jc .badHandle 
    mov byte [errorLocus], eLocChr
    test word [rdi + sft.wDeviceInfo], devCharDev   ;Bit must be set
    jz .invalidFunction

    mov rbx, qword [rdi + sft.qPtr] ;Get ptr to device driver
    or byte [Int24bitfld], critCharDev
    xchg rbx, rsi   ;Swap back
    xor eax, eax
    jmp short .ioctlStringCommon
.ioctlStringBlockDevice:
    ;Here we are for block devices only
    ;esi has the drive number
    mov byte [errorLocus], eLocDsk
    mov eax, esi
    push rbx
    call getCDS ;Returns in al the unit number of the drive
    pop rbx
    jnc .okDrive
    mov eax, errBadDrv
    jmp extErrExit
.okDrive:
    mov rsi, qword [workingCDS]
    mov rsi, qword [rsi + cds.qDPBPtr]
    mov al, byte [rsi + dpb.bUnitNumber]
    mov byte [rbx + ioReqPkt.unitnm], al
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr]
    mov al, byte [rsi + dpb.bMediaDescriptor]
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] 
.ioctlStringCommon:
    ;rsi -> Device Driver header
    ;rbx -> Request Header
    ;al = Media descriptor
    mov byte [rbx + ioReqPkt.medesc], al
    test word [rsi + drvHdr.attrib], devDrvIOCTLCtl
    jz .invalidFunction ;If not supported, invalid function error 
    ;Make request now
    push rcx    ;Push xfrctr
    push rdx    ;Buffer Ptr
    mov word [rbx + ioReqPkt.status], 0
    call goDriver
    pop rdx
    pop rcx
;Check status, if bad, reuse ecx and rdx for tfrlen and bufptr and go again
    test word [rbx + ioReqPkt.status], drvErrStatus
    jz extGoodExit
    jmp failIOCTLCall

.ioStatusFunctions:
;al = 0 -> Get input status 
;al = 1 -> Get output status
    call derefSFTPtr
    jc .badHandle 
    lea rbx, primReqPkt
    mov byte [rbx + statusReqPkt.hdrlen], statusReqPkt_size
    mov byte [errorLocus], eLocUnk
    test word [rdi + sft.wDeviceInfo], devRedir  ;File cannot be redir!
    jnz .invalidFunction
    mov byte [Int24bitfld], 0
    mov ecx, drvINSTATUS
    mov edx, drvOUTSTATUS
    test al, al
    cmovnz ecx, edx
    mov byte [rbx + statusReqPkt.cmdcde], cl
    test word [rdi + sft.wDeviceInfo], devCharDev
    jz .ioStatDisk
    mov byte [errorLocus], eLocChr
    or byte [Int24bitfld], critCharDev
    mov rsi, qword [rdi + sft.qPtr]
    xor al, al
    jmp short .ioStatCommon
.ioStatDisk:
    mov byte [errorLocus], eLocDsk
    mov rsi, qword [rdi + sft.qPtr] ;Get DPB ptr
    mov al, byte [rsi + dpb.bUnitNumber]
    mov byte [rbx + statusReqPkt.unitnm], al
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr]
.ioStatCommon:
    call goDriver
    test word [rbx + statusReqPkt.status], drvErrStatus
    jnz failIOCTLCall
    test word [rbx + statusReqPkt.status], drvBsyStatus
    jz .notBusyExit
    mov al, -1  ;Device Busy/EOF
    return 
.notBusyExit:
;Device ready/NotEOF
    xor eax, eax
    return

.devControl:
;al = 0 -> Is media removable?
;al = 1 -> Is device local or remote?
;al = 2 -> Is handle local or remote?
    test al, al
    jz .remTest
    test al, 1
    jnz .deviceTest
;Handle Local or Remote
    call derefSFTPtr    ;Get ptr in rdi
    jc .badHandle 
    movzx edx, word [rdi + sft.wDeviceInfo]
.devControlExit:
    call getUserRegs
    mov word [rsi + callerFrame.rdx], dx
    xor eax, eax
    return 
.deviceTest:
;Drive Local or Remote
;For more compat, get the driver attrib word if local with bit 12 clear
;Else, just get the CDS word and swap subst and net
    mov al, bl
    call getCDS
    jnc .devTestOkDrive
    mov eax, errBadDrv
    jmp extErrExit
.devTestOkDrive:
    mov rdi, qword [workingCDS]
    mov dx, word [rdi + cds.wFlags]
    test dx, cdsRedirDrive   ;Check the remote bit
    jnz .remoteDrive
    mov rdi, qword [rdi + cds.qDPBPtr]
    mov rdi, qword [rdi + dpb.qDriverHeaderPtr]
    mov dx, word [rdi + drvHdr.attrib]
    and dx, ~3000h  ;Clear bits 12 and 13
    jmp short .devControlExit
.remoteDrive:
    mov bx, dx
    or dx, 1000h    ;Set bit 12 for remote (its at subst)
    and bx, cdsSubstDrive   ;Isolate the subst bit
    jz short .devControlExit    ;Jump to exit if no subst bit
    or dx, 8000h    ;Else, set bit 15 for subst in returning word!
    jmp short .devControlExit   ;Store it

.remTest:
;Drive letter in bl
    mov al, bl
    call getCDS
    jnc .remTestOkDrive
.remTestBadDrv:
    mov eax, errBadDrv
    jmp extErrExit
.remTestOkDrive:
    mov rdi, qword [workingCDS] ;Get the CDS
    test word [rdi + cds.wFlags], cdsRedirDrive
    jnz .remTestBadDrv
    lea rbx, primReqPkt
    mov rsi, qword [rdi + cds.qDPBPtr]
    mov al, byte [rsi + dpb.bUnitNumber]    ;Get the unit number
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Get driver ptr in rsi
    test word [rsi + drvHdr.attrib], devDrvOpClRem
    jz .invalidFunction
    mov byte [rbx + remMediaReqPkt.hdrlen], remMediaReqPkt_size
    mov byte [rbx + remMediaReqPkt.cmdcde], drvREMMEDCHECK
    mov word [rbx + remMediaReqPkt.status], 0
    mov byte [rbx + remMediaReqPkt.unitnm], al
    call goDriver
    movzx eax, word [rbx + remMediaReqPkt.status]   ;Get the status
    and eax, drvBsyStatus   ;Clear all but bit 9
    shr eax, 9  ;Move the busy bit to al[0]
    return

.setSharingCount:
;Input: cx = Share Count
;       dx = Share Retry
    mov word [shareCount], cx
    test dx, dx
    jz .invalidFunction
    mov word [shareDelay], dx
    xor eax, eax
    jmp extGoodExit

.genericCharDevIOCTL:
    call .getHandleInfo
    jmp short .ioctlReqMake
.genericBlokDevIOCTL:
    call .getBlkDevInfo
.ioctlReqMake:
;rsi must point to the driver header here
    test word [rsi + drvHdr.attrib], devDrvIOCTL
    jnz .supportsIOCTL
.badFunction:
    mov byte [errorLocus], eLocUnk
    mov eax, errInvFnc
    jmp extErrExit
.supportsIOCTL:
    ;Setup the request header
    call .doIOCTLrequest
    jz extGoodExit
    jmp failIOCTLCall

.getHandleInfo:
;Returns in rsi a pointer to the driver for the handle device
    call getSFTPtr  ;Get in rdi the SFT ptr
    jnc .ghiOkHdl
.ghiBadHdl:
    mov byte [errorLocus], eLocChr
    pop rax ;Pop the return addr, return through DOS immediately
    mov eax, errBadHdl
    jmp extErrExit  ;Exit as if in line with main routine body
.ghiOkHdl:
    test word [rdi + sft.wDeviceInfo], devRedir
    jnz .ghiBadHdl
    test word [rdi + sft.wDeviceInfo], devCharDev
    jz .ghiBadHdl
    mov rsi, qword [rdi + sft.qPtr] ;Get the driver ptr in rsi
    return

.getBlkDevInfo:
;Returns in rsi a ptr to the block device driver
    mov al, bl  ;Move the drive number from bl to al
    push rcx
    push rdx
    call getCDS ;Sets the current CDS
    pop rdx
    pop rcx
    jc .gbdiBadDrv
    ;Check the CDS not net, subst or join
    mov rsi, qword [workingCDS]
    movzx eax, word [rsi + cds.wFlags]
    test ax, cdsValidDrive  ;If not valid, fail
    jz .gbdiBadDrv
    test ax, cdsRedirDrive|cdsJoinDrive|cdsSubstDrive|cdsRdirLocDrive
    jz .gbdiOk
.gbdiBadDrv:
    mov byte [errorLocus], eLocDsk
    pop rax ;Pop the return addr, return through DOS immediately
    mov eax, errBadDrv  ;Error code if error
    jmp extErrExit
.gbdiOk:
    ;CDS is good,now get the DPB
    mov rsi, qword [rsi + cds.qDPBPtr]  ;DPB ptr in rsi
    mov rdi, qword [rsi + dpb.qDriverHeaderPtr] ;Driver ptr in rdi
    mov al, byte [rsi + dpb.bUnitNumber]
    mov byte [primReqPkt + ioctlReqPkt.unitnm], al
    mov rsi, rdi   ;Get the driver ptr in rsi
    return


.doIOCTLrequest:
;Does IOCTL request
;Input: cx = Major/Minor bytes packed in cx
;       rdx -> Ptr to control packet
;Output: ZF=ZE: No error.
;        ZF=NZ: Error. Get error code.
;       rbx -> Left pointing to the request packet
;       eax = Status word
    lea rbx, primReqPkt
    mov byte [rbx + ioctlReqPkt.hdrlen], ioctlReqPkt_size
    mov byte [rbx + ioctlReqPkt.cmdcde], drvIOCTL
    mov word [rbx + ioctlReqPkt.status], 0
    mov word [rbx + ioctlReqPkt.majfun], cx ;Store maj and min together
    mov qword [rbx + ioctlReqPkt.ctlptr], rdx
    mov rdi, rsi    ;Save the driver header ptr in rdi
    call getUserRegs
    mov rax, qword [rsi + callerFrame.rsi]
    mov qword [rbx + ioctlReqPkt.rsival], rax
    mov rax, qword [rsi + callerFrame.rdi]
    mov qword [rbx + ioctlReqPkt.rdival], rax
    mov rsi, rdi
    call goDriver
    movzx eax, word [rbx + ioctlReqPkt.status]
    test eax, drvErrStatus
    return

.getDrvLogicalDevice:
    mov al, bl
    lea rbx, primReqPkt
    mov byte [rbx + getDevReqPkt.cmdcde], drvGETDRVMAP
    mov byte [rbx + getDevReqPkt.hdrlen], getDevReqPkt_size
    mov word [rbx + getDevReqPkt.status], 0
    push rbx
    call getCDS
    mov byte [workingDrv], al
    pop rbx
    jc .remTestBadDrv
    mov rdi, qword [workingCDS]
    mov rdi, qword [rdi + cds.qDPBPtr]
    mov rsi, qword [rdi + dpb.qDriverHeaderPtr]
    mov al, byte [rdi + dpb.bUnitNumber]
    mov byte [rbx + getDevReqPkt.unitnm], al

    test word [rsi + drvHdr.attrib], devDrvIOCTL
    jz .invalidFunction
    call goDriver
    test word [rbx + getDevReqPkt.status], drvErrStatus
    jz .getDrvOk
    jmp failIOCTLCall
.getDrvOk:
    mov al, byte [rbx + getDevReqPkt.unitnm]    ;Get the byte
    return
.setDrvLogicalDevice:
    mov al, bl
    lea rbx, primReqPkt
    mov byte [rbx + setDevReqPkt.cmdcde], drvSETDRVMAP
    mov byte [rbx + setDevReqPkt.hdrlen], setDevReqPkt_size
    mov word [rbx + setDevReqPkt.status], 0
    push rbx
    call getCDS
    mov byte [workingDrv], al
    pop rbx
    jc .remTestBadDrv
    mov rdi, qword [workingCDS]
    mov rdi, qword [rdi + cds.qDPBPtr]
    mov rsi, qword [rdi + dpb.qDriverHeaderPtr]
    mov al, byte [rdi + dpb.bUnitNumber]
    mov byte [rbx + setDevReqPkt.unitnm], al
    test word [rsi + drvHdr.attrib], devDrvIOCTL
    jz .invalidFunction
    call goDriver
    xor al, al
    test word [rbx + getDevReqPkt.status], drvErrStatus
    retz    ;Return if OK, else fail

.queryIOCTLHdl:
    call .getHandleInfo
    jmp short .queryCmn
.queryIOCTLDev:
    call .getBlkDevInfo
.queryCmn:
;If Query is set, then we should be able to make the IOCTL Query request
    test word [rsi + drvHdr.attrib], devDrvIOCTLQ
    jz .badFunction
    test word [rsi + drvHdr.attrib], devDrvIOCTL
    jz .badFunction
    call .doIOCTLrequest
    jz extGoodExit
;Else, the driver reports it doesn't recognise the command function!
    mov eax, errAccDen
    jmp extErrExit

failIOCTLCall:
;Called to fail IOCTL calls that don't trigger Int 24h
;rbx -> Driver request packet
    movzx edi, word [rbx + ioctlReqPkt.status]
    and edi, 0FFh   ;Save the low byte only
    call xlatHardError
    movzx eax, word [errorExCde] 
    jmp extErrExit

