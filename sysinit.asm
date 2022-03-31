; We arrive here with the following values in the registers.
; rbx =  LBA of first Logical Block after SCP/BIOS
; dx  = Int 33h boot device number
; fs  = userbase pointer (pointer to first usable block of RAM)
tempPSP:    ;Here to allow the loader to use Int 41h once it is loaded high
    dw 0AA55h           ;Initial signature
    db (100h-2) dup (90h)   ;Duplicate NOPs for the PSP
;First make space for the MCB
    push rdx    ;Save dl on stack briefly
    mov ecx, 0C0000100h ;Read FS MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in
    add rdi, mcb_size   ;Make space for the MCB
    mov eax, edi
    mov rdx, rdi
    shr rdx, 20h
    wrmsr   ;Write the new value to FS MSR
    pop rdx
;------------------------------------------------;
;              Connect Debugger                  ;
;------------------------------------------------;
    mov eax, 0C501h ;Connect debugger
    int 35h
;------------------------------------------------;
;           Sanitise the data area               ;
;------------------------------------------------;
    mov ecx, dSegLen
    xor al, al
    push rdi    ;Temp save rdi on the stack
    rep stosb
    pop rdi

;------------------------------------------------;
;          Start saving Basic DOS data           ;
;------------------------------------------------;
    mov byte fs:[bootDrive], dl ;Save the boot drive in memory
;Copy DOS to its final resting place
    mov qword fs:[dosSegPtr], rdi 
    mov rbp, rdi    ;Save the start of dosSeg in rdx 
    add rdi, dSegLen ;Move destination past end of data area
    lea rsi, section.resSeg.start  ;Get RIP relative address to copy high
    mov ecx, 1000h
    rep movsq

    int 31h ;Get number of Int 33h devices in r8b
    shr r8, 3*8   ;Isolate byte 3 of r8
    mov byte fs:[numRemMSD], r8b    ;Save number of physical int 33h devs
    mov byte fs:[lastdrvNum], 5     ;Last drive is by default 5
    mov byte fs:[numLRemDrives], 0     ;Number of logical drives

;------------------------------------------------;
;          Find largest sector size              ;
;------------------------------------------------;
largestSectorSearch:
    xor dl, dl
    xor edi, edi    ;Use this as the counter for the largest sector size
.lss:
    mov ah, 88h
    int 33h
    cmp edi, eax
    cmovb edi, eax  ;Only replace ebp if eax is greater
    inc dl
    cmp dl, r8b
    jne .lss
    mov word fs:[maxBytesSec], di

;------------------------------------------------;
;          Driver Adjustments and inits          ;
;------------------------------------------------;
;Modify the pointers in nData before putting them in the data area
    add qword [nData + drvHdr.nxtPtr], rbp
    add qword [nData + drvHdr.strPtr], rbp
    add qword [nData + drvHdr.intPtr], rbp
;Copy the Null driver to its location in Sysvars
    mov ecx, drvHdr_size
    lea rsi, qword [nData]
    lea rdi, qword [rbp + nulDevHdr]
    rep movsb   

;Adjust the addresses in the other driver headers 
    mov rsi, conHdr ;Point to the first non-NUL dev in chain
    mov ecx, 12      ;12 drivers in data area
    lea rsi, qword [rsi + rbp]  ;Get effective addr of driver header
adjDrivers:
    call adjustDrvHdr
    loop adjDrivers

;Open NUL
    lea rbx, qword [rbp + charReqHdr]
    mov byte [rbx + openReqPkt.hdrlen], openReqPkt_size
    mov byte [rbx + openReqPkt.status], 0
    call qword [rbp + nulDevHdr + drvHdr.strPtr]
    call qword [rbp + nulDevHdr + drvHdr.intPtr]
;Open CON
conInit:    ;Rather than keeping this resident... do it here
.ci0:
    mov ah, 01      ;Get buffer status
    int 36h
    jz .ci1      ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short .ci0
.ci1:
    mov eax, 0500h  ;Set page zero as the default page
    int 30h
    mov ah, 02h
    xor edx, edx    ;Set screen cursor to top right corner
    mov bh, dl      ;Set cursor for page 0
    int 30h
    mov bh, 07h     ;Grey/Black attribs
    mov eax, 0600h  ;Clear whole screen
    int 30h

    ;Save ptr to ConHdr in Sysvars
    lea rax, qword [rbp + conHdr]
    mov qword fs:[conPtr], rax

    ;Save ptr to ClkHdr in Sysvars
    lea rax, qword [rbp + clkHdr]
    mov qword fs:[clockPtr], rax

;------------------------------------------------;
;          Kernel inits and adjustments          ;
;------------------------------------------------;
;Adjust Int 41h address table
adjInt41h:
    mov ecx, kernelDispatchTableL/8 ;Number of elements in table
    mov rbx, kernelDispatchTable ;Get EA of table
    lea rbx, qword [rbp+rbx]    ;Point to the start of the relocated table 
.ai41h:
    add qword [rbx], rbp    ;Add base address value to entry in reloc table
    add rbx, 8              ;Each entry is size 8
    dec ecx
    jnz .ai41h  ;Keep looping until all entries have been adjusted

;Adjust Interrupt Entries Int 40h-49h
adjInts:
    mov bl, 40h
    mov eax, 0F007h ;Get the descriptor
    int 35h
    mov ecx, 40h    ;Start from interrupt 40h
    lea rdi, intData
    mov esi, eax    ;Move segment selector info to esi
.ai0:
    mov eax, 0F008h ;Set the descriptor
    mov rbx, qword [rdi]    ;Get address pointed to by rdi
    add rbx, rbp            ;Add the relocated base to rbx
    int 35h
    add rdi, 8
    inc ecx
    cmp ecx, 4Ah
    jne .ai0

;------------------------------------------------;
;         Link DOS to temporary Buffer           ;
;------------------------------------------------;
tempBufferInit:
    lea rdi, qword [rbp + tmpBufHdr]
    mov qword fs:[bufHeadPtr], rdi  ;Save pointer to temp buffer "list"
    xor eax, eax
    dec rax
    stosq   ;.nextBufPTr, end of list
    stosb   ;.driveNumber, Free entry
    inc rax
    stosb   ;.bufferFlags, No flags
;------------------------------------------------;
;         Init msd driver, create DPB            ;
;------------------------------------------------;
storageInits:
;First save dpb pointer in sysvars
    lea rbx, qword [rbp + firstDPB]
    mov qword fs:[dpbHeadPtr], rbx
;Open Mass Storage
    lea rbx, qword [rbp + diskReqHdr]
    mov byte [rbx + initReqPkt.hdrlen], initReqPkt_size
    mov byte [rbx + initReqPkt.cmdcde], 00h     ;MSD init
    mov word [rbx + initReqPkt.status], 0       ;Zero status word
    mov al, byte fs:[numLRemDrives]
    mov byte [rbx + initReqPkt.drvnum], al      ;First unit is drive A
    call qword [rbp + msdHdr + drvHdr.strPtr]
    call qword [rbp + msdHdr + drvHdr.intPtr]
    ;Check if it returned OK first!
    test word [rbx + initReqPkt.status], 8000h  ;Test the error bit
    jnz errorInit   ;If the bit is set, halt execution
    mov al, byte [rbx + initReqPkt.numunt]
    mov byte fs:[numLRemDrives], al
    mov byte [rbp + msdHdr + drvHdr.drvNam], al ;Save # of units in name field

    mov rdx, qword [rbx + initReqPkt.optptr]    ;Get ptr to bpbPtrTbl in rdx
    mov rdi, rbp ;Save rbp in rdi temporarily
    xor cl, cl  ;Clear counter
    mov rbp, fs:[dpbHeadPtr]  ;Get first DPB address in rdi
.si0:   
    mov rsi, qword [rdx]    ;Get pointer to device media bpb
    mov ah, 53h ;Fill dpb with translated BPB data
    int 41h
;Add other data to DPB
    mov byte [rbp + dpb.bDriveNumber], cl ;Remember, rbp points to dpb!!
    mov byte [rbp + dpb.bUnitNumber], cl
    push rax
    lea rax, qword [rdi + msdHdr]   ;Get ptr to msd driver header
    mov qword [rbp + dpb.qDriverHeaderPtr], rax
    pop rax
    inc cl
    cmp cl, al  ;When equal, we are have finished
    je .si1
    push rax
    lea rax, qword [rbp + dpb_size] ;Load address of next dpb to rax
    mov qword [rbp + dpb.qNextDPBPtr], rax  ;Save pointer
    mov rbp, rax        ;Now move current device pointer over
    pop rax
    add rdx, 8  ;Goto next pointer in table
    jmp short .si0
.si1:
;Remember to now place a -1 in the qNextDPBPtr field 
    mov qword [rbp + dpb.qNextDPBPtr], -1
    mov rbp, rdi    ;Now return to rbp a pointer to the head of dos segment
;------------------------------------------------;
;                 Temp CDS inits                 ;
;------------------------------------------------;
tempCDS:
;Build a temporary CDS for Drive A to use it for booting
    lea rdi, qword [rbp + initCDS]
    mov qword fs:[cdsHeadPtr], rdi
    mov ecx, 67 ;Buffer length
    xor eax, eax
    mov rbx, rdi    ;Save CDS pointer in rbx
    rep stosb   ;Zero out the path string
    mov rdi, rbx
    mov al, "A"
    stosb
    mov al, ":"
    stosb
    mov al, "\"
    stosb
    mov rdi, rbx
    mov word [rdi + cds.wFlags], cdsPhysDrive   ;Must be a physical drive
    mov rbx, qword fs:[dpbHeadPtr]  ;Get the DPB of first drive in rbx
    mov qword [rdi + cds.qDPBPtr], rbx
    mov word [rdi + cds.wBackslashOffset], 2    ;Skip the A:
    ;On FAT12/16, startcluster = 0 => Root Dir Sector
    ;On FAT32, startcluster = 0 => Alias for root cluster. 
    ;   Read dpb.dFirstUnitOfRootDir for first cluster of root dir
    mov dword [rdi + cds.dStartCluster], eax    ;eax was zeroed before
;------------------------------------------------;
;     Set up general PSP areas and DOS vars      ;
;------------------------------------------------;
    ;Additional DOS Vars init
    xor eax, eax
    mov byte fs:[currentDrv], al ;Current Drive = Drive A
    mov byte fs:[breakFlag], al  ;Break off
    mov byte fs:[verifyFlag], al ;Write only
    mov byte fs:[singleDrv], al  ;Only used on single drive systems
    mov byte fs:[critErrFlag], al   ;Not in critical error
    mov byte fs:[inDOS], al      ;Not in DOS
    mov byte fs:[errorDrv], -1   ;No error drive
    mov word fs:[lastRetCode], ax   ;Last return code is 0, no error

    ;SYSVARS PSP Init
    lea rbx, qword [tempPSP]
    mov qword fs:[currentPSP], rbx    ;Save current PSP
    push rbx
    add rbx, psp.dta
    mov qword fs:[currentDTA], rbx    ;Save current DTA
    pop rbx
    mov word [rbx + psp.return], 0CD40h ;DOS return function
    mov dword [rbx + psp.unixEntry], 0CD40CB00h  ;Last byte overlaied
    mov qword [rbx + psp.startSeg], rbx ;Save start segment of app
    mov qword [rbx + psp.parentPtr], rbx ;Save self as parent Process
    mov qword [rbx + psp.prevPSP], rbx  ;Save self as previous PSP
    xor eax, eax
    lea rdi, qword [rbx + psp.jobFileTbl]
    stosq   ;8 bytes
    stosq   ;16 bytes
    stosd   ;20 bytes
    mov qword [rbx + psp.envPtr], -1    ;No environment
    mov word [rbx + psp.xtraHdlSz], ax  ;No size
    mov byte [rbx + psp.xtraHdlNum], -1 ;Unused
    mov rdx, rbx
    mov eax, 3542h  ;Get pointer for Int 42h in rbx
    int 41h
    mov qword [rdx + psp.oldInt42h], rbx
    mov eax, 3543h
    int 41h
    mov qword [rdx + psp.oldInt43h], rbx
    mov eax, 3544h
    int 41h
    mov qword [rdx + psp.oldInt44h], rbx

    mov ecx, psp_size - psp.fcb1    ;Clear the dta and fcb space
    lea rdi, qword [rdx + psp.fcb1] ;Point to fcb1
    rep stosb   ;Clear DTA and FCBs
;------------------------------------------------;
;          Default File Handle Creation          ;
;------------------------------------------------;
defaultFileHandles:
;Fill in the default file table entries
    lea rbx, qword [rbp + firstSftHeader]
    mov qword [rbx + sfth.qNextSFTPtr], -1  ;Last sfth in chain
    mov word [rbx + sfth.wNumFiles], 5      ;5 default files
    mov qword fs:[sftHeadPtr], rbx  ;Save ptr to this sft header in SysVars
;GOTO FIRST FILE 
    add rbx, sfth_size  ;Goto first driver
;Write CON
    mov word [rbx + sft.wNumHandles], 0 ;Nothing pointing to this file yet
    mov word [rbx + sft.wOpenMode], critErrHdl | denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], charDevConIn|charDevConOut|charDevFastOut|devCharDev
    mov rax, qword fs:[conPtr]  ;Get pointer to CON device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    mov rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
    lea rsi, qword [.dfhCon]
    ;11 chars in 8.3 name
    movsq   ;8 chars
    movsw   ;10 chars
    movsb   ;11 chars
    mov rax, qword fs:[currentPSP]  ;Get current PSP
    mov qword [rbx + sft.qPSPOwner], rax
;GOTO NEXT ENTRY
    add rbx, sft_size   ;Goto next SFT
;Write AUX
    mov word [rbx + sft.wNumHandles], 0 ;Nothing pointing to this file yet
    mov word [rbx + sft.wOpenMode], critErrHdl | denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], charDevNoEOF| devCharDev 
    ;No EOF when writing to the device
    mov rax, qword [rbp + auxHdr]  ;Get pointer to AUX device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    mov rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
    lea rsi, qword [.dfhAux]
    ;11 chars in 8.3 name
    movsq   ;8 chars
    movsw   ;10 chars
    movsb   ;11 chars
    mov rax, qword fs:[currentPSP]  ;Get current PSP
    mov qword [rbx + sft.qPSPOwner], rax
;GOTO NEXT ENTRY
    add rbx, sft_size   ;Goto next SFT
;Write PRN
    mov word [rbx + sft.wNumHandles], 0 ;Nothing pointing to this file yet
    mov word [rbx + sft.wOpenMode], critErrHdl | denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], charDevNoEOF| devCharDev 
    ;No EOF when writing to the device
    mov rax, qword [rbp + prnHdr]  ;Get pointer to PRN device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    mov rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
    lea rsi, qword [.dfhPrn]
    ;11 chars in 8.3 name
    movsq   ;8 chars
    movsw   ;10 chars
    movsb   ;11 chars
    mov rax, qword fs:[currentPSP]  ;Get current PSP
    mov qword [rbx + sft.qPSPOwner], rax
    jmp short .dfhExit
.dfhCon db "CON        "
.dfhAux db "AUX        "
.dfhPrn db "PRN        "
.dfhExit:
;------------------------------------------------;
;               Load CONFIG.SYS                  ;
;------------------------------------------------;
;------------------------------------------------;
;              Process CONFIG.SYS                ;
;------------------------------------------------;
;------------------------------------------------;
;       Load User Drivers from CONFIG.SYS        ;
;------------------------------------------------;
;------------------------------------------------;
;                 Create a CDS                   ;
;------------------------------------------------;
;------------------------------------------------;
;                   MCB inits                    ;
;------------------------------------------------;
mcbInit:
    mov eax, 0E801h ;Get the Extended memory arena sizes
    int 35h
    movzx ecx, cx   ;cx = # of bytes between USER_BASE and 16Mb
    movzx edx, dx   ;dx = # 64kb pages between 16Mb and 4Gb
    shl ecx, 0Ah   ;Multiply by 1024 to get number of bytes
    shl edx, 10h  ;Multiply by 65536 to get number of bytes
;Build the DOS segment's MCB header
    mov rbx, rbp
    sub rbx, mcb_size   ;Point rbx to the start of the MCB

    mov qword fs:[mcbChainPtr], rbx ;Save rbx in data area

    mov qword [rbx + mcb.owner], mcbOwnerDOS
    mov dword [rbx + mcb.blockSize], ecx ;Use Max lo mem size for now
    mov byte [rbx + mcb.marker], "Z"
    test edx, edx   ;Is edx 0?
    jz .mcbExit ;If it is, skip the next bit
;We have memory above 16Mb, change alloc to M and decrease size
    mov byte [rbx + mcb.marker], "M"
    sub dword [rbx + mcb.blockSize], mcb_size   ;Decrease allocation

    mov ecx, dword [rbx + mcb.blockSize]    ;Get the decreased size
    add rbx, rcx    ;Walk chain
;Now at the memory hole
    ;Holes are only declared if hole has usable ram on both sides of it
    mov byte [rbx + mcb.marker], "M"
    mov eax, 1000000h   ;16Mb
    sub eax, ebx    ;Sub ptr from 16Mb to get hole size
    mov qword [rbx + mcb.owner], mcbOwnerHole   ;Memory hole
    mov dword [rbx + mcb.blockSize], eax

    add rbx, rax    ;Walk chain
    mov byte [rbx + mcb.marker], "Z"
    mov qword [rbx + mcb.owner],mcbOwnerFree
    sub edx, mcb_size   ;Make space for the mcb
    mov dword [rbx + mcb.blockSize], edx
.mcbExit:
;------------------------------------------------;
;           Load Command interpreter             ;
;------------------------------------------------;

;Test Error Case
    mov ah, 00110000b
    mov al, 00h
    mov edi, 0Ch
    int 44h

    lea rdx, qword [strtmsg]   ;Get the absolute address of message
    mov ah, 09h
    int 41h
l1:
    mov ah, 01h  ;Write with echo
    int 41h
    cmp al, 0
    je l2
    jmp short l1
l2:
    mov ah, 07h
    int 41h
    cmp al, 42h
    jne l1
l3:
    mov word fs:[CLOCKrecrd + clkStruc.dateWord], 0
    lea rbx, qword [rbp + charReqHdr] ;Get the address of this request block
    lea rax, qword [rbp + CLOCKrecrd]
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read the time
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 06
    call qword [rbp + clkHdr + drvHdr.strPtr]
    call qword [rbp + clkHdr + drvHdr.intPtr]

    mov ah, 03h
    xor bh, bh
    int 30h
    xor dl, dl  ;0 column
    mov ah, 02h
    int 30h

    lea rbx, qword [rbp + CLOCKrecrd]
    movzx eax, byte [rbx + clkStruc.hours]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, ":"
    int 30h
    movzx eax, byte [rbx + clkStruc.minutes]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, ":"
    int 30h
    movzx eax, byte [rbx + clkStruc.seconds]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, "."
    int 30h
    movzx eax, byte [rbx + clkStruc.hseconds]
    call .clkHexToBCD
    jmp l1
.clkHexToBCD:
;Converts a Hex byte into two BCD digits
;Takes input in each nybble of al
    push rbx
    mov rbx, 0Ah  ;Divide by 10
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .chtb0
    add dl, 'A'-'0'-10
.chtb0:
    mov cl, dl    ;Save remainder byte
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .chtb1
    add dl, 'A'-'0'-10
.chtb1:
    mov ch, dl    ;Save remainder byte

    mov al, ch    ;Get most sig digit into al
    mov ah, 0Eh
    int 30h
    mov al, cl    ;Get least sig digit into al
    mov ah, 0Eh
    int 30h
    pop rbx
    ret
;--------------------------------
;       PROCS FOR SYSINIT       :
;--------------------------------
adjustDrvHdr:
;Input: rsi = Effective address of driver in DOS segment
;       rbp = Ptr to the start of the DOS segment
;Output: rsi = EA of next header in DOS segment
    add qword [rsi + drvHdr.nxtPtr], rbp    ;Adjust address
    add qword [rsi + drvHdr.strPtr], rbp
    add qword [rsi + drvHdr.intPtr], rbp
    add rsi, drvHdr_size
    ret
errorInit:
;If a critical error occurs during sysinit, fail through here
    lea rdx, hltmsg
    mov ah, 09h
    int 41h
    ;cli ;Clear interrupts
    ;mov al, -1
    ;mov dx, 0A1h    ;PIC2 data
    ;out dx, al      ;Mask all lines
    ;mov dx, 21h     ;PIC1 data
    ;out dx, al      ;Mask all lines
.ei0:
    hlt
    jmp short .ei0

;--------------------------------
;       DATA FOR SYSINIT        :
;--------------------------------
strtmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,"$"
hltmsg  db "Error initialising SCPDOS.SYS. System halting...",0Ah,0Dh,"$"
conName db "CON",0
auxName db "AUX",0
prnName db "PRN",0
intData:
    dq terminateProcess ;Int 40h
    dq functionDispatch ;Int 41h
    dq terminateHandler ;Int 42h
    dq ctrlCHandler     ;Int 43h
    dq critErrorHandler ;Int 44h
    dq absDiskRead      ;Int 45h
    dq absDiskWrite     ;Int 46h
    dq terminateResident    ;Int 47h
    dq inDosHandler     ;Int 48h
    dq fastOutput       ;Int 49h
nData:
    dq conHdr
    dw 08004h
    dq nulStrat
    dq nulIntr
    db "NUL     " ;Default NUL data
