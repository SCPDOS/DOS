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
    shr r8, 8   ;Isolate bytes 1 and 2 of r8
    mov ax, r8w
    mov byte fs:[numRemDrv], ah    ;Save number of physical int 33h removable drvs
    mov byte fs:[numFixDrv], al    ;Save number of physical hard drives
    mov byte fs:[lastdrvNum], 5    ;Last drive is by default 5
    mov byte fs:[numLogDrv], 0     ;Number of logical drives
    ;If no detected Int 33h devices, halt 
    shr r8, 2*8
    test r8b, r8b
    jz errorInit
;------------------------------------------------;
;          Kernel inits and adjustments          ;
;------------------------------------------------;
;Adjust Int 41h address table
adjInt41h:
    mov ecx, kernelDispatchTableL/8 ;Number of elements in table
    mov rbx, kernelDispatchTable ;Get EA of table
    lea rbx, qword [rbp + rbx]    ;Point to the start of the relocated table 
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
.ai1:
    int 35h
    add rdi, 8
    inc ecx
    cmp ecx, 4Ah
    jne .ai0

;++++++++++++++++++++++++++++++++++++++++++++++++;
;    DOS INTERRUPTS CAN BE USED FROM HERE ON     ;
;++++++++++++++++++++++++++++++++++++++++++++++++;
    %if DEBUG
    xchg bx, bx
    xor dl, dl
    mov eax, 0E3h   ;Set 9600,n,8,1
    int 34h
debugPopUpMsg:
    push rbx
    push rbp
    lea rbx, qword [debPrintNullString + rbp]
    lea rbp, .msg
    call rbx
    jmp short .exit
.msg:   db 0Ah,0Dh,"SCP/BIOS Boot complete.",0Ah,0Dh
        db "SCP/DOS Kernel Debugger Connected on COM1:9600,n,8,1",0Ah,0Dh,0
.exit:
    pop rbp
    pop rbx
    %endif

;Now adjust int 42h 43h and 44h correctly using DOS
    lea rdx, errorInit ;Get segment start address
    mov eax, 2542h  ;Int 42, set vector
    int 41h
    lea rdx, errorInit ;Get segment start address
    mov eax, 2544h
    int 41h

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
    ;lea rbx, qword [rbp + diskReqHdr]
    ;mov byte [rbx + initReqPkt.hdrlen], initReqPkt_size
    ;mov byte [rbx + initReqPkt.cmdcde], 00h     ;MSD init
    ;mov word [rbx + initReqPkt.status], 0       ;Zero status word
    ;mov al, byte fs:[numLogDrv]
    ;mov byte [rbx + initReqPkt.drvnum], al      ;First unit is drive A
    ;call qword [rbp + msdHdr + drvHdr.strPtr]
    ;call qword [rbp + msdHdr + drvHdr.intPtr]
    ;Check if it returned OK first!
    ;test word [rbx + initReqPkt.status], 8000h  ;Test the error bit
    ;jnz errorInit   ;If the bit is set, halt execution
    ;mov al, byte [rbx + initReqPkt.numunt]
    ;mov byte fs:[numLogDrv], al
    ;mov byte [rbp + msdHdr + drvHdr.drvNam], al ;Save # of units in name field

    ;mov rdx, qword [rbx + initReqPkt.optptr]    ;Get ptr to bpbPtrTbl in rdx
    call diskInit
    mov rdi, rbp ;Save rbp in rdi temporarily
    mov al, byte fs:[numLogDrv]
    lea rdx, qword [rbp + msdDriver.msdBPBTbl]
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
;          Find largest sector size              ;
;------------------------------------------------;
sectorSizeSearch:
;Done by reading BPB's for each drive
    ;xchg bx, bx
    lea rbx, qword [rbp + msdDriver.msdBPBTbl] ;Get first pointer to BPB
    
    ;Go thru each block individually
    xor eax, eax
    mov rdx, qword [rbx]    ;Get bpb pointer into rdx
.findLargest:
    cmp ax, word [rdx + bpb.bytsPerSec]
    cmovb ax, word [rdx + bpb.bytsPerSec] ;Only replace ax if the word is above ax
    add rbx, 8 ;Goto next entry
    mov rdx, qword [rbx]    ;Get next bpb pointer into rdx
    test rdx, rdx   ;Are we at the end?
    jnz .findLargest    ;Nope, keep checking!
    mov word fs:[maxBytesSec], ax
    
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
    lea rdi, qword [rbx + psp.jobFileTbl]
    mov rax, 0000000201000000h  ;Store default handles in JFT
    stosq   ;8 bytes
    xor eax, eax
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

    mov ecx, (psp_size - psp.fcb1)/4    ;Clear the dta and fcb space
    lea rdi, qword [rdx + psp.fcb1] ;Point to fcb1
    rep stosd   ;Efficiently Clear DTA and FCBs
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
    mov word [rbx + sft.wNumHandles], 3 ;Sysinit stdin/out/err
    mov word [rbx + sft.wOpenMode], critErrHdl | denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], charDevConIn|charDevConOut|charDevFastOut|charDevNoEOF|devCharDev 
    ;No EOF when reading from the device
    mov rax, qword fs:[conPtr]  ;Get pointer to CON device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    lea rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
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
    mov word [rbx + sft.wNumHandles], 1 ;Sysinit stdaux
    mov word [rbx + sft.wOpenMode], critErrHdl | denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], charDevNoEOF| devCharDev 
    ;No EOF when reading from the device
    mov rax, qword [rbp + auxHdr]  ;Get pointer to AUX device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    lea rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
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
    mov word [rbx + sft.wNumHandles], 1 ;Sysinit stdprn
    mov word [rbx + sft.wOpenMode], critErrHdl | denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], devCharDev 
    ;Want EOF when reading from to the device
    mov rax, qword [rbp + prnHdr]  ;Get pointer to PRN device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    lea rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
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
;   Setup Final Data Areas With Overrides from   ;
;                  CONFIG.SYS                    ;
;------------------------------------------------;
;------------------------------------------------;
;                   MCB inits                    ;
;------------------------------------------------;
mcbInit:

.mcbExit:
;------------------------------------------------;
;           Load Command interpreter             ;
;------------------------------------------------;
    lea rdx, qword [strtmsg]   ;Get the absolute address of message
    mov ah, 09h
    int 41h
    %if DEBUG

    %endif
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
;Int 42h, 43h and 44h point here during sysinit
    lea rbp, hltmsg
    mov eax, 1304h
    int 30h
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
strtmsg db "Starting SCP/DOS...",0Ah,0Dh,"$"
hltmsg  db "Error initialising SCPDOS.SYS. System halting...",0
conName db "CON",0
auxName db "AUX",0
prnName db "PRN",0
intData:
    dq terminateProcess ;Int 40h
    dq functionDispatch ;Int 41h
    dq errorInit        ;Int 42h, If sysinit terminates, halt system
    dq ctrlCHandler     ;Int 43h, ignore any CTRL+C during init
    dq errorInit        ;Int 44h, If critical error in sysinit, halt system
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

diskInit:
    ;We create a function to deal with BPB parsing etc
    ;Start with the first primary partition on each hard disk (until max)
    ;   They dont have to be bootable
    ;Then go back and look for other partitions partitions. 
    ;   Add each other primary or logical ptn (until max)
    ;Then finish with removable devices. First two devs become A: and B: resp.
    ;Use r8 as device counter
    lea rdi, [rbp + msdDriver.msdBPBblks]    ;Prepare to write BPBs
    cmp byte fs:[numFixDrv], 0 ;Do we have any fixed drives?
    jz .remInit ;No? Go to removables
    mov r8, 2   ;Device number 2 = C:
    mov dl, 80h ;Start with HDD 0
.primary:
    cmp byte fs:[numLogDrv], 3  ;Are we at maximum devices (A: B: reserved)?
    je .remInit
    xor ecx, ecx    ;Sector 0
    call .initReadSector ;Sets rbx to msdtempbuffer
    jc .primaryEpilog
    ;Here, check MBR or BPB
    cmp word [rbx + 1FEh], 0AA55h
    jne .primaryEpilog  ;Not a valid MBR or BPB, skip disk
    ;Now check if BPB or MBR
    mov al, byte [rbx]  ;rbx is pointed to the temp buffer by initreadsector
    mov ah, byte [rbx + 2]
    cmp ax, 090EBh  ;WinDOS and SCP compatible (always generate short jmp)
    je .primaryEpilog ;Will process these during Extended Ptn search
    ;Here with a MBR. Search the MBR for the first Primary Partition
    ;Look for CHS/LBA types (01h/04h/06h/0Bh/0Ch/0Eh) for primary ptns
    add rbx, mbr.mbrEntry1 ;Point rbx to mbrEntry1
    mov cl, 4
.checkPrimary:
    mov al, byte [rbx + mbrEntry.ptnType]
    cmp al, 01
    je .primaryFound
    cmp al, 04
    je .primaryFound
    cmp al, 06
    je .primaryFound
    cmp al, 0Bh
    je .primaryFound
    cmp al, 0Ch
    je .primaryFound
    cmp al, 0Eh
    je .primaryFound
    add rbx, mbrEntry_size  ;Goto next entry byte
    dec cl
    jz .primaryEpilog
    jmp short .checkPrimary
.primaryFound:
    ;Copy the first sector of this partition into memory
    mov ecx, dword [rbx + mbrEntry.lbaStart]    ;Get lba for volume start
    call .initReadSector
    jc .primaryEpilog
    ;Now verify this is a BPB
    mov al, byte [rbx]  ;rbx is pointed to the temp buffer by initreadsector
    mov ah, byte [rbx + 2]
    cmp ax, 090EBh  ;WinDOS and SCP compatible (always generate short jmp)
    jne .primaryEpilog   ;If not, skip
    ;Now copy data to internal tables
    mov rsi, rbx    ;Point rsi to the temp buffer
    mov ecx, bpbEx_size/8   ;Copy BPB
    push rdi
    rep movsq   ;Copy the BPB
    pop rsi ;Get the pointer to the copied bpb into rsi
    ;Store BIOS map value and BPBblk pointer in bpbTbl
    lea rbx, qword [rbp + msdDriver.msdBIOSmap + r8]
    ;Add device count to rbx to point to correct entry
    mov byte [rbx], dl  ;Store BIOS map value 
    lea rbx, qword [rbp + msdDriver.msdBPBTbl + 8*r8]
    mov qword [rbx], rsi
    inc r8  ;Goto next logical drive
    inc byte fs:[numLogDrv] ;Increment the number of valid drives we have
.primaryEpilog:
    inc dl  ;Goto next BIOS drive
    mov dh, dl
    and dh, 7Fh ;Clear bit 7
    cmp dh, byte fs:[numFixDrv]    ;Have we gone thru all hard drives?
    jne .primary    ;Whilst we have fewer, go back
.extended:
;We have gone through all the devices once
    ;cmp byte fs:[numLogDrv], 3  ;Are we at maximum devices (A: B: reserved)?
    ;je .remInit ;If yes, get removable devices
    ;mov dl, 80h ;Go back to hard drive 80h
    ;xor ecx, ecx    ;Get MBR back
    ;call .initReadSector
    ;Now we eventually search MBR for a FAT extended partition
.remInit:
;Now handle removable devices, at least 2 rem. devs.
    mov r9, r8  ;Save number of next device in r9b
    xor dl, dl  ;Start with removable device 0
    mov r8b, dl ;Once r8b becomes 2, go past the disk drives
    ;rdi points to the space for the subsequent bpb's
.removables:
    xor ecx, ecx    ;Read sector 0
    call .initReadSector
    jc .removableEpilogue   ;Goto next device
    ;Now verify this is a BPB
    mov al, byte [rbx]  ;rbx is pointed to the temp buffer by initreadsector
    mov ah, byte [rbx + 2]
    cmp ax, 090EBh  ;WinDOS and SCP compatible (always generate short jmp)
    jne .removableEpilogue   ;If not, skip
    ;Now copy data to internal tables
    mov rsi, rbx    ;Point rsi to the temp buffer
    mov ecx, bpbEx_size/8   ;Copy BPB
    push rdi
    rep movsq   ;Copy the BPB
    pop rsi ;Get the pointer to the copied bpb into rsi
    ;Store BIOS map value and BPBblk pointer in bpbTbl
    lea rbx, qword [rbp + msdDriver.msdBIOSmap + r8]
    ;Add device count to rbx to point to correct entry
    mov byte [rbx], dl  ;Store BIOS map value 
    lea rbx, qword [rbp + msdDriver.msdBPBTbl + 8*r8]
    mov qword [rbx], rsi
    inc r8  ;Goto next logical drive
    inc byte fs:[numLogDrv] ;Increment the number of valid drives we have    
.removableEpilogue:
    inc dl  ;Goto next BIOS device now
    cmp dl, byte fs:[numRemDrv] ;Are we past last rem dev?
    je .end
    cmp r8, 2 ;Are we back at drive C: ?
    je .re0
    add r8b, r9b    ;Add the number of fixed disk volumes
.re0:
    cmp r8b, 5  ;Are we at logical device 5 (F:, not supported)?
    jb .removables
.end:
    cmp byte fs:[numRemDrv], 1  ;Do we have only 1 removable device?
    je .singleRemDev
    ret
.singleRemDev:
    ;Copy Drive A: BPB pointer and BIOS map data for Drive B:
    lea rbx, qword [rbp + msdDriver.msdBIOSmap]
    mov dl, byte [rbp + msdDriver.msdBIOSmap]   ;Get drive A: BIOS map
    mov byte [rbx + 1], dl  ;Store in byte for Drive B:
    lea rbx, qword [rbp + msdDriver.msdBPBTbl] 
    mov rdx, qword [rbx]    ;Get BPB pointer of Drive A:
    mov qword [rbx + 8], rdx    ;Store in qword for Drive B:
    inc byte fs:[numLogDrv] ;Gotta register the phantom drive!
    ret
.initReadSector:
;Called with sector number in rcx and BIOS device number in dl
    mov ah, 82h ;Read
    mov al, 1   ;One sector
    lea rbx, qword [rbp + msdTempBuffer]  ;Into temporary buffer
    int 33h
    ret