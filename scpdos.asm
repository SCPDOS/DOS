[map all scpdos.map]
[DEFAULT REL]
BITS 64
%include "driverStruc.inc"
%include "fatStruc.inc"
%include "dosStruc.inc"

Segment dSeg nobits align=1 
    dosSegPtr   resq 1    ;Pointer to the data Segment itself
    bootDrive   resb 1    ;The Int 33h device we booted from
    requestHdr  resb ioReqPkt_size   
    ;The device driver header with space for the largest possible packet
    sysVarsPtr  resq 1    ;Pointer to dpbHeadPtr, head of Sys Vars struc below
    mcbChainPtr resq 1    ;Pointer to the MCB chain
    dpbHeadPtr  resq 1    ;Pointer to the first DPB in the DPB chain
    sftHeadPtr  resq 1    ;Pointer to the first SFT header in SFT chain
    clockPtr    resq 1    ;Pointer to the current active CLOCK$ device header
    ;                    The last driver loaded with the CLOCK$ bit[3] set 
    conPtr      resq 1    ;Pointer to the current active CON device header 
    ;                    The last driver loaded with the STDIN bit[0] set
    maxBytesSec resw 1    ;Maximum number of bytes per sector (size of buffers)
    bufHeadPtr  resq 1    ;Pointer to the head of the disk buffer chain
    cdsHeadPtr  resq 1    ;Pointer to the head of the CDS array
    sfcbHeadPTr resq 1    ;Pointer to the head of the System FCB chain
    numSafeSFCB resw 1    ;Number of protected FCBs (y in FCBS=x,y)
    numMSDdrv   resb 1    ;Number of mass storage devices detected in system
    lastdrvNum  resb 1    ;Value of LASTDRIVE (default = 5) [Size of CDS array]
    numJoinDrv  resb 1    ;Number of Joined Drives
    nulDevHdr   resb drvHdr_size


    inDOS       resb 1    ;Inc on each DOS call, dec when leaving
    breakFlag   resb 1    ;If set, check for CTRL+C on all DOS calls
    defaultDrv  resb 1    ;Default, last accessed drive
    currentPSP  resq 1    ;Address of current PSP

    critStack   resq 41
    critStakTop resq 1
    IOStack     resq 199
    IOStakTop   resq 1
    DiskStack   resq 199
    DiskStakTop resq 1
    dSegLen     equ     $

Segment .text align=1
; We arrive here with the following values in the registers.
; rbx =  LBA of first Logical Block after SCP/BIOS
; dx  = Int 33h boot device number
; fs  = userbase pointer (pointer to first usable block of RAM)
    dw 0AA55h           ;Initial signature
    mov byte fs:[bootDrive], dl ;Save the boot drive in memory

    mov ecx, 0C0000100h ;Read FS MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in

    mov qword fs:[dosSegPtr], rdi 
    mov rbp, rdi    ;Save the start of dosSeg in rdx 
    add rdi, dSegLen ;Move destination past end of data area
    lea rsi, section.resSeg.start  ;Get RIP relative address to copy high
    mov ecx, 1000h
    rep movsq

;Modify the pointers in nData before putting them in the data area
    add qword [nData + drvHdr.nxtPtr], rbp
    add qword [nData + drvHdr.strPtr], rbp
    add qword [nData + drvHdr.intPtr], rbp
;Copy the Null driver to its location in Sysvars
    mov ecx, drvHdr_size
    lea rsi, qword [nData]
    lea rdi, qword [rbp + nulDevHdr]
    rep movsb   

    xchg bx, bx
    ;Open NUL
    lea rbx, qword [rbp + nulDevHdr + drvHdr.strPtr]    ;Get ptr to strat ptr
    mov rbx, qword [rbx]    ;Get strat ptr
    xor al, al
    call rbx

    ;Open CON
    mov rbx, conDriver
    lea rbx, qword [rbp+rbx]
    xor al, al
    call rbx

    ;Open Mass Storage
    mov rbx, msdDriver
    lea rbx, qword [rbp+rbx]
    xor al, al
    call rbx

    lea rbp, qword [startmsg]   ;Get the absolute address of message
    mov eax, 1304h
    int 30h
l1:
    xor ax, ax
    int 36h
    mov ah, 0Eh
    int 30h
    jmp short l1

startmsg db "Starting SCP/DOS...",0Ah,0Dh,0
nData:
    dq conHdr
    dw 08004h
    dq nulStrat
    dq nulIntr
    db "NUL     " ;Default NUL data

Segment resSeg follows=.text align=1 vfollows=dSeg valign=1 
;-----------------------------------:
;       Misc System routines        :
;-----------------------------------:
findLRUBuffer: 
;Finds least recently used buffer, links it and returns ptr to it in rbx
;Input: Nothing
;Output: rbx = Pointer to the buffer to use
    push rdx
    mov rbx, qword [bufHeadPtr]
    cmp qword [rbx + bufferHdr.nextBufPtr], -1  ;Check if 1st entry is last
    jne .flb1
    pop rdx
    ret
.flb1:
    mov rdx, rbx    ;Save a ptr to the previous buffer header
    mov rbx, qword [rdx + bufferHdr.nextBufPtr] ;Get next buffer header ptr
    cmp qword [rbx + bufferHdr.nextBufPtr], -1 ;Check if at LRU buffer
    jne .flb1   ;If not LRU, keep walking, else process
    mov qword [rdx + bufferHdr.nextBufPtr], -1  ;Make prev node the LRU node
    mov rdx, qword [bufHeadPtr]    ;Now copy old MRU buffer ptr to rdx
    mov qword [bufHeadPtr], rbx    ;Sysvars to point to new buffer
    mov qword [rbx + bufferHdr.nextBufPtr], rdx
    pop rdx
    ret

findDPB:
;Finds the DPB for a given drive
;Input:   dl = Drive number (0=A, 1=B etc...)
;Output: al = 00, rbx = Pointer to the DPB
;        al = -1, Failed, no DPB for device, rbx destroyed
    mov rbx, qword [dpbHeadPtr]
.fd1:
    xor al, al
    cmp byte [rbx + dpb.bDriveNumber], dl
    je .fd2
    mov rbx, qword [rbx + dpb.qNextDPBPtr]
    mov al, -1
    cmp rbx, -1 ;If rbx followed last item in list, no DPB exists for dl
    jne .fd1
.fd2:
    ret
;-----------------------------------:
;       File System routines        :
;-----------------------------------:
fatProc:
;-----------------------------------:
;        Interrupt routines         :
;-----------------------------------:
terminateProcess:   ;Int 40h
functionDispatch:   ;Int 41h Main function dispatcher
terminateHandler:   ;Int 42h
ctrlCHandler:       ;Int 43h
critErrorHandler:   ;Int 44h
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address
;ecx = Number of sectors to read (max 255 for now)
;rdx = Start LBA to read from
    movzx rax, al   ;Zero extend DOS drive number 
    mov al, byte [msdDriver.msdBIOSmap + rax] ;Get translated BIOS num into al
    xchg rax, rcx
    xchg rcx, rdx
    mov ah, 82h
    int 33h
    iretq
absDiskWrite:       ;Int 46h
    movzx rax, al   ;Zero extend DOS drive number 
    mov al, byte [msdDriver.msdBIOSmap + rax] ;Get translated BIOS num into al
    xchg rax, rcx
    xchg rcx, rdx
    mov ah, 83h
    int 33h
    iretq
terminateResident:  ;Int 47h
inDosHandler:       ;Int 48h
;Called when DOS idle
    iretq
fastOutput:         ;Int 49h
;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
passCommand:        ;Int 4Eh
multiplex:          ;Int 4Fh
;-----------------------------------:
;          Driver routines          :
;-----------------------------------:
drivers:
conHdr:
    dq auxHdr
    dw 0813h
    dq commonStrat
    dq conDriver
    db "CON     "
auxHdr:
    dq prnHdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "AUX     "
prnHdr:
    dq clkHdr
    dw 0A040h
    dq commonStrat
    dq lpt1Hdr
    db "PRN     "
clkHdr:
    dq msdHdr
    dw 08008h
    dq commonStrat
    dq clkDriver
    db "CLOCK$  "
msdHdr:
    dq com1Hdr
    dw 00800h   ;Once Generic IO implemented, change to 00840h
    dq commonStrat
    dq msdDriver
    db 0,0,0,0,0,0,0,0
com1Hdr:
    dq com2Hdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "COM1    "
com2Hdr:
    dq com3Hdr
    dw 08000h
    dq commonStrat
    dq com2Intr
    db "COM2    "
com3Hdr:
    dq com4Hdr
    dw 08000h
    dq commonStrat
    dq com3Intr
    db "COM3    "
com4Hdr:
    dq lpt1Hdr
    dw 08000h
    dq commonStrat
    dq com4Intr
    db "COM4    "
lpt1Hdr:
    dq lpt2Hdr
    dw 0A040h
    dq commonStrat
    dq lptDriver
    db "LPT1    "
lpt2Hdr:
    dq lpt3Hdr
    dw 0A040h
    dq commonStrat
    dq lptDriver
    db "LPT2    "
lpt3Hdr:
    dq -1
    dw 0A040h
    dq commonStrat
    dq lptDriver
    dq "LPT3    "

commonStrat:
;DOS calls this function with rbx=Ptr to request header
    mov qword [reqHdrPtr], rbx
    ret
reqHdrPtr  dq 0    ;Where the default device drivers store the ReqPtr

nulStrat:
    mov word [rbx + drvReqHdr.status], 0100h    ;Set done bit directly
nulIntr:
    ret

conDriver:
    push rax
    push rbx
    mov rbx, qword [reqHdrPtr]
    mov al, byte [rbx + drvReqHdr.cmdcde]
    test al, al
    jz conInit
    cmp al, 4
    jz conRead
    cmp al, 5
    jz conNondestructiveRead
    cmp al, 6
    jz conExit
    cmp al, 7
    jz conFlushInputBuffers
    cmp al, 8
    jz conWrite
    cmp al, 9
    jz conWrite
;All other cases fall through here
conExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rbx
    pop rax
    ret
conInit:    ;Function 0
    push rdx
    ;Flush keyboard buffer
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
    pop rdx
    jmp short conExit
conIORead:
    mov word [rbx + drvReqHdr.status], 8003h    ;Error, unknown command!
    jmp short conExit
conRead:    ;Function 4
    push rdi
    push rcx
    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cr1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
    xor eax, eax
    int 36h
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short .cr1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rdi
    jmp short conExit
conNondestructiveRead:  ;Function 5
    mov ah, 01h     ;Get key if exists
    int 36h
    jz .cnr           ;If zero clear => no key, go forwards
    ;Keystroke available
    mov byte [rbx + nonDestInNoWaitReqPkt.retbyt], al   ;Move char in al
    jmp short conExit
.cnr: ;No keystroke available
    mov word [rbx + nonDestInNoWaitReqPkt.status], 0300h   ;Set busy bit
    jmp short conExit
conFlushInputBuffers:   ;Function 7
    mov ah, 01      ;Get buffer status
    int 36h
    jz conExit      ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short conFlushInputBuffers
conWrite:   ;Function 8 and 9
    push rsi
    push rcx
    mov rsi, qword [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    int 49h ;Fast print char
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rsi
    jmp conExit

clkDriver:

comDriver:
com1Intr:
    mov byte [comDevice], 0
    jmp short comIntr
com2Intr:
    mov byte [comDevice], 1
    jmp short comIntr
com3Intr:
    mov byte [comDevice], 2
    jmp short comIntr
com4Intr:
    mov byte [comDevice], 3
comIntr:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    mov rbx, qword [reqHdrPtr]
    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 4
    jz comRead
    cmp al, 5
    jz comNondestructiveRead
    cmp al, 8
    jz comWrite
    cmp al, 9
    jz comWrite
;All other cases fall through here
comExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
comRead:
    push rdi
    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cr1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
    mov eax, 02h    ;Recieve 
    mov dx, word [comDevice]    ;Get transacting com device
    int 34h ;Recieve Char
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short .cr1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rdi
    jmp short comExit
comNondestructiveRead:
    mov word [rbx + nonDestInNoWaitReqPkt.status], 0200h    ;Set busy bit 
    jmp short comExit
comWrite:
 mov rsi, qword [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    mov ah, 01h ;Move function number into ah
    mov dx, word [comDevice]
    int 34h ;Transmit char
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    jmp short comExit
comDevice   db 0

lptDriver:    ;Drivers for LPT 1, 2, 3
    push rdi
    mov rdi, qword [reqHdrPtr]
    mov word [rdi + drvReqHdr.status], 0100h    ;Done bit set
    pop rdi
    ret

msdDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    mov rbx, qword [reqHdrPtr]  ;Get the ptr to the req header in rbx
    mov al, byte [rbx + drvReqHdr.cmdcde]   ;Get command code in al
    cmp al, 24  ;Check cmd num is valid
    ja .msdError
    test al, al
    jz .msdInit
    cmp al, 01
    jz .msdMedChk
    cmp al, 02
    jz .msdBuildBPB
    cmp al, 03
    jz .msdIOCTLRead
    cmp al, 04
    jz .msdRead
    cmp al, 08
    jz .msdWrite
    cmp al, 09
    jz .msdWriteVerify
    cmp al, 12
    jz .msdIOCTLWrite
    cmp al, 13
    jz .msdDevOpen
    cmp al, 14
    jz .msdDevClose
    cmp al, 15
    jz .msdRemovableMedia
    cmp al, 19
    jz .msdGenericIOCTL
    cmp al, 23
    jz .msdGetLogicalDev
    cmp al, 24
    jz .msdSetLogicalDev
.msdError:
.msdDriverExit:
    or word [rbx + drvReqHdr.status], 0100h ;Set done bit
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.msdInit:            ;Function 0
    int 31h ;Get number of Int 33h devices in r8b
    movzx r8, r8b   ;Keeps real count
    mov eax, r8d
    cmp al, 1
    ja .mi1
    inc al ;Make it two
.mi1:
    mov edx, 5
    cmp eax, edx
    cmova eax, edx  ;If num of drives is greater than 5, consider only first 5
    mov byte [msdHdr + drvHdr.drvNam], al ;Save num of drvs in drvr hdr
    mov byte [rbx + initReqPkt.numunt], al ;And in req packet
    add byte [numMSDdrv], r8b ;Add the true number of devices to total
    xor ebp, ebp    ;Use bpl as device counter, cmp to r8b
    lea rdi, qword [.msdBPBblks]
    push rbx
.mi2:
    mov edx, ebp
    lea rbx, qword [driverDataPtr]  ;Get effective address of scratch space
    xor ecx, ecx    ;Sector 0
    mov eax, 8201h       ;Read 1 sector
    int 33h
    jc .msdInitError

    lea rsi, qword [driverDataPtr]  ;Point to start of data
    mov ecx, bpbEx_size/8
    rep movsq   ;Move the BPB data into the right block

    inc ebp
    cmp rbp, r8 ;Have we written the BPB for all physical drives?
    jne .mi2  ;No? Go again

    lea rdi, qword [.msdBPBTbl]  ;Point to start of table
    lea rdx, qword [.msdBPBblks]
.mi3:
    mov qword [rdi], rdx   ;Move the block entry ptr to rdi
    add rdx, bpbEx_size      ;Make rdx point to the next block entry
    dec ebp
    jnz .mi3  ;If not zero yet, go again

    pop rbx
    lea rdx, qword [.msdBPBTbl]  ;Get far pointer 
    mov qword [rbx + initReqPkt.optptr], rdx  ;Save ptr to array
    lea rdx, qword [driverDataPtr]
    mov qword [rbx + initReqPkt.endptr], rdx    ;Save free space ptr
    jmp .msdDriverExit
.msdInitError:
    pop rbx
    jmp .msdDriverExit
.msdMedChk:          ;Function 1
;Once the BIOS function is implmented that reads the changeline, use that!
;For BIOSes that dont support the changeline, the following procedure will 
; suffice.
    movzx rax, byte [rbx + mediaCheckReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]    ;Translate unitnum to BIOS num
    test dl, 80h    ;If it is a fixed disk, no change!
    jnz .mmcNoChange
;Now we test Media Descriptor
    mov dl, byte [rbx + mediaCheckReqPkt.medesc]    ;Media descriptor
    mov rdi, qword [.msdBPBTbl + 8*rax]
    mov rdi, qword [rdi]    ;Dereference rdi
    cmp byte [rdi + bpb32.media], dl    ;Compare media descriptor bytes
    je .mmcUnsure
.mmcChange: ;Fail safe, always assume the device has changed
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    mov qword [rbx + mediaCheckReqPkt.desptr], .msdDefLabel ;Temp, ret def label
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    jmp .msdDriverExit
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    jmp .msdDriverExit

.msdBuildBPB:        ;Function 2
    mov rsi, rbx
    movzx rax, byte [rsi + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer
    xor ecx, ecx    ;Read Sector 0
    mov eax, 8201h  ;LBA Read 1 sector
    int 33h
    jc .mbbpbError
    xchg rbx, rsi    ;Transf Buf(rbx) <-> ReqHdr(rsi)
    movzx rax, byte [rbx + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov rdi, qword [.msdBPBTbl + 8*rax] ;Get pointer to pointer to buffer
    mov rdi, qword [rdi] ;Dereference to get pointer to buffer 
    mov qword [rbx + bpbBuildReqPkt.bpbptr], rdi ;rdi -> final bpb resting place
    mov ecx, bpbEx_size/8
    rep movsq   ;Move the BPB data into the right space
    jmp .msdDriverExit
.mbbpbError:
.msdIOCTLRead:       ;Function 3, returns done
    jmp .msdDriverExit
.msdRead:            ;Function 4
    mov rbp, rbx
    mov ah, 82h ;LBA Read Sectors
    call .msdBlkIOCommon
    mov rbx, rbp
    jmp .msdDriverExit
.msdWrite:           ;Function 8
    mov rbp, rbx
    mov ah, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    mov rbx, rbp
    jmp .msdDriverExit
.msdWriteVerify:     ;Function 9, writes sectors then verifies them
    mov rbp, rbx
    mov ah, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    mov ah, 84h ;LBA Verify Sectors
    call .msdBlkIOCommon
    mov rbx, rbp
    jmp .msdDriverExit
.msdIOCTLWrite:      ;Function 12, returns done
    jmp .msdDriverExit
.msdDevOpen:         ;Function 13
    movzx rax, byte [rbx + openReqPkt.unitnm]
    inc byte [.msdHdlCnt + rax]  ;Inc handle cnt for given unit
    jmp .msdDriverExit
.msdDevClose:        ;Function 14
    movzx rax, byte [rbx + closeReqPkt.unitnm]
    dec byte [.msdHdlCnt + rax]  ;Dec handle cnt for given unit
    jmp .msdDriverExit
.msdRemovableMedia:  ;Function 15
    movzx rax, byte [rbx + remMediaReqPkt.unitnm]
    mov al, byte [.msdBIOSmap + rax]    ;Get BIOS number
    test al, 80h
    jz .msdDriverExit   ;If removable, busy bit is clear
    mov word [rbx + remMediaReqPkt.status], 20h ;Set Busy bit
    jmp .msdDriverExit
.msdGenericIOCTL:    ;Function 19
    jmp .msdDriverExit
.msdGetLogicalDev:   ;Function 23
    mov al, byte [.msdCurDev]
    mov byte [rbx + getDevReqPkt.unitnm], al
    jmp .msdDriverExit
.msdSetLogicalDev:   ;Function 24
    mov al, byte [rbx + getDevReqPkt.unitnm]
    mov byte [.msdCurDev], al
    jmp .msdDriverExit

.msdBlkIOCommon:  ;Does block IO
;Called with rbp containing old rbx value and ah with function number
;Error handled by caller
    movzx rax, byte [rbp + ioReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req
    mov rcx, qword [rbp + ioReqPkt.strtsc]  ;Get start sector
    mov al, byte [rbp + ioReqPkt.tfrlen]    ;Get number of sectors, max 255
    mov rbx, qword [rbp + ioReqPkt.bufptr]  ;Get Memory Buffer
    int 33h
    ret

.msdDefLabel db "NO NAME ",0 ;Default volume label
;LASTDRIVE default is 5
.msdCurDev   db 0  ;The device to be referenced by the driver is saved here!
.msdBIOSmap  db 5 dup (0)    ;Translates DOS drive number to BIOS number
.msdHdlCnt   db 5 dup (0)    ;Keeps a count of open handles to drive N
.msdBPBTbl   dq 5 dup (0)    ;BPB pointer table to be returned
.msdBPBblks  db 5*bpbEx_size dup (0) ;Max 5 bpb records of exFAT bpb size

driverDataPtr: