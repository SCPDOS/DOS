; We arrive here with the following values in the registers.
; rbx =  LBA of first Logical Block after SCP/BIOS
; dx  = Int 33h boot device number
; fs  = userbase pointer (pointer to first usable block of RAM)

    dw 0AA55h           ;Initial signature
    movzx r15, dl       ;Save the drive letter in r15
    mov r14, rbx        ;Save next sector number
    lea rsi, sysInitldr
    mov edi, 600h   ;Hardcoded address, 600h
    mov ecx, 512/8      ;TMP: DOS boot device MUST HAVE 512 byte sectors.
    rep movsq   ;Copy over
    mov eax, 600h   ;Push the new address to go to
    push rax
    ret ;Jump to this value (600h + whatever the size here is)
sysInitldr:
;Now the tough part, load DOS to 800
    mov esi, 10h    ;Use as a loop counter
.read:
    mov dl, r15b    ;Get Drive number
    mov rbx, 800h   ;Load at next 512 byte marker
    mov ecx, r14d   ;Get this sector LBA (first sector after BIOS)
    inc ecx         ;and want the next sector (DOS AND BIOS MUST BE CONTIGUOUS)
    mov al, 65h     ;Load a large number of sectors (about 51.7k)
    mov ah, 82h     ;Read LBA
    int 33h
    jc .readFail
    push qword 800h
    ret   ;No error? Yay, DOS loaded.
.readFail:
    dec esi
    jnz .read
    lea rbp, .msg   ;Print error message
    mov eax, 1304h
    int 30h
    int 38h ;If an error, fall into SYSDEBUG
.msg db "SCP/DOS Load Error",0Ah,0Dh,0
    db 100h-($-$$) dup 00h ;Fill with nulls  
tempPSP:    ;Here to allow the loader to use Int 41h once it is loaded high
    ;Store space for a static PSP
    db 100h dup 00h
;END OF FIRST SECTOR!!
;DOS SYSINIT BEGINS HERE
;First move the alignment of the DOSSEG to 4Kb
initBegin:
    cld ;Ensure all writes are done the right way firstly!
    mov ecx, 0C0000100h ;Read FS MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in
    mov rsi, rdi        ;Save userbase in rsi temporarily
    and rdi, ~0FFFh
    add rdi, 1000h      ;Make this pointer 4Kb aligned!
    mov eax, edi
    mov rdx, rdi
    shr rdx, 20h
    wrmsr   ;Write the new value to FS MSR
;------------------------------------------------;
;              Connect Debugger                  ;
;------------------------------------------------;
%if DEBUG
;Only connect if in debug mode
    mov eax, 0C501h ;Connect debugger
    int 35h
%endif
;------------------------------------------------;
;           Sanitise the data area               ;
;------------------------------------------------;
; This call initialises ALL fields in the DOS    ;
; data area with 0's. Thus any fields, which     ;
; need to be initialised with a 0 value, are     ;
; initialised for free.                          ;
;------------------------------------------------;
    mov ecx, dSegLen
    xor al, al
    push rdi    ;Temp save rdi on the stack
    rep stosb
    pop rdi
;------------------------------------------------;
;          Start saving Basic DOS data           ;
;------------------------------------------------;
    mov byte fs:[bootDrive], r15b ;Save the boot drive in memory
;Copy DOS to its final resting place
    mov qword fs:[dosSegPtr], rdi 
    mov qword fs:[biosUBase], rsi
    mov rbp, rdi    ;Save the start of dosSeg in rbp
    add rdi, dSegLen ;Move destination past end of data area
    lea rsi, section.resSeg.start  ;Get RIP relative address to copy high
    mov ecx, 1000h
    rep movsq

    int 31h ;Get number of Int 33h devices in r8b
    shr r8, 8   ;Isolate bytes 1 and 2 of r8
    mov ax, r8w
    mov byte fs:[numRemDrv], ah    ;Save num of phys int 33h rem drives
    mov byte fs:[numFixDrv], al    ;Save number of physical hard drives
    mov byte fs:[lastdrvNum], 5    ;Last drive is by default 5
    mov byte fs:[numFiles], 5      ;Default 20 files, at start 5
    mov byte fs:[numBuffers], 1    ;Default 30 buffers, at start 1 
    mov word fs:[shareCount], 3    ;Retry the repeat 3 times before failing
    mov word fs:[shareDelay], 1    ;Go through one multiple of countdown loop
    ;If no detected Int 33h devices, halt 
    shr r8, 2*8
    test r8b, r8b
    jz errorInit
;------------------------------------------------;
;          Add additional page tables            ;
;------------------------------------------------;
;This will allow for up to 64Gb of addressible space
    mov rdi, rbp
    ;Each entry is a 2Mb (200000h) multiple from 4Gb (100000000h)
    mov ecx, dosAPTsize/8   ;This many entries as qwords
    push rdi
    mov rax, 100000000h | 83h ;Make each pde 2Mb, present and r/w
pdtLoop:
    stosq
    add rax, 200000h
    dec ecx
    jnz pdtLoop
    pop rax ;Get the pointer back to the top of the memory area in rax
;Now we add every 4kb page to the page directory pointer table
;15 4kb pages to consider
    mov rdi, cr3    ;Get Page level 4 table pointer
    mov rdi, qword [rdi] ;Go to next level
    and rdi, ~0FFh  ;Strip bottom two nybbles
    add rdi, 4*8    ;Go to 4th entry
    mov ecx, 60
    or rax, 3h      ;present and r/w
pdptLoop:
    stosq
    add rax, 1000h  ;Goto next 4kb page
    dec ecx
    jnz pdptLoop
    mov rdi, cr3
    mov cr3, rdi
;------------------------------------------------;
;                   MCB inits                    ;
;------------------------------------------------;
mcbInit:
    mov eax, 0E820h
    int 35h
    ;rax has pointer to USERBASE, rsi has pointer to memory map
    call .mcbFindAddress
    jnc .mcbi1  ;If found, proceed
    ;Here, we try use E801h
    mov eax, 0E801h
    int 35h
    movzx eax, ax   ;ax has 1Kb blocks from userbase to ISA hole (if pres)
    movzx ebx, bx   ;cx has 64Kb blocks from 16Mb to PCI hole
    test eax, eax
    jz .worst
    shl eax, 9      ;Multiply by 9 to get number of bytes
    shl ebx, 16     ;Multiply by 16 to get number of bytes
    mov dword fs:[loProtMem], eax
    mov dword fs:[hiProtMem], ebx
    jmp mcbBuild
.worst:
    ;Get USERBASE pointer and subtract it from 2Mb
    mov eax, 200000h
    mov rbx, qword fs:[biosUBase]   ;Get userbase
    sub eax, ebx
    mov dword fs:[loProtMem], eax  ;The leftover goes here
    jmp mcbBuild 
.mcbi1:
    mov rdx, qword [rax]    ;Save the userbase in rdx
    mov rbx, 100000001h ;Valid entry signature
    cmp qword [rax + 16], rbx ;If entry is marked as invalid, fail boot
    jne .mcbFail
    mov rax, qword [rax + 8]    ;Get arena size in rax
    ;PCI hole always exists so this value will always be a dword
    mov dword fs:[loProtMem], eax
    mov rbx, rdx    ;Get userbase into rbx
    add rbx, rax    ;Check if it goes above 16Mb?
    cmp rbx, 1000000h  
    ja .skipISA
;Here we deal with ISA hole issues
    mov eax, 0E820h
    int 35h
    mov rax, 1000000h
    call .mcbFindAddress
    jc mcbBuild  ;If address doesnt exist, must not be any memory above 16MB
    mov rbx, 100000001h ;Valid entry signature
    cmp qword [rax + 16], rbx ;If entry is marked as invalid, ignore domain
    jne mcbBuild  
    mov rbx, qword [rax + 8]
    mov dword fs:[hiProtMem], ebx   ;Save data 
.skipISA:
    mov eax, 0E820h
    int 35h
    mov rax, 100000000h ;4Gb boundary
    call .mcbFindAddress
    jc mcbBuild    ;If no memory above 4Gb, proceed as normal
    mov rbx, 100000001h ;Valid entry signature
    cmp qword [rax + 16], rbx ;If entry is marked as invalid, ignore domain
    jne mcbBuild   
    mov rbx, qword [rax + 8]
    ;If this size is above 60Gb, store 60Gb as this is max long arena size!
    mov rcx, 0F00000000h    ;60Gb value
    cmp rbx, rcx
    cmova rbx, rcx  ;Move the value of rcx into rbx IF it is above
    mov qword fs:[longMem], rbx   ;Save data 
    jmp mcbBuild
.mcbFindAddress:
;Takes an address in rax and tries to find the 24 byte entry in the memory map
;Entry: rax = Address of arena to search for
;       rsi = Pointer to memory map
;       ecx = Number of 24 byte entries
;Exit:  CF=NC : rax = Pointer to 24 byte entry 
;       CF=CY : value in rax not found
    push rsi
    push rcx
    push rax
.mfa0:
    cmp rax, qword [rsi]
    je .mcbAddressFound
    add rsi, 24 ;Goto next entry
    dec ecx
    jns .mfa0
.mcbNoAddressFound: ;If ecx changes sign, we have used up all entries
    pop rax
    pop rcx
    pop rsi
    stc
    ret
.mcbAddressFound:
    mov rax, rsi    ;Save pointer to entry in rax
    pop rcx ;Pop old rax value off
    pop rcx
    pop rsi
    clc
    ret
.mcbFail:
    lea rbp, mcbFailmsg
    mov eax, 1304h
    int 30h
    jmp errorInit

mcbBuild:
;Actually build the MCB chain here
;Start by computing the difference between userbase and DOS area
;This value needs to be subtracted from loProtMem to get free memory
    mov rbx, qword fs:[biosUBase]
    lea rsi, qword [rbp + dosMCB]  ;Get the fs relative address of this ptr
    push rsi    ;Save ptr
    add rsi, mcb.program    ;Point to free space
    sub rsi, rbx    ;Get difference from userbase and first byte after DOS
    sub dword fs:[loProtMem], esi  ;Hide DOS data and code segs
    pop rbx
    mov byte [rbx + mcb.marker], mcbMarkEnd  ;Mark as end of chain
    mov qword [rbx + mcb.owner], mcbOwnerDOS
    mov esi, dword fs:[loProtMem]
    shr esi, 4  ;Shift down by a nybble to get paragraphs
    mov dword [rbx + mcb.blockSize], esi
    mov qword fs:[mcbChainPtr], rbx ;Save pointer

    ;Now check the hiProtMem count. If it is 0, skip ISA hole computations.
    cmp dword fs:[hiProtMem], 0
    jz .skipISA
    ;Here if an ISA hole exists, place a MCB around it
    sub dword [rbx + mcb.blockSize], (mcb_size>>4)    
    ;Remove one MCB worth of space from alloc
    xor ecx, ecx
    mov ecx, dword [rbx + mcb.blockSize]
    add ecx, (mcb_size >> 4)    ;Add one as the block starts AFTER the MCB
    shl ecx, 4  ;Convert from paragraphs
    mov byte [rbx + mcb.marker], mcbMarkCtn  ;Change marker in anchor
    add rbx, rcx   ;Point rbx to next space
    mov byte [rbx + mcb.marker], mcbMarkCtn
    mov qword [rbx + mcb.owner], mcbOwnerHole
    mov rcx, 1000000h   ;Move 16Mb in rcx
    mov rax, rbx    ;Get mcb pointer in rax
    add rax, mcb_size
    sub rcx, rax    ;Take their difference
    shr ecx, 4
    mov dword [rbx + mcb.blockSize], ecx    ;Save the difference
    shl ecx, 4  ;Get bytes again
    add rbx, mcb_size
    add rbx, rcx
    ;RBX should now be at 16Mb
    mov byte [rbx + mcb.marker], mcbMarkEnd
    mov qword [rbx + mcb.owner], mcbOwnerFree
    mov ecx, dword fs:[hiProtMem]
    shr ecx, 4  ;Get paragraphs
    sub ecx, (mcb_size>>4)  ;Reserve space for one mcb
    mov dword [rbx + mcb.blockSize], ecx
.skipISA:
    ;Now check the longMem count. If it is 0, skip PCI hole computations.
    ;rbx points to a block with "Z" marker
    cmp dword fs:[longMem], 0
    jz .exit
    ;Add PCI hole MCB
    sub dword [rbx + mcb.blockSize], (mcb_size>>4)
    ;Remove one MCB worth of space from alloc
    xor ecx, ecx
    mov ecx, dword [rbx + mcb.blockSize]
    add ecx, (mcb_size >> 4)    ;Add one as the block starts AFTER the MCB
    shl ecx, 4  ;Get bytes
    mov byte [rbx + mcb.marker], mcbMarkCtn ;Change marker in prev MCB
    add rbx, rcx   ;Point rbx to next space
    mov byte [rbx + mcb.marker], mcbMarkCtn
    mov qword [rbx + mcb.owner], mcbOwnerHole
    mov rcx, 100000000h   ;Move 4Gb in rcx
    mov rax, rbx    ;Get mcb pointer in rax
    add rax, mcb_size
    sub rcx, rax    ;Take their difference
    shr ecx, 4
    mov dword [rbx + mcb.blockSize], ecx    ;Save the difference
    shl ecx, 4  ;Get bytes again
    add rbx, mcb_size
    add rbx, rcx
    ;RBX should now be at 4Gb
    mov byte [rbx + mcb.marker], mcbMarkEnd
    mov qword [rbx + mcb.owner], mcbOwnerFree
    mov rcx, qword fs:[longMem]
    shr rcx, 4
    sub ecx, (mcb_size>>4)  ;Reserve space for one mcb
    mov dword [rbx + mcb.blockSize], ecx
.exit:
    ;The last arena doesn't need to reserve space for one more MCB
    add dword [rbx + mcb.blockSize], (mcb_size>>4)
;------------------------------------------------;
;          Kernel inits and adjustments          ;
;------------------------------------------------;
;Adjust Interrupt Entries Int 00h-15h
adjExceptions:
    xor bl, bl
    mov eax, 0F007h ;Get the descriptor
    int 35h
    xor ecx, ecx    ;Start from interrupt 00h
    lea rdi, exceptData
    mov esi, eax    ;Move segment selector info to esi
.ai0:
    mov eax, 0F008h ;Set the descriptor
    mov rbx, qword [rdi]    ;Get address pointed to by rdi
    add rbx, rbp            ;Add the relocated base to rbx
.ai1:
    int 35h
    add rdi, 8
    inc ecx
    cmp ecx, 21
    jne .ai0
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
    cmp ecx, 50h
    jne .ai0
;++++++++++++++++++++++++++++++++++++++++++++++++;
;    DOS INTERRUPTS CAN BE USED FROM HERE ON     ;
;++++++++++++++++++++++++++++++++++++++++++++++++;
    %if DEBUG
debugPopUpMsg:
    push rbx
    push rbp
    push rcx
    push rdx
    mov ecx, 53 ;A large number of lines
    xor edx, edx    ;COM 1
.cls:
    mov eax, 010Ah ;Transmit Line feed
    int 34h
    dec ecx
    jnz .cls
    lea rbx, qword [debPrintNullString + rbp]
    lea rbp, .msg
    call rbx
    jmp short .exit
.msg:   db 0Ah,0Dh,"SCP/DOS Kernel Debugger Connected on COM1:2400,n,8,1",0Ah,0Dh,0
.exit:
    pop rdx
    pop rcx
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
;NUL opened
;Open CON
conInit:    ;Rather than keeping this resident... do it here
;Start by hooking int 3Bh and int 49h as part of the CON driver
    lea rdx, qword [rbp + fastOutput]
    mov eax, 2549h
    int 41h ;Hook int 49h (fast CON output)
    lea rdx, qword [rbp + ctrlBreak]
    mov eax, 253Bh
    int 41h ;Hook int 3Bh
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
    mov qword fs:[vConPtr], rax

    ;Fix the ext ESC function handler address
    lea rax, qword [rbp + editKeys]
    mov qword fs:[extKeyFunc], rax

;CLOCK init prock
;Set the time and date using the RTC (if present)
clkInit:
   ;Save ptr to ClkHdr in Sysvars first
    lea rax, qword [rbp + clkHdr]
    mov qword fs:[clockPtr], rax
;CH - Hours, in BCD
;CL - Minutes, in BCD
;DH - Seconds, in BCD
    mov ah, 02  ;Read RTC Time
    int 3Ah
    jc .clkEnd  ;if error, just exit
    ;Now set the driver time. Convert From BCD to Decimal
    movzx eax, ch   ;Hours
    call .bcd2Bin
    mov ch, al
    movzx eax, cl     ;Mins
    call .bcd2Bin
    mov cl, al
    movzx eax, dh   ;Seconds
    call .bcd2Bin
    xchg ah, al ;Move seconds to ah, and 0 to al (hseconds)
    mov edx, eax
    mov ah, 2Dh ;DOS Set Time
    int 41h
;Now get the date from RTC
;CL - Year, in BCD
;DH - Month, in BCD
;DL - Day, in BCD
    mov ah, 04  ;Read RTC Date
    int 3Ah
    jc .clkEnd
;Year from RTC is assumed to be after 2000 (coz, you know... its 2022)
    movzx eax, cl   ;Convert Year to Binary
    call .bcd2Bin
    movzx ecx, al   ;Zero extend to ecx (because ch needs to be 0)
    add ecx, 20  ;Add 20 years to turn it to a year offset from 1980
    movzx eax, dh   ;Get Months
    call .bcd2Bin
    mov dh, al  
    movzx eax, dl   ;Get day
    call .bcd2Bin
    mov dl, al
    mov ah, 2Bh ;DOS Set Date
    int 41h
    jmp short .clkEnd
.bcd2Bin:
;Converts a packed BCD value in al (zx to eax) to a decimal value in eax
    push rcx
    mov ecx, eax
    and eax, 0Fh ;Delete the upper nybble from al
    and ecx, 0F0h    ;Isolate the second digit (high nybble)
    jecxz .b2bexit ;Exit if this is zero
    shr ecx, 4   ;Shift it to the low nybble
.b2blp:
    add al, 10  ;Otherwise, keep adding 10  
    dec ecx
    jnz .b2blp
.b2bexit:
    pop rcx
    ret
.clkEnd:
;------------------------------------------------;
;         Link DOS to temporary Buffer           ;
;------------------------------------------------;
tempBufferInit:
    lea rdi, qword [rbp + tmpBufHdr]
    mov qword fs:[bufHeadPtr], rdi  ;Save pointer to temp buffer "list"
    xor eax, eax
    dec rax
    stosq   ;.nextBufPtr, end of list
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
    call diskInit
    mov rdi, rbp ;Save rbp in rdi temporarily
    mov al, byte fs:[numPhysVol]
    test al, al ;If no media with valid filesystems were detected, stop boot
    jz errorInit
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
;                CDS array inits                 ;
;------------------------------------------------;
    mov ecx, 5    ;Use as a counter, build 5 CDS entries
    lea rdi, qword [rbp + cdsArray] ;Setup array
    call makeCDSArray
    jmp short initialCDSWritten ;Go past the function
makeCDSArray:
;Input: ecx = Size of array
;       rdi = Pointer to the start of the CDS array
;Ouput: rdi = first byte past the end of the table
    mov qword fs:[cdsHeadPtr], rdi
    mov rbx, qword fs:[dpbHeadPtr]
    mov eax, 005C3A41h      ;"A:\"+NULL char
.tempCDS:
    mov dword [rdi + cds.sCurrentPath], eax
    mov qword [rdi + cds.qDPBPtr], rbx
    mov dword [rdi + cds.dStartCluster], 0  ;Root dir for all!
    mov word [rdi + cds.wBackslashOffset], 2    ;Skip the X:
    xor edx, edx    ;Use edx for flags
    cmp rbx, -1 ;Is rbx an invalid DPB ptr?
    je .skipValidCDS
    mov edx, cdsValidDrive  ;If not, set drive to valid and...
    mov rbx, qword [rbx + dpb.qNextDPBPtr]  ;... go to next DPB
.skipValidCDS:
    mov word [rdi + cds.wFlags], dx ;Store the flags now
    inc eax ;Increment the drive letter
    add rdi, cds_size   ;Goto next array entry
    dec ecx
    jnz .tempCDS
    ret
initialCDSWritten:
;------------------------------------------------;
;     Set up general PSP areas and DOS vars      ;
;------------------------------------------------;
;Additional DOS Vars init and fixups
    mov byte fs:[errorDrv], -1   ;No error drive
    mov byte fs:[switchChar], "/"  ;Default switch char
    lea rdi, qword [rbp + extAsciiTbl]  ;Get the load of dflt extascii tbl
    mov qword fs:[ctryTbl + countryStruc.mapaddr], rdi ;Store in country table

;Server Table setup
    lea rdi, serverDispTbl  ;Get pointer to table
    mov qword [serverDispTblPtr], rdi   ;Store to use

;Set network machine name to... nothing!
    lea rdi, qword [rbp + machineName]
    mov ecx, 10h    ;16 chars long
    mov al, SPC ;Space char
    rep stosb   ;Fill with space chars


;Patch Data Table init
    lea rdi, qword [rbp + critPtchTbl]
    lea rax, qword [rbp + dosCrit1Enter]
    stosq   ;Store this address and increment rdi by 8 to next tbl entry
    lea rax, qword [rbp + dosCrit1Exit]
    stosq
    lea rax, qword [rbp + dosCrit2Enter]
    stosq
    lea rax, qword [rbp + dosCrit2Exit]
    stosq

;Initial PSP Init
    lea rbx, qword [tempPSP]
    mov qword fs:[currentPSP], rbx    ;Save current PSP
    push rbx
    add rbx, psp.dta
    mov qword fs:[currentDTA], rbx    ;Save current DTA
    pop rbx
    mov word [rbx + psp.return], 040CDh ;DOS return function
    mov word [rbx + psp.unixEntry], 041CDh  ;Int 41h!
    mov byte [rbx + psp.unixEntry + 2], 0CBh    ;Return!
    mov qword [rbx + psp.allocSize], 0    ;Size of allocation (dummy value)
    mov qword [rbx + psp.parentPtr], rbx ;Save self as parent Process
    mov qword [rbx + psp.prevPSP], rbx  ;Save self as previous PSP
    lea rdi, qword [rbx + psp.jobFileTbl]
    mov rax, 0FFFFFF0200010101h  ;Store default handles in JFT
    stosq   ;8 bytes
    xor eax, eax
    push rax    ;Save 0 on the stack
    dec rax ;Turn into all -1 (free entry)
    stosq   ;16 bytes
    stosd   ;20 bytes
    pop rax ;Get 0 back
    mov qword [rbx + psp.envPtr], rax    ;No environment
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
;                                                ;
;   Note: Devices are opened AUX, CON then PRN   ;
;------------------------------------------------;
defaultFileHandles:
;Fill in the default file table entries
    lea rbx, qword [rbp + firstSftHeader]
    mov qword [rbx + sfth.qNextSFTPtr], -1  ;Last sfth in chain
    mov word [rbx + sfth.wNumFiles], 5      ;5 default files
    mov qword fs:[sftHeadPtr], rbx  ;Save ptr to this sft header in SysVars
    mov rdx, 2020202020202020h  ;Prepare the 8 spaces after the default names
;GOTO FIRST FILE 
    add rbx, sfth_size  ;Goto first driver
;Write AUX
    mov word [rbx + sft.wNumHandles], 1 ;Sysinit stdaux
    mov word [rbx + sft.wOpenMode], denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], charDevNoEOF| devCharDev 
    ;No EOF when reading from the device
    mov rax, qword [rbp + auxHdr]  ;Get pointer to AUX device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    lea rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
    ;11 chars in 8.3 name
    lea rsi, auxName
    mov ecx, 3
    rep movsb   ;Move the three bytes
    mov rax, rdx
    stosq   ;Eight spaces left to print
    mov rax, qword fs:[currentPSP]  ;Get current PSP
    mov qword [rbx + sft.qPSPOwner], rax
;GOTO NEXT ENTRY
    add rbx, sft_size   ;Goto next SFT
    ;Write CON
    mov word [rbx + sft.wNumHandles], 3 ;Sysinit stdin/out/err
    mov word [rbx + sft.wOpenMode], denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], charDevConIn|charDevConOut|charDevFastOut|charDevNoEOF|devCharDev 
    ;No EOF when reading from the device
    mov rax, qword fs:[vConPtr]  ;Get pointer to CON device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    lea rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
    ;11 chars in 8.3 name
    lea rsi, conName
    mov ecx, 3
    rep movsb   ;Move the three bytes
    mov rax, rdx
    stosq   ;Eight spaces left to print
    mov rax, qword fs:[currentPSP]  ;Get current PSP
    mov qword [rbx + sft.qPSPOwner], rax
;GOTO NEXT ENTRY
    add rbx, sft_size   ;Goto next SFT
;Write PRN
    mov word [rbx + sft.wNumHandles], 1 ;Sysinit stdprn
    mov word [rbx + sft.wOpenMode], denyNoneShare | RWAccess
    mov byte [rbx + sft.bFileAttrib], archiveFile | systemFile | hiddenFile
    mov byte [rbx + sft.wDeviceInfo], devCharDev 
    ;Want EOF when reading from to the device
    mov rax, qword [rbp + prnHdr]  ;Get pointer to PRN device
    mov qword [rbx + sft.qPtr], rax
    ;Ignore disk related fields and Date/Time of open
    lea rdi, qword [rbx + sft.sFileName]  ;Get file name space pointer
    ;11 chars in 8.3 name
    lea rsi, prnName
    mov ecx, 3
    rep movsb   ;Move the three bytes
    mov rax, rdx
    stosq   ;Eight spaces left to print
    mov rax, qword fs:[currentPSP]  ;Get current PSP
    mov qword [rbx + sft.qPSPOwner], rax
;Zero word 0 of entry 4 and 5
    add rbx, sft_size   ;Goto SFT 4
    xor eax, eax
    ;To save some bytes, clear dword (which encompasses numHandles and openmode
    ; which is ok as these are empty entries, ready to be used)
    mov dword [rbx + sft.wNumHandles], eax
    add rbx, sft_size   ;Goto SFT 5
    mov dword [rbx + sft.wNumHandles], eax
;------------------------------------------------;
;               Setup Share Hooks                ;
;------------------------------------------------;
    lea rdi, qword [rbp + shareHooks]
    lea rbx, qword [rbp + goodDfltShareHook]
    lea rax, qword [rbp + badDfltShareHook]
    stosq   ;Store bad for openFileCheck
    xchg rax, rbx
    stosq   ;Store good for open
    stosq   ;Store good for close
    xchg rax, rbx
;Store bad for close for machine, task, name, lock and unlock file
    mov ecx, 5
    rep stosq
    xchg rax, rbx
    stosq   ;Store good for check file lock exists
    xchg rax, rbx
;Store bad for open file, update fcb from sft and get fst cluster of fcb
    mov ecx, 3
    rep stosq
    xchg rax, rbx
    stosq   ;Store good for close dup file share
    xchg rax, rbx
    stosq   ;Store bad for close handles for new file opened 
    stosq   ;Store bad for update dir information
;------------------------------------------------;
;             Print Welcome Message              ;
;------------------------------------------------;
    lea rdx, strtmsg
    mov ah, 09h
    int 41h    
;------------------------------------------------;
;               Load CONFIG.SYS                  ;
;------------------------------------------------;
;Setup stackframe, workout base 
    lea rdi, qword [rbp + secondDPB]
    ;Check if this DPB is the last dpb. The first two ALWAYS exist.
    mov rax, qword [rdi + dpb.qNextDPBPtr]
    cmp rax, -1 ;Was second DPB the last one?
    je setupFrame   ;If so, jump
    mov rdi, rax    ;Move rdi to thirdDPB
    mov rax, qword [rdi + dpb.qNextDPBPtr]
    cmp rax, -1 ;Was third DPB the last one?
    je setupFrame   ;If so, jump
    mov rdi, rax    ;Move rdi to fourthDPB
    mov rax, qword [rdi + dpb.qNextDPBPtr]
    cmp rax, -1 ;Was fourth DPB the last one?
    je setupFrame   ;If so, jump
    mov rdi, rax    ;Move rdi to fifthDPB
setupFrame:
    add rdi, dpb_size   ;Else, goto end of the dpb rdi points to
    push rbp
    mov rbp, rsp
    sub rsp, cfgFrame_size
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Store the end pointer here
    mov qword [rbp - cfgFrame.newBuffers], buffersDefault
    mov qword [rbp - cfgFrame.newSFTVal], filesDefault
    mov qword [rbp - cfgFrame.newFCBSVal], fcbsDefault
    mov qword [rbp - cfgFrame.newProtFCBSVal], safeFcbsDeflt
    mov qword [rbp - cfgFrame.newLastdrive], lastDriveDeflt

    mov al, byte fs:[bootDrive]
    test al, 80h    ;Was boot drive hard disk?
    jz notHDD
;Set Current Drive to C:
    mov dl, 2
    mov ah, 0Eh ;Select C: Drive
    int 41h
notHDD:
    lea rdx, cfgspec    ;CONFIG.SYS, must be on bootdrive for now
    mov ah, 3Dh ;Open file for reading
    mov al, ReadAccess
    int 41h
    jc noCfg  ;If no CONFIG.SYS found, just use defaults that are already setup
;------------------------------------------------;
;              Process CONFIG.SYS                ;
;------------------------------------------------;
;Create a stack frame with the following order.
;Values greater than max are set to max. Values less than min are set to min.
; New Buffers value.        Default = 30, Min = 1, Max = 99
; New SFT value.            Default = 20, Min = 8, Max = 254
; New FCBS value.           Default = 4,  Min = 4, Max = 254
; New protected FCBS value. Default = 0,  Min = 0, Max = New FCBS value
; New CDS value.            Default = 5,  Min = 5, Max = 26
;
;Remember to maintain the base of occupied memory on stack (endPtr)
;-------------------------------------------------------------------------;
; CONFIG.SYS processing pseudocode:-
;
; _START:
; Read file one byte at a time a until a EOF or CR encountered.
; If (EOF encountered)
;   Insert a terminating ^Z to the end of the line. 
;   Close handle.
; Parse the line from beginning looking for a DOS terminating char.
;   If (CR or EOF encoutered before terminating char)
;       Bad line error msg. 
;       If (CR encountered)
;           Goto _START
;       Else
;           Goto _EXIT
;   Else 
;       If (Keyword AND not DEVICE) 
;           Store it's value on stack or change internal variable value
;       If (DEVICE)
;           Move endPtr after end of line and try load the driver.
;           If (driver doesn't exist or fails to init)
;               print bad driver error msg.
;       Else 
;           Bad line error msg
;       If (line terminated by CR)
;           Goto _START
; _EXIT:
;-------------------------------------------------------------------------;
; Note:
; If driver a Block Device Driver, build all the DPB's for it (up until max)
;   directly after the driver pointer as returned by the driver. Then, 
;   adjust the memory pointer and start loading next line.
;   The space marked as "endPtr" can be used as a buffer by the disk buffers.
; Once EOF has been reached, we jmp to noCfg which configures the other
;   data structures according to the values on the stack frame.
;-------------------------------------------------------------------------;
;Start CONFIG.SYS parsing here
configParse:
    mov qword [rbp - cfgFrame.cfgHandle], rax
    mov qword [rbp - cfgFrame.lastLine], 0
.newLine:
;Keeps the new line unless a DEVICE= command read it, which adjusts endPtr
    mov rdx, qword [rbp - cfgFrame.endPtr]  ;Start reading into here
    mov qword [rbp - cfgFrame.linePtr], rdx	;Use var for start of line ptr
.nextChar:
    mov rbx, qword [rbp - cfgFrame.cfgHandle]   ;Move the handle into ebx
    cmp bx, -1
    je .stopProcessError
    mov eax, 3F00h  ;Read handle
    mov ecx, 1  ;Read one byte
    int 41h
    jc .stopProcessError
    test ecx, ecx	;If this is zero, EOF reached
    jnz .notEOF
    mov qword [rbp - cfgFrame.lastLine], -1	;Note we are at EOF
.notEOF:
    inc qword [rbp - cfgFrame.endPtr]	;Goto next byte
    movzx eax, byte [rdx]
    cmp al, CR
    je .endOfLine
    cmp al, LF
    je .endOfLine
    cmp al, "a"
    jb .notChar
    cmp al, "z"
    ja .notChar
    push rax    ;Push rax on stack as the argument to normalise
    mov eax, 1213h  ;Uppercase the char
    int 4fh
    mov byte [rdx], al  ;Replace the char with the capitalised form
    pop rax ;Pop into rax to renormalise the stack
.notChar:
    inc rdx ;Now move our local pointer to the next byte
    jmp short .nextChar
.endOfLine:
;rdx points to terminating char
;First find the length of the instruction word
    xor ecx, ecx
.cmdNameLenGet:
    lodsb
    call .isCharTerminal
    jz .endOfCommandFound
    inc ecx
    cmp ecx, 10 ;If shorter than longest command, keep looping
    jb .cmdNameLenGet
;Else, fall through in error
.endOfCommandFound:
;ecx has the length of the command
    cmp ecx, 10
    je .stopProcessError
    lea rdi, .keyTbl ;Put rdi at the table to search for
.cmdSearch:
    cmp byte [rdi], -1
    je .stopProcessError
    cmp byte [rdi], cl
    jne .gotoNextCmd
    ;Candidate command found, check said command is the command we want
    mov rsi, qword [rbp - cfgFrame.linePtr]
    cmp rsi, -1 ;Error?
    je .stopProcessError
    push rdi
    push rcx
    inc rdi ;Go to next char
    repe cmpsb  ;Compare whilst the strings are equal
    pop rcx
    pop rdi
    jne .gotoNextCmd    ;If not equal, just goto next command
    ;Else, rdi + rcx points to the word ptr of the function
    ;rdx points to the terminating char of the line 
    push rdx    ;This is to know whether we continue processing or end now
    lea rsi, .keyTbl
    mov rax, rsi    ;Keep a copy in rax
    add rsi, qword [rdi + rcx + 1]  ;This is the offset from .keyTbl
    add rsi, rax    ;So add the EA of the head of the tbl before calling
    clc ;Esure flags are happy before entering
    call rsi    ;Call this function
    pop rdx
    jc .stopProcessError    ;If the function returns CF=CY, error exit
    test qword [rbp - cfgFrame.lastLine], -1 ;If we concluded at EOF, exit
    jnz .cfgExit
    jmp .newLine
.gotoNextCmd:
    movzx eax, byte [rdi]
    add eax, 3
    add rdi, rax
    jmp short .cmdSearch
.isCharTerminal:
;Input: AL = Char to check
;Output: ZF=ZE -> Char terminal
;        ZF=NZ -> Char not terminal
    cmp al, "="
    rete
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, ";"
    rete
    return
.stopProcessError:
    lea rdx, .speLine
    mov eax, 0900h
    int 41h
    ;Reset all values to default
    mov qword [rbp - cfgFrame.newBuffers], buffersDefault
    mov qword [rbp - cfgFrame.newSFTVal], filesDefault
    mov qword [rbp - cfgFrame.newFCBSVal], fcbsDefault
    mov qword [rbp - cfgFrame.newProtFCBSVal], safeFcbsDeflt
    mov qword [rbp - cfgFrame.newLastdrive], lastDriveDeflt
    jmp .cfgExit
.speLine:   db CR,LF,"Unrecognised command in CONFIG.SYS",CR,LF,"$"
.keyTbl: 
    db 5, "BREAK"           ;DONE
	dw .breakHandler - .keyTbl
    db 7, "BUFFERS"         ;DONE
	dw .bufHandler - .keyTbl
	db 7, "COUNTRY"         ;Ignored for now
	dw .countryScan - .keyTbl
	db 6, "DEVICE"          ;DONE
	dw .drvLoader - .keyTbl
	db 4, "FCBS"            ;DONE (to be ignored for a while now)
	dw .fcbHandler - .keyTbl
	db 5, "FILES"           ;DONE
	dw .sftHandler - .keyTbl
	db 9, "LASTDRIVE"       ;DONE
	dw .lastdriveHandler - .keyTbl
	db 5, "SHELL"           ;Ignored for now
	dw .shellHandler - .keyTbl
	db 6, "STACKS"          ;Ignored for now
	dw .stacksHandler - .keyTbl
    db 8, "DRIVPARM"
    dw .drivParm - .keyTbl  ;Ignored for now
	db -1	;End of table marker
.breakHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 6  ;Go past BREAK=
    ;This must be the word ON or OFF 
    xor edx, edx    ;Clear CF and default to OFF
    cmp word [rsi], "ON"
    je .breakOn
    cmp word [rsi], "OF"
    jne .breakBad
    cmp byte [rsi + 2], "F"
    je .breakCommon
.breakBad:
    stc
    return
.breakOn:
    inc edx ;Go from OFF to ON  (keeps CF=NC)
.breakCommon:
    mov eax, 3301h  ;Set break to value in dl
    int 41h
    return

.bufHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 8  ;Go past BUFFERS=
    ;This must be at most three digits, anything else is a failure
    mov rdi, rsi    ;Save the start in rdi
    xor ecx, ecx
    lodsb   ;Get the first char. Must be between ASCII '0' and '9'
    cmp al, "0"
    jb .bufHandlerErr
    cmp al, "9"
    ja .bufHandlerErr
    inc ecx ;Increment char counter
    lodsb   ;Get second char
    call .bufHandlerTermCheck
    je .bufHandlerProcess   ;If it is a terminating char, exit
    cmp al, "0"
    jb .bufHandlerErr
    cmp al, "9"
    ja .bufHandlerErr
    lodsb   ;Check no more chars!
    call .bufHandlerTermCheck
    jne .bufHandlerErr
.bufHandlerProcess:
    xor edx, edx    ;Accumulate value in edx
    mov rsi, rdi    ;Go back to the first number
.bufHandlerLp:
    lodsb   ;Get the digit
    call .bufHandlerMul
    jecxz .bufHandlerPrepExit
    dec ecx
    jmp short .bufHandlerLp 
.bufHandlerPrepExit:
;edx has the value now, so place it in stack
    mov ecx, buffersDefault
    test edx, edx
    cmovz edx, ecx  ;Replace zero with default if the user specified 0 buffers
    mov qword [rbp - cfgFrame.newBuffers], rdx
    clc
    return
.bufHandlerMul:
    sub al, "0" ;Convert to a binary value
    mul cl  ;Multiply al by cl, answer in ax
    movzx eax, ax
    add edx, eax
    return
.bufHandlerErr:
    stc
    return
.bufHandlerTermCheck:
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, CR
    rete
    cmp al, LF
    rete
    return
.countryScan:
    return
.drvLoader:
;We first try to read the driver into the byte after rdx.
;If we cannot open the file, or we can open but not read the whole file
; we error with Bad or missing filename msg, and proceed as if nothing happened 
; (CF=NC). 
; Thus we DO NOT adjust .endPtr or .linePtr and recycle that space for the 
; next line.
;If the open succeeded and we were able to read the whole driver into memory, 
; we pass the lineptr to the driver and call init for the driver.
; Once the driver returns, if the DONE bit is set, we read the offset of 
; free memory above the driver and add that to the endPtr. If the driver
; is a block driver, we add to the endPtr the space for "Units supported" 
; number of DPBs.
    mov rsi, rdx    ;Save the ptr to past the end of the line in rsi
    mov rdi, qword [rbp - cfgFrame.linePtr]
    add rdi, 7  ;Go past DEVICE= to the pathname
    mov rdx, rdi    ;Prepare rdx for the open
    mov eax, SPC
.drvFindEndOfFileName:
    scasb  ;Is this char the space?
    je .fileNameFound
    ;Was the char terminal?
    cmp byte [rdi - 1], CR
    je .drvBad
    cmp byte [rdi - 1], LF
    je .drvBad
    jmp short .drvFindEndOfFileName
.fileNameFound:
    mov byte [rdi - 1], 0   ;Null terminate the path to the file
    mov eax, 3D00h  ;Read only file
    int 41h
    jc .drvBad
    mov byte [rdi - 1], " " ;Replace the null with a space now again
    movzx ebx, ax   ;Get the handle in ebx
    mov word [.drvHandle], ax   ;Save the handle in variable
    xor edx, edx    ;Move the handle to the end of the file
    mov eax, 4202h  ;LSEEK to SEEK_END
    int 41h
    mov ecx, eax    ;Get the file size in ecx
    xor edx, edx    ;Move the handle to the start of the file
    mov eax, 4200h  ;LSEEK to SEEK_SET (start of the file)
    int 41h
    ;Now we read ecx bytes to rsi as rsi points to first byte past the end
    ; of the DEVICE= line 
    mov rdx, rsi    ;Point to first byte past the end of DEVICE= line
    mov esi, ecx    ;Save the number of bytes to read in esi
    mov eax, 3F00h  ;Read handle    
    int 41h
    jc .drvBadClose
    cmp esi, ecx    ;Were all bytes read in?
    jne .drvBadClose
    ;Ok, full file read in, now prepare to call driver init routine
    mov rsi, rdx    ;Move ptr to driver header to rsi
    lea rbx, .drvInitStruc
    mov byte [rbx + initReqPkt.hdrlen], initReqPkt_size
    mov byte [rbx + initReqPkt.cmdcde], drvINIT
    mov word [rbx + initReqPkt.status], 0
    mov byte [rbx + initReqPkt.numunt], 0
    mov rax, qword [rbp - cfgFrame.linePtr] ;Get the line pointer
    add rax, 7  ;Goto the first byte past DEVICE=
    mov qword [rbx + initReqPkt.endptr], rax
    mov qword [rbx + initReqPkt.optptr], 0
    movzx eax, byte fs:[numPhysVol]
    dec eax ;Get a 0 based count
    mov byte [rbx + initReqPkt.drvnum], al
    call qword [rsi + drvHdr.strPtr]  ;Passing rbx through here
    call qword [rsi + drvHdr.intPtr]
    test word [rbx + initReqPkt.status], drvDonStatus
    jz .drvBadClose
    test word [rbx + initReqPkt.status], drvErrStatus
    jnz .drvBadClose
    ;Now check that the driver wants to be installed
    cmp rsi, qword [rbx + initReqPkt.endptr]    ;This is for char and blk devs
    je .drvWantsClose
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz .drvChar
    cmp byte [rbx + initReqPkt.numunt], 0
    je .drvWantsClose
.drvChar:
    ;Otherwise, this init passed, now build the structures we need.
    ;First adjust .endPtr
    mov rax, qword [rbx + initReqPkt.endptr]    ;Get the end pointer
    mov qword [rbp - cfgFrame.endPtr], rax  ;Move it here
    ;Now we link the driver into the driver chain
    mov rdi, qword [nulDevHdr + drvHdr.nxtPtr]  ;Get next ptr from nul drvr
    mov qword [rsi + drvHdr.nxtPtr], rdi    ;And store it here
    mov qword [nulDevHdr + drvHdr.nxtPtr], rsi  ;And link nul to this driver
    ;Now if we are a char device, we are done so check here
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz .drvWantsCloseChar  ;We are complete
    ;Now for block devices, we get the BPB ptr array and numUnits supported
    movzx ecx, byte [rbx + initReqPkt.numunt]
    mov rbx, qword [rbx + initReqPkt.optptr]    ;Get the BPB array pointer

    mov rdx, rsi    ;Move the driver pointer to rdx
    mov rsi, qword [rbp - cfgFrame.endPtr]  ;Build DPB array here
    mov rdi, rsi    ;Move rdi here too, to point to first new DPB later
    push rcx
    push rdx
    xor edx, edx
    mov eax, dpb_size
    mul ecx ;Multiply the number of DPB's needed with the size of a dpb
    add qword [rbp - cfgFrame.endPtr], rax  ;Add this value to endPtr
    pop rdx ;Get back the driver ptr in rdx
    pop rcx ;Get back the number of units count
    
    xchg rbp, rbx   ;Swap stack frame ptr and BPB array ptr
    xchg rsi, rbp   ;Swap BPB array and DPB space ptrs
.drvBuildDPB:
    mov eax, 5300h
    int 41h
    add rsi, bpbEx_size ;Goto next bpb in array
    ;Adjust fields in DPB
    inc byte fs:[numPhysVol] 
    mov al, byte fs:[numPhysVol]
    mov byte [rbp + dpb.bDriveNumber], al
    mov byte [rbp + dpb.bUnitNumber], ch
    mov qword [rbp + dpb.qDriverHeaderPtr], rdx
    lea rax, qword [rbp + dpb_size] ;Point to next DPB
    mov qword [rbp + dpb.qNextDPBPtr], rax
    inc ch  ;Increment unit number 
    cmp cl, ch  ;Are we done?
    je .dpbInitDone
    add rbp, dpb_size   ;Go to space for next DPB
    jmp short .drvBuildDPB
.dpbInitDone:
;Make sure we now make the last qNextDPBPtr = -1
    mov qword [rbp + dpb.qNextDPBPtr], -1
    ;Now we set the old last dpb to point to the first one
    mov rsi, qword fs:[dpbHeadPtr]
.drvDPBLp:
    cmp byte [rsi + dpb.qNextDPBPtr], -1
    je .drvLastDPBFound
    mov rsi, qword [rsi + dpb.qNextDPBPtr]  ;Goto next DPB
    jmp short .drvDPBLp
.drvLastDPBFound:
    mov qword [rsi], rdi    ;Chain this dpb now to the first new dpb
    mov rbp, rbx    ;Return the stack frame ptr to rbp
;And we are done!
.drvWantsClose:
;If the driver wants to not install silently, it can here
    movzx ebx, word [.drvHandle] ;Get the handle back, close it and proceed
    mov eax, 3E00h  
    int 41h 
    clc ;Never return with CF=CY
    return  
.drvBadClose:
    movzx ebx, word [.drvHandle]    ;Get back handle to close
    mov eax, 3E00h  ;Close handle in ebx
    int 41h
.drvBad:
    lea rdx, .drvBadMsg
    mov eax, 0900h
    int 41h
    clc ;Never return with CF=CY
    return
.drvWantsCloseChar:
;Final checks, to see if we are CLOCK$ or CON
    test word [rsi + drvHdr.attrib], devDrvConIn
    jz .dwccClock
    mov qword [vConPtr], rsi
.dwccClock:
    test word [rsi + drvHdr.attrib], devDrvClockDev
    jz .drvWantsClose
    mov qword [clockPtr], rsi
    jmp short .drvWantsClose
.drvBadMsg: db "Bad or missing filename",CR,LF,"$"
.drvInitStruc: db initReqPkt_size dup (0)  
.drvHandle: dw -1

.fcbHandler:
    return
.sftHandler:
;This reads the line to set the number of FILE to between 1 and 254
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 6  ;Go past FILES=
    ;This must be at most three digits, anything else is a failure
    mov rdi, rsi    ;Save the start in rdi
    xor ecx, ecx
    lodsb   ;Get the first char. Must be between ASCII '0' and '9'
    cmp al, "0"
    jb .sftHandlerErr
    cmp al, "9"
    ja .sftHandlerErr
    inc ecx ;Increment char counter
    lodsb   ;Get second char
    call .sftHandlerTermCheck
    je .sftHandlerProcess   ;If it is a terminating char, exit
    cmp al, "0"
    jb .sftHandlerErr
    cmp al, "9"
    ja .sftHandlerErr
    inc ecx ;Increment char counter
    lodsb   ;Get third char
    call .sftHandlerTermCheck
    cmp al, "0"
    jb .sftHandlerErr
    cmp al, "2" ;Max BUFFERS=254 soooo, sorry buddy!
    ja .sftHandlerErr
    lodsb   ;Check no more chars!
    call .sftHandlerTermCheck
    jne .sftHandlerErr
.sftHandlerProcess:
    xor edx, edx    ;Accumulate value in edx
    mov rsi, rdi    ;Go back to the first number
.sftHandlerLp:
    lodsb   ;Get the digit
    call .sftHandlerMul
    jecxz .sftHandlerPrepExit
    dec ecx
    jmp short .sftHandlerLp 
.sftHandlerPrepExit:
;edx has the value now, so place it in stack
    mov ecx, filesDefault  ;Get default if the user specifies less than min
    cmp edx, 8
    cmovb edx, ecx
    mov qword [rbp - cfgFrame.newSFTVal], rdx
    clc
    return
.sftHandlerMul:
    sub al, "0" ;Convert to a binary value
    mul cl  ;Multiply al by cl, answer in ax
    movzx eax, ax
    add edx, eax
    return
.sftHandlerErr:
    stc
    return
.sftHandlerTermCheck:
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, CR
    rete
    cmp al, LF
    rete
    return
.lastdriveHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 10  ;Go past LASTDRIVE=
    lodsb   ;Get this char
    movzx eax, al   ;Zero extend to eax
    push rax    ;Push on stack
    mov eax, 1213h  ;Uppercase the char
    int 4Fh
    pop rbx
    cmp al, "Z"
    ja .sftHandlerErr
    cmp al, "A"
    jb .sftHandlerErr
    cmp byte [rsi], CR
    je .ldProceed
    cmp byte [rsi], LF
    je .ldProceed
    cmp byte [rsi], TAB
    je .ldProceed
    cmp byte [rsi], SPC
    jne .sftHandlerErr
.ldProceed:
    sub al, "A" ;Convert into a number
    movzx eax, al   ;Zero extend in case DOS rets something dumb in upper bits
    mov edx, lastDriveDeflt
    cmp eax, lastDriveDeflt
    cmovb eax, edx
    mov qword [rbp - cfgFrame.newLastdrive], rax
    clc
    return
.ldBad:
    stc
    return
.shellHandler:
    return
.stacksHandler:
    return
.drivParm:
    return

.cfgExit:
    mov rbx, qword [rbp - cfgFrame.cfgHandle] ;Get the handle back
    mov eax, 3eh    ;Close the handle
    int 41h ;bx already has the handle
;------------------------------------------------;
;   Setup Final Data Areas With Overrides from   ;
;                  CONFIG.SYS                    ;
;------------------------------------------------;
;Add additional buffers. Start from tmpBufHdr
;Add additional SFT entries. By default, 1 new SFT header, with 15 SFT entries
;Add additional FCBS.
;Create a larger CDS if needed.
noCfg:
;Start with buffers:
    mov rcx, qword [rbp - cfgFrame.newBuffers]    ;Get new buffers size
    mov byte fs:[numBuffers], cl    ;Store this value in var
    ;Now do the allocation at rdi. Each buffer = maxSectorSize + bufferHdr_size
    movzx ebx, word fs:[maxBytesSec]    ;Get buffer sector size
    add ebx, bufferHdr_size ;rbx has the size to add
    ;Each buffer has no flags, drive number must be -1
    mov rdi, qword [rbp - cfgFrame.endPtr]  ;Get current allocation end pointer
    mov qword fs:[bufHeadPtr], rdi  ;Reset the var here
    mov rsi, rdi    ;Points rsi to first new buffer space
    xor eax, eax    ;Use for sanitising buffer headers
    jecxz .lastBuffer
.bufferLoop:
    add rdi, rbx    ;Goto next buffer space
    mov qword [rsi + bufferHdr.nextBufPtr], rdi ;Point to next buffer
    mov word [rsi + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags
    mov qword [rsi + bufferHdr.bufferLBA], rax
    mov byte [rsi + bufferHdr.bufFATcopy], al
    mov dword [rsi + bufferHdr.bufFATsize], eax
    mov qword [rsi + bufferHdr.driveDPBPtr], rax
    mov qword [rsi + bufferHdr.owningFile], rax
    mov rsi, rdi    ;Move rsi to next buffer position
    dec ecx
    jnz .bufferLoop
.lastBuffer:
    add rdi, rbx    ;Goto past the last buffer
    mov qword [rsi + bufferHdr.nextBufPtr], -1 ;Point to no buffer
    mov word [rsi + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags
    mov qword [rsi + bufferHdr.bufferLBA], rax
    mov byte [rsi + bufferHdr.bufFATcopy], al
    mov dword [rsi + bufferHdr.bufFATsize], eax
    mov qword [rsi + bufferHdr.driveDPBPtr], rax
    mov qword [rsi + bufferHdr.owningFile], rax
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here

;Now build a new SFT header for the number of files specified by user
    mov rcx, qword [rbp - cfgFrame.newSFTVal]
    cmp ecx, 5  ;If we are not adding anything, skip building SFT
    je .skipSFT
    mov rsi, qword fs:[sftHeadPtr]  ;Get the current only SFT head pointer
    mov qword [rsi + sfth.qNextSFTPtr], rdi ;Move rdi as new SFT pointer
    sub cx, word [rsi + sfth.wNumFiles] ;Remove the number of files we already have
    mov word [rdi + sfth.wNumFiles], cx ;Move remaining files here
    mov qword [rdi + sfth.qNextSFTPtr], -1  ;Last table in chain
    add rdi, sfth_size  ;Goto sft area, now need to compute size
    mov eax, sft_size
    mul ecx ;Multiply number of sft with their size to get allocation
    add rdi, rax    ;Add that many bytes to rdi
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here
.skipSFT:
;FCBS at rdi
    mov qword fs:[fcbsHeadPtr], rdi ;Setup the fcbs var here
    mov qword [rdi + sfth.qNextSFTPtr], -1  ;No more FCBS headers for now
    mov rcx, qword [rbp - cfgFrame.newFCBSVal]
    mov word [rdi + sfth.wNumFiles], cx ;Move this value here
    mov eax, sft_size
    mul ecx ;Multiply number of sft with their size to get allocation
    add rdi, rax    ;Add that many bytes to rdi
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here
    mov rcx, qword [rbp - cfgFrame.newProtFCBSVal] ;Get number of safe FCBs
    mov word fs:[numSafeSFCB], cx   ;And save that there
;And CDS now
    mov rcx, qword [rbp - cfgFrame.newLastdrive]
    mov byte fs:[lastdrvNum], cl ;Save this value
    mov qword fs:[cdsHeadPtr], rdi  ;Point cdsHeadPtr here
    call makeCDSArray
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here

;Computation of new space is complete, now work out how many bytes this is
    mov rsp, rbp    ;Return stack pointer to original position
    pop rbp
    lea rbx, qword [rbp + dosDynamicArea]
    sub rdi, rbx    ;Gives difference now
    lea ebx, dword [edi + 11h]  ;Add 11 to round up a paragraph
    shr ebx, 4  ;Convert to paragraphs
;Resize DOS allocation before loading COMMAND.COM
    mov r8, qword fs:[mcbChainPtr] ;Get ptr
    add r8, mcb.program
    mov ah, 4Ah
    int 41h
;Now we close all five default handles and open AUX, CON and PRN.
    mov r8, qword fs:[currentPSP]
    xor ebx, ebx
closeHandlesLoop:
    mov eax, 3e00h  ;Close
    int 41h
    inc ebx ;Goto next handle
    cmp ebx, 6
    jne closeHandlesLoop
    lea rdx, auxName
    mov eax, 3D02h   ;Open read/write
    int 41h
    mov ebx, eax
    mov ecx, 3  ;
    mov eax, 4600h  ;DUP2
    int 41h
    mov eax, 3e00h
    int 41h ;Close the original handle
    mov eax, 3D02h  ;Open read/write
    lea rdx, conName
    int 41h
    mov ebx, eax    ;Move file handle to ebx
    mov ecx, 1
    mov eax, 4600h  ;DUP2
    int 41h
    inc ecx         ;Goto next handle
    mov eax, 4600h  ;DUP2
    int 41h
    lea rdx, prnName
    mov eax, 3D02h
    int 41h       ;Open file

    
    %if DEBUG && ALLOCTEST
;Test Allocation, Growth and Deallocation
    mov r15, qword fs:[currentPSP]
    mov qword fs:[currentPSP], 5A5Ah ;5A5Ah is a reserved addr
    mov ebx, 10 ;Allocate 10 paragraphs pls
    mov ah, 48h ;Allocate
    int 41h

    mov byte [rax - 10h], 0 ;Trash chain
    mov r8, rax ;Move the pointer to r8
    mov ebx, 20 ;Increase allocation to 20 paragraphs
    mov ah, 4Ah
    int 41h

    mov ah, 49h ;Free r8
    int 41h
    mov qword fs:[currentPSP], r15
    %endif

    %if ENDSTATUS
debugFinal:
    ;Print system state
    push rbp    ;Only rbp really matters here
    mov r8, rbp

    lea rbp, .msg2
    lea r9, qword [r8 + debPrintNullString]
    call r9

    lea rbx, qword [.msg + 15]
    mov rax, qword fs:[biosUBase]

    lea r9, qword [r8 + overlayQword]
    call r9

    add rbx, 19+8
    mov rax, qword fs:[dosSegPtr]
    call r9

    add rbx, 19+8
    mov rax, qword fs:[mcbChainPtr]
    call r9

    add rbx, 20+8
    lea r9, qword [r8 + overlayDword]
    mov eax, dword fs:[loProtMem]
    call r9

    add rbx, 10+8
    mov eax, dword fs:[hiProtMem]
    call r9

    add rbx, 10+8
    mov rax, qword fs:[longMem]
    lea r9, qword [r8 + overlayQword]
    call r9

    add rbx, 19+8
    mov rax, qword fs:[dpbHeadPtr]
    call r9

    add rbx, 19+8
    mov rax, qword fs:[sftHeadPtr]
    call r9

    add rbx, 19+8
    mov rax, qword fs:[bufHeadPtr]
    call r9

    add rbx, 19+8
    mov rax, qword fs:[cdsHeadPtr]
    call r9

    lea r9, qword [r8 + overlayByte]

    add rbx, 25+19
    movzx rax, byte fs:[numPhysVol]
    call r9

    add rbx, 30
    movzx rax, byte fs:[numFixDrv]
    call r9

    add rbx, 30
    movzx rax, byte fs:[numRemDrv]
    call r9

    add rbx, 16
    movzx rax, byte fs:[bootDrive]
    add al, "A"
    mov byte [rbx], al

    lea rbp, .msg
    lea r9, qword [r8 + debPrintNullString]
    call r9
    pop rbp
    jmp l1
.msg:   db "BIOS user base FFFFFFFFFFFFFFFFh",0Ah,0Dh ;15 chars to number
        db "DOS Seg FFFFFFFFFFFFFFFFh",0Ah,0Dh
        db "MCBptr  FFFFFFFFFFFFFFFFh ",0Ah,0Dh
        db "Arena1: FFFFFFFFh ",
        db "Arena2: FFFFFFFFh ",
        db "Arena3: FFFFFFFFFFFFFFFFh",0Ah,0Dh
        db "DPBptr  FFFFFFFFFFFFFFFFh",0Ah,0Dh
        db "SFTptr  FFFFFFFFFFFFFFFFh",0Ah,0Dh
        db "bufPtr  FFFFFFFFFFFFFFFFh",0Ah,0Dh
        db "CDSptr  FFFFFFFFFFFFFFFFh",0Ah,0Dh
        db "Number of Logical Drives FFh",0Ah,0Dh
        db "Number of Fixed Drives   FFh",0Ah,0Dh
        db "Number of Removable Drvs FFh",0Ah,0Dh
        db "Boot drive Z:",0Ah,0Dh
        db "Loading COMMAND.COM...",0Ah,0Dh,0
.msg2:  db 0Ah,0Dh,"End of boot summary",0Ah,0Dh,0
    %endif
l1:
    ;Load COMMAND.COM
    ;Get currentPSP ptr
    mov ah, 62h ;Get current PSP ptr in rdx
    int 41h
    mov ah, 19h ;Get current Drive letter in al
    int 41h
    add al, "A"
    mov byte [cmdLine], al  ;Store drive letter at start of command line
    lea rbx, cmdBlock
    lea rax, qword [rdx + psp.fcb1]
    mov qword [rbx + execProg.pfcb1], rax
    lea rax, qword [rdx + psp.fcb2]
    mov qword [rbx + execProg.pfcb2], rax
    lea rdx, cmdLine
    mov qword [rbx + execProg.pCmdLine], rdx    ;Store command line here
    mov eax, 4B00h  ;Exec Prog
    int 41h
    lea rdx, badCom
    mov ah, 09h ;Print message
    int 41h
    jmp errorInit.ei0
    
;--------------------------------
;       PROCS FOR SYSINIT       :
;--------------------------------
adjustDrvHdr:
;Input: rsi = Effective address of driver in DOS segment
;       rbp = Ptr to the start of the DOS segment
;Output: rsi = EA of next header in DOS segment
    add qword [rsi + drvHdr.strPtr], rbp
    add qword [rsi + drvHdr.intPtr], rbp
    cmp qword [rsi + drvHdr.nxtPtr], -1 ;End of chain?
    je .exit
    add qword [rsi + drvHdr.nxtPtr], rbp    ;Adjust address
    add rsi, drvHdr_size
.exit:
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
    pause
    jmp short .ei0
noCmdCom:
    lea rdx, badCom
    mov ah, 09h
    int 41h
    jmp short errorInit.ei0
;--------------------------------
;       DATA FOR SYSINIT        :
;--------------------------------
strtmsg db "Starting SCP/DOS...",0Ah,0Dh,"$"
mcbFailmsg db "Memory Allocation Error",0Ah,0Dh,0
hltmsg  db "Error initialising SCPDOS.SYS. System halting...",0
badCom  db "Bad or missing Command interpreter",0Ah,0Dh,"$"
conName db "CON",0
auxName db "AUX",0
prnName db "PRN",0

cfgspec db "CONFIG.SYS",0 ;ASCIIZ for CONFIG
cmdLine db "_:\COMMAND.COM",0
cmdBlock:
    istruc execProg
    at execProg.pEnv,       dq 0    ;Keep at 0 to "copy" DOS's environment ptr
    at execProg.pCmdLine,   dq 0
    at execProg.pfcb1,      dq 0    ;Set to DOS's fcb 1 and 2
    at execProg.pfcb2,      dq 0
    iend
exceptData:
    dq i0
    dq i1
    dq i2
    dq i3
    dq i4
    dq i5
    dq i6
    dq i7
    dq i8
    dq i9
    dq i10
    dq i11
    dq i12
    dq i13
    dq i14
    dq i15
    dq i16
    dq i17
    dq i18
    dq i19
    dq i20
    dq i21

intData:
    dq terminateProcess ;Int 40h
    dq functionDispatch ;Int 41h
    dq errorInit        ;Int 42h, If sysinit terminates, halt system
    dq defaultIretq     ;Int 43h, ignore any CTRL+C during init
    dq dosDefCritErrHdlr 
    dq absDiskRead      ;Int 45h
    dq absDiskWrite     ;Int 46h
    dq terminateRes     ;Int 47h
    dq defaultIretq     ;Int 48h
    dq defaultIretq     ;Int 49h
    dq defaultIretq     ;Int 4Ah
    dq defaultIretq     ;Int 4Bh
    dq defaultIretq     ;Int 4Ch
    dq defaultIretq     ;Int 4Dh
    dq defaultIretq     ;Int 4Eh
    dq multiplexHdlr    ;Int 4Fh, multiplex default handler
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
    cmp byte fs:[numPhysVol], 3  ;Are we at maximum devices (A: B: reserved)?
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
    inc byte fs:[numPhysVol] ;Increment the number of valid drives we have
.primaryEpilog:
    inc dl  ;Goto next BIOS drive
    mov dh, dl
    and dh, 7Fh ;Clear bit 7
    cmp dh, byte fs:[numFixDrv]    ;Have we gone thru all hard drives?
    jne .primary    ;Whilst we have fewer, go back
.extended:
;We have gone through all the devices once
    ;cmp byte fs:[numPhysVol], 3  ;Are we at maximum devices (A: B: reserved)?
    ;je .remInit ;If yes, get removable devices
    ;mov dl, 80h ;Go back to hard drive 80h
    ;xor ecx, ecx    ;Get MBR back
    ;call .initReadSector
    ;Now we eventually search MBR for a FAT extended partition
.remInit:
;Start by linking the default BPB's in the pointers table in the event that
; for some reason the removable drives stop working or dont exist.
;This forces the hard drives to start at C:
    push rbx
    lea rbx, qword [rbp + msdDriver.msdBPBblks] ;Get default drive A block ptr
    mov qword [rbp + msdDriver.msdBPBTbl], rbx  ;Store in ptrs table
    add rbx, bpbEx_size ;Goto next ptr
    mov qword [rbp + msdDriver.msdBPBTbl + 8], rbx  ;Store next pointer
    pop rbx
;Now handle removable devices, at least 2 rem. devs.
    mov r9, r8  ;Save number of next device in r9b
    xor dl, dl  ;Start with removable device 0
    mov r8b, dl ;Once r8b becomes 2, go past the disk drives
    ;rdi points to the space for the subsequent bpb's
    cmp byte fs:[numRemDrv], 0  ;Just skip removable init if no rem drives
    jnz .removables
    add byte fs:[numPhysVol], 2 ;Pretend we have two more drives (A: and B:)
    ret ;and return!
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
    inc byte fs:[numPhysVol] ;Increment the number of valid drives we have    
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
    inc byte fs:[numPhysVol] ;Gotta register the phantom drive!
    ret
.initReadSector:
;Called with sector number in rcx and BIOS device number in dl
    mov ah, 82h ;Read
    mov al, 1   ;One sector
    lea rbx, qword [rbp + msdTempBuffer]  ;Into temporary buffer
    int 33h
    ret
