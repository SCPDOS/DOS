ejectPoint: ;Address of the ejection code

installInterrupt:
;Writes the interrupt in the right place in the table
    ;al = Interrupt number
    ;rdx -> Handler to install
    sidt [myIdt]
    movzx eax, al
    xchg rdx, rax
    shl rdx, 4h     ;Multiply IDT entry number by 16
    add rdx, qword [myIdt.base]    
    mov word [rdx], ax  ;Get low word into offset 15...0
    shr rax, 10h    ;Bring next word low
    mov word [rdx + 6], ax  ;Get low word into offset 31...16
    shr rax, 10h    ;Bring last dword low
    mov dword [rdx + 8], eax
    ret
myIdt:
.limit  dw 0
.base   dq 0

conInit:
;Start by hooking int 3Bh and int 29h as part of the CON driver
    push rax
    push rbx    ;Save the pointer to the request packet on the stack
    push rcx
    push rdx
    lea rdx, qword [fastOutput]
    mov eax, 29h
    call installInterrupt
    lea rdx, qword [ctrlBreak]
    mov eax, 3Bh
    call installInterrupt
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
    pop rcx
    pop rbx
    pop rax
devDrvExit:
;Must be jumped to with rbx -> initReqPkt
    mov word [rbx + initReqPkt.status], drvDonStatus ;Set done bit
    push rax
    lea rax, ejectPoint
    mov qword [rbx + initReqPkt.endptr], rax
    pop rax
    return

clockInit:
    push rbx    ;Push the pointer to the request header here
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
    int 21h
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
    add ecx, 2000   ;ecx is from 2000
    movzx eax, dh   ;Get Months
    call .bcd2Bin
    mov dh, al  
    movzx eax, dl   ;Get day
    call .bcd2Bin
    mov dl, al
    mov ah, 2Bh ;DOS Set Date
    int 21h
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
    pop rbx ;Get the pointer back
    jmp devDrvExit

msdInit:
;Drive letter assignment works as follows:
; A and B always reserved for removable devices even if none.
; C,... for as many fixed disk partitions present in system.
; Any additional removable devices then come after the fixed disks.


;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
;    mov byte [7c02h], 0
;TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST 
;Start by setting up the interrupt vectors!
    mov eax, 3539h  ;Get original Int 39h
    int 21h
    mov qword [i39Org], rbx
    mov eax, 2539h  ;Setup our handler
    lea rdx, dosInt39h
    int 21h

    mov eax, 3533h  ;Get original Int 33h
    int 21h
    mov qword [i33Org], rbx
    mov qword [i33Next], rbx    ;This is also the low level handler
    mov eax, 2533h  ;Setup our handler
    lea rdx, dosInt33h
    int 21h

    mov eax, 352Fh  ;Get previous Int 2Fh handler
    int 21h
    mov qword [i2FNext], rbx
    mov eax, 252Fh
    lea rdx, i2fhSwap33h
    int 21h


;Then set up the link pointers to the load address of the 
; drive table.
    mov ecx, drvBlkTblL - 1
    lea rbp, msdDriver.drvBlkTbl      ;Point to the first drive block
    mov rsi, rbp
.dskTblLp:
    mov rdi, rsi            ;Mov current block to current block
    add rsi, drvBlk_size    ;Point current block to next block
    mov qword [rdi + drvBlk.pLink], rsi    ;Store next blk ptr in cur block
    dec ecx                 ;Decrement the count
    jnz .dskTblLp
    mov qword [rsi + drvBlk.pLink], -1     ;Store end of table here
;Now get number of Int 33h information packed in r8
    int 31h
    ;Returns:
    ;r8[Byte 0] = Number of COM ports
    ;r8[Byte 1] = Number of fixed disks
    ;r8[Byte 2] = Number of units on EHCI bus
    ;r8[Byte 3] = Number of Int 33h units
    mov qword [msdDriver.inBuffer], r8   
    movzx eax, byte [msdDriver.inBuffer + 3]
    test eax, eax
    jz .noDevs
    movzx ebx, byte [msdDriver.inBuffer + 1]
    sub eax, ebx    ;Get remdevs in eax
    mov byte [remDrv], al    ;Save num of phys int 33h rem drives
    mov byte [fixDrv], bl    ;Save number of physical hard drives
    mov byte [physVol], 0    ;Initialise reported volumes to 0
;Start by doing fixed disks.
    test ebx, ebx   ;If no fixed disks, proceed with removables
    jz .doRem
    mov byte [biosDrv], 80h ;Start from first fixed disk
    mov byte [dosDrv], 2    ;Start from drive C:
.hdLp:
;Read the first sector of the hard drive. If a valid BPB is detected,
; we read the BPB in. Else, we attempt to interpret the sector as an MBR.
    xor ecx, ecx    ;Load sector 0 of the disk
    call .ptnUpdateBpb
    jc .mbrFnd  ;If CF=NC, this sector has a valid BPB. No MBR.
;The odd case where there is a BPB on the hard disk :)
    mov byte [rbp + drvBlk.bDevType], typeHard  ;The media type is hard drive
    or word [rbp + drvBlk.wDevFlgs], devFixed | devOwnDrv
    call .advDiskPtrs
    jmp short .fatDiskOk
.mbrFnd:
    jne .gotoNextDisk   ;If CF=CY and ZF=NZ, invalid disk! Goto next disk!
;Now reread sector 0 (it might have been overwritten by a attempted FAT read)
    call msdDriver.bbpbReadBS
;Now we check if we have a valid MBR signature.
    cmp word [msdDriver.inBuffer + mbr.mbrSig], 0AA55h
    jne .gotoNextDisk
    call .processMbr    ;This disk is done.
;If an error reading this disk at some partition, its oki to do the next check
.fatDiskOk:
;Now we check if we have saturated fixed partitions? If so, do Removables now
    call .physCheckEnd   ;If ZF=ZE, then we should end!
    je .doRem
.gotoNextDisk:
    inc byte [biosDrv]  ;Goto next fixed disk
    movzx eax, byte [biosDrv]   ;Get new device
    and al, ~80h    ;Drop the upper bit
    cmp al, byte [fixDrv]
    jne .hdLp
;------------------------------------------------
;   Here we start doing removable devices
;------------------------------------------------
.doRem:
    movzx eax, byte [physVol]
    mov byte [fixPtn], al
    mov byte [biosDrv], 0   ;Start from drive 0
    mov byte [dosDrv], 0    ;And give it DOS A:
    cmp byte [remDrv], 0
    je .noRems
.remLp:
    xor ecx, ecx        ;Load sector 0 of the disk
    call .ptnUpdateBpb  ;If can't get a BPB, its ok. Remdevs here :)
    movzx edx, byte [rbp + drvBlk.bBIOSNum]
    mov eax, 8800h      ;Get disk parameters for changeline status in eax
    int 33h
    jc .remSkipDisk     ;If we cant query the drive, then we skip this.
    test eax, 10h       ;Set if we have changeline support
    jz .remNext
    or word [rbp + drvBlk.wDevFlgs], devChgLine
.remNext:
    or word [rbp + drvBlk.wDevFlgs], devOwnDrv  ;I OWN MYSELF! :)
    call .advDiskPtrs           ;Move rbp to the next drive block
    cmp byte [physVol], drvBlkTblL  ;If we just added our last volume, exit! :)
    je .msdExit
.remSkipDisk:
    inc byte [biosDrv]          ;Else, goto next remdev
    movzx eax, byte [biosDrv]   ;Get the bios drive number
    cmp al, byte [remDrv]       ;Once they are equal, we are done!
    je .msdExit
    cmp al, 3                   ;Else, did we process two remdevs?
    jb .remLp                   ;If not, do normal processing.
;Here we specially now shift the numbers past the hard drive partitions.
;Keep processing biosDrvs as normal. We just got to update DOS drive letter
; to go past the letters assigned to the fixed disk.
    mov al, byte [fixPtn]       ;Get the number of partitions from fixed
    add al, 2   ;Add two to this number to account for A and B.
    mov byte [dosDrv], al
    jmp .remLp
.msdExit:
    test byte [physVol], -1 ;Did we fail to initialise ANY devices?
    jz .noDevs  ;If so, we pretend we have two drives and hope defaults work!
    test byte [physVol], -1
    jz .noRems
    cmp byte [remDrv], 1
    jne .skipSingle
;Here we do the A: >-< B: jank.
;We know rbp points to what should the block for B:. The block for A:
; is right behind it.
.doSingle:
    mov rsi, rbp
    sub rsi, drvBlk_size    ;Go back a drvBlk (yuck!)
    or word [rsi + drvBlk.wDevFlgs], devMulti  ;Indicate multiple drives now
;Now we transfer the BIOS number and set drive number to 1
    movzx eax, byte [rsi + drvBlk.bBIOSNum]
    mov byte [rbp + drvBlk.bBIOSNum], al
    mov byte [rbp + drvBlk.bDOSNum], 1  ;Indicate B: drive
    lea rdi, qword [rbp + drvBlk.bpb]
    add rsi, drvBlk.bpb
    mov ecx, (drvBlk_size - 10)  ;8 bytes Link ptr and 2 id bytes
    rep movsb
    and word [rbp + drvBlk.wDevFlgs], ~devOwnDrv   ;Clear B:'s ownership!
    inc byte [physVol]  ;And add this drive to the count!
.skipSingle:
    movzx ebx, byte [physVol]   ;Get the number of detected volumes
    push rbx    ;Save this count on the stack
    dec ebx     ;Turn into an offset into the table
    mov eax, drvBlk_size    ;Get the size of a table entry
    mul ebx     ;Multiply the size by the offset, get table offset in rax
    lea rsi, msdDriver.drvBlkTbl    ;Go to the start of the table
    mov qword [rsi + rax + drvBlk.pLink], -1 ;Cut the table at this entry
    lea rbx, bpbArray
.buildBPBArrayLp:
    lea rdi, qword [rsi + drvBlk.bpb]       ;Get the BPB ptr for this entry
    movzx eax, byte [rsi + drvBlk.bDOSNum]  ;Get the DOS number for this entry
    mov qword [rbx + 8*rax], rdi            ;Use as offset into ptr array
    mov rdi, rsi                            ;Save this ptr before walking
    mov rsi, qword [rsi + drvBlk.pLink]     ;Now walk the table
    cmp rsi, -1 ;Did we read the end of the table?
    jne .buildBPBArrayLp    ;No... keep going
    mov rsi, rbx    ;Move the bpbArray pointer into rsi now
;
;Now we set the .optptr, .endptr and .numunt in driver request block
;
    mov rbx, qword [reqPktPtr]  ;Get the request packet ptr back
;All previous drivers return the "worst case" eject pointer. Here we return
; the real eject pointer
    add rdi, drvBlk_size    ;Mov rdi past end of last drvBlk 2 first free byte
    mov qword [rbx + initReqPkt.endptr], rdi    ;save as real eject point
    pop rax         ;Get back the number of detected volumes
    mov byte [rbx + initReqPkt.numunt], al  ;Store number of volumes
    mov qword [rbx + initReqPkt.optptr], rsi    ;Store the bpbArray here
    mov word [msdDriver.fnTbl], 0 ;Now prevent init from firing again
    return
.noRems:
;Pretend we do have something. If we are here, "worst case" we have 
; three fixed disk partitions. rbp points to the fourth one so pretend
; we just did the first remdev. This drive gets registered as having
; no changeline so if somehow this changes, worst case, no changeline.
    mov rbp, qword [rbp + drvBlk.pLink]
    inc byte [physVol]  ;Add the pretend A: drive to the count!
;Here we setup A: drive to be a pretend 1.44Mb drive
    jmp .doSingle
.noDevs:
;If we have no drives, then suppose we have two default drives
; A: and B: which are NOT the same!
    mov byte [physVol], 2   ;Identify entries A and B of system Ok!
;Now set them as their own owners and not shared status.
    lea rsi, msdDriver.drvBlkTbl
    or word [rsi + drvBlk.wDevFlgs], devOwnDrv
    mov rsi, qword [rsi + drvBlk.pLink]
    or word [rsi + drvBlk.wDevFlgs], devOwnDrv
    jmp .skipSingle
;------------------------
; Procedures for init
;------------------------

.processMbr:
;We have an MBR in the MBR table. Now we attempt to interpret it.
;If we return CF=CY, disk read failed. We skip this disk.
    call .copyMbr           ;Copy MBR table over from buffer
;HACK! In the case of disk 80h, we search for the first active primary
; ptn. We process it, invalidate the entry then start again
    cmp byte [biosDrv], 80h ;Only do this for the first fixed disk
    jne .pmbrNoHack
    lea rsi, mbrE   ;Point to the table
    mov ecx, 4
.pmbrHackLp:
    cmp byte [rsi + mbrEntry.ptnAtrib], 80h    ;If found, do hack
    je .pmbrHackFnd
.pmbrHackRet:
    add rsi, mbrEntry_size  ;Goto next entry
    dec ecx
    jnz .pmbrHackLp
    jmp short .pmbrNoHack   ;If disk one has no active primary, do normal.
.pmbrHackFnd:
    movzx eax, byte [rsi + mbrEntry.ptnType]
    call .checkDataPtn  ;If this is an extended partition, ignore it!
    jne .pmbrHackRet    ;And keep searching
    mov byte [mbrEtry], 3   ;Initialise the terminating condition
    call .pmbrLp ;Call the normal procedure, rsi -> Table entry
;Ignore any read errors, but we start again.
    mov byte [rsi + mbrEntry.ptnType], 0    ;Have that we ignore it now
;Now we fall down and start again
.pmbrNoHack:
    mov byte [mbrEtry], 0   ;Start from the first entry in the MBR
    lea rsi, mbrE
.pmbrLp:
    mov al, byte [rsi + mbrEntry.ptnType]
    call .checkMbrPtnType
    jnz .pmbrNext  ;Jump if not a valid partition type (includes free ptns)
    call .checkDataPtn
    je .pmbrDodata
    call .processEPtn   ;Here we process EBR
    retc                ;If this returns CF=CY, read error on disk. Exit!
    jmp short .pmbrXtrejoin   ;Now go to the next MBR entry
.pmbrDodata:
    mov ecx, dword [rsi + mbrEntry.lbaStart] ;And the partition start
    call .ptnUpdateBpb ;And update the BPB. If this fails, we skip the disk
    jnc .pmbrOk
    retnz ;If we had a read error, just exit!
;Else setup size and start sector and add the unformatted bit to the flags.
    call .getUfmtSize
    or word [rbp + drvBlk.wDevFlgs], devUnFmt  ;Register ptn. Freeze IO.
.pmbrOk:
    or word [rbp + drvBlk.wDevFlgs], devFixed | devOwnDrv
    mov byte [rbp + drvBlk.bDevType], typeHard  ;The media type is hard drive
    movzx eax, byte [rsi + mbrEntry.ptnAtrib]
    and eax, ptnActive
    or eax, ptnPrimary
    mov word [rbp + drvBlk.wPtnType], ax
    call .advDiskPtrs
.pmbrXtrejoin:
    call .physCheckEnd   ;If ZF=ZE, then we should end!
    rete 
.pmbrNext:
    inc byte [mbrEtry]  ;Increment the counter
    cmp byte [mbrEtry], 4
    rete
    add rsi, mbrEntry_size
    jmp short .pmbrLp

.processEPtn:
;Process all logical process in an extended partition. rsi points to
; the mbr entry in the mbrE table that describes this extended partition.
;Thus we always know the "root" sector of this extended partition.
    mov ecx, dword [rsi + mbrEntry.lbaStart]    ;Get the start of extended ptn
.peplp:
    call .readSector    ;Read the EBR sector in (sector number in ecx)
    retc    ;If we cant read the EBR in, assume end of logical partition.
    cmp word [msdDriver.inBuffer + ebr.mbrSig], 0AA55h
    je .pep1   ;If this doesnt have a valid ebr signature, end ext ptn parsing.
    stc
    return
.pep1:
    call .copyEbr       ;Save the EBR
;Now we check the saved EBR is valid.
    movzx eax, byte [ebrE + mbrEntry.ptnType]
    call .checkEbrPtnType   ;Here we dont allow another extended case
    jnz .pepNextEbr ;If this is not a valid ptn type, goto next
    movzx eax, byte [ebrE + mbr_size + mbrEntry.ptnType]    ;Get link ptn type
    call .checkDataPtn  ;This MUST NOT be a Data ptn
    je .pepNextEbr  ;If it is a data ptn, we ignore this entry as it is invalid.
;Now we get the absolute start sector of this partition.
    add ecx, dword [ebrE + mbrEntry.lbaStart]   ;Add relative start
    call .ptnUpdateBpb  ;Read and interpret the BPB (preserves sector number)
    jnc .pepOk
    retnz ;If we had a read error, just exit!
;Else setup size and start sector and add the unformatted bit to the flags.
    call .getUfmtSize
    or word [rbp + drvBlk.wDevFlgs], devUnFmt  ;Register ptn. Freeze IO.
.pepOk:
;If here, this partition will be given a CDS entry.
;Now we go to the next logical partition in the extended partition and
; move to the next drive
    or word [rbp + drvBlk.wDevFlgs], devFixed | devOwnDrv
    mov byte [rbp + drvBlk.bDevType], typeHard  ;The media type is hard drive
    movzx eax, byte [rsi + mbrEntry.ptnAtrib]
    and eax, ptnActive
    or eax, ptnLogical
    mov word [rbp + drvBlk.wPtnType], ax
    call .advDiskPtrs
    call .physCheckEnd   ;If ZF=ZE, then we should end!
    rete 
.pepNextEbr:
;Else we now walk the disk linked list.
    mov ecx, dword [ebrE + mbr_size + mbrEntry.numSectors] ;Get rel strt of ptn
    test ecx, ecx   ;Is the start sector of the next logical ptn 0?
    retz    ;Return if so.
    add ecx, dword [rsi + mbrEntry.lbaStart]    ;Else, make it absolute sector
    jmp .peplp

.getUfmtSize:
;Moves the partition geometry information into drvBlk from MBR. This is only
; a necessity if the medium doesn't have a trustworthy BPB. Doesn't touch CHS 
; stuff because bleugh. That can be computed by FORMAT.
;
;This is done so that unformatted partitions are mounted by DOS. They cannot 
; be used, and so needs to be FORMATed. Thus, NumFAT=0 should only really 
; happen on hard drive partitions at boot.
;
;Input: rsi -> MBR/EBR entry for this partition
;       rbp -> drvBlk for this drive we are setting up
;       rcx = Sector number of the start of the partition
;
;Output: The following fields in the drvBlk are filled in
;       bNumFAT = Number of FATs on partition (0 as unformatted)
;       dHiddSec = Starting sector of the partition
;       dTotSec32 = 32 bit size of partition (if large enough)
;       wTotSec16 = 16 bit size of partition or 0 if 32 bit in use
    mov byte [rbp + drvBlk.bNumFAT], 0 ;Indicate no FATs on ufmt ptn.
    mov dword [rbp + drvBlk.dHiddSec], ecx  ;Set start sector of partition
    mov ecx, dword [rsi + mbrEntry.numSectors]
    cmp ecx, 0FFFFh
    jbe .gpgSmol
    mov dword [rbp + drvBlk.dTotSec32], ecx
    xor ecx, ecx
.gpgSmol:
    mov word [rbp + drvBlk.wTotSec16], cx
    clc
    return

.physCheckEnd:
    cmp byte [physVol], drvBlkTblL - 2   ;Once here we are done!
    return

.advDiskPtrs:
    call .getNumCyl     ;Computes the number of cylinders in the BPB
    call .xfrDfltBpb    ;Finish drvBlk init by transfering dfltBPB
    mov rbp, qword [rbp + drvBlk.pLink]    ;Go to the next disk entry.
    inc byte [dosDrv]       ;Go to the next DOS device
    inc byte [physVol]
    return

.copyEbr:
;Copies the EBR from the EBR sector in the buffer
    push rcx
    push rsi
    push rdi
    lea rdi, mbrE
    mov ecx, 2*mbrEntry_size
    jmp short .cpmbr
.copyMbr:
;Copies the MBR from the MBR sector in the buffer
    push rcx
    push rsi
    push rdi
    lea rdi, mbrE
    mov ecx, 4*mbrEntry_size
.cpmbr:
    lea rsi, qword [msdDriver.inBuffer + mbr.mbrEntry1]
    rep movsb
    pop rdi
    pop rsi
    pop rcx
    return

.readSector:
;Input: ecx = Sector to read
    movzx edx, byte [biosDrv]
    mov eax, 8201h  ;LBA Read One sector
    lea rbx, msdDriver.inBuffer
    int 33h
    return

.ptnUpdateBpb:
;Setups up the call for below on the current partition.
;Input: ecx = Number of hidden sectors (preserved)
;Output:
;   CF=NC: All ok, BPB entry in rbp filled.
;   CF=CY and ZF=ZE: Bad BPB read.
;   CF=CY and ZF=NZ: Sector read failed.
    mov word [rbp + drvBlk.wDevFlgs], 0    ;Clean the flags to start with
    mov dword [rbp + drvBlk.dHiddSec], ecx
    movzx eax, byte [dosDrv]
    mov byte [rbp + drvBlk.bDOSNum], al ;Save the DOS number
    movzx eax, byte [biosDrv]   ;Get the BIOS drive
    mov byte [rbp + drvBlk.bBIOSNum], al
    push rcx
    push rsi    ;Save the mbr entry ptr
    call msdDriver.updateBpb
    jc .pubBad
    call msdDriver.moveVolIds
    pop rsi
    pop rcx
    clc         ;Always clean even if no ids
    return
.pubBad:
    pop rsi
    pop rcx
    cmp al, drvBadMed ;Bad Partition?
    stc         ;Ensure we set the CF again
    return

.getNumCyl:
;Computes the number of cylinders on the media. Not really to be used.
;Input: rbp -> drvBlk
    push rax
    push rcx
    push rdx
    movzx eax, word [rbp + drvBlk.wNumHeads]
    movzx ecx, word [rbp + drvBlk.wSecPerTrk]
    mul ecx ;Get sectors per cylinder in eax. edx = 0
    test eax, eax   ;If ax is 0, store zero! Phoney CHS data in BPB.
    jz .gncExit     ;This prevents CHS IOCTL from occuring.
    mov ecx, eax    ;Save this number in ecx
    movzx eax, word [rbp + drvBlk.wTotSec16]
    test eax, eax   ;If this is zero, get the 32 bit count of sectors
    cmovz eax, dword [rbp + drvBlk.dTotSec32]
    div ecx     ;sectors/(sectors/cylinder) = whole cylinders in eax
    test edx, edx
    jz .gncExit
    inc eax     ;Inc the cylinder count to account for not whole divide
.gncExit:
    mov edx, 0FFFFh   ;A default Max cylinder value, since CHS is for floppies.
    cmp eax, edx
    cmova eax, edx
    mov word [rbp + drvBlk.wNumCyl], ax
    clc ;Ensure CF is clear
    pop rdx
    pop rcx
    pop rax
    return

.xfrDfltBpb:
;If a drive is removable, we check the BIOS reported values and 
; build a BPB around that. Else, we trust the bpb and blindly copy it.
;
; TEMP TEMP: FOR NOW WE JUST ALWAYS BLINDLY TRUST THE BPB.
;
    push rcx
    push rsi
    push rdi
    lea rsi, qword [rbp + drvBlk.bpb]
    lea rdi, qword [rbp + drvBlk.sDfltBPB]
    mov ecx, bpb32_size
;Copies garbage into the reserved 12 bytes at the end of the BPB32
; but thats ok since we dont use it and those fields are reserved.
    rep movsb
    pop rdi
    pop rsi
    pop rcx
    return

.checkEbrPtnType:
;Input: al = Partition type
;Output: ZF=ZE => Valid partition type found
;        ZF=NZ => Not a valid partition type (05h and 0Fh not valid)
    call .checkMbrPtnType
    retne           ;Bubble up the not equal if not in the partition table
.checkDataPtn:
    cmp al, 05h     ;Extended Partition which should use CHS for addressing
    je .ceptBad
    cmp al, 0Fh     ;Extended Partition which should use LBA for addressing
    je .ceptBad
    cmp eax, eax    ;Set the Zero flag
    return
.ceptBad:
    test eax, eax   ;Clears the Zero flag (as eax is not zero)
    return

.checkMbrPtnType:
;Input: al = Partition type
;Output: ZF=ZE => Valid partition type found
;        ZF=NZ => Not a valid partition type
    push rcx
    push rdi
    lea rdi, .ptnTbl
    mov ecx, .ptnTblL
    repne scasb ;Find the entry in al
    pop rdi
    pop rcx
    return

;Table contents:
; 01h - FAT 12 Partition. CHS addressing should be used.
; 04h - FAT 16 Partition up to 32MB. CHS addressing should be used.
; 05h - Extended Partition in MBR found. CHS addressing should be used.
; 06h - FAT 16 Partition over 32MB. CHS addressing should be used.
; 0Bh - FAT 32 Partition. CHS addressing should be used.
; 0Ch - FAT 32 Partition. LBA addressing should be used.
; 0Eh - FAT 16 Partition. LBA addressing should be used.
; 0Fh - Extended Partition in MBR found. LBA addressing should be used.
.ptnTbl db 01h, 04h, 05h, 06h, 0Bh, 0Ch, 0Dh, 0Eh, 0Fh
.ptnTblL    equ $ - .ptnTbl 
