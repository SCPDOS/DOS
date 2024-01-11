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
    mov word [rbx], drvDonStatus ;Set the drive done status bit
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
    add ecx, 20  ;Add 20 years to turn it to a year offset from 1980
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
    ;We create a function to deal with BPB parsing etc
    ;Start with the first primary partition on each hard disk (until max)
    ;   They dont have to be bootable
    ;Then go back and look for other partitions partitions. 
    ;   Add each other primary or logical ptn (until max)
    ;Then finish with removable devices. First two devs become A: and B: resp.
    ;Use r8 as device counter
    ;Use r15 as the pointer to the next free BPB entry
    ;First set up the two default BPB's if no removable drives
    push rbx    ;Push the pointer to the driver parameter block

    int 31h ;Get number of Int 33h devices in r8b
    shr r8, 8   ;Isolate bytes 1 and 2 of r8
    mov ax, r8w
    mov byte [remDrv], ah    ;Save num of phys int 33h rem drives
    mov byte [fixDrv], al    ;Save number of physical hard drives
    mov byte [physVol], 0    ;Initialise this byte to 0

    lea r15, [msdDriver.msdBPBblks]    ;Point to the BPB storage place
    cmp byte [fixDrv], 0 ;Do we have any fixed drives?
    jz .remInit ;No? Go to removables
    mov r8, 2   ;Device number 2 = C:
    mov dl, 80h ;Start with HDD 0
.primary:
    cmp byte [physVol], 3  ;Are we at maximum devices (A: B: reserved)?
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
    call .readSectorAndAddDataToTables
.primaryEpilog:
    inc dl  ;Goto next BIOS drive
    mov dh, dl
    and dh, 7Fh ;Clear bit 7
    cmp dh, byte [fixDrv]    ;Have we gone thru all hard drives?
    jne .primary    ;Whilst we have fewer, go back
.extended:
;We have gone through all the devices once
    ;cmp byte [physVol], 3  ;Are we at maximum devices (A: B: reserved)?
    ;je .remInit ;If yes, get removable devices
    ;mov dl, 80h ;Go back to hard drive 80h
    ;xor ecx, ecx    ;Get MBR back
    ;call .initReadSector
    ;Now we eventually search MBR for a FAT extended partition
.remInit:
;Start by linking the default BPB's in the pointers table in the event that
; for some reason the removable drives stop working or dont exist.
    lea rsi, qword [msdDriver.dfltBPB]  ;Point to the default BPB
    lea rdi, qword [msdDriver.msdBPBTbl]  ;Point to the BPB ptr table
    mov qword [rdi], rsi    ;Store the pointer in the first two entries
    mov qword [rdi + 8], rsi
;This forces the hard drives to start at C:
    mov r9, r8  ;Save number of next device after fixed drive in r9
    xor dl, dl  ;Start with removable device 0
    mov r8b, dl ;Once r8b becomes 2, go past the disk drives
    ;rdi points to the space for the subsequent bpb's
    cmp byte [remDrv], 0  ;Just skip removable init if no rem drives
    jnz .removables
    add byte [physVol], 2 ;Pretend we have two more drives (A: and B:)
    ret ;and return!
.removables:
    xor ecx, ecx    ;Read sector 0
    call .readSectorAndAddDataToTables
.removableEpilogue:
    inc dl  ;Goto next BIOS device now
    cmp dl, byte [remDrv] ;Are we past last rem dev?
    je .end
    cmp r8, 2 ;Are we back at drive C: ?
    jne .re0
    mov r8b, r9b    ;Return to this drive number
.re0:
    cmp r8b, 5  ;Are we at logical device 5 (F:, not supported)?
    jb .removables
.end:
    cmp byte [remDrv], 1  ;Do we have only 1 removable device?
    je .singleRemDev
.msdExit:
    pop rbx ;rbx points to the parameter block
    ;Now we set the .optptr, .endptr and .numunt
    push rax
    movzx eax, byte [physVol]   ;Get the number of detected volumes
    mov byte [rbx + initReqPkt.numunt], al
    lea rax, msdDriver.msdBPBTbl    ;Get the BPB table here
    mov qword [rbx + initReqPkt.optptr], rax
    pop rax
    jmp devDrvExit  ;Sets .endptr and the status word
.singleRemDev:
    ;Copy Drive A: BPB pointer and BIOS map data for Drive B:
    lea rbx, qword [msdDriver.msdBIOSmap]
    mov dl, byte [msdDriver.msdBIOSmap]   ;Get drive A: BIOS map
    mov byte [rbx + 1], dl  ;Store in byte for Drive B:
    lea rbx, qword [msdDriver.msdBPBTbl] 
    mov rdx, qword [rbx]    ;Get BPB pointer of Drive A:
    mov qword [rbx + 8], rdx    ;Store in qword for Drive B:
    mov byte [msdDriver.msdSingleFlag], -1   ;Set this mode on
    mov byte [msdDriver.msdSingleDrv], 0 ;Start on drive A
    inc byte [physVol] ;Gotta register the phantom drive!
    jmp short .msdExit
.initReadSector:
;Called with sector number in rcx and BIOS device number in dl
    mov ah, 82h ;Read
    mov al, 1   ;One sector
    lea rbx, qword [msdTempBuffer]  ;Into temporary buffer
    int 33h
    ret

.readSectorAndAddDataToTables:
;Input:
;ecx = Sector number to read
;r15 -> bpb array entry for the BPB
;r8 = Logical Drive number (offset into arrays)
    call .initReadSector
    retc   ;Goto next device
    ;Now verify this is a BPB
    mov al, byte [rbx]  ;rbx is pointed to the temp buffer by initreadsector
    mov ah, byte [rbx + 2]
    cmp ax, 090EBh  ;WinDOS and SCP compatible (always generate short jmp)
    retne   ;If not, skip
    ;Now copy data to internal tables
    mov rsi, rbx    ;Point rsi to the temp buffer
    push rcx
    mov ecx, bpbEx_size/8   ;Copy BPB
    mov rdi, r15
    rep movsq   ;Copy the BPB
    pop rcx
    ;Store BIOS map value and BPBblk pointer in bpbTbl
    lea rbx, qword [msdDriver.msdBIOSmap]
    add rbx, r8
    ;Add device count to rbx to point to correct entry
    mov byte [rbx], dl  ;Store BIOS map value 
    lea rbx, qword [msdDriver.msdBPBTbl]
    lea rbx, qword [rbx + 8*r8]
    mov qword [rbx], r15
    inc r8  ;Goto next logical drive
    inc byte [physVol] ;Increment the number of valid drives we have
    add r15, bpbEx_size  ;Goto next table entry
    return