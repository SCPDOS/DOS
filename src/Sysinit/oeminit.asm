%use masm

OEMINIT PROC NEAR
;Sets the Statistical data and adds any page tables that are needed
;Entered with:
;           rsi -> BIOS userbase
;           rdi -> APT load area (page aligned)
;           r15 -> Boot drive
;------------------------------------------------;
;      Start saving basic DOS data to the        ; 
;                OEM Variables                   ;
;------------------------------------------------;
    xor eax, eax    ;Drive A:
    mov ebx, 2      ;Drive C:
    test r15b, 80h  ;Is the hard drive bit set?
    cmovnz eax, ebx
    mov byte [DFLTDRIVE], al    ;Set default drive
;Copy DOS to its final resting place
    mov qword [biosUBase], rsi  ;Local OEM variable
    mov byte [FILES], 8         ;Default 8 files, initially 5 only
    mov byte [BUFFERS], 30      ;Default 30 buffers, at start 1
    mov byte [LASTDRIVE], 5     ;Default Last drive value
    mov dword [OEMVERSION], 0   ;CSM BIOS
    mov qword [OEMPTR], 0       ;No pointer
    mov byte [OEMBIOS], 0       ;Use SCPBIOS/SCPDOS kernel names
   ;If no detected Int 33h devices, halt 
    int 31h ;Get number of Int 33h devices in r8b
    shr r8, 3*8
    test r8b, r8b
    jz OEMHALT
    mov rdi, rsi
    and rdi, ~0FFFh ;round up to next 4Kb page after biosUBase
    add rdi, 1000h
;------------------------------------------------;
;      Add additional page tables before the     ;
;                   data area.                   ;
;------------------------------------------------;
;This will allow for up to 64Gb of addressible space
    ;Each entry is a 2Mb (200000h) multiple from 4Gb (100000000h)
    mov ecx, aptSize/8   ;This many entries as qwords
    push rdi        ;rdi points to the APT space
    mov rax, 100000000h | 83h ;Make each pde 2Mb, present and r/w
pdtLoop:
    stosq
    add rax, 200000h
    dec ecx
    jnz pdtLoop
    pop rax ;Get the pointer back to the top of the memory area in rax
;Now we add every 4kb page to the page directory pointer table
;15 4kb pages to consider
    mov qword [FINALDOSPTR], rdi ;rdi now points to where dSeg will go
    push rdi
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

    mov rdi, cr3    ;Reload cr3 to make the system aware of new table entries
    mov cr3, rdi    ;(is this strictly necessary?)
;------------------------------------------------;
;       Set Device Driver Chain and Exit         ;
;------------------------------------------------;  
;Before we exit, set the device driver chain
    mov rax, driverChain
    pop rdi
    add rax, rdi    ;Add the DOSSEG base address
    mov qword [OEMDRVCHAIN], rax  ;Store this value here
    ret
aptSize equ 60*4096 ;(APT = Additional Page Tables)
OEMINIT ENDP

OEMMCBINIT PROC NEAR
    mov eax, 0E820h ;Get memory map
    int 35h ; rsi has pointer to memory map
    mov rax, qword [biosUBase]
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
    mov dword [loProtMem], eax
    mov dword [hiProtMem], ebx
    jmp mcbBuild
.worst:
    ;Get USERBASE pointer and subtract it from 2Mb
    mov eax, 200000h
    mov rbx, qword [biosUBase]   ;Get userbase
    sub eax, ebx
    mov dword [loProtMem], eax  ;The leftover goes here
    jmp mcbBuild 
.mcbi1:
    mov rdx, qword [rax]    ;Save the userbase in rdx
    mov rbx, 100000001h ;Valid entry signature
    cmp qword [rax + 16], rbx ;If entry is marked as invalid, fail boot
    jne .mcbFail
    mov rax, qword [rax + 8]    ;Get arena size in rax
    ;PCI hole always exists so this value will always be a dword
    mov dword [loProtMem], eax
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
    mov dword [hiProtMem], ebx   ;Save data 
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
    mov qword [longMem], rbx   ;Save data 
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
    jmp OEMHALT
mcbFailmsg db "Memory Allocation Error",0Ah,0Dh,0

mcbBuild:
;Actually build the MCB chain here
;Start by computing the difference between userbase and DOS area
    mov rbx, qword [biosUBase]
    mov rsi, qword [MCBANCHOR]  
    mov eax, dword [rsi + mcb.blockSize]    ;Get the size of the block
    shl rax, 4      ;Convert to number of allocated bytes
    add rax, mcb_size   ;Add the mcb itself to the count
    add rax, rsi    ;Add the pointer to the mcb to get pointer to free space
    mov rdi, rax    ;Save this value as the pointer to the next MCB
    sub rax, rbx    ;Get difference from userbase and first byte after DOS
    sub dword [loProtMem], eax  ;Remove difference from the free bytes count
    jc OEMHALT                  ;If this carries, fail
    cmp dword [loProtMem], 8000h   ;Need a minimum of 32Kb free space.
    jb OEMHALT
    mov byte [rsi + mcb.marker], mcbMarkCtn ;Now mark anchor as not end
    mov rbx, rdi    ;Get the pointer to the free space back
    mov byte [rbx + mcb.marker], mcbMarkEnd  ;Mark as end of chain
    mov qword [rbx + mcb.owner], mcbOwnerFree
    xor esi, esi
    mov esi, dword [loProtMem]
    sub esi, mcb_size   ;Now remove one mcb's worth of space
    shr esi, 4  ;Shift down by a nybble to get paragraphs
    mov dword [rbx + mcb.blockSize], esi

    ;Now check the hiProtMem count. If it is 0, skip ISA hole computations.
    cmp dword [hiProtMem], 0
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
    mov ecx, dword [hiProtMem]
    shr ecx, 4  ;Get paragraphs
    sub ecx, (mcb_size>>4)  ;Reserve space for one mcb
    mov dword [rbx + mcb.blockSize], ecx
.skipISA:
    ;Now check the longMem count. If it is 0, skip PCI hole computations.
    ;rbx points to a block with "Z" marker
    cmp dword [longMem], 0
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
    mov rcx, qword [longMem]
    shr rcx, 4
    sub ecx, (mcb_size>>4)  ;Reserve space for one mcb
    mov dword [rbx + mcb.blockSize], ecx
.exit:
    ret
OEMMCBINIT ENDP

OEMHALT PROC    NEAR
;If a critical error occurs during sysinit, fail through here
;Int 42h, 43h and 44h point here during sysinit
    lea rbp, hltmsg
    mov eax, 1304h
    int 30h
.ei0:
    hlt
    pause
    jmp short .ei0
hltmsg  db "Error initialising SCPDOS.SYS. System halting...",0
OEMHALT ENDP

OEMCALLBK PROC NEAR
    ret
OEMCALLBK ENDP

;OEM Only variables are permitted in this file. They are not 
; visible to SYSINIT
biosUBase   dq 0
loProtMem   dd 0
hiProtMem   dd 0
longMem     dq 0