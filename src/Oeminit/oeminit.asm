%use masm
;SYSINIT doesnt care about the internal structure of the OEMINIT module.
;Thus, an OEM is free to arrange code and data within the OEMINIT module,
; as they please. OEMINIT is always the first module linked to in the DOS
; binary blob file and therefore an OEM can guarantee that the first byte 
; of the OEMINIT module will be the first byte executed by the machine.
;SYSINIT starts being invoked only once OEMINIT jumps to the symbol SYSENTRY.
;OEMINIT can even be an .EXE or .ELF executable if the firmware allows it, 
; as long as it can link with SYSINIT by EXPORTING and IMPORTING the right
; symbols, its ok! Also, the DOS linker script requires that the OEMINIT 
; module be the first thing in the executable file, with the default
; kernel drivers being the at the end, after the DOS, in the segment
; kDrvText, kDrvData and kDrvBSS.
;OEMINIT has no BSS segment, but has otext and odata where it can link 
; itself into.
;---------------------------------------------------------------------------;
;PUBLIC PROCEDURES needed to link with SYSINIT:                             ;
;---------------------------------------------------------------------------;
; OEMMCBINIT -> Does MCB chain building as SYSINIT doesn't know how to read ;
;   any memory maps. Thats on the OEM to parse and build for us.            ;
; OEMHALT -> If anything goes wrong during the initial phase of SYSINIT,    ;
;   it will use this routine to print a message and halt the machine.       ;
; OEMCALLBK -> Used to finalise any setup before xfring control to SHELL=   ;
;   At this point, DOS is ready to be used.                                 ;
;---------------------------------------------------------------------------;
;EXTERN VARS needed to link with SYSINIT:                                   ;
;---------------------------------------------------------------------------;
; These vars need to be initialised before jumping to SYSENTRY              ;
;---------------------------------------------------------------------------;
;FINALDOSPTR dq ?    ;Pointer to where dSeg should be loaded                ;
;FILES       db ?    ;Default number of FILES                               ;
;BUFFERS     db ?    ;Default number of BUFFERS                             ;
;DFLTDRIVE   db ?    ;Default drive number (0-25), this is the boot drive   ;
;LASTDRIVE   db ?    ;Default last drive number (0-25)                      ;
;OEMBIOS     db ?    ;Set if to use IO.SYS or clear if to use SCPBIOS.SYS   ;
;OEMPTR      dq ?    ;Pointer to store at biosPtr                           ;
;OEMVERSION  dd ?    ;BIOS number, to be used by drivers for id-ing         ;
;---------------------------------------------------------------------------;
; These vars are initialised by SYSINIT, to be used in OEMMCBINIT           ;
; These vars are undefined outside of OEMMCBINIT                            ;
;---------------------------------------------------------------------------;
;MCBANCHOR   dq ?    ;Pointer to the Anchor MCB, part of dSEg               ;
;---------------------------------------------------------------------------;
; These vars are initialised by SYSINIT, to be used in OEMCALLBK            ;
; These vars are undefined outside of OEMCALLBK                             ;
;---------------------------------------------------------------------------;
;OEMMEMPTR   dq ?    ;Var to save ptr to the 64Kb block passed to OEMCALLBK ;
;---------------------------------------------------------------------------;
;

OEMRELOC PROC NEAR  ;OEMINIT Entry point from SCP/BIOS
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
    db 200h-($-$$) dup 90h ;Fill rest of the sector with NOPs
;END OF FIRST SECTOR!!
;Now move the alignment of the DOSSEG to 4Kb boundary
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
;Now sets the Statistical data and adds any page tables that are needed
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

;----------------------------------------------------------------
;                       PIC Remap procedure                     :
;----------------------------------------------------------------
;Remapping the IRQ lines to Interrupts 0F0h - 0FFh
    cli
    mov al, 0FFh    ;Mask all interrupts 
    out 021h, al
    out 0A1h, al
    sidt [oemIDTptr]    ;Get the idt here
    mov rsi, qword [oemIDTptr.Base] ;Get the base ptr
    mov rdi, rsi
    add rsi, 020h*10h
    add rdi, 0F0h*10h
    mov ecx, 2*10h    ;Copy the hardware IRQ pointers high!
    rep movsq

    mov al, 11h        ;bit 10h and 1h = Start initialisation
    out 020h, al
    out 080h, al    
    out 0A0h, al
    out 080h, al    
    mov al, 0F0h       ;PIC1 to take Int 0F0h - F7h
    out 021h, al
    out 080h, al    
    add al, 8        ;PIC2 to take Int  F8h - FFh
    out 0A1h, al 
    out 080h, al    
    mov al, 4
    out 021h, al    ;Tell PIC 1 that there is a PIC 2 at IRQ2 (00000100)
    out 080h, al    
    dec al
    dec al
    out 0A1h, al    ;Tell PIC 2 its cascade identity (00000010)
    out 080h, al
    mov al, 01h        ;Initialise in 8086 mode
    out 021h, al
    out 080h, al    
    out 0A1h, al
    out 080h, al    
    lidt [oemIDTptr] 
    xor eax, eax    ;Unmask all interrupts 
    out 021h, al
    out 0A1h, al
    sti

    jmp SYSENTRY    ;Now all vars setup, we can proceed!
OEMSYSINIT ENDP
aptSize equ 60*4096 ;(APT = Additional Page Tables)

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
;Int 22h, 23h and 24h point here during sysinit
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
oemIDTptr:      ;Local IDT pointer
    .Limit  dw 0
    .Base   dq 0