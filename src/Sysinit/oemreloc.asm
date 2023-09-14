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
    jmp sysinit
;Jump with rsi -> BIOS userbase
;          rdi -> APT load area (page aligned)
;          r15 -> Boot drive