;Generic MBR program
BITS 16
ORG 600h    ;We start at 7C00h but relocate ourselves low

ep:
    cli ;Pause interrupts for copy low
    cld
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 7C00h
    sti
    mov si, sp
    mov di, 600h
    mov cx, 100h    
    rep movsw
    jmp 0:main  ;Set CS to 0 too
main:
    mov byte [btdrv], dl    ;Save the boot drive in case its not 80h
    

msg1:   db "Invalid partition table.", 0
msg2:   db "Error loading operation system",0
msg3:   db "Missing operating system",0
btdrv:  db 0    ;Var for the boot drive
    times (01BEh - ($-$$)) 00h    ;Pad the partition table to the right place

tbl1:   db 10h dup (0)
tbl2:   db 10h dup (0)
tbl3:   db 10h dup (0)
tbl4:   db 10h dup (0)

        dw 0AA55h