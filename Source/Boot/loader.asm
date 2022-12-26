BITS 16
ORG	600h

relocBase   equ 600h ;Relocate to 600h
loadAddress equ 800h
    jmp short start
    nop
;---------------------------Tables--------------------------
    osname: db 'SCPDOSv1'

    ;Start of BIOS Parameter Block

    bypsec: dw 0200h     ;bytes per sector (200h=512)                  
    secpcl: db 01h       ;sectors per cluster                          
    ressec: dw 0001h     ;reserved sectors                             
    numFAT: db 02h       ;number of FATs                               
    nortdr: dw 00E0h     ;number of root directory entries             
    nosect: dw 0B40h     ;number of sectors (1440 sectors per side)    
    medesc: db 0F0h      ;media descriptor (f0=FDD)                    
    FATsec: dw 0009h     ;number of sectors per FAT                    
    sectrc: dw 0012h     ;number of sectors/tracks                     
    numhed: dw 0002h     ;number of read/write heads                   
    numhid: dd 00000000h ;number of hidden sectors                     
    nsecfs: dd 00000000h ;number of "huge" sectors in the FS (FAT)     

    ;End of BPB

    ldrvnu: db 00h       ;logical drive number, 80h=first HDD, 00h=1st FDD
    res1:   db 00h       ;reserved sector 1, BS reserved, used in boot
    extsig: db 29h       ;Extended boot signature (29h = EBPB signature)

    ;Start of Extended BPB
    sernum: dd 1C2A0D0Fh       ;serial number of drive
    vollbl: db 'NO NAME    '  ;default volume label name
    fstype: db 'FAT12   '     ;file system type
    
;-----------------------------------------------------------
;Non BPB additional variables
startSector:        dd 33   ;Usually read as a word
numberOfSectors:    dw 58   ;Number of sectors to read
start: 
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 8000h
    mov si, 7C00h 
    mov di, relocBase
    mov cx, 100h
    cld             ;Ensure writes are in the write direction
    rep movsw
    sti
    jmp 0:s1       ;Far jump to the next instruction

s1:
    cmp byte [relocBase + 509], -1  ;Any non -1 value is non-bootable
    jne fail
    mov si, dx  ;Save drive number in si
    mov ax, 0e801h
    int 15h
    cmp ax, 800h    ;Get number of Kb
    jae .s2         ;Above or equal, OK!
    xor al, al      ;Error code
    cmp cx, 800h
    jb fail
.s2:
    mov ax, 03h
    int 10h ;set video mode
;sectrc used and numhed used for sectors per track and number of heads
    mov dx, si
    mov byte [drvnum], dl   ;Save the drive byte from dl
    test dl, 80h
    jz readFloppy
;If the boot device is emulated as a hard drive, 
;   use BIOS extensions as CHS is buggy.
    mov si, pktptr
    mov di, si
    xor ax, ax
    mov cx, 8
    rep stosw   ;Store 8 zero words
    mov word [si], 0010h    ;Packet size and reserved zero
    mov ax, word [numberOfSectors]
    mov word [si + 2], ax   ;Number of sectors to transfer
    mov word [si + 4], loadAddress   ;Offset of buffer
    mov word [si + 6], 0      ;Segment of buffer
    mov ax, word [startSector]
    mov word [si + 8], ax     ;Starting sector
    mov ax, 4200h
    int 13h
    mov ah, 6
    jc fail
    jmp short launchSCP
readFloppy:
    mov si, 10h     ;Up to 16 error retries
    mov di, word [numberOfSectors]  ;Copy MAXIMUM 58 sectors!!!!
    mov bp, word [startSector]      ;Start at LBA 33
    mov bx, loadAddress    ;Start copy buffer at 800h
readDisk:
;Convert bp into CHS for int 13h
    push bp         ;Save the current LBA on the stack temporarily
;Sector
    mov ax, bp		;mov LBA into ax to get head and sec num
	div byte [sectrc]	;divide ax by the low byte of sectrc	
	inc ah			;increment the remainder to get sectors
	mov cl, ah		;save the remainder in its ret register
;--------------------------------------				
;Head
	xor ah, ah		;nullify the remainder for the next part
	div byte [numhed]	;divide ax by the low byte of numhed
	mov ch, ah		;Save the head in ch
;--------------------------------------				
;Cylinder
	mov ax, word [numhed]	;mov numhead into ax
	mul word [sectrc]	    ;multiply ax by sec/trc
	xchg bp, ax		;switch bp and ax so that we can divide them
	div bp			;Divide them here!
	mov dh, al		;Save the result in dh
;--------------------------------------				
    xchg ch, dh     ;Swap ch and dh for return value
    pop bp          ;Return the current LBA
    mov dl, byte [drvnum]   ;we saved the drive in medesc
    mov ax, 0201h           ;Disk read, one sector at a time
    int 13h
    jc diskError	    ; Error detected, restart file copy
    add bx, 200h        ; Goto next sector position
    inc bp
    dec di
    jnz readDisk
launchSCP:
;Construct SCPBIOS SysInit Parameter Table
    mov bx, SysInitTable    ;es points to segment, get table to bx
    jmp 0:loadAddress ; go to the next file
diskError: 
    xor ax, ax 		; Disk reset
    int 13h			
    dec si
    jnz readDisk	; Reset disk and read sector again
;-------------------------------Errors------------------------------
fail:
    mov si, .msg
.write: ;destroys registers ax and bx
    lodsb
    cmp al, 0 ;check for zero
    je .cont
    mov ah, 0Eh	;TTY output
    mov bx, 0007h ;colour
    int 10h
    jmp short .write
.cont:
    xor ax, ax
    int 16h	;await keystroke
    int 18h	;Reset
.msg: db "Non System Disk or Disk Error.",0Ah,0Dh,0

times 509-0Ch-($-$$) db 0E8h
SysInitTable:
    .lengthb    db 0Ch
    .numSecb    db 1
    .resWord    dw 00h
    .FileLBA    dq 5Bh  ;Start at Sector 91 (first sector of load)

bootOnFlag:     db -1   ;Bootable signature
                db 55h
                db 0AAh

Segment .bss nobits start=502h
drvnum  resb  1 ;Drive number
    alignb 4
pktptr  resq  2 ;Packet Pointer, 16 bytes in size