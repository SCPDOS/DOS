;FDISK BIOS implementers subroutine file.
;All functions that need to be implemented by BIOS implementers 
; are contained within this file with a description of what goes where and how.
;All used registers are expected to be preserved across a call.


;DISK FUNCTIONS
;These functions are for Disk actions
biosGetNumberOfFixedDisks:
;Saves the number of Fixed Disks in numDisks
;Output: byte [numDisks] = Number of physical hard disks
    push rax
    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15
    int 31h
    shr r8, 8   ;Isolate bytes 1 of r8
    mov byte [numDisks], r8b    ;Save number of physical hard drives
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rax
    return

biosReadSector:
;Reads one sector.
;Input: byte [curentDisk] = 1 based disk number to transact on
;       qword [xferBuffer] = Ptr to where to read from disk to
;       qword [sectorNum] = Number of disk sector to read
;Output: CF=NC, Nothing went wrong
;        CF=CY, Error in disk read.
    push rax
    push rbx
    push rcx
    push rdx
    mov rbx, qword [xferBuffer]
    mov rcx, qword [sectorNum]
    call biosgetBIOSDiskNumber  ;Get disk number in dl in our case
    mov eax, 8201h
    int 33h
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return
    
biosWriteSector:
;Writes one sector.
;Input: byte [curentDisk] = 1 based disk number to transact on
;       qword [xferBuffer] = Ptr to where to write to disk from
;       qword [sectorNum] = Number of disk sector to write to disk
;Output: CF=NC, Nothing went wrong
;        CF=CY, Error in disk write.
    push rax
    push rbx
    push rcx
    push rdx
    mov rbx, qword [xferBuffer]
    mov rcx, qword [sectorNum]
    call biosgetBIOSDiskNumber
    mov eax, 8301h
    int 33h
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return

biosResetHardDisk:
;Reset currentDisk
;Input: byte [curentDisk] = 1 based disk number to reset
;Output: CF=NC, Nothing went wrong
;        CF=CY, Failed to reset the disk system. Abort...
    push rax
    push rdx
    call biosgetBIOSDiskNumber
    xor eax, eax
    int 33h
    pop rdx
    pop rax
    return

biosGetHardDiskParameters:
;Get the size of the hard disk
;Output: qword [curDiskSize] = Number of sectors on disk
;        word [sectorSize] = Bytes/Sector value
    push rax
    push rbx
    push rcx
    push rdx
    call biosgetBIOSDiskNumber
    mov eax, 8800h
    int 33h
    ;Int 33h/8800h returns
    ;rbx = Sector size in bytes
    ;rcx = Last LBA block
    sub rcx, 2
    mov qword [curDiskSize], rcx
    mov word [sectorSize], bx
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return


;BIOS implementer utility function
biosgetBIOSDiskNumber:
;A utility function, can be left as just return if unnecessary. 
;Used to translate currentDisk from an FDISK number to a BIOS specific 
; value.
;This is only used in this file and is implementer specific.
;In this case, 
;Output: dl = BIOS disk number
    movzx edx, byte [currentDisk]
    dec edx
    or edx, 80h ;Set SCPBIOS fixed disk bit
    return
