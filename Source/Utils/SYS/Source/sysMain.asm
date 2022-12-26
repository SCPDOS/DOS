startSys:
    jmp short .cVersion
.vNum:          db 1
.cVersion:
    push rax
    mov ah, 30h
    int 41h
    cmp al, byte [.vNum] ;Version 1
    jbe .okVersion
    pop rax
    lea rdx, badVerStr
.printExit:
    mov ah, 09h
    int 41h
    jmp badExit ;Exit to caller or DOS to print bad command interpreter line
.okVersion:
;Check the passed argument is ok (flag in al)
    pop rax
    cmp al, -1
    jnz .driveOk
.badDrive:
    lea rdx, badDrvLtr
    jmp short .printExit
.driveOk:
;Now fetch the drive we are working on
    mov dl, byte [r8 + psp.fcb1] ;Get the fcb 1 based drvNum
    dec dl  ;Turn it into a 0 based number
    mov byte [sysDrive], dl
    add dl, "A"
    mov byte [rootDir], dl
    mov byte [biosDest], dl
    mov byte [dosDest], dl
    ;Now get current drive
    mov eax, 1900h  ;Get current drive as the source of the copy
    int 41h
    add al, "A" ;Convert to an ASCII char
    mov byte [biosFile], al
    mov byte [dosFile], al
;Now step 1, search root dir, if a file is found
    lea rdx, rootDir
    mov ecx, dirInclusive
    mov eax, 4E00h
    int 41h
    jnc .fileFound
    cmp al, errNoFil
    je .rootDirOk
.badRoot:
    lea rdx, badRootDir
    mov ah, 09h
    int 41h
    jmp badExit
.fileFound:
;A File was found, if it is a Volume Label, do a find next
    lea rdi, qword [r8 + psp.dta]   ;Get ptr to the find first block
    cmp byte [rdi + ffBlock.attrib], dirVolumeID
    jne .badExit
    mov eax, 4F00h  ;Find Next
    int 41h
    jnc .badRoot    ;Another file found, exit bad
    cmp al, errNoFil
    jne .badRoot
.rootDirOk:
    clc ;Clear CF
;Now open two handles to the two files
    lea rdx, biosFile
    mov eax, 3D00h  ;Open in Read-Only mode
    int 41h
    jc badHandleOpen
    mov word [biosHdlSrc], bx

    lea rdx, biosDest
    mov ecx, dirReadOnly | dirHidden | dirSystem    ;Attribute
    mov eax, 3C00h
    int 41h
    jc badHandleCreate
    mov word [biosHdlDest], bx

badHandleCreate:
    lea rdx, badCreate
    jmp short badPrint
badHandleOpen:
    lea rdx, badOpen
badPrint:
;Generic Print entry point
    mov eax, 0900h
    int 41h
badExit:
;DOS will close the handles for us if needed
    mov eax, 4CFFh
    int 41h

;Utility functions
freeResources:
    movsx ebx, word [biosHdlSrc]
    cmp ebx, -1
    call freeHandle
    movsx ebx, word [biosHdlDst]
    cmp ebx, -1
    call freeHandle
    movsx ebx, word [dosHdlSrc]
    cmp ebx, -1
    call freeHandle
    movsx ebx, word [dosHdlDst]
    cmp ebx, -1
    call freeHandle
    
freeHandle:
    retz    ;Return if equal
    mov eax, 3E00h
    int 41h
    return
copyFile:
;Copies 1024 byte chunks
;If a CF=CY error is returned, something bad happened. Exit
