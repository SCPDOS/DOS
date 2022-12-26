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
    ;Now get current drive
    mov eax, 1900h  ;Get current drive 
    int 41h
    add al, "A" ;Convert to an ASCII char
    mov byte [biosFile], al
    mov byte [dosFile], al
    mov byte [rootDir], al
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
.rootDirOk:

badExit:
    mov eax, 4CFFh
    int 41h