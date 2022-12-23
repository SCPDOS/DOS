;This is the disk formatting utility for SCP/DOS 1.0
;Uses the undocumented LBA based Generic IO interface

;Supports exactly one command line argument, the drive letter.
;Invoked as so: FORMAT x: where the colon is necessary.

;>>> 7 Steps to Disk Domination <<<
;1) Format begins by turning the drive offline by cleaning the 
;    cdsValidDrive bit in the device CDS. 
;2) Format begins by ascertaining how large the volume/device is.
;    -If the device is removable, format will get the device parameters to 
;      ascertain the size of the volume.
;    -If the device fixed, format will use the MBR (and eventually GPT) to 
;      ascertain the size of the volume and gets device parameters to get the 
;      sector size.
;3) Format will then choose which FAT to use and build the BPB accordingly
;    and write it to disk.
;4) Format will then rebuild the disk DPB from the new BPB.
;5) Format will then create two fresh FAT tables.
;6) Format will then clean the root directory (FAT12/16) or allocate a 
;    cluster and sanitise it (FAT32)
;7) Finally, format re-enables cdsValidDrive and exits.

;If a ^C is invoked during the format procedure, we prompt the user 
; for the "are you sure you wish to abandon the format" and that "this may
; result in an unusable volume that will need reformatting" message.
;If they respond with Y, we re-enable the CDS and return to DOS to exit.

;Note Format does not format the full medium and uses Int 45h to read 
; sectors from the old format and Int 46h to write new sectors to the 
; volume. 
;Format also doesnt depend on any old BPB's or anything like so.
;Any old FAT (or other FS) data structures are considered nukable.


;We start by checking that the version number is OK
;al has flag if the passed argument is ok
;r8 points to the PSP
start:
    jmp short .cVersion
.vNum:    db 1
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
    int 40h ;Exit to caller or DOS to print bad command interpreter line
.okVersion:
;Check the passed argument is ok (flag in al)
    pop rax
    cmp al, -1
    jnz .driveOk
    lea rdx, badDrvLtr
    jmp short .printExit
.driveOk:
;Now save the old default drive, and set our drive to default.
;On exit this must be restored
    mov ah, 19h ;Get Current Default Drive in al
    int 41h
    mov byte [oldDrive], al
    add al, "A"
    mov byte [driveLetter], al  ;Store for error message

; Here we now hook ^C so that if the user calls ^C we restore DOS state
; (i.e. default drive and reactivate the drive if it is deactivated)

    mov dl, byte [r8 + psp.fcb1 + fcb.driveNum] ;Get the fcb 0 based drvNum
    mov byte [fmtDrive], dl
    mov ah, 0Eh ;Select Drive
    int 41h
;Now we check that the associate drive is not a network, subst or join.
; If it is, fail. Else, we deactivate
    mov ah, 52h
    int 41h ;Get in rbx a ptr to list of lists
    add rbx, 2Ah  ;Point rbx to cdsHeadPtr
    mov rsi, qword [rbx]    ;Get the ptr to the CDS array
    movzx ecx, byte [fmtDrive]
    jecxz .atCurrentCDS
.walkCDSArray:
    add rsi, cds_size
    dec ecx
    jnz .walkCDSArray
.atCurrentCDS: 


.breakRoutine:
;This subroutine is called by ^C
;Prompts the user for what they want to do.
    lea rdx, cancel
    mov ah, 09h
    int 41h
    mov ah, 01h ;Get a char
    int 41h
    cmp al, "y"
    je short .breakReturnExit
    cmp al, "Y"
    je short .breakReturnExit
    cmp al, "n"
    je short .breakReturnNoExit
    cmp al, "N"
    jmp short .breakRoutine 
.breakReturnNoExit:
    iretq   ;Just exit away back to the instantiating task
.breakReturnExit:




;Data area here
oldDrive    db -1       ;Old default drive (0 based)
fmtDrive    db -1       ;Drive we are operating on (0 based)
fmtDrvInv   db 0        ;If set to -1, the drive needs to be reactivated
oldxtBreak  db 0        ;If set to -1, og state of xtBreak was set
;Format Data here
fatType     db -1       ;0 = FAT12, 1 = FAT16, 2 = FAT32, -1 = No FAT

;Messages go here
badVerStr   db 0Ah,0Dh,"Invalid DOS Version",0Ah,0Dh,"$"
badDrvLtr   db 0Ah,0Dh,"Invalid Drive Specified",0Ah,0Dh,"$"

cancel      db 0Ah,0Dh,"Are you sure you wish to abort formatting drive "
driveLetter db "A?", 0Ah,0Dh
            db "Doing so will result in an unusable volume. Y/N?",0Ah,0Dh,"$"
