;Data equates and struct declarations

cmdBufferL  equ 128 ;Length of a command line buffer
driveSpecL  equ 2   ;Space for X:
dirPathL    equ 64  ;Length of a directory path
fcbNameL    equ 11  ;8.3 => 11 chars in name
filenameL   equ fcbNameL + 1 ;Add a dot for the name.ext separator
fileNameZL  equ filenameL + 1   ;Add a space for a terminating null
fullDirPathL equ driveSpecL + dirPathL
fullDirPathZL equ fullDirPathL + 1   ;Add one for terminating null
fileSpecZL  equ fullDirPathL + fileNameZL ;One full asciiz pathspec
cmdNameL    equ filenameL + 1 + 1   ;1 for the count prefix,1 for end null

;Use PSP FCB's for switch buffers
fcb1        equ psp.fcb1
fcb2        equ psp.fcb2
;Use the dta as the built command line buffer.
cmdLineCnt  equ psp.parmList
cmdLine     equ psp.progTail


;Struct Declarations
struc drvHdr  ;Device Driver Header for character and block devices
    .nxtPtr resq 1  ;Pointer to the next driver header, -1 if at the end
    .attrib resw 1  ;Attribute Word
    .strPtr resq 1  ;Strategy Entry Pointer
    .intPtr resq 1  ;Interrupt Entry Pointer
    .drvNam resb 8  ;Driver name (Char) or Number of units byte (Block)
endstruc

struc execProg  ;For use with EXEC-ing a child task
    .pEnv       resq 1  ;Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   resq 1  ;Ptr to the command line to be placed at PSP + 80h
    .pfcb1      resq 1  ;Ptr to the first FCB (parsed argument 1)
    .pfcb2      resq 1  ;Ptr to the second FCB  (parsed argument 2)
endstruc

;Directory attribute equates
    dirReadOnly     equ 01h
    dirHidden       equ 02h
    dirSystem       equ 04h
    dirVolumeID     equ 08h
    dirDirectory    equ 10h
    dirArchive      equ 20h
    dirCharDev      equ 40h ;Never written to disk, used to represent a Char Dev
    dirLongName     equ dirReadOnly | dirHidden | dirSystem | dirVolumeID
    ;If any of the three bits are set, then ALL three bits are set
    ; in addition to whatever the user passed to search for.
    dirInclusive    equ dirHidden | dirSystem | dirDirectory
    dirIncFiles     equ dirHidden | dirSystem