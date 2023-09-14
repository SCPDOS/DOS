;FCB functions.
;FCBs may only be generally used for file access FAT 12/16 volumes. 
;On FAT 32 volumes things are a bit more restricted.

;The following functions ARE currently supported for general FAT 32 files:
;   deleteFileFCB       (to allow for easy wildcard deletion)
;   renameFileFCB       (to allow for easy wildcard renaming)
;   parseFilename       (I mean, this function is useful anyway)
;   getFileSizeFCB      (sets the randRecrd field rounded up for file size)
;   findFirstFileFCB    (allows easy access to the file directory data)
;   findNextFileFCB     (ditto the above)

;The following functions are NOT currently supported for general FAT 32 files:
;   openFCB
;   closeFCB
;   createFCB
;   randomReadFCB
;   randomWriteFCB
;   randBlockReadFCB
;   randBlockWriteFCB
;   sequentialReadFCB
;   sequentialWriteFCB
;An attempt to run these functions on a FAT 32 volume will result in returning 
; al = -1 and an extended error code of 05 - Access Denied unless a device

;FAT 32 volumes will support all functions for Volume Labels using xFCBs.
;Reading and Writing to the Volume label will silently return ok.
;Volume labels will be editable by being created/opened/closed.
;If the current directory is not the root, Volume Label work will assume the 
; root directory always.

findFirstFileFCB:  ;ah = 11h
;Input: rdx -> FCB
    mov qword [workingFCB], rdx ;Store FCB ptr in variable
    mov rsi, rdx
    cmp byte [rsi], -1
    je .notExt1
    add rsi, exFcb.driveNum
.notExt1:
    movzx eax, byte [rsi]
    push rax    ;Push on stack the drive number
    lea rdi, buffer1    ;Use buffer 1 to build path in
    call fcbInitRoutine ;Build path and canonicaliseFilename
    jnc .fcbOk
    pop rbx ;Just pop into next reg to preserve error code
    jmp fcbErrExit
.fcbOk:
    lea rdi, buffer1
    call getFilePathNoCanon ;Now hit disk for this file
    jnc .fileFound
    pop rbx
    jmp fcbErrExit
.fileFound:
;Now we build an FFBlock internally
    lea rdi, dosffblock
    push rdi
    call setupFFBlock
    pop rsi
;Now we use the provided FCB to store the created FFblock, to be used by us only
    mov rdi, qword [workingFCB]
    test byte [extFCBFlag], -1
    jz .notExt2
    add rdi, exFcb.driveNum ;Go to the drive number 
.notExt2:
    pop rbx ;Get back the drive number in bl
    test ebx, ebx
    jnz .notCurrentDrive
    movzx ebx, byte [currentDrv]
    inc ebx ;Turn into a 1 based drive number
.notCurrentDrive:
    lodsb   ;Get search drive from FFBlock in al
    inc rdi ;Go past the given drive number in the FCB
    mov ecx, 5 ;Copy the 20 bytes in ffBlock after ffBlock.driveNum
    movsd
    stosb   ;Store the find first search drive number at the end of the FCB
    mov rdi, qword [currentDTA] ;Now copy current directory to DTA 
    lea rsi, curDirCopy ;Point rsi to the current directory copy
    test byte [extFCBFlag], -1
    jz .notExt3
    mov eax, -1
    stosb   ;Store at first byte of DTA the extfcb signature
    inc eax ;Make it zero
    stosd
    stosb   ;Store 5 bytes of zero
    movzx eax, byte [searchAttr]
    stosb   ;Store the search attributes
.notExt3:
    mov eax, ebx    ;Get specified drive number in eax
    stosb
    mov ecx, fatDirEntry_size/8
    rep movsq   ;Copy the directory entry for the file over
    jmp fcbGoodExit
    
findNextFileFCB:   ;ah = 12h
    mov qword [workingFCB], rdx ;Store FCB ptr in variable
    mov byte [extFCBFlag], 0    ;Assume normal FCB for now
    mov byte [searchAttr], 0    ;Set dir search attr to normal for now too
    mov rsi, rdx

    cmp byte [rsi], -1
    jne .notExt
    dec byte [extFCBFlag]   ;Make it -1 to set it
    add rsi, exFcb.attribute
    lodsb   ;Get search attr in al
    mov byte [searchAttr], al
.notExt:
    ;rsi points to drive letter
    lea rdi, dosffblock ;Set rdi to point to the dosffblock
    xor eax, eax
    lodsb   ;Get the FCB drive letter
    push rax    ;Push drive letter on the stack
    mov al, byte [rsi + 20] ;Get the byte I left at the end of the ffblock copy
    stosb   ;Store this as the search drive in the ffblock
    mov ecx, 5
    rep movsd   ;Copy 20 bytes now to the ffblock
    push qword [currentDTA] ;Save original currentDTA
    lea rdi, dosffblock
    push rdi    ;Set SDA ffblock as currentDTA
    pop qword [currentDTA]
    call findNextMain
    pop qword [currentDTA]  ;Get back original current DTA
    jnc findFirstFileFCB.fileFound  ;Go build a new FFBlock for the found file
    mov rdi, qword [workingFCB] ;If no more files or error, get working FCB ptr
    test byte [rdi], -1
    jz .notExt2
    add rdi, exFcb.driveNum
.notExt2:
    pop rbx ;Get the drive letter back into bl
    mov byte [rdi], bl
    jmp fcbErrExit  ;And exit bad

deleteFileFCB:     ;ah = 13h
    lea rdi, buffer1
    push rdi
    call fcbInitRoutine ;Build path and find file to delete
    pop rdi ;Point rdi to the canonised path
    jc fcbErrExit
    call getFilePathNoCanon ;Get the file
    jc fcbErrExit
    call outerDeleteMain
    jc fcbErrExit
    jmp fcbGoodExit

renameFileFCB:     ;ah = 17h
;Input: rdx -> User FCB
    mov qword [workingFCB], rdx
    ;First we get the drive letter 
    mov rsi, rdx
    cmp byte [rsi], -1
    jne .notExt
    add rsi, exFcb.driveNum
.notExt:
    xor eax, eax
    lodsb
    push rax    ;Push the drive letter on the stack for now
    lea rdi, buffer1    ;Store the canonicalised filename here 
    call fcbInitRoutine ;Store the first filename in its place
    jc .badPop
    push qword [fname1Ptr]  ;Move ptr to source name to other pos temporarily
    pop qword [fname2Ptr]   ;Will be xchg'd later
    pop rax ;Get drive letter back
    lea rdi, buffer2
    call fcbInitName2
    jc short .bad
    mov rax, qword [fname2Ptr]  ;Get the old source ptr in rax
    xchg qword [fname1Ptr], rax ;Swap ptr positions 
    mov qword [fname2Ptr], rax  ;Now place destination pattern in correct place
    call renameMain
    jnc fcbGoodExit
    jmp short .bad
.badPop:
    pop rbx ;Pop drive number off stack
.bad:
    jmp fcbErrExit

getFileSizeFCB:    ;ah = 23h
;This function which can be used to test existance of file without opening it 
;Input: rdx -> FCB
    mov qword [workingFCB], rdx
    lea rdi, buffer1
    push rdi
    call fcbInitRoutine
    pop rdi
    jc fcbErrExit
    call getFilePathNoCanon
    jc fcbErrExit
    mov eax, dword [curDirCopy + fatDirEntry.fileSize]  ;Get filesize in eax
    ;Now we gotta set up FCB randRecrd Field
    mov rsi, qword [workingFCB]
    cmp byte [rsi], -1
    jne .notExtended
    add rsi, exFcb.driveNum ;Go to drive number field
.notExtended:
;rsi points to the drive number now
    movzx ebx, word [rsi + fcb.recordSize]  ;Get the record size
    div ebx ;Divide filesize (in bytes) into # of records
    test edx, edx
    jz .noRemainder
    inc eax ;Increment number of records by 1 if there is a remainder in edx
.noRemainder:
    mov dword [rsi + fcb.randRecrd], eax    ;Now write # of records to fcb
    jmp fcbGoodExit

setDTA:            ;ah = 1Ah, Always can be used
;Called with:
;   rdx = Pointer to the new default DTA
    mov rbx, qword [oldRSP]
    mov rdx, qword [rbx + callerFrame.rdx]
    mov qword [currentDTA], rdx
    ret

getDTA:            ;ah = 2Fh, Always can be used
    mov rdx, qword [oldRSP]
    mov rbx, qword [currentDTA] ;Get current DTA
    mov qword [rdx + callerFrame.rbx], rbx
    return
    
parseFilenameFCB:  ;ah = 29h, Always can be used
;Input:
;rsi points to a command line to parse
;rdi points to a future unopened FCB
;al = parsing control bits. 
; al[0] = If set, leading filename separators are scanned off
; al[1] = If set, we set the drive ID byte if a drive letter was found
; al[2] = If set, will copy the filename to the FCB filename field
; al[3] = If set, will copy the extension to the FCB extension field
;Output: al = 0 => No wildcard chars (DOS calls these global filename chars)
;        al = 1 => Wildcards found
;        al = -1 => Drive specifier invalid
;rsi -> to the first char past the parsed filename
;rdi -> First byte of the FCB 

    call parseNameToFCB
    push rsi    ;rsi points now past the filename that was parsed
    call getUserRegs
    pop qword [rsi + callerFrame.rsi]
    return  ;al now contains dl, the signature, special unique return type

createFileFCB:     ;ah = 16h
;rdx -> Extended FCB
;       MUST BE EXTENDED. 
;       MUST HAVE ATTRIBUTE OF 08h, VOLID, else will fail
    cmp byte [rdx + exFcb.extSig], -1
    jne .exit
    cmp byte [rdx + exFcb.attribute], dirVolumeID
    jne .exit
    ;Here we search for a volume ID in the root directory.
    ; If one exists, we replace the dir entry name field,
    ; sync the BPB field and invalidate the DPB (to rebuid the BPB).
    ; Else, we build a dir entry for it, sync the BPB and invalidate the DPB.
    ;If ANY bits other than ValidCDS are set in the CDS, fail the operation.
.exit:
    mov eax, errAccDen
    jmp fcbErrExit

;=================================================================
;=================================================================
;These functions will be marked as reserved for future expansion.
; One idea will be to create a handle based record IO interface.
; We will be able to do IO on records of predefined length 
; on a file handle, thus removing the need for an FCB but still
; maintaining the usefulness of record based IO.
;=================================================================
;=================================================================
openFileFCB:       ;ah = 0Fh
closeFileFCB:      ;ah = 10h
sequentialReadFCB: ;ah = 14h
sequentialWriteFCB:;ah = 15h
randomReadFCB:     ;ah = 21h
randomWriteFCB:    ;ah = 22h
setRelRecordFCB:   ;ah = 24h
randBlockReadFCB:  ;ah = 27h
randBlockWriteFCB: ;ah = 28h
    mov eax, errAccDen
    jmp fcbErrExit


;--------------------------------
;  Common FCB related Routines  :
;--------------------------------
fcbCheckDriveType:
;Sets volIncmpFCB if the volume is not FAT12 or 16. This prevents us 
; from doing file io to files on such volumes (unless they are volume lbls)
;Input: qword [workingDPB] = DPB for transacting volume. 
;       qword [workingCDS] = CDS for transacting volume.
;       curDirCopy = Current Directory for found file (for char dev)
;If a net CDS, automatic fail (for now).
    mov byte [volIncmpFCB], -1  ;Assume incompatible volume unless otherwise
    push rcx
    push rdi
    push rbp
    pushfq
    test byte [curDirCopy + fatDirEntry.attribute], dirCharDev
    jnz .okToGo ;If the file is a char dev, its always ok for FileIO
    call testCDSNet ;If CF=CY => Net CDS (with and without CDS)
    jc .exit
    ;rdi has cds ptr now
    mov rbp, qword [rdi + cds.qDPBPtr]  ;Get dpb ptr in rbp
    call getFATtype
    cmp ecx, 1  ;0 = FAT12, 1 = FAT16
    ja .exit
.okToGo:
    mov byte [volIncmpFCB], 0   ;Clear this to permit usage
.exit:
    popfq
    pop rbp
    pop rdi
    pop rcx
    return
fcbInitName2:
;Must be called after fcbInitRoutine has been run once
;Input: rdi -> Buffer to use to build the X:FILENAME.EXT,0 pathspec
;       rdx -> UserFCB
;       eax[0] = Drive number (0 based)
    push rbp
    mov rbp, rsp
    sub rsp, 15
    push rdi
    lea rdi, qword [rbp - 15]
    mov rsi, rdx
    test byte [extFCBFlag], -1
    jz .notExtended
    add rsi, exFcb.driveNum
.notExtended:
;rsi points to the drive letter of the common fcb portion of the renameFcb
    add rsi, renameFcb.newName  ;rsi goes to the second filename
    jmp short fcbInitRoutine.rename2EP
fcbInitRoutine:
;Checks if the FCB is extended or normal, and fills the initial variables
;Input: rdx -> User FCB
;       rdi -> Buffer to use to build the X:FILENAME.EXT,0 pathspec
    push rbp
    mov rbp, rsp
    sub rsp, 15    ;Make 15 char space on stack
    ;This space is used to store X:FILENAME.EXT,0
    push rdi    ;Save the internal destination pathname buffer 
    lea rdi, qword [rbp - 15]
    mov byte [extFCBFlag], 0    ;Assume normal FCB initially
    mov byte [searchAttr], 0    ;Default search attributes
    call isFCBExtended  ;Sets rsi to point to the drive letter (if extended)
    jz .notExtended
    mov byte [extFCBFlag], -1
    mov al, byte [rdx + exFcb.attribute]    ;Get the search attribute
    mov byte [searchAttr], al
.notExtended:
    lodsb  ;rsi points to the normal fcb part, advance to filename
.rename2EP:
    call getCDS ;Get the CDS (preserves rdi)
    jc .badDisk
    call storeZeroBasedDriveNumber  ;Store X: on stack space, add two to rdi
    lea rbx, asciiCharProperties
    mov ecx, 11 ;11 chars in a filename
    push rsi    ;rsi -> fcb.filename
.nameCharCheck:
    lodsb   ;Get the char in al
    xlatb   ;Get the char signature in al
    test al, 8
    jz .badDisk
    dec ecx
    jnz .nameCharCheck
    pop rsi ;Point back to the start of the name field in the FCB
    mov rbx, rdi    ;Save ptr to stackbuffer + 2 (past X:)
    call FCBToAsciiz
    pop rdi ;Get back the ptr the SDA buffer to store the full pathname into
    cmp byte [rbx], 0   ;Is our path X:,0?
    je .badDisk
    lea rsi, qword [rbp - 15]   ;Point rsi to the stack string
    push rbp
    call canonicaliseFileName   ;Canonicalise filename
    pop rbp
    jc .badDisk
    call fcbCheckDriveType  ;Set the volume compatibility bit for operation
    jmp short .jiggleStack  ;Skip the error
.badDisk:
    mov al, errPnf  ;DOS does this... so will I
    stc
.jiggleStack:
    mov rsp, rbp
    pop rbp
.exit:
    return

storeZeroBasedDriveNumber:
;Input: al => 0 based drive letter
;       rdi -> Points to buffer to store the X: in
    inc al
storeOneBasedDriveNumber:
;Input: al => 1 based drive letter
;       rdi -> Points to buffer to store the X: in
    add al, "@"
    mov ah, ":"
    stosw
    return

isFCBExtended:
;Input: rdx = FCB ptr
;Output: rsi -> Drive letter of FCB
;        rdx -> FCB first byte
;ZF=NZ => Extended FCB, ZF=ZY => Normal FCB
    mov rsi, rdx
    cmp byte [rsi], -1
    jne .notExtended
    add rsi, exFcb.driveNum
.notExtended:
    cmp rdx, rsi
    return

parseNameToFCB:
;rsi points to a command line to parse
;rdi points to a future unopened FCB
;al = parsing control bits. 
; al[0] = If set, leading filename separators are scanned off
; al[1] = If set, we set the drive ID byte if a drive letter was found
; al[2] = If set, will copy the filename to the FCB filename field
; al[3] = If set, will copy the extension to the FCB extension field
;
;Separators include : . ; , = + TAB SPACE
;Terminators include separators and , < > | / " [ ] and all ctrl chars

    mov byte [fcbSpaceOk], 0    ;Don't allow spaces in filename
    xor edx, edx    ;Use dl to keep drive name/state of operation
    test al, 2  ;Set drive letter>
    jz .skipDriveLetter
    mov byte [rdi + fcb.driveNum], dl   ;Clear this for usage
.skipDriveLetter:
    inc rdi ;Go past the drive letter field in the FCB
    mov ecx, 8  ;Copy Filename
    test al, 4  ;Do we copy the filename to the fcb field?
    xchg eax, ebx
    mov al, " " ;Clear the field
    jz .clearFilenameField
    add rdi, rcx    ;Move to the extension field of the fcb
    xor ecx, ecx
.clearFilenameField:
    rep stosb
    mov ecx, 3
    test bl, 8  ;Skip clearing extension field?
    jz .clearExtensionField
    add rdi, rcx
    xor ecx, ecx
.clearExtensionField:
    rep stosb
    xchg eax, ecx
    stosd   ;Initialise curBlock and recordSize like DOS does in parseFilename
    sub rdi, 10h    ;Go back to head of FCB
    test bl, 1  ;Skip scanning preceeding spaces
    jz .dontScanOff
    call skipSpacesAndTabs
    call isCharDelimType
    jnz .skipIfDelim
    inc rsi
.dontScanOff:
    call skipSpacesAndTabs  ;Skip a char
.skipIfDelim:
    call uppercaseCharAtPtr 
    jz .skipSettingDriveLetter  ;Skip if first char not a possible drive letter
    cmp byte [rsi], ":" ;Is the next char a drive separator?
    jne .skipSettingDriveLetter
    inc rsi ;Goto next char
    sub al, "@" ;Convert into a 1 based drive number
    jbe .invalidDriveLetter ;If less than 0 or 0, fail
    push rax    ;Save the 1 based drive letter
    call setDrive   ;Verifies if this drive letter is valid
    pop rax
    jnc .validDriveLetter
.invalidDriveLetter:
    mov dl, -1
.validDriveLetter:
    stosb
    inc rsi ;Align pointers...
    dec rdi 
.skipSettingDriveLetter:
    dec rsi
    inc rdi
    
getFCBFilename:
;Input: rsi points to first char of filename
;       rdi points to storage buffer for filename
;       dl contains the signature if drive invalid
;Output:
;       Fields filled
;       al = Return signature
    mov ecx, 8
    call getFCBNameField
    cmp byte [rsi], "." ;Name sep?
    jne .noExt
    mov ecx, 3
    inc rsi ;Skip this char
    call forceFCBNameField
.noExt:
    mov al, dl  ;Store the return signature
    return

getFCBNameField:
;Checks if the field is appropriate before copying it
    call uppercaseCharAtPtr
    jnz .okName  ;If ZF not set, we can proceed!
    add rdi, rcx    ;Advance rdi by ecx chars
    dec rsi ;Point to previous char
    return
.okName:
    dec rsi ;Go back now a char
forceFCBNameField:
;Forcefully copies the name from rsi to rdi
    call uppercaseCharAtPtr
    jc .terminatingSpace
    jnz .notSep
    test byte [fcbSpaceOk], -1
    jz .terminatingSpace
    cmp al, " "
    jne .terminatingSpace
.notSep:
    jecxz forceFCBNameField    ;For the last char, now immediately loop around
    dec ecx
    cmp al, "*" ;Big wildcard?
    jne .notBigWildcard
    mov al, "?"
    rep stosb
.notBigWildcard:
    stosb   ;Stores the uppercased char
    cmp al, "?"
    jne forceFCBNameField
    or dl, 1    ;Set dl to 1 to indicate wildcard char
    jmp short forceFCBNameField
.terminatingSpace:
    mov al, " "
    stosb
    dec rsi ;Point to this trailing space
    return