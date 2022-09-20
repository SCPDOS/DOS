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
; al = -1 and an extended error code of 05 - Access Denied

;FAT 32 volumes will support all functions for Volume Labels using xFCBs.
;Reading and Writing to the Volume label will silently return ok.
;Volume labels will be editable by being created/opened/closed.
;If the current directory is not the root, Volume Label work will assume the 
; root directory always.

openFileFCB:       ;ah = 0Fh
    mov eax, errAccDen
    jmp fcbErrExit
closeFileFCB:      ;ah = 10h
    mov eax, errAccDen
    jmp fcbErrExit
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
    call fcbInitRoutine
    jnc .fcbOk
    pop rbx ;Just pop into next reg to preserve error code
    jmp fcbErrExit
.fcbOk:
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
    mov eax, errAccDen
    jmp fcbErrExit
deleteFileFCB:     ;ah = 13h
    mov eax, errAccDen
    jmp fcbErrExit
sequentialReadFCB: ;ah = 14h
    mov eax, errAccDen
    jmp fcbErrExit
sequentialWriteFCB:;ah = 15h
    mov eax, errAccDen
    jmp fcbErrExit
createFileFCB:     ;ah = 16h
    mov eax, errAccDen
    jmp fcbErrExit
renameFileFCB:     ;ah = 17h
    mov eax, errAccDen
    jmp fcbErrExit
setDTA:            ;ah = 1Ah, Always can be used
;Called with:
;   rdx = Pointer to the new default DTA
    mov rbx, qword [oldRSP]
    mov rdx, qword [rbx + callerFrame.rdx]
    mov qword [currentDTA], rdx
    ret
randomReadFCB:     ;ah = 21h
    mov eax, errAccDen
    jmp fcbErrExit
randomWriteFCB:    ;ah = 22h
    mov eax, errAccDen
    jmp fcbErrExit
getFileSizeFCB:    ;ah = 23h
    mov eax, errAccDen
    jmp fcbErrExit
setRelRecordFCB:   ;ah = 24h
    mov eax, errAccDen
    jmp fcbErrExit
randBlockReadFCB:  ;ah = 27h
    mov eax, errAccDen
    jmp fcbErrExit
randBlockWriteFCB: ;ah = 28h
    mov eax, errAccDen
    jmp fcbErrExit
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


getDTA:            ;ah = 2Fh, Always can be used
    mov rdx, qword [oldRSP]
    mov rbx, qword [currentDTA] ;Get current DTA
    mov qword [rdx + callerFrame.rbx], rbx
    return

;--------------------------------
;  Common FCB related Routines  :
;--------------------------------
fcbCheckDriveType:
;Sets volIncmpFCB if the volume is not FAT12 or 16. This prevents us 
; from doing file io to files on such volumes (unless they are volume lbls)
;Input: qword [workingDPB] = DPB for transacting volume. 
;       qword [workingCDS] = CDS for transacting volume.
;If a net CDS, automatic fail (for now).
    mov byte [volIncmpFCB], -1  ;Assume incompatible volume unless otherwise
    push rcx
    push rdi
    push rbp
    pushfq
    call testCDSNet ;If CF=CY => Net CDS (with and without CDS)
    jc .exit
    ;rdi has cds ptr now
    mov rbp, qword [rdi + cds.qDPBPtr]  ;Get dpb ptr in rbp
    call getFATtype
    cmp ecx, 1  ;0 = FAT12, 1 = FAT16
    ja .exit
    mov byte [volIncmpFCB], 0   ;Clear this to permit usage
.exit:
    popfq
    pop rbp
    pop rdi
    pop rcx
    return
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
    call isFCBExtended  ;Moves rsi to point to the drive letter (if extended)
    jz .notExtended
    mov byte [extFCBFlag], -1
    mov al, byte [rdx + exFcb.attribute]    ;Get the search attribute
    mov byte [searchAttr], al
.notExtended:
    lodsb  ;rsi points to the normal fcb part, advance to filename
    call getCDS ;Get the CDS (preserves rdi)
    jc .badDisk
    call fcbCheckDriveType   ;Set the volume compatibility bit for operation
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
    call getFilePath   ;Canonicalise and hit disk to find file
    pop rbp
    jnc .jiggleStack
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