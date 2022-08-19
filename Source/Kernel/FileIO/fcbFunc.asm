

;FCB functions. Except where explicitly stated, if the selected drive 
; for the operation is FAT 32, the request will immediately fail, unless 
; the operation is to create\delete\find the volume label. 
;FCBs may only be used to access data files on FAT 12/16 drives. 

;I am considering using the Extended FCB space to store additional information
; for FAT32... but need to figure it out and I dont really care.


openFileFCB:       ;ah = 0Fh
closeFileFCB:      ;ah = 10h
findFirstFileFCB:  ;ah = 11h
findNextFileFCB:   ;ah = 12h
deleteFileFCB:     ;ah = 13h
sequentialReadFCB: ;ah = 14h
sequentialWriteFCB:;ah = 15h
createFileFCB:     ;ah = 16h
renameFileFCB:     ;ah = 17h
    ret
setDTA:            ;ah = 1Ah, Always can be used
;Called with:
;   rdx = Pointer to the new default DTA
    mov rbx, qword [oldRSP]
    mov rdx, qword [rbx + callerFrame.rdx]
    mov qword [currentDTA], rdx
    ret
randomReadFCB:     ;ah = 21h
randomWriteFCB:    ;ah = 22h
getFileSizeFCB:    ;ah = 23h
setRelRecordFCB:   ;ah = 24h
randBlockReadFCB:  ;ah = 27h
randBlockWriteFCB: ;ah = 28h
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
    return  ;al now contains dl, the signature


getDTA:            ;ah = 2Fh, Always can be used
    mov rdx, qword [oldRSP]
    mov rbx, qword [currentDTA] ;Get current DTA
    mov qword [rdx + callerFrame.rbx], rbx
    return

;------------------------------
;Common FCB related Routines
;------------------------------
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