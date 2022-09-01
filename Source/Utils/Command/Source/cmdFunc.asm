dir:
    mov byte [dirPrnType], 0    ;Clear DIR flags
    mov byte [dirLineCtr], 0
    mov byte [dirFileCtr], 0
    mov byte [dirPathOff], 0    
    mov byte [dirVolLbl], -1    ;Mark as no label
    ;Start by scanning for the switches
    lea rdi, cmdBuffer + 1  ;Goto command line input chars count
    movzx ecx, byte [rdi]   ;Get number of chars typed
    inc rdi ;Goto first char typed in
    mov rsi, rdi    ;Use rsi as start of buffer counter
    mov al, byte [switchChar]   ;Scan for switchchars
.switchScan:
    repne scasb ;Scan for a switchchar
    jecxz .switchScanDone
    mov al, byte [rdi]  ;Get the byte pointed to by rdi
    and al, 0DFh    ;UC it
    cmp al, "W" ;Wide print mode?
    jne .notWideSw
    or byte [dirPrnType], 1 ;Set the correct bit
    jmp short .switchScan
.notWideSw:
    cmp al, "P" ;Pause mode?
    jne .badParam   ;If a switch other than /P or /W, fail
    or byte [dirPrnType], 2 ;Set correct bit
    jmp short .switchScan
.switchScanDone:
;If no args, only switches, we search CWD
;If one arg, search that 
;If more than one, fail
    lea rdi, cmdBuffer + 2
    mov rsi, rdi
    call skipSpaces ;Skip leading spaces
    add rsi, 3  ;Go past the DIR (always three chars)
    cmp byte [rsi], CR
    je .eocReached
.pathSearch:
    call skipSpaces ;Now skip intermediate spaces to next non-space
    mov al, byte [switchChar]   ;Is this a switch?
    cmp byte [rsi], al
    je .pathSearchSwitch    ;If a switch, skip it, find switch terminator
    cmp byte [dirPathOff], 0    ;Did we previously get an argument?
    jne .badParam ;If so, error
    mov rax, rsi    ;Else, compute the offset in the cmdBuffer
    sub rax, rdi    ;Offset from the cmdBuffer + 2
    add al, 2       ;Make it an offset from cmdBuffer
    mov byte [dirPathOff], al   ;And save it!
.pathSearchSwitch:
    call findTerminatorOrEOC
    jc .eocReached
    inc rsi ;Go to next char
    jmp short .pathSearch
.eocReached:
    cmp byte [dirPathOff], 0
    je .dirCWD
    jmp .badParam   ;Temp measure
.dirCWD:
    ;Build current working dir and append a \*.* to it 
    ;First we search the root for a label
    lea rdi, dirVolPathBuf
    lea rsi, searchSpec ;Will need to create the same X:\ here too
    call getCurrentDrive    ;Get current drive number (0 based) in al
    mov byte [dirDrv], al   ;Store the 0 based drive number in al
    add al, "A"
    mov ah, ":" ;ax has X: now to store 
    stosw
    mov word [rsi], ax
    add rsi, 2
    mov al, byte [pathSep]
    stosb
    mov byte [rsi], al
    mov eax, 002A2E2Ah  ;*.*,0
    stosd
    mov ah, 2Fh ;Get current DTA in rbx
    int 41h 
    push rbx    ;Preserve it on the stack
    lea rdx, cmdFFBlock
    mov ah, 1Ah ;Set DTA to internal ffblock
    int 41h
    lea rdx, dirVolPathBuf
    mov cx, dirVolumeID
    mov ah, 4Eh ;Find first
    int 41h
    jc .skipVolLbl
    lea rsi, qword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, dirVolLbl
    mov ecx, 11 ;Get the 11 chars of the volume label
.dirLblCopy:
    lodsb   ;Get the first char
    cmp al, 0
    je .skipVolLbl
    cmp al, "."
    je .dirLblSkipStore
    stosb
.dirLblSkipStore:
    dec ecx
    jnz .dirLblCopy
.skipVolLbl:
;Print volume label information now
    call .dirPrintVolInfo
    lea rdi, searchSpec + 3 ;Go to the fourth char in the field
    mov ah, 47h ;Get Current Working Directory
    mov rsi, rdi    ;rsi points to buffer to write to
    mov dl, byte [searchSpec]
    sub dl, "@" ;Get 1 based drive letter
    int 41h ;Overrwrite it with the current directory
    lea rdi, searchSpec
    call strlen
    dec ecx
    mov byte [rdi + rcx], "$"   ;Replace the null with a string terminator
    lea rdx, dirMain
    mov ah, 09h
    int 41h
    mov rdx, rdi    ;Print the current directory we are working on
    mov ah, 09h
    int 41h
    lea rdx, crlf
    mov ah, 09h
    int 41h
    lea rdx, crlf   ;Add an extra free line
    mov ah, 09h
    int 41h
    ;Now we search for the files
    mov al, byte [pathSep]
    cmp byte [rdi + rcx - 1], al
    je .noAddSlash  ;Deals with special case of root dir
    mov byte [rdi + rcx], al
    inc ecx
.noAddSlash:
    mov dword [rdi + rcx], 002A2E2Ah ;and add a *.*,0
    mov rdx, rdi    ;Ptr to search for in rdx
    mov ecx, dirReadOnly | dirDirectory
    mov ah, 4Eh ;Find first
    int 41h
    jc .dirNoMoreFiles
.findNext:
    call .dirPrintFileData  ;Print the file information
    mov ah, 4Fh
    int 41h
    jnc .findNext 
.dirNoMoreFiles:
    pop rdx 
    mov ah, 1Ah ;Return back the original DTA
    int 41h
    test byte [dirPrnType], 1
    jz .dirNoEndNewLine
    lea rdx, crlf   ;Only need this for /W
    mov ah, 09h
    int 41h
.dirNoEndNewLine:
    ;Now we print the number of files and the number of bytes on the disk
    mov ecx, 8  ;Print 8 spaces
    mov dl, " "
.dirNumOffSpc:
    mov ah, 02h
    int 41h
    dec ecx
    jnz .dirNumOffSpc
    xor edx, edx
    movzx eax, byte [dirFileCtr]   ;Get number of files
    call printDecimalWord
    lea rdx, dirOk
    mov ah, 09h
    int 41h
    lea rdx, threeSpc
    mov ah, 09h
    int 41h
    mov eax, 3600h ;Get disk info
    mov dl, byte [dirDrv]
    int 41h ;Get disk free space info
    movzx eax, ax   ;Sectors per Cluster 
    movzx ecx, cx   ;Bytes per Sector
    or ebx, ebx ;Clear the upper bits of rbx
    mul ecx ;Get bytes per cluster
    mul ebx ;Multiply to the number of free clusters on the disk
    ;edx:eax now has the number of free bytes on the disk
    call printDecimalWord
    lea rdx, bytesOk
    mov ah, 09h
    int 41h
    return

.dirPrintVolInfo:
    lea rdx, crlf
    mov ah, 09h
    int 41h
    lea rdx, volMes
    mov ah, 09h
    int 41h
    mov dl, byte [dirVolPathBuf]   ;Print the drive letter out
    mov ah, 02h
    int 41h
    cmp byte [dirPathOff], -1   ;No volume ID marker
    jne .dirVolIDOk
    lea rdx, volNo
    mov ah, 09h
    int 41h
    lea rdx, crlf
    mov ah, 09h
    int 41h
    return
.dirVolIDOk:
    lea rdx, volOk
    mov ah, 09h
    int 41h
    lea rdi, dirVolLbl
    call strlen
    dec ecx
    mov byte [rdi + rcx], "$"   ;Replace the null with a string terminator
    lea rdx, dirVolLbl
    mov ah, 09h
    int 41h
    lea rdx, crlf
    mov ah, 09h
    int 41h
    return

.dirPrintFileData:
;Use fcbCmdSpec to build the file name with space
;Start by print the name (same for both cases)
    lea rsi, qword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, fcbCmdSpec
    call asciiFilenameToFCB
    lea rdx, fcbCmdSpec
    mov ecx, 8  ;Print 8 chars
    mov ebx, 1  ;STDOUT
    mov ah, 40h ;Write handle
    int 41h
    push rdx
    mov dl, " "
    mov ah, 02h ;Print char
    int 41h
    pop rdx
    add rdx, 8  ;Go to ext field
    mov ecx, 3  ;Print three chars
    mov ebx, 1  ;STDOUT
    mov ah, 40h ;Write handle
    int 41h
    test byte [dirPrnType], 1
    jnz .widePrint
;Normal print (Name space ext <> File size <> Acc Date <> Acc Time)
    ;Now check if a DIR
    test byte [cmdFFBlock + ffBlock.attribFnd], dirDirectory
    jz .dirPrintNotDir
    lea rdx, dirLbl
    mov ah, 09h
    int 41h
    lea rdx, threeSpc
    mov ah, 09h
    int 41h
    jmp short .dirPrintFileDT
.dirPrintNotDir:
;Here we print the file size
    mov dl, " "
    mov ah, 02h
    int 41h
    mov eax, dword [cmdFFBlock + ffBlock.fileSize]
    call getDecimalWord
    mov rbx, rcx
    bswap rbx
    mov ecx, 8
.dirPrintFileSizePrep:
    test bl, bl ;Any leading null's get replaced with a space
    jne .dirPrintFileSize
    mov ah, 02h
    mov dl, " "
    int 41h
    shr rbx, 8  ;Get next byte
    dec ecx
    cmp ecx, 1
    jne .dirPrintFileSizePrep   ;Always print 1 byte for size
.dirPrintFileSize:
    mov dl, bl
    mov ah, 02h
    int 41h
    shr rbx, 8  ;Get next byte
    dec ecx
    jnz .dirPrintFileSize
    lea rdx, threeSpc
    mov ah, 09h
    int 41h
.dirPrintFileDT:
    lea rdx, twoSpc
    mov ah, 09h
    int 41h
    ;Here we print the DATE AND TIME eventually
    lea rdx, crlf
    mov ah, 09h
    int 41h
    jmp short .dirPrintNameExit
.widePrint:
;If /W, print name space ext space space space space
    lea rdx, fourSpc
    mov ah, 09h ;Print string
    int 41h
.dirPrintNameExit:
    inc byte [dirFileCtr]   ;Increment file counter
    inc byte [dirLineCtr]
    cmp byte [dirLineCtr], 23
    retne
    lea rdx, pauseMes
    mov ah, 09h
    int 41h
    mov ah, 01h ;Wait for a char from STDIN
    int 41h
    mov byte [dirLineCtr], 0
    lea rdx, crlf   ;Force new line
    mov ah, 09h
    int 41h
    return
    
.badParam:
    lea rdx, badParm
    mov ah, 09h
    int 41h
    return

chdir:
    test byte [arg1Flg], -1
    jnz .changeDir
    ;Print CWD
.printCWD:
    call putCWDInPrompt ;Exactly the same procedure
    call printCRLF
    return
.printDiskCWD:
;Print CWD for a specified drive
    mov dl, byte [r8 + fcb1 + fcb.driveNum] ;Get 1 based drive number in dl
    mov al, dl
    add al, "@" ;Get the UC letter
    mov ah, ":"
    lea rdi, searchSpec
    stosw   ;Store X:, rdi+=2
    mov al, byte [pathSep]
    stosb   ;Store pathSep, inc rdi
    mov ah, 47h ;Get Current Working Directory
    mov rsi, rdi    ;rsi points to buffer to write to
    int 41h
    call strlen
    add ecx, 2 ;Add two for the X:
    mov ah, 40h ;Write to handle
    mov ebx, 1  ;STDOUT
    lea rdx, searchSpec
    int 41h
    call printCRLF
    return
.changeDir:
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je .badDrv  ;IF the drive is good, but FCB name blank, either X: or \ 
    cmp byte [r8 + fcb1 + fcb.filename], " "
    jne .getFQPath
    ;Now we double check that on the command line we have . or ..
    movzx eax, byte [arg1Off]
    lea rsi, cmdBuffer
    add rsi, rax
    mov al, byte [pathSep]
    cmp byte [rsi], al  ;Is the first char a pathsep?
    je .getFQPath
    cmp byte [rsi], "."
    jne .printDiskCWD
    ;If the path is . or .., its acceptable, else fail
.getFQPath:
    call buildCommandPath   ;Else build a fully qualified pathname
    jc .badDir  ;If this returns CF=CY, its a badDir
    lea rdx, searchSpec
    mov ah, 3Bh ;CHDIR
    int 41h
    jc .badDir
    return

.badDrv:
    lea rdx, badDrv
    mov eax, 0900h
    int 41h
    return
.badDir:
    lea rdx, badDir
    mov eax, 0900h
    int 41h
    return


mkdir:
    test byte [arg1Flg], -1
    jz .badParams
    test byte [arg2Flg], -1
    jnz .badParams
    ;We have exactly one argument
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je .badDrv  ;If a drive was specified and was bad, jump
    call buildCommandPath
    jc .badMake
    lea rdx, searchSpec
    mov eax, 3900h  ;MKDIR
    int 41h
    jc .badMake   ;Return if not carry
    mov ah, 0Dh
    int 41h ;Flush to disk
    return
.badMake:   ;Else, bad make
    lea rdx, badMD
    mov eax, 0900h
    int 41h
    return
.badDrv:
    lea rdx, badDrv
    mov eax, 0900h
    int 41h
    return
.badParams:
    lea rdx, badArgs
    mov eax, 0900h
    int 41h
    return

rmdir:
    test byte [arg1Flg], -1
    jz .badParams
    test byte [arg2Flg], -1
    jnz .badParams
    ;We have exactly one argument
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je .badDrv  ;If a drive was specified and was bad, jump
    call buildCommandPath
    jc .badRemove
    lea rdx, searchSpec
    mov eax, 3A00h  ;RMDIR
    int 41h
    jc .badRemove   ;Return if not carry
    mov ah, 0Dh
    int 41h ;Flush to disk
    return
.badRemove:   ;Else, bad make
    lea rdx, badRD
    mov eax, 0900h
    int 41h
    return
.badDrv:
    lea rdx, badDrv
    mov eax, 0900h
    int 41h
    return
.badParams:
    lea rdx, badArgs
    mov eax, 0900h
    int 41h
    return
copy:
    return
erase:
    return
date:
    return
time:
    return
ctty:
    return
cls:  
    mov eax, 4400h  ;Get device info
    mov ebx, 1      ;for handle 1
    int 41h         ;in dx
    test edx, devCharDev
    jz .doAnsi  ;Make files register an ansi cls sequence
    test edx, charDevFastOut
    jz .doAnsi
    ;Test if Int 49h uses Int 30h
    ;Tests if within the first 1024 bytes we have the sequence Int 30h (30CD)
    ;Int 49h MUST be terminated with a IRETQ, within 1024 bytes
    mov eax, 3549h  ;Get the vector for interrupt 49h
    int 41h
.biosCheck:
    cmp word [rbx], 30CDh
    je .biosConfirmed
    cmp word [rbx], 0CF48h   ;CFh = IRET, 48h=REX.W
    je .doAnsi
    inc rbx
    jmp short .biosCheck
.biosConfirmed:
    ;Supports a SCP/BIOS compatible routine, use BIOS   
    mov ah, 0Bh  ; Set overscan to black (when Graphics becomes supported)
    xor ebx, ebx
    int 30h
    mov ah, 0Fh ;Get screen mode
    int 30h
    movzx edx, ah   ;Get number of columns in dl
    dec dl
    mov dh, 25  ;Number of rows is standard
    xor eax, eax
    mov ecx, eax
    mov bh, 7   ;Screen attributes
    mov ah, 6   ;Scroll
    int 30h
    xor edx, edx    ;Set cursor coordinates to top left of screen
    mov bh, 0   ;Page 0
    mov ah, 2
    int 30h
    return
.doAnsi:
;If an ANSI driver is not installed, this will simply insert blank lines
;4 chars in the ansi routine
;Will just put the ANSI escape sequence on the screen if it doesn't 
; understand ANSI codes
    lea rsi, ansiCls
    mov ecx, 4
    mov ah, 06h ;Raw char output
.ansiLp:
    lodsb   ;Get the char in 
    mov dl, al
    int 41h
    dec ecx
    jnz .ansiLp
    return

break:
    test byte [arg1Flg], -1
    jnz .argumentProvided
    ;Here we just get the status of break
    mov eax, 3300h  ;Get break status in dl
    int 41h
    mov bl, dl
    lea rdx, breakIs
    mov ah, 09h
    int 41h
    lea rdx, onMes
    lea rcx, offMes
    test bl, bl ;IF bl = 0, break is off
    cmovz rdx, rcx
    mov ah, 09h
    int 41h
    return
.argumentProvided:
    lea rsi, qword [r8 + fcb1 + fcb.filename]  ;Point to the first fcb name
    lodsd   ;Read the word
    mov ebx, eax
    and eax, 0DFDFh  ;Convert first two chars to uppercase
    shr ebx, 10h     ;Get high word low
    cmp bx, "  " ;Two spaces is a possible ON 
    je .maybeOn
    cmp ax, "OF"
    jne .badArgument
    and bx, 0FFDFh ;Convert only the third char to UC. Fourth char MUST BE SPACE
    cmp bx, "F "
    jne .badArgument
    ;Set off
    xor edx, edx    ;DL=0 => BREAK is off
    jmp short .setBreak
.maybeOn:
    cmp ax, "ON"
    jne .badArgument
    ;Set on
    mov edx, 1
.setBreak:
    mov eax, 3301h  ;Set break
    int 41h
    return
.badArgument:
    lea rdx, badOnOff
    mov ah, 09h
    int 41h
    return

verify:
    test byte [arg1Flg], -1
    jnz .argumentProvided
    ;Here we just get the status of break
    mov eax, 5400h  ;Get verify status in al
    int 41h
    mov bl, al
    lea rdx, verifyIs
    mov ah, 09h
    int 41h
    lea rdx, onMes
    lea rcx, offMes
    test bl, bl ;IF bl = 0, break is off
    cmovz rdx, rcx
    mov ah, 09h
    int 41h
    return
.argumentProvided:
    lea rsi, qword [r8 + fcb1 + fcb.filename]  ;Point to the first fcb name
    lodsd   ;Read the word
    mov ebx, eax
    and eax, 0DFDFh  ;Convert first two chars to uppercase
    shr ebx, 10h     ;Get high word low
    cmp bx, "  " ;Two spaces is a possible ON 
    je .maybeOn
    cmp ax, "OF"
    jne .badArgument
    and bx, 0FFDFh ;Convert only the third char to UC. Fourth char MUST BE SPACE
    cmp bx, "F "
    jne .badArgument
    ;Set off
    xor eax, eax    ;AL=0 => VERIFY is off
    jmp short .setVerify
.maybeOn:
    cmp ax, "ON"
    jne .badArgument
    ;Set on
    xor eax, eax
    inc eax ;AL=1 => VERIFY is on
.setVerify:
    mov ah, 2Eh  ;Set Verify
    int 41h
    return
.badArgument:
    lea rdx, badOnOff
    mov ah, 09h
    int 41h
    return

rename:
    return
truename:
    test byte [arg1Flg], -1
    jnz .argumentProvided
    lea rdx, badArgs
    mov ah, 09h
    int 41h
    return
.argumentProvided:
    call buildCommandPath
    ;Explicitly call Truename if we remove truename from this function
    lea rdi, searchSpec
    call strlen
    dec ecx ;Don't print terminating null
    lea rdx, searchSpec
    mov ebx, 01
    mov ah, 40h
    int 41h
    call printCRLF
    return


launchChild:
;We run EXEC on this and the child task will return via applicationReturn
    return

