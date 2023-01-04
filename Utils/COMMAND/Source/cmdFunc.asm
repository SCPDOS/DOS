;Note! Each function must setup the DTA to use for itself.
;There is no requirement to preserve the DTA across a call.
;Only the Int 4Eh entry point will preserve the callers DTA.

;Common Error Messages, jumped to to return from
noSelfCopyError:
    lea rdx, noSelfCopy
    jmp short badCmn
badParamError:
    lea rdx, badParm
    jmp short badCmn
badDriveError:
    lea rdx, badDrv
    jmp short badCmn
badArgError:
    lea rdx, badArgs
    jmp short badCmn
badFileError:
    lea rdx, badSpec
    jmp short badCmn
badDupFnf:
;Hybrid error message
    lea rdx, dupName
    jmp short badCmn
badDirError:
    lea rdx, badDir
badCmn:
    mov eax, 0900h
    int 41h
    stc ;Return with CY => Error occured
    return

dir:
    ;breakpoint
    mov byte [dirPrnType], 0    ;Clear DIR flags
    mov byte [dirLineCtr], 0
    mov byte [dirFileCtr], 0
    mov byte [dirPathArg], 0    ;Null terminate the start of the buffer
    mov rax, "????????"
    lea rdi, dirSrchPat ;Start also by initialising the search pattern
    stosq
    inc rdi ;Go past dot
    mov word [rdi], ax
    mov byte [rdi + 2], al
    ;Start by scanning for the switches
    lea rdi, cmdBuffer + 1  ;Goto command line input chars count
    movzx ecx, byte [rdi]   ;Get number of chars typed
    inc rdi ;Goto first char typed in
    mov rsi, rdi    ;Use rsi as start of buffer counter
    mov al, byte [switchChar]   ;Scan for switchchars
.switchScan:
    repne scasb ;Scan for a switchchar
    jecxz .switchScanDone
    and al, 0DFh    ;UC it
    cmp al, "W" ;Wide print mode?
    jne .notWideSw
    or byte [dirPrnType], 1 ;Set the correct bit
    jmp short .switchScan
.notWideSw:
    cmp al, "P" ;Pause mode?
    jne badParamError   ;If a switch other than /P or /W, fail
    or byte [dirPrnType], 2 ;Set correct bit
    jmp short .switchScan
.switchScanDone:
;If no args, only switches, we search CWD
;If one arg, search that 
;If more than one, fail
    lea rsi, cmdBuffer + 2
    call skipSpaces ;Skip leading spaces
    add rsi, 3  ;Go past the DIR (always three chars)
.lp:
    call skipSpaces ;Skip spaces after
    lodsb   ;Get first non space char
    call isALEndOfCommand   ;If this is the end char CR or "|", exit
    jz .eocNoNull
    cmp al, ">"
    je .eocNoNull
    cmp al, "<"
    je .eocNoNull
    cmp al, byte [switchChar]  ;Is al a switch char?
    jne .notSwitch
    ;Now we skip the switch if it was a switch
    call findTerminatorOrEOC    ;Go past the switch
    jc .eocNoNull  ;If we reach the EOC, exit,
    jmp short .lp
.notSwitch:
    ;If not a switch, should be a path. Copy to buffer and keep searching
    cmp byte [dirPathArg], 0    ;If a second path provided, error
    jne badArgError
    lea rdi, dirPathArg ;Store the path to search here AS WRITTEN BY USER
    dec rsi ;Go back to the start of the string
.nameCopy:
    lodsb
    call isALEndOfCommand
    jz .eocReached
    call isALterminator
    jz .terminateCopy
    stosb
    jmp short .nameCopy
.terminateCopy:
    xor eax, eax
    stosb   ;Store a terminating null here if a terminator found.
    jmp short .lp ;Now search if another 
.eocReached:
    xor eax, eax
    stosb   ;Store a terminating null here if a terminator found.
.eocNoNull:
    cmp byte [dirPathArg], 0    ;If no path provided, use CWD for current drive
    je .currentDrv
    ;Here we check if we have a drvSpec and path or just drvSpec
    lea rsi, dirPathArg
    cmp byte [rsi + 1], ":"  ;Is this a colon (drvspec check)
    jne .currentDrv
    ;Here the drive is specified, so lets parse filename to verify if drv ok
    mov byte [r8 + fcb1 + fcb.driveNum], 0  ;Clear this byte by default
    lea rdi, qword [r8 + fcb1]
    mov eax, 2901h   ;Parse filename
    int 41h
    cmp al, -1
    je badDriveError    ;If the drive is bad, bad parameter
    ;Else the drive in the fcb is valid
    movzx eax, byte [r8 + fcb1 + fcb.driveNum]
    dec al  ;Convert to 0 based drive number
    mov byte [dirDrv], al
    jmp short .dirPrintVol
.currentDrv:
    call getCurrentDrive    ;Get current drive number (0 based) in al
    mov byte [dirDrv], al   ;Store the 0 based drive number in al
.dirPrintVol:
    movzx eax, byte [dirDrv] 
    call volume.dirEP
    cmp byte [dirPathArg], 0    ;Null path here, 
    je .printCWD
    cmp byte [dirPathArg + 3], 0    ;Was this X:,0?
    je .printCWD
    ;Here we have a path
    ;Temp measure, we just fall through ignoring the path provided
.printCWD:
    mov dl, byte [dirDrv]
    mov al, dl
    add al, "A"
    mov ah, ":"
    mov word [searchSpec], ax
    mov al, byte [pathSep]
    mov byte [searchSpec + 2], al
    lea rsi, searchSpec + 3  ;Make space for a X:"\"
    mov ah, 47h ;Get Current Working Directory
    inc dl  ;Convert to 1 based number
    int 41h
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
    lea rdx, crlf2
    mov ah, 09h
    int 41h
    call .searchForFile
    return
    ;If we get no path spec or just a X: path spec then we 
    ; search the current working directory for that pathspec
    ;If we get an actual pathspec, we first save the CWD for that drive
    ; and then we try to make the pathspec the CWD. 
    ;   If it works, we then search *.* in that folder and return the og CWD.
    ;   If it fails, we then search one level up, for the search pattern
    ;    that we compute.

    
    ;Now we need to print the path to the folder we will be searching in
.searchForFile:
    ;Now we search for the files
    ;RCX must have the number of chars to the end of the pathspec
    lea rdi, searchSpec
    mov al, byte [pathSep]
    cmp byte [rdi + rcx - 1], al
    je .noAddSlash  ;Deals with special case of root dir
    mov byte [rdi + rcx], al
    inc ecx
.noAddSlash:
    lea rdi, qword [rdi + rcx]
    lea rsi, dirSrchPat
    mov rdx, rdi    ;Ptr to search for in rdx
    movsq
    movsd
    xor al, al
    stosb   ;Store the terminating null
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
    test byte [dirPrnType], 1
    jz .dirNoEndNewLine
    lea rdx, crlf   ;Only need this for /W
    mov ah, 09h
    int 41h
.dirNoEndNewLine:
    ;Now we print the number of files and the number of bytes on the disk
    lea rdx, fourSpc
    mov ah, 09h
    int 41h
    mov ah, 09h ;Print four Spaces twice
    int 41h
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
    inc dl  ;Function 36h wants the 1 based number
    int 41h ;Get disk free space info
    movzx eax, ax   ;Sectors per Cluster 
    movzx ecx, cx   ;Bytes per Sector
    or ebx, ebx ;Clear the upper bits of rbx
    mul ecx ;Get bytes per cluster
    mul rbx ;Multiply to the number of free clusters on the disk
    ;rax now has the number of free bytes on the disk
    call printDecimalWord
    lea rdx, bytesOk
    mov ah, 09h
    int 41h
    return

.dirPrintFileData:
;Use fcbCmdSpec to build the file name with space
;Start by print the name (same for both cases)
;We first check if the file has attributes hidden/system and hide them if so
    test byte [cmdFFBlock + ffBlock.attribFnd], dirIncFiles
    retnz   ;Simply return if either bit is set
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
    push rcx
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
    pop rbx
.dirPrintFileSizeLoop:
    mov dl, bl
    mov ah, 02h
    int 41h
    shr rbx, 8  ;Get next byte
    dec ecx
    jnz .dirPrintFileSizeLoop
    lea rdx, twoSpc
    mov ah, 09h
    int 41h
.dirPrintFileDT:
    mov dl, " "
    mov ah, 02h
    int 41h
    movzx eax, word [cmdFFBlock + ffBlock.fileDate]
    xor ebx, ebx    ;Ensure we print 2 digit year
    call printDate
    lea rdx, twoSpc
    mov ah, 09h
    int 41h
    movzx eax, word [cmdFFBlock + ffBlock.fileTime]
    call printTime
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
    je badDriveError  ;IF the drive is good, but FCB name blank, either X: or \ 
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
    jc badDirError  ;If this returns CF=CY, its a badDir
    lea rdx, searchSpec
    mov ah, 3Bh ;CHDIR
    int 41h
    jc badDirError
    return

mkdir:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError
    ;We have exactly one argument
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je badDriveError  ;If a drive was specified and was bad, jump
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 3900h  ;MKDIR
    int 41h
    retnc
.badMake:   ;Else, bad make
    lea rdx, badMD
    mov eax, 0900h
    int 41h
    return

rmdir:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError
    ;We have exactly one argument
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je badDriveError  ;If a drive was specified and was bad, jump
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 3A00h  ;RMDIR
    int 41h
    retnc   ;Return if not carry
.badRemove:   ;Else, bad make
    lea rdx, badRD
    mov eax, 0900h
    int 41h
    return

copy:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jz badArgError
    lea rsi, cmdBuffer
    movzx eax, byte [arg1Off]
    add rsi, rax    ;Go to the start of the command
    ;rsi points to terminating char
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, sourcePath ;Store this in sourcePath
.copyName1:
    lodsb
    call isALEndOfCommand
    je badParamError
    call isALterminator
    jz .endOfName1
    stosb
    jmp short .copyName1
.endOfName1:
    xor eax, eax
    stosb   ;Store this 0 at rdi
    lea rsi, cmdBuffer
    movzx eax, byte [arg2Off]
    add rsi, rax    ;Go to the start of the command
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, destPath
.copyName2:
    lodsb
    call isALEndOfCommand
    je .endOfName2
    call isALterminator
    jz .endOfName2
    stosb
    jmp short .copyName2
.endOfName2:
    xor eax, eax
    stosb   ;Store this 0 at rdi
;Before we open, we check if the two filenames are equal
; If so, crap out.
    lea rsi, sourcePath
    lea rdi, destPath
    mov eax, 121Eh
    int 4Fh
    jz .sameFilename
    ;Open source with read permission
    ;Open destination with write permission
    lea rdx, sourcePath
    mov eax, 3D00h  ;Read open
    int 41h
    jc badParamError
    mov word [sourceHdl], ax
    lea rdx, destPath
    mov eax, 3C00h  ;Create the file
    xor ecx, ecx    ;No file attributes
    int 41h
    jc .badExit
    mov word [destHdl], ax
    xor esi, esi
    lea rdx, copyBuffer
.copyLoop:
    mov ecx, 128
    movzx ebx, word [sourceHdl]
    mov ah, 3Fh ;Read
    int 41h
    jc .badExit
    test eax, eax
    jz .okExit
    add esi, eax
    ;mov eax, EOF
    ;mov rdi, rdx
    ;mov ecx, 128
    ;repne scasb ;Scan for an EOF
    ;mov eax, 128
    ;sub eax, ecx    ;If an EOF found, only print up to it
    mov ecx, eax
    movzx ebx, word [destHdl]
    mov ah, 40h ;Write
    int 41h
    jc .badExit
    cmp eax, 128    ;Change this for writing from Char devices in ASCII mode
    jnb .copyLoop
.okExit:
    call .leaveCopyClose
    lea rdx, crlf
    mov ah, 09h
    int 41h
    lea rdx, fourSpc
    mov ah, 09h
    int 41h
    mov ah, 02h
    mov dl, "1" ;1 File(s) copied
    int 41h
    lea rdx, copyOk
    mov ah, 09h
    int 41h
    return
.sameFilename:
    call .leaveCopyClose ;Close the handles
    jmp noSelfCopyError
.leaveCopyClose:
    mov bx, word [sourceHdl]
    mov eax, 3E00h
    int 41h
    mov bx, word [destHdl]
    mov eax, 3E00h
    int 41h
    return
.badExit:
;Prototypically use badParamError for error reporting... sucks I know
    mov bx, word [sourceHdl]
    cmp bx, -1
    je .skipSource
    mov eax, 3E00h  ;Close this handle
    int 41h
.skipSource:
    mov bx, word [destHdl]
    cmp bx, -1
    je badParamError
    mov eax, 3E00h
    int 41h
    jmp badParamError

erase:
    test byte [arg1Flg], -1
    jz badArgError
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 4100h  ;Delete File 
    xor ecx, ecx
    int 41h
    jc badArgError
    return
date:
    lea rdx, curDate
    mov ah, 09h
    int 41h
    mov ah, 2Ah ;DOS get date
    int 41h
	;AL = day of the week (0=Sunday)
	;CX = year (1980-2099)
	;DH = month (1-12)
	;DL = day (1-31)
    mov word [td1], cx
    mov byte [td3], dl
    mov byte [td4], dh
    movzx eax, al
    mov ebx, eax
    shl ebx, 1   ;Multiply by 2
    add eax, ebx ;Make it 3 times 
    lea rdx, dayName
    lea rdx, qword [rdx + rax]  ;Go to the right day name
    mov ecx, 3  ;Print three chars
    mov ebx, 1  ;STDOUT
    mov ah, 40h ;Write to handle
    int 41h
    mov dl, " "
    mov ah, 02h
    int 41h
;       eax[0:4] = Day of the month, a value in [0,...,31]
;       eax[5:8] = Month of the year, a value in [0,...,12]
;       eax[9:15] = Number of years since 1980, a value in [0,...,127]
    movzx eax, word [td1]   ;Get this word
    shl eax, 9 ;Move it high to pack it properly
    movzx ebx, byte [td4]
    shl ebx, 5  ;Shift the date to the right position
    or eax, ebx ;Add this date to eax
    movzx ebx, byte [td3]
    or eax, ebx
    mov ebx, 1  ;Four digit year pls
    call printDate

    lea rdx, newDate
    mov ah, 09h
    int 41h
    lea rdx, ukDate
    lea rax, usDate
    lea rbx, jpDate
    cmp byte [ctryData + countryStruc.dtfmt], 1
    cmova rdx, rbx
    cmovb rdx, rax
    mov ah, 09h
    int 41h

    lea rdx, qword [r8 + cmdLineCnt]
    mov ah, 0Ah
    int 41h
    push rdx
    lea rdx, crlf
    mov ah, 09h
    int 41h
    pop rdx
    cmp byte [rdx + 1], 0   ;If the user typed nothing...
    rete    ;Exit!
    return

time:
    lea rdx, curTime
    mov ah, 09h
    int 41h
    mov ah, 2Ch ;DOS get time
    int 41h
    ;CH = hour (0-23)
	;CL = minutes (0-59)
	;DH = seconds (0-59)
	;DL = hundredths (0-99)
    mov byte [td1], cl
    mov byte [td2], ch
    mov byte [td3], dl
    mov byte [td4], dh
    movzx eax, ch
    call printTime.printHours

    mov dl, byte [ctryData + countryStruc.timeSep]
    mov ah, 02h
    int 41h

    movzx eax, byte [td1]   ;Minutes
    call printTime.printMinutesAlt

    mov dl, byte [ctryData + countryStruc.timeSep]
    mov ah, 02h
    int 41h

    movzx eax, byte [td4]   ;Seconds
    call printTime.printMinutesAlt

    mov dl, "."
    mov ah, 02h
    int 41h

    movzx eax, byte [td3]   ;Hundreths
    call printTime.printMinutesAlt

    lea rdx, newTime
    mov ah, 09h
    int 41h

    lea rdx, qword [r8 + cmdLineCnt]
    mov ah, 0Ah
    int 41h
    push rdx
    lea rdx, crlf
    mov ah, 09h
    int 41h
    pop rdx
    cmp byte [rdx + 1], 0   ;If the user typed nothing...
    rete    ;Exit!
    return
ctty:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError
    lea rsi, cmdBuffer
    movzx eax, byte [arg1Off]
    add rsi, rax  ;Goto the first char of the argument
    cmp byte [rsi + 1], ":" ;If a drive is specified, check if valid
    jne .noDrive
    movzx eax, byte [arg1FCBret]
    cmp al, -1
    je badDriveError
.noDrive:
    ;Now we open the provided file
    call copyArgumentToSearchSpec
    lea rdx, searchSpec
    mov eax, 3D02h  ;Open in read/write mode
    int 41h
    jc badFileError
    movzx ebx, ax   ;Save the handle in ebx
    mov eax, 4400h  ;Get device word
    int 41h
    test dl, 80h    ;Test if this device is a char device
    jz .badCharDev  ;If this bit is 0 => Disk file
    ;Now we set this handle to be STDIO
    or dl, 3    ;Set STDIO bits
    xor dh, dh
    mov eax, 4401h  ;Now we set the device word
    int 41h
    ;Now we DUP2 for STDIN/OUT/ERR
    xor ecx, ecx    ;STDIN
    mov ah, 46h
    int 41h
    inc ecx         ;STDOUT
    mov ah, 46h
    int 41h
    inc ecx         ;STDERR
    mov ah, 46h
    int 41h
    mov ah, 3Eh ;Now we close the original handle
    int 41h
    return
.badCharDev:
    lea rdx, badDev
    mov ah, 09h
    int 41h
    mov ah, 3Eh ;Close opened handle
    int 41h
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
    jne .badOnOff
    and bx, 0FFDFh ;Convert only the third char to UC. Fourth char MUST BE SPACE
    cmp bx, "F "
    jne .badOnOff
    ;Set off
    xor edx, edx    ;DL=0 => BREAK is off
    jmp short .setBreak
.maybeOn:
    cmp ax, "ON"
    jne .badOnOff
    ;Set on
    mov edx, 1
.setBreak:
    mov eax, 3301h  ;Set break
    int 41h
    return
.badOnOff:
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
    jne .badOnOff
    and bx, 0FFDFh ;Convert only the third char to UC. Fourth char MUST BE SPACE
    cmp bx, "F "
    jne .badOnOff
    ;Set off
    xor eax, eax    ;AL=0 => VERIFY is off
    jmp short .setVerify
.maybeOn:
    cmp ax, "ON"
    jne .badOnOff
    ;Set on
    xor eax, eax
    inc eax ;AL=1 => VERIFY is on
.setVerify:
    mov ah, 2Eh  ;Set Verify
    int 41h
    return
.badOnOff:
    lea rdx, badOnOff
    mov ah, 09h
    int 41h
    return

rename:
    test byte [arg1Flg], -1
    jz badArgError
    test byte [arg2Flg], -1
    jz badArgError
    lea rsi, cmdBuffer
    movzx eax, byte [arg1Off]
    add rsi, rax    ;Go to the start of the command
    ;rsi points to terminating char
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, sourcePath ;Store this in sourcePath
.copyName1:
    lodsb
    call isALEndOfCommand
    je badParamError
    call isALterminator
    jz .endOfName1
    stosb
    jmp short .copyName1
.endOfName1:
    xor eax, eax
    stosb   ;Store this 0 at rdi
    lea rsi, cmdBuffer
    movzx eax, byte [arg2Off]
    add rsi, rax    ;Go to the start of the command
    cmp byte [rsi + 1], ":" ;If dest path char 2 is :, must be X:, not allowed
    je badParamError
    lodsb   ;Get first char in AL
    dec rsi ;Go back to this char
    call isALEndOfCommand
    jc badParamError
    lea rdi, destPath
.copyName2:
    lodsb
    call isALEndOfCommand
    je .endOfName2
    call isALterminator
    jz .endOfName2
    stosb
    jmp short .copyName2
.endOfName2:
    xor eax, eax
    stosb   ;Store this 0 at rdi
    lea rdx, sourcePath
    lea rdi, destPath
    mov eax, 5600h
    int 41h
    retnc   ;Return if all oki!
    cmp al, errBadDrv
    je badDriveError
    cmp al, errBadFmt
    je badDirError
    jmp badDupFnf
;TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP
touch:
;Temporarily used to create files
    test byte [arg1Flg], -1
    jz badArgError
    call buildCommandPath
    lea rdx, searchSpec
    mov eax, 3C00h  ;Create file 
    xor ecx, ecx
    int 41h
    jc .touchError
    movzx ebx, ax
    mov eax, 3e00h  ;Close file immediately
    int 41h
    return
.touchError:
    lea rdx, touchErr
    jmp badCmn
;TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP
truename:
    test byte [arg1Flg], -1
    jz badArgError
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

volume:
    lea rsi, cmdBuffer + 2  ;Get the command buffer
    call skipSpaces
    add rsi, 3  ;Go past the VOL command
    call skipSpaces
    lodsb   ;Get the first char, and point rsi to next char
    call isALEndOfCommand   ;If this char is end of command, use current drive
    jnz .checkDriveLetter
    call getCurrentDrive    ;Get 0-based current drive number in al
    jmp short .dirEP
.checkDriveLetter:
    cmp byte [rsi], ":" ;If this is not a :, fail
    jne badDriveError
    mov rdi, rsi    ;Save start of drive spec in rsi
    inc rsi  ;Go past the X: spec
    call skipSpaces
    lodsb   ;Get the non-space char in al
    call isALEndOfCommand   ;The next non-space char must be terminator
    jne badDriveError
;This argument should've been parsed into FCB1 so use that result
    mov al, byte [arg1FCBret]   ;Get the response from the parse
    test al, -1
    jnz badDriveError ;Can't have either wildcards nor be invalid (obviously)
    movzx eax, byte [r8 + fcb1 + fcb.driveNum] ;Get the 1-based drive number
    dec eax ;Convert to 0 based number
.dirEP: ;Must be called with VALID 0 based drive number in al
    add eax, "A" ;Get ASCII representation of 0 based number
    mov byte [volPathBuf], al   ;Store ASCII letter here
    lea rdx, cmdFFBlock     ;Use this as the DTA for this request
    mov ah, 1Ah
    int 41h
    lea rdx, volPathBuf
    mov cx, dirVolumeID
    mov ah, 4Eh ;Find first
    int 41h
    jc .skipVolLbl
    lea rsi, qword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, volLblSpc
    mov ecx, 11 ;Get the 11 chars of the volume label
.dirLblCopy:
    lodsb   ;Get the first char
    cmp al, 0
    je .skipVolLbl  ;Jump with CF=NC
    cmp al, "."
    je .dirLblSkipStore
    stosb
.dirLblSkipStore:
    dec ecx
    jnz .dirLblCopy
    ;Fallthru with CF=NC
.skipVolLbl:
;Print volume label information now
;Propagates the CF if CF was set    
    pushfq
    lea rdx, crlf
    mov ah, 09h
    int 41h
    lea rdx, volMes
    mov ah, 09h
    int 41h
    mov dl, byte [volPathBuf]   ;Print the drive letter out
    mov ah, 02h
    int 41h
    popfq
    jnc .volIDOk
    lea rdx, volNo
    mov ah, 09h
    int 41h
    lea rdx, crlf
    mov ah, 09h
    int 41h
    return
.volIDOk:
    lea rdx, volOk
    mov ah, 09h
    int 41h
    lea rdi, volLblSpc
    call strlen
    dec ecx
    mov byte [rdi + rcx], "$"   ;Replace the null with a string terminator
    lea rdx, volLblSpc
    mov ah, 09h
    int 41h
    lea rdx, crlf
    mov ah, 09h
    int 41h
    return

version:
    lea rdx, crlf
    mov ah, 09h
    int 41h
    lea rdx, dosVer
    mov ah, 09h
    int 41h
    call .printVersionNumber
    lea rdx, crlf
    mov ah, 09h
    int 41h
    return
.printVersionNumber:
    mov ah, 30h ;Get version numbers, al = Major, ah = Minor
    int 41h
    push rax
    movzx eax, al
    call printDecimalWord
    mov dl, "."
    mov ah, 02h
    int 41h
    pop rax
    movzx eax, ah
    call printDecimalWord
    return


memory:
    mov rbx, qword [sysVars]
    test rbx, rbx
    jnz .sysvarsOK
    lea rdx, memBad0
    mov ah, 09h
    int 41h
    jmp freezePC.altEP
.sysvarsOK:
    ;Use rsi to store DOS memory, rdi to store Free memory and rbp for Hole
    ; and rcx to store Application memory
    xor esi, esi
    xor edi, edi
    xor ebp, ebp
    xor ecx, ecx
    lea rbx, qword [rbx - 8]    ;Go back a qword
    mov rbx, qword [rbx]
.memLp:
    cmp byte [rbx], mcbMarkCtn
    je .validMCB
    cmp byte [rbx], mcbMarkEnd
    jne .badMCBFound
.validMCB:
    mov eax, dword [rbx + mcb.blockSize]
    shl rax, 4  ;Convert to bytes
    cmp qword [rbx + mcb.owner], mcbOwnerDOS
    jne .notDOS
    add rsi, rax    ;Add to DOS count
    jmp short .gotoNext
.notDOS:
    cmp qword [rbx + mcb.owner], mcbOwnerFree
    jne .notFree
    add rdi, rax    ;Add to Free space count
    jmp short .gotoNext
.notFree:
    cmp qword [rbx + mcb.owner], mcbOwnerHole
    jne .notHole
    add rbp, rax    ;Add to Hole count
    jmp short .gotoNext
.notHole:
    add rcx, rax    ;Add to Application count
.gotoNext:
    cmp byte [rbx], mcbMarkEnd
    je .endOfWalk
    lea rbx, qword [rbx + mcb.program + rax]
    jmp short .memLp
.endOfWalk:
    
    lea rdx, memDOS
    mov ah, 09h
    int 41h
    mov rax, rsi
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 41h

    test rbp, rbp
    jz .skipHole
    lea rdx, memHole
    mov ah, 09h
    int 41h
    mov rax, rbp
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 41h
.skipHole:

    lea rdx, memApp
    mov ah, 09h
    int 41h
    mov rax, rcx
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 41h

    lea rdx, memFree
    mov ah, 09h
    int 41h
    mov rax, rdi
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 41h

    lea rdx, memSys
    mov ah, 09h
    int 41h
    mov rax, rsi
    add rax, rdi
    add rax, rcx
    add rax, rbp
    call .mcbPrintAmount
    lea rdx, memByte
    mov ah, 09h
    int 41h

    lea rdx, crlf
    mov ah, 09h
    int 41h
    return

.mcbPrintAmount:
    push rcx
    push rsi
    push rdi
    push rbp
    call printDecimalWord
    pop rbp
    pop rdi
    pop rsi
    pop rcx
    return
.badMCBFound:
    lea rdx, memBad2
    mov ah, 09h
    int 41h
    jmp freezePC.altEP

type:
    test byte [arg1Flg], -1 ;If this not set, error
    jz badArgError
    test byte [arg2Flg], -1
    jnz badArgError         ;If this set, error
    lea rsi, cmdBuffer
    movzx eax, byte [arg1Off]
    add rsi, rax    ;Point rsi to this argument
    cmp byte [rsi], CR
    je badArgError
    cmp byte [rsi + 1], ":" ;If a drive is specified, check if valid
    jne .noDrive
    movzx eax, byte [arg1FCBret]
    cmp al, -1
    je badDriveError
.noDrive:
    ;Now we open the provided file
    call copyArgumentToSearchSpec
    lea rdx, searchSpec
    mov eax, 3D00h  ;Open in read only mode
    int 41h
    jc badFileError
    lea rdx, qword [r8 + psp.dta]
    movzx ebx, ax    ;Save the file handle in ebx
.lp:
    mov ecx, 128    ;Read 128 bytes at a time
    mov ah, 3Fh ;Read handle
    int 41h
    mov ecx, eax
    jecxz .exit
    push rbx    ;Save the original in handle
    mov ebx, 1  ;STDOUT
    mov ah, 40h
    int 41h
    pop rbx ;Get back the original read handle
    jc .exitBad
    cmp eax, ecx
    je .lp
    dec ecx ;One less for a ^Z
    cmp eax, ecx
    jne .exitBad
.exit:
    mov ah, 3Eh ;Close handle
    int 41h
    return
.exitBad:
    ;Print a disk error message... use a table to build the message but for
    ; now, just exit
    ;If it is a char device, don't print a error
    jmp short .exit

exit:
    test byte [permaSwitch], -1
    retnz   ;Return if the flag is set
    mov rax, qword [realParent] ;Get actual parent...
    cmp rax, -1
    rete    ;If the real parent is -1 => Original Command Interpreter.
    mov qword [r8 + psp.parentPtr], rax ;and restore parent pointer

    mov rdx, qword [parentInt42]
    mov qword [r8 + psp.oldInt42h], rdx
    mov eax, 2542h
    int 41h

    mov eax, 4C00h  ;Exit now okay
    int 41h
    return  ;If the exit wasn't successful for some reason, return as normal

launchChild:
;We run EXEC on this and the child task will return via applicationReturn
;Here we must search the CWD or all path componants before failing
;Also this command must be a .COM, .EXE or .BAT so check that first
    lea rdx, cmdFFBlock
    mov ah, 1Ah     ;Set DTA for task
    int 41h

    mov eax, dword [cmdFcb + fcb.fileext]   ;Get a dword, with dummy byte 3
    and eax, 00FFFFFFh  ;Clear byte three
    or eax,  20000000h  ;Add a space so it is like "COM "
    cmp eax, "    " ;Only if we have four spaces do we proceed here
    je .noExt
    call checkExtensionExec ;ZF=ZE => Executable
    jnz .dfltErrExit
    ;!!!!!!!!!!!TEMPORARY MEASURE TO AVOID LAUNCHING BAT FILES!!!!!!!!!!!
    jc .dfltErrExit ;Remove this when ready to launch batch files
    ;!!!!!!!!!!!TEMPORARY MEASURE TO AVOID LAUNCHING BAT FILES!!!!!!!!!!!
    ;So it is a com or exe that we are searching for for now
    lea rdi, cmdPathSpec
    mov rdx, rdi
    jmp short .search
.noExt:
    ;If the filename has no extension, append a .*
    ;Use bl as flags. bl[0] => COM found, bl[1] => EXE found, bl[2] => BAT found
    xor ebx, ebx
    lea rdi, cmdPathSpec
    mov rdx, rdi
    xor eax, eax
    mov ecx, -1
    repne scasb
    dec rdi ;Point to the terminating null
    mov rbp, rdi    ;Temporarily store the ptr to the . in rbp
    mov ax, ".*"
    stosw
    xor al, al  ;Store terminating null
    stosb
.search:
    mov ecx, dirIncFiles
    mov ah, 4Eh ;Find First File
    int 41h
    jc .dfltErrExit
    call .noExtCheckExt
.moreSearch:
    mov ah, 4Fh
    int 41h
    jc .noMoreFiles
    call .noExtCheckExt
    jmp short .moreSearch
.noMoreFiles:
    test ebx, ebx
    jz .dfltErrExit
;So we have a valid executable
    mov rdi, rbp    ;Get back ptr to the .*,0
    test ebx, 1
    jz .launchexebat
    mov eax, ".COM"
    jmp short .launch
.launchexebat:
    test ebx, 2
    jz .launchbat
    mov eax, ".EXE"
    jmp short .launch
.launchbat:
;Temporary For BAT
    jmp .dfltErrExit
.launch:
    stosd
    xor al, al
    stosb   ;Store the terminating null
    lea rbx, launchBlock
    xor eax, eax
    mov qword [rbx + execProg.pEnv], rax    ;Tell DOS to copy my current Env
    lea rax, qword [r8 + cmdLineCnt]
    mov qword [rbx + execProg.pCmdLine], rax
    lea rax, qword [r8 + fcb1]
    mov qword [rbx + execProg.pfcb1], rax
    lea rax, qword [r8 + fcb2]
    mov qword [rbx + execProg.pfcb2], rax
    lea rdx, cmdPathSpec
    mov eax, 4B00h  ;Load and execute!
    int 41h
    jmp .dfltErrExit    ;If something goes wrong, error out
.noExtCheckExt:
    ;mov eax, dword [cmdFFBlock + ffBlock.asciizName + filename.fExt]
    lea rsi, dword [cmdFFBlock + ffBlock.asciizName]
    lea rdi, fcbCmdSpec
    call asciiFilenameToFCB
    mov eax, dword [fcbCmdSpec + filename.fExt]
    and eax, 00FFFFFFh  ;Clear byte three
    or eax,  20000000h  ;Add a space so it is like "COM "
    cmp eax, "COM "
    jne .neceexe
    or ebx, 1
    return
.neceexe:
    cmp eax, "EXE "
    jne .necebat
    or ebx, 2
    return
.necebat:
    cmp eax, "BAT "
    retne
    or ebx, 4
    return

.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 41h
    return