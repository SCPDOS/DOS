;Generic Find First and Find Next functions here

searchDir:
    return
genericFindNext:
    return

adjustSearchAttr:
;Converts the search attributes as chosen by the caller
; to our internal requirements. This is ONLY called for 
; file searches, not intermediate directory searches. 
; Intemediate dir searches are conducted with dir bit only set
;Input: eax = User selected search mask
;Output: eax = Modified search mask
    test eax, dirVolumeID   ;Is the volume id bit set?
    jnz .esaVolume  ;If yes, replace the attributes with volume id only
    test eax, dirInclusive  ;Do we have an inclusive bit set?
    retz   ;If not, we return with al unchanged
    or eax, dirInclusive    ;Else set all the inclusive bits
    return  ;And exit
.esaVolume:
    mov eax, dirVolumeID    ;Make this a volume ID exclusive search
    return



setupFFBlock:
;Sets up the find first block for the search
;Uses currentDrv and fcbName
;Input: al = Search attributes
    mov byte [dosffblock + ffBlock.attrib], al 
    push rax
    push rsi
    push rdi
    movzx eax, byte [currentDrv]  ;Get the 0 based current drive number
    mov byte [dosffblock + ffBlock.driveNum], al
    lea rsi, fcbName
    lea rdi, qword [dosffblock + ffBlock.template]
    movsq   ;Move 8 chars
    movsw   ;Move 2 chars
    movsb   ;Move the final char
    pop rdi
    pop rsi
    pop rax
    return

getDrvLetterFromPath:
;Gets the drive letter for the path in al
;Input: rsi = Buffer to process
;Output: If al = 0, rsi NOT incremented by 2. Else, rsi incremented by 2 
;       ZF=ZE and al = 0 => Null path
;       ZF=NZ and al = 0 => Relative path, splice flag = 0 or Net path
;       ZF=NZ and al = -1 => Bad drive number
;       ZF=NZ and al = 1 based drive number => All oki, but may be relative
    xor al, al
    cmp byte [rsi], 00h ;Is this a null path?
    retz    ;Return if a null path
    cmp byte [rsi + 1], ":" ;Path separator?
    retne   ;If not equal, Relative path or network path
    lodsw   ;Get first word, rsi += 2
    ;Make char lower case if its not and then convert to offset from "a" - 1
    or al, 20h  ;Set the bit for lowercase chars
    sub al, 60h
    retnz ;If the number is non-zero, then a potentially valid drive number
    mov al, -1  ;Else not a valid drive number
    return
getDirPath:
    xor al, al   ;Set to Directory
    jmp short getPath
getFilePath:
    mov al, -1  ;Set to File
getPath:
;Determines whether the path is spliced or not and transfers the chars
; from the user buffer into an internal buffer, normalising them.
; Single and double dot entries are left as is, but the wildcard * is converted
; to ?. Wildcards can only be present in the LAST portion of the given path.
; If the portion with wildcards does not end with an ASCII null, we fail the 
; request with pnf. If there is a redirector which needs to normalise the path, 
; we let it do its thing and return.
;If the user requests data from a remote server (i.e. a pathspec starting with
; \\) then wildcards, the colon and dots are forbidden.
;If a remote user requests data (dosInvoke = -1), then the pathspec must be an
; absolute path (no wildcards or dots) and must begin with a drive letter 
; (converted from using machine name by the net client program).
;We check if we are a net invoke to ensure that the pathspec that was recieved
; was good.
;Called with:
; rdi = SDA Buffer for filename
; rsi = Potentially unqualified filename
; al = 0 => Search for Dir only. al != 0 => Search for File (or dir)
    mov byte [fileDirFlag], al  
    mov al, -1
    mov byte [spliceFlag], al   ;Set splice for Full path by default
    mov byte [filspcExist], al  ;We are searching for a file that exists
    mov qword [fname1Ptr], rdi  ;Save the SDA buffer we are using for this file
    cbw  ;make ah = -1
    mov word [lastPartOff], ax  ;Store -1 for the last part of the path
    test byte [dosInvoke], -1   ;Was it invoked via server? -1 = Server
    jz .notServer
    ;In this case, the client network program will have correctly
    ; substituted the drive letter for the path before making the request.
    ;Thus we can immediately assume the existance of a drive letter in the path 
    call getDrvLetterFromPath
    call getCDS ;Get the cds for the drive letter on the path
    ;Do nothing for now
    mov rdi, qword [workingCDS]
    call dosCrit1Enter
    call ensureDiskValid
    call dosCrit1Exit
.serverExit:
    mov al, errPnf  ;If CF=CY, use this error code
    return
.notServer:
    ;Make Redir request to qualify the filename if NOT invoked by server call
    mov qword [workingCDS], -1  ;Set workingCDS to unknown
    mov eax, 1123h
    int 4fh ;CF=CY if not resolved. CF=NC if resolved
    retnc  ;Return if resolved
    call getDrvLetterFromPath ;Get the drive letter in al (or -1)
    pushfq  ;Save the flag state on stack
    push rax    ;Save whether rsi is incremented by 2
    mov ax, word [rsi]   ;Get the word pointed to by rsi
    call swapPathSeparator  ;Convert al if it is a path separator
    xchg ah, al ;Now swap al into ah to check if we on a network path (i.e. \\)
    call swapPathSeparator  ;Returns ZF=ZE if al = "/" or "\"
    jnz .notNet
    cmp ah, al  ;If they are equal, we have a net path
    jne .notNet
    pop rax ;We are in a net situation, so rsi is pointing at "\\"
    popfq
    movsw   ;Tfr the two chars rsi, rdi + 2
.moveNetChars:
    lodsb   ;Get the third char into al and inc rsi
    call uppercaseChar  ;Make char in al uppercase
    test al, al
    jz .netEnd
    call swapPathSeparator  ;If path sep, swap it
    mov rbx, rdi    ;Store current buffer offset in rbx
    stosb
    jnz .moveNetChars  ;If not a path separating char in al, keep looking
    call .mainlp    ;Now expand the pathspec portion
    return
.netEnd:
    stosb
    return
.notNet:
;This is the normal case; paths can be relative, or absolute.
    pop rax ;Get the drive letter back
    popfq   ;Get the flag state back
    jnz .notNull    ;If ZF=ZE, the path is a null path, errExit
.pnfErr:
    mov al, errPnf  ;Null path error
    stc
    return
.notNull:
    cmp al, -1  ;Bad drive letter?
    jne .driveOk    ;Jump if ok drive letter
    mov al, errBadDrv   ;Bad drive letter specified
    stc
    return
.driveOk:
    test al, al
    jz .curRelPath ;If al = 0, the path is definitely relative (rel curr. drv.)
    ;al now has 1-based drive number, rsi has been incremented by 2.
    cmp byte [rsi], 0   ;Is this pathspec "X",":",0?
    je .pnfErr  ;Throw error if it is a malformed path
    lodsb   ;Move rsi to the third char, get char in al
    dec rsi ;Move rsi back to point to the previous char
    call swapPathSeparator  ;ZF=ZE if path separator
    ;If al is a path separator, then this path is absolute.
    jz .relMain ;If relative, rsi points to first char in path
    ;Here the path is absolute. Now point rsi to first char past "\"
    inc rsi
    jmp short .mainlp
.curRelPath:
;This is only jumped to if we are relative the current drive
;rsi points to first char in path
    mov al, byte [currentDrv]   ;Get current drive (0-based number)
    inc al  ;Turn it into a 1 based drive number
.relMain:
    mov byte [spliceFlag], 0    ;Set Splice flag to indicate Relative to CDS
.commonDir:
;rsi points to the start of the string we will be appending
    call .prepareDir    ;Prepare the dir if the drive is subst/join drive
    ;Search now for parent \, if in root, point to self
    push rdi
    cmp byte [rdi - 2], ":" ;If the char before the current \ is :, we at root
    je .inRoot
    sub rdi, 2  ;Skip the free space and the "\"
.findParent:
    cmp byte [rdi], "\"
    je .parentFound
    dec rdi ;Go back a further char
    jmp short .findParent
.parentFound:
    inc rdi ;Goto first free char space after the "\"
.inRoot:
    mov rbx, rdi    ;Store this as the parent pointer
    pop rdi
.mainlp:    ;Now we transfer each directory portion
    call .copyPathspec  ;Now we copy the pathspec, no spaces
    retc    ;Return if the carry flag is set, error code in al
    test al, al
    retz    ;Return if al is the null char
    ;Else, now we go to the next part
    jmp short .mainlp

.prepareDir:
;Used to transfer the current directory if it is necessary.
;Always necessary if the user specified a subst/join drive. Else only if 
; relative
;Input: al = 1-based drive letter
;Output: rdi = Pointing at where to place chars from source string
    push rsi
    call setDrive   ;Set internal variables, working CDS etc etc
    mov rdi, qword [workingCDS] ;CDS pointer in rdi
    push rdi
    call dosCrit1Enter
    call getDiskDPB  ;Update the working DPB ptr for this request
    call dosCrit1Exit
    pop rsi          ;Move CDS pointer in rsi
    mov rdi, qword [fname1Ptr] ;Get the ptr to the filename buffer we will use
    ;If this CDS is a subst drive, copy the current path to backslashOffset
    ;If this CDS is a join drive, copy the first 3 chars and up to the next 
    ;   terminating char (\, / or Null)
    ;If the path is to be spliced, then we copy the whole CDS current path
    ;If the CDS is not subst drive, nor to be spliced, we copy first two chars.
    test word [rsi + cds.wFlags], cdsJoinDrive
    jnz .prepDirJoin
    test word [rsi + cds.wFlags], cdsSubstDrive
    jnz .prepDirSubst
.prepMain:
;Ok so now preliminary copy complete, now we check if path spliced
    test byte [spliceFlag], -1
    jz .prepLoop ;If this flag is zero, we loop
    ;Else we copy the first two chars only (X:)
    movsw  
    jmp short .prepDirExit
.prepLoop:
    lodsb
    stosb
    test al, al ;If al was null, then we stop
    jnz .prepLoop
.prepDirExit:
    mov al, "\"
    stosb   ;Store the path separator and increment rdi
    pop rsi
    return
.prepDirJoin:
    push rcx
    push rsi
    add rsi, 2  ;Goto the backslash for the dir
    mov ecx, 2  ;Instantiate ecx to copy X:
.prepDirJoin1:
    lodsb   ;Get the char
    test al, al ;Null char?
    jz .prepDirJoin2
    call swapPathSeparator
    jz .prepDirJoin2
    inc ecx ;Accrue length to copy
    jmp short .prepDirJoin1
.prepDirJoin2:
    pop rsi ;Return rsi to the start of the buffer
    jmp short .prepDirCopy1
.prepDirSubst:
    push rcx
    movzx ecx, word [rdi + cds.wBackslashOffset]
.prepDirCopy1:
    rep movsb   ;Copy the string over
    pop rcx
    test byte [spliceFlag], -1
    jnz .prepDirExit    ;If not relative, exit as we put the "root dir" marker
    jmp short .prepLoop ;Else, need to copy CDS now too as part of path
.copyPathspec:
;1) Copies a path portion from the source buffer to the destination
;2) Advances rsi to the next null, \ or /
;3) Expands all * to ?'s
;4) Understands \. means "this directory" and can be ignored with rsi moving to
;    next path separator
;5) Understands \.. means "parent directory" and rdi should be changed to rbx
;    with rsi moving to path separator
;6) Each name in destination is at most 12 chars long, to account for the dot
;
;INPUT:     rsi = First char of pathspec to qualify
;           rdi = Points to where to store it
;
;RETURN:    rsi = First char of next pathspec or past terminating null
;           rdi = First char of next space to store next pathspec
;           al = Last char stored (either \ or NULL)
;           CF=NC = OK path
;           CF=CY = PATH OR FILE NOT FOUND
;               IF A WILDCARD FOUND IN A SUBDIR NAME, RETURN PNF.
    push rdi    ;Save the pointer into the user buffer
    mov byte [fcbSpaceOk], -1    ;Set to be ok to have space in the name
    lea rdi, fcbName
    push rdi
    mov ecx, 3
    mov eax, "    " ;Four spaces
    rep stosd   ;Store 12 spaces
    pop rdi ;Point rdi back to fcb name head

    lodsb   ;Get first char from user path in al
    dec rsi ;And move rsi to point back to it
    cmp al, "."   ;Handle starting dot separately
    je .cpsDots
;First char is not a dot, so now check if starts with E5h? 
;If so, store 05h in its place
    cmp al, 0E5h
    jne .cpsMainLoop
    inc rsi ;Push rsi to point to next char
    mov al, 05h
    stosb   ;Store the char, rsi is pointing at next char
    mov ecx, 8 ;8 chars to move over, when ecx = 0, the char must be . or term
    mov ch, 1   ;Set that we are in name field
.cpsMainLoop:
    lodsb   ;Get the char in al and advance rsi
    test al, al ;Is it the null char?
    jz .cpsProcessName  ;If so, terminate
    call uppercaseChar  ;Else uppercase it...
    call swapPathSeparator  ;And if it is a pathsep, convert it before exiting
    jz .cpsProcessName
    cmp al, "." ;Filename extension separator
    je .cpsExtension
    cmp ecx, 0100h  ;If ch = 1 and cl = 0, then look for either . or terminator
    je .cpsMainLoop
    jecxz .cpsCharSkip ;If ch = 0 and cl = 0, scan for next terminator
    ;If we have space in the filename, we check to see if the next char is *
    cmp al, "*" ;Wildcard?
    je .cpsWildcard
    stosb   ;Else store the converted char in al and inc rdi
    dec cl  ;One less char left to tfr
    jmp short .cpsMainLoop
.cpsExtension:
;rsi has been incremented past the extension field. Discard the . in al
    mov ecx, 3  ;Set to 3 chars left, in extension (ch = 0)
    lea rdi, qword [fcbName + filename.fExt]    ;Goto the extension field
    jmp short .cpsMainLoop
.cpsDots:
    stosb   ;Store the first dot
    lodsb   ;Check now if we have a second dot
    cmp al, "."
    jne .cpsCharSkip
    stosb   ;Store the second dot
.cpsCharSkip:
    call .cpsPtrSkip    ;Now we are skipping any chars that arent terminators
    jmp short .cpsProcessName
.cpsWildcard:
    ;cl has the number of chars of ? to store 
    mov al, "?"
    push rcx
    movzx ecx, cl   ;Temporarily extend cl to ecx
    rep stosb   ;Store that many ? in buffer and return cl to 0
    pop rcx
    test ch, 1  ;Is this bit set? If so, we jump to .cpsExtension
    jnz .cpsExtension   ;Now fill the extension field
    ;Else, we process filename
    jmp short .cpsCharSkip
.cpsPtrSkip:
;Now advance rsi past the next pathsep or null char
;Output: al = Terminator char (either \ or null)
;        rsi -> First char of next pathspec (if al = \)
    lodsb
    test al, al ;Is this null?
    retz
    call swapPathSeparator
    retz
    jmp short .cpsPtrSkip
.cpsProcessName:
    ;Now search the current directory for this filename
    ;Find first using SDA ffBlock
    ;If al = 0, we have a file name
    ;If al = \, we have subdirectory. NO WILDCARDS ALLOWED IF \
    push rsi    ;Save the current position of the pointer in the user buffer
;Evaluate whether we are searching for a file for a directory
    test al, al
    jz .cpsPNfile
    ;Fall if subdir
    push rdi
    lea rdi, fcbName
    mov al, "?" ;Search for wildcard
    mov ecx, 12
    repne scasb
    pop rdi
    je .cpsPnf  ;Path not found if a ? found in the name
    mov al, dirDirectory    ;We want a directory only search.
    jmp short .cpsPNMain
.cpsPNfile:
    ;Here if we are searching for a file
    movzx eax, word [searchAttr]    ;Get the search attributes
    call adjustSearchAttr   ;Edit the search attributes as needed
.cpsPNMain:
    call setupFFBlock   ;Sets up the internal ff block, attribs in al
    ;Now the internal ff block is setup, conduct search.

    call searchDir

    pop rsi
    pop rdi ;rdi points to where it's ok to place pathspec
    ;Copy filename over
    return
.cpsPnf:
    mov eax, errPnf
    jmp short .cpsErrExit
.cpsFnf:
    mov eax, errFnf
.cpsErrExit:
    pop rsi
    pop rdi
    stc ;Set carry
    return