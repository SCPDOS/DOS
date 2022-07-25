;Generic Find First and Find Next functions here

genericFindFirst:
    return
genericFindNext:
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
qualifyDirPath:
    xor al, al   ;Set to Directory
    jmp short qualifyPath
qualifyFilePath:
    mov al, -1  ;Set to File
qualifyPath:
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
    cmp byte [rdi - 2], "\" ;If the char before the current \ is :, net root
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
.gotoNextPathspec:
    inc rbx ;Advance the parent pointer
    cmp byte [rbx], "\"
    je .mainlp
    jmp short .gotoNextPathspec

.prepareDir:
;Used to transfer the current directory if it is necessary.
;Always necessary if the user specified a subst/join drive. Else only if 
; relative
;Input: al = 1-based drive letter
;Output: rdi = Pointing at where to append additional chars from source string
    push rsi
    call setDrive   ;Set internal variables, working CDS etc etc
    mov rdi, qword [fname1Ptr] ;Get the ptr to the filename buffer we will use
    mov rsi, qword [workingCDS] ;CDS pointer in rsi
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
    jmp short .prepMain
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
;           rbx = First char of the parent directory name in destination buffer
;
;RETURN:    rsi = First char of next pathspec or past terminating null
;           rdi = First char of next space to store next pathspec
;           al = Last char stored (either \ or NULL)
;           CF=NC = OK path
;           CF=CY = BAD WILDCARD PATH
    mov ecx, 12 ;12 chars per portion (8+1+3 for the dot)
    lodsb   ;Get first char in al
    dec rsi ;And move rsi to point back to it
    cmp al, "."   ;Handle dot separately
    je .cpsDots
;First char is not a dot, so now check if starts with E5h? 
;If so, store 05h in its place
    cmp al, 0E5h
    jne .cpsMainLoop
    inc rsi ;Push rsi to point to next char
    mov al, 05h
    stosb   ;Store the char, rsi is pointing at next char
    dec ecx ;One char already stored, now proceed as normal.
.cpsMainLoop:
    lodsb   ;Get the first char in al and advance rsi
    test al, al ;Is it the null char?
    jz .cpsPadBlank  ;If so, terminate
    call uppercaseChar  ;Else uppercase it...
    call swapPathSeparator  ;And if it is a pathsep, convert it before exiting
    jz .cpsPadBlank
    cmp al, "*" ;Wildcard?
    je .cpsWildcard
    jecxz .cpsOflow ;If ecx = 0 here, scan for the next terminating char to use
    stosb   ;Store the converted char in al and inc rdi
    dec ecx ;One fewer char left in spec
    cmp ecx, 4  ;Are we at the dot?
    jne .cpsMainLoop
    lodsb   ;Get the char at rsi and inc rsi (this should be a dot so set dot)
.cpsStoreDot:
    mov al, "."
    stosb   ;Store the dot. Inc rdi
    dec ecx ;Now set ecx to 3
    jmp short .cpsMainLoop
.cpsPadBlank:
;Skip expanding spaces
    ;jecxz .cpsEndSpec   ;If all chars used up, skip padding
    ;mov al, " " ;Space char
    ;rep stosb   ;Store that many spaces
.cpsEndSpec:
    stosb   ;Store terminating null or "\"
    clc
    return
.cpsOflow:
;Now we scan to find the next terminating char to terminate spec with.
;ecx is zero here
    lodsb
    test al, al
    jz .cpsEndSpec
    call swapPathSeparator
    jz .cpsEndSpec  
    jmp short .cpsOflow
.cpsDots:
;No file name can start with a period so any following chars 
; are ignored, up to the pathsep or the null
;If we have a period, we simply ignore this entry for the destination string and
; rdi remains where it is.
;If we have two periods, since we pass a pointer to the parent directory in
; rbx, we move rdi to it.
;In both cases, we then advance rsi to the next /, \ or null
    lodsw
    sub rsi, 2  ;Go back to start of string
    cmp ax, ".."    ;Is this a parent ptr request?
    je .cpsDotDot
    xor ecx, ecx    ;Ensure no padding occurs
    jmp short .cpsOflow ;Now look for next terminator
.cpsDotDot:
    cmp byte [rbx - 1], ":" ;If the char before the parent \ is :, fail
    je .cpsBadExit
    cmp byte [rbx - 1], "\" ;If the char before the parent \ is \, net fail
    je .cpsBadExit
    mov rdi, rbx    ;Reposition the destination ptr 
    xor ecx, ecx    ;Ensure no padding occurs
    jmp short .cpsOflow ;Now look for next terminator
.cpsWildcard:
;We expand * to ecx - 4 ?'s if ecx > 4. If ecx < 4, then ecx ?'s
    cmp ecx, 4
    jb .cpsWCext
    push rcx
    sub ecx, 4
    mov al, "?"
    rep stosb
    pop rcx
    lodsb   ;Move char after first * into al, and move rsi to first char past .
    test al, al
    jz .cpsEndSpec
    call swapPathSeparator
    jz .cpsBadExit ;Wildcards cannot be used if the next char is pathsep
    cmp al, "." ;Filename separator?
    jne .cpsBadExit
    mov ecx, 4
    jmp short .cpsStoreDot
.cpsWCext:
    push rcx
    mov al, "?" ;Store ecx many ?'s now
    rep stosb
    pop rcx
    lodsb   ;Get next char in al. Must be terminator. If pathsep, fail
    test al, al
    jz .cpsEndSpec
;Else fall through as bad
.cpsBadExit:
    stc
    mov al, errPnf  ;Set path not found error in al if CF=CY
    return