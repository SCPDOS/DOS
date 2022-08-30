commandStart:
    ;Resize Allocation, jump here with endpointer in rbx
    ;Ideally would have this jettisoned too but cannot guarantee
    ; that the jump to safety won't be gobbled up when multitasking
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [rbx + r8 + 11h]    ;Get # of bytes for COMMAND.COM
    shr ebx, 4  ;Convert to paragraphs
    mov ah, 4Ah ;Realloc
    neg r8  ;Convert -r8 to r8
    int 41h
    jmp short commandMain
applicationReturn:  ;Return point from a task, all regs preserved
    mov eax, 4D00h ;Get Return Code
    int 41h
    mov word [returnCode], ax
;Reset our PSP vectors (and IVT copies) in the event they got mangled
    lea rdx, critErrorHandler
    mov qword [r8 + psp.oldInt44h], rdx
    mov eax, 2544h
    int 41h
    lea rdx, int43h
    mov qword [r8 + psp.oldInt43h], rdx
    mov eax, 2543h
    int 41h
    lea rdx, applicationReturn
    mov qword [r8 + psp.oldInt42h], rdx
    mov eax, 2542h
    int 41h
;Close all handles from 5->MAX
    movzx ecx, word [numHdls]
    mov ebx, 5
.handleClose:
    mov ah, 3Eh ;File close
    int 41h
    inc ebx ;Goto next file
    cmp ebx, ecx
    jbe .handleClose    ;Keep looping whilst below or equal
commandMain:
;Setup Commandline
    cli
    mov rsp, qword [stackBottom]    ;Reset internal stack pointer pos
    sti
    cld ;Ensure stringops are done the right way
    mov byte [inBuffer], 80h    ;Reset the buffer length
.inputMain:
    call clearCommandLineState
    call printCRLF
    call printPrompt

    lea rdx, inBuffer
    mov eax, 0C0Ah  ;Do Buffered input
    int 41h
    call printCRLF  ;Note we have accepted input

;First check we had something typed in of length greater than 1
;Must be greater than 0 as executable commands must have extension and filename
    cmp byte [inBuffer + 1], 1  ;Check input length valid
    jbe .dfltErrExit
    ;Copy over the input text
    lea rsi, inBuffer
    lea rdi, cmdBuffer
    mov ecx, cmdBufferL   ;Straight up copy the buffer over
    rep movsb
    xor eax, eax
    mov word [cmdStartOff], ax  ;Clear start and end Off positions
    call parseInput
    call doCommandLine
    jmp short .inputMain
.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 41h
    jmp short .inputMain

parseInput:
;EndOff is set up before entering this part
;Copies a nicely formatted version of the input command line
; without any redirections to psp.dta
    lea rsi, qword [cmdBuffer + 2]  ;Goto the command buffer
    lea rdi, qword [r8 + cmdLine]   ;Go to the command line in the psp
    movzx ebx, byte [cmdEndOff] ;Get the old end offset
    add rsi, rbx    ;Move rsi to the start of this new command
    call skipSpaces ;Skip any preceeding spaces
    lodsw   ;Get the first two chars into ax
    mov word [cmdDrvSpec], ax ;Store these chars as if they are the drvspec
    sub rsi, 2  ;Go back to the start of the command
    push rsi
    push rdi
    lea rdi, cmdPathSpec    ;We copy the command name/path here
    push rdi
    call copyCommandTailItem
    pop rdi
    pushfq  ;Save the CF state
    call strlen
    dec ecx ;Drop the terminating char from the count
    pop rax ;Get the CF state in al
    pop rdi
    pop rsi
    rep movsb   ;Now we copy the command into the psp command line
    test al, 1  ;Was CF set?
    jnz .exit   ;If an embedded CR was found in the filename, exit!
.cmdLineProcess:
    call skipSpaces ;Go to the next char in the input line
.redirFound:
    lodsb   ;Get first non-space char (setupRedir skips spaces before ret)
    cmp al, CR  ;If this was a CR, we stop processing
    je .exit
    call checkAndSetupRedir ;If not, check if we have a redir element
    jc .exit    ;CF=CY only if pipe, which is equivalent to CR when processing
    jz .redirFound  ;If we had a < > or >>, proceed to check if next char CR
    ;Else we process the first two switches and copy any arguments
    mov al, " "
    stosb   ;Store a space to make space for the command file parameter
    dec rsi ;Move rsi back to the first char
    test byte [arg1Flg], -1
    jnz .arg2
.arg1:
    mov byte [arg1Flg], -1
    call skipSpaces
    mov rax, rsi
    lea rbx, cmdBuffer
    sub rax, rbx
    mov byte [arg1Off], al  ;Store the offset 
    jmp short .argCommon
.arg2:
    test byte [arg2Flg], -1
    jnz .argCommon
    mov byte [arg2Flg], -1
    call skipSpaces
    mov rax, rsi
    lea rbx, cmdBuffer
    sub rax, rbx
    mov byte [arg2Off], al  ;Store the offset 
    jmp short .argCommon
.argCommon:
    ;More than two arguments, we do nothing more than just copy the command
    ; over. If we encounter an embedded CR, exit there too
    call skipSpaces
    cmp byte [rsi], CR  ;Are we at the end of the commandline?
    je .exit
    ;If not, we copy it over
    call copyCommandTailItem    ;Stores a terminating null we dont want
    lea rdi, qword [rdi - 1]    ;Point back at the inserted terminating null
    jnc .cmdLineProcess
.exit:
    lea rbx, cmdBuffer
    dec rsi
    sub rsi, rbx    ;Get the offset into the command line
    mov ebx, esi
    mov byte [cmdEndOff], bl    ;Store the offset to the terminating char
    mov al, CR
    stosb   ;Store the terminating CR in the psp command line
    ;Now compute the command line length 
    lea rdi, qword [r8 + cmdLine] 
    mov al, CR
    xor ecx, ecx    ;ONLY USE ECX!!!
    dec ecx ;rcx = -1
    repne scasb
    not ecx
    dec cl  ;Dont include terminating CR
    lea rdi, qword [r8 + cmdLineCnt]
    mov byte [rdi], cl
    ;Before returning, we copy the command name to cmdName
    lea rdi, cmdPathSpec
    mov rbx, rdi    ;Use rbx as the ptr to the first char in the commandspec
    xor al, al  ;Search for the terminating null
    mov ecx, fileSpecZL ;Max number of chars the length could be
    repne scasb
    dec rdi ;Go to the last char in the command
    mov rsi, rdi
    std ;Now we go backwards to where rsi = rbx OR byte [rsi] = pathSep
.keepSearching:
    lodsb
    cmp al, byte [pathSep]
    je .cmdStartFnd
    cmp rsi, rbx
    jne .keepSearching
    dec rsi ;Go back two to go forwards again
    dec rsi
.cmdStartFnd:
    inc rsi
    inc rsi ;Go past the pathsep
    cld ;Go the sane way again
    xor ecx, ecx
    lea rdi, qword [cmdName + 1]    ;First byte is for the length of the name
    push rsi    ;Save the location of the start byte of the command name
.cmdGetChar:
    lodsb
    test al, al ;Did we find the terminating null?
    jz .nameLenFnd
    cmp al, "." ;Extension sep also terminates
    je .nameLenFnd
    and al, 0DFh    ;Else uppercase the char
    stosb   ;and store it
    inc ecx
    cmp ecx, 11 ;Max command length is 11
    jb .cmdGetChar
.nameLenFnd:
    mov byte [cmdName], cl  ;Store the name length now
    ;Now finally, create a FCB filespec
    lea rdi, fcbCmdSpec
    push rdi
    mov ecx, fcbNameL
    mov al, " " ;Fill with spaces
    rep stosb
    pop rdi
    pop rsi ;Get back the location of the start byte of the command name
    call asciiToFCB
    lea rsi, fcbCmdSpec
    lea rdi, cmdSpec
    call FCBToAsciiz
    return

doCommandLine:
    lea rsi, qword [cmdBuffer + 2]  ;Goto the command buffer
    lea rdi, cmdFcb
    mov eax, 2901h  ;Skip leading blanks
    int 41h
    movzx ebx, word [cmdDrvSpec]    ;Get the drive specifier
    cmp bh, ":"
    jne .noDriveSpecified
    mov dl, bl      ;Move the drive letter in dl
    and dl, 0DFh    ;Make the drive letter upper case
    sub dl, "A"     ;And make it a 0 based drive letter
    cmp al, -1  ;Int 41h returns AL = -1 if bad drive specified
    je .badDrive
    ;If drive specified and cmdName length = 2 => X: type command
    cmp byte [cmdName], 2
    jne .noDriveSpecified   ;Drive specified but proceed as normal
    mov ah, 0Eh ;Set drive to dl
    int 41h 
    mov ah, 19h
    int 41h     ;Get current drive
    cmp al, dl  ;If the drive was set, all is well
    rete
.badDrive:
    lea rdx, badDrv
    mov ah, 09h
    int 41h
    stc
    return
.noDriveSpecified:
;Now we set the two FCB's in the command line
    test byte [arg1Flg], -1
    jz .fcbArgsDone
    movzx eax, byte [arg1Off]   ;Get the first argument offset
    lea rsi, cmdBuffer
    add rsi, rax    ;Point to first argument
    lea rdi, qword [r8 + fcb1]
    mov eax, 2901h
    int 41h
    mov byte [arg1FCBret], al
    test byte [arg2Flg], -1
    jz .fcbArgsDone
    movzx eax, byte [arg2Off]
    lea rsi, cmdBuffer
    add rsi, rax    ;Point to first argument
    lea rdi, qword [r8 + fcb2]
    mov eax, 2901h
    int 41h
    mov byte [arg2FCBret], al
.fcbArgsDone:
    lea rsi, cmdBuffer
    lea rbx, cmdName
    mov eax, 0AE00h ;Installable command check
    mov edx, 0FFFFh
    mov ch, -1
    int 4Fh ;Return: al = -1 if this command a extension to COMMAND.COM
            ;        al = 0  if the command should be executed as usual
    mov eax, 0AE00h ;Installable command check
    mov edx, 0FFFFh
    xor ch, ch  ;Second call uses ch = 0
    int 4Fh
    jz .executeInternal
    ;Here we execute externally and return to the prompt
    ; as if it was an internal execution
    lea rsi, inBuffer ;Point to this built buffer
    lea rbx, cmdFcb
    mov eax, 0AE01h ;Execute command!
    mov edx, 0FFFFh
    mov ch, -1
    int 4Fh 
    return
.executeInternal:
;Now we compare the name in the cmdFcb field to our commmand list
;rsi points after the command terminator in the command tail
    lea rbx, functionTable
.nextEntry:
    movzx ecx, byte [rbx]   ;Get name entry length
    cmp cl, -1  ;Are we at the end of the table?
    je .external      ;If so, check externally now
    cmp byte [cmdName], cl  ;Is command length the same as the tbl entry length?
    jnz .gotoNextEntry  ;If not, goto next entry
    ;Here they have the same length so lets see if the name is the same
    push rsi
    ;ecx has the length to compare
    push rcx
    lea rsi, qword [rbx + 1]
    lea rdi, qword [cmdName + 1]   ;Go to the name portion
    rep cmpsb   ;Check the strings are equal
    pop rcx
    pop rsi
    jne .gotoNextEntry
    ;Here it was found both strings are equal
    lea rdi, qword [rbx + rcx + 1]  ;make rdi point to offset from startLbl
    movzx rbx, word [rdi]
    lea rdi, startLbl
    add rbx, rdi
    call rbx    ;Call this function...
    return  ;... and return
.gotoNextEntry:
    add rbx, 3      ;Go past the first count byte and the address word
    add rbx, rcx    ;Go past the length of the command name too
    jmp short .nextEntry

.external:
;Here we must search the CWD or all path componants before failing
;Also this command must be a .COM, .EXE or .BAT so check that first
    jmp .dfltErrExit    ;Catch all for now
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
    
    jmp .dfltErrExit
.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 41h
    return
.noExt:
    ;Here we must search for the first file with a valid extension.
    ;Use bl as flags. bl[0] => COM found, bl[1] => EXE found, bl[2] => BAT found
    xor ebx, ebx
    ;If relative path, search CWD. If absolute path, search absolute path.
    ;If nothing, only then loop through each dir in the path for provided
    ; pathspec (relative case), or filename (absolute case)



checkExtensionExec:
;Checks the extension field of cmdFcb is .COM, .EXE, .BAT in that order
;Returns: ZF=ZE if executable. ZF=NZ if not executable.
;         If ZF=ZE and CF=CY => Batch file
    mov eax, dword [cmdFcb + fcb.fileext]   ;Get a dword, with dummy byte 3
    and eax, 00FFFFFFh  ;Clear byte three
    or eax,  20000000h  ;Add a space so it is like "COM "
    cmp eax, "COM "
    rete
    cmp eax, "EXE "
    rete
    cmp eax, "BAT "
    retne
    stc
    return

checkAndSetupRedir:
;Checks and sets up redir as appropriate
;Input: al = First char to check, if al < > >> or |, handled appropriately
;       rsi points to the first char after the char in al in cmdBuffer
;Output: ZF=NZ => No redir
;        ZF=ZY => Redir
;           rsi is moved to the first non-terminating char after redir filespec
;CF=CY if pipe set or an embedded CR found
    push rdi
    cmp al, "<"
    je .inputRedir
    cmp al, ">"
    je .outputRedir
    cmp al, "|"
    je .pipeSetup
    clc
.redirExit:
    pop rdi
    return
.inputRedir:
    mov byte [redirIn], -1  ;Set the redir in flag
    lea rdi, rdrInFilespec
    call skipSpaces ;Skip spaces between < and the filespec
    call copyCommandTailItem
    jc .redirExit
    call skipSpaces
    xor al, al
    jmp short .redirExit
.outputRedir:
    mov byte [redirOut], 1
    cmp byte [rsi], ">" ;Was this a > or a >>
    jne .notDouble
    inc byte [redirOut] ;Inc to make it 2
.notDouble:
    lea rdi, rdrOutFilespec
    call skipSpaces
    call copyCommandTailItem
    jc .redirExit
    call skipSpaces
    xor al, al
    jmp short .redirExit
.pipeSetup:
    mov byte [pipeFlag], -1
    xor al, al
    stc
    pop rdi
    return

copyCommandTailItem:
;Copies a sentence from the command tail until a terminator is found.
;Stores a terminating null in the destination
;Input: rsi = Start of the item to copy
;       rdi = Location for copy
;Output: Sentence copied with a null terminator inserted.
; If CF=CY, embedded CR encountered
    lodsb
    cmp al, CR
    je .endOfInput
    call isALterminator
    jz .exit
    stosb
    jmp short copyCommandTailItem
.endOfInput:
    call .exit
    stc 
    return
.exit:
    xor al, al
    stosb
    return


int4Eh:   ;Interrupt interface for parsing and executing command lines
;Input: rsi points to the count byte of a command line
    push r8
    push r9
    mov ah, 51h ;Get Current PSP in rdx
    int 41h
    push rdx    ;Save on the stack
    lea rbx, qword [startLbl - psp_size]    ;Get a psp ptr for this COMMAND.COM
    mov ah, 50h ;Set this version of COMMAND.COM as the current PSP
    int 41h
    mov r8, rbx ;Set to point to the command.com psp
    mov r9, rbx
    lea rdi, qword [r8 + cmdLine]
    mov ecx, 10h    ;7Fh chars + 1 count byte / 8
    rep movsq   ;Copy command line over
    ;call doCommandLine
    pop rbx ;Get Old current PSP in rbx
    mov ah, 50h ;Set Current PSP
    int 41h
    pop r9
    pop r8
    iretq