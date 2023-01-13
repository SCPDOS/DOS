commandStart:
    ;Resize Allocation, jump here with endpointer in rbx
    ;Ideally would have this jettisoned too but cannot guarantee
    ; that the jump to safety won't be gobbled up when multitasking
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [rbx + r8]    ;Get # of bytes for COMMAND.COM and stack
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
    test byte [pipeNumber], -1
    jnz commandMain.pipeProceed ;Skip the handle closing when pipe active
    call cleanUpRedir   ;Clean up redirection once we are done
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
    mov rsp, qword [stackTop]    ;Reset internal stack pointer pos
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
    je .dfltErrExit
    jb .inputMain
    ;Copy over the input text
    lea rsi, inBuffer
    lea rdi, cmdBuffer
    mov ecx, cmdBufferL   ;Straight up copy the buffer over
    rep movsb
    xor eax, eax
    mov word [cmdStartOff], ax  ;Clear start and end Off positions
.pipeLoop:
    call parseInput
    call doCommandLine
.pipeProceed:
    call cleanUpRedir
    test byte [pipeNumber], -1  ;If we have any pipes active, we proceed here
    jz .inputMain
    call clearCommandState  ;Else, clear the command state and start again
    jmp short .pipeLoop
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
    call copyCommandTailItemProgram
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
    ;More than two arguments? Do nothing more than just copy it
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
    inc byte [cmdEndOff] ;Goto  first char past terminating char for next bit
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
    lea rdi, qword [cmdName + 1]    ;First byte is for the length of the name
    push rdi    ;Cleanse the field before usage (not strictly necessary)
    mov ecx, cmdNameL
    xor al, al
    rep stosb
    pop rdi
    xor ecx, ecx
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
    je launchChild      ;If so, check externally now
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
.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 41h
    return


checkExtensionExec:
;Checks the extension field of cmdFcb is .COM, .EXE, .BAT in that order
;Returns: ZF=ZE if executable. ZF=NZ if not executable.
;         If ZF=ZE and CF=CY => Batch file
    mov eax, dword [cmdFcb + fcb.fileext]   ;Get a dword, with dummy byte 3
    and eax, 00FFFFFFh  ;Clear byte three
    or eax,  20000000h  ;Add a space so it is like "COM "
    and eax, 0FFDFDFDFh ;Uppercase the three letters
    cmp eax, "COM "
    rete
    cmp eax, "EXE "
    rete
    cmp eax, "BAT "
    retne
    stc
    return

redirFailure:
    lea rdx, redirErrMsg
    mov ecx, redirErrMsgL
    jmp short redirPipeFailureCommon
pipeFailure:
    lea rdx, pipeErrMsg
    mov ecx, pipeErrMsgL
redirPipeFailureCommon:
;This routine is called if any problems happen during 
;This routine tries to close whatever handles are not -1 and delete
; pipe files if the pipe count is not 0
;It resets all variables and proceeds.
    mov eax, 4000h  ;Write handle
    mov ebx, 2  ;Write to STDERR
    int 41h
    xor ebx, ebx    ;Select STDIN
    call .closeHandle
    inc ebx         ;Select STDOUT
    call .closeHandle
    mov eax, 3D02h  ;Open read/write
    lea rdx, conName
    int 41h
    mov ebx, eax    ;Move file handle to ebx
    mov eax, 4500h  ;DUP
    int 41h
    mov word [redirIn], 0  ;Clear both flags
    movzx ebx, word [redirSTDIN]
    call .closeHandle
    ;Close and zero both STDIN and STDOUT handle vars
    mov word [redirSTDIN], -1
    movzx ebx, word [redirSTDOUT]
    call .closeHandle
    mov word [redirSTDOUT], -1
    movzx ebx, word [pipeSTDIN]
    call .closeHandle
    mov word [pipeSTDIN], -1
    movzx ebx, word [pipeSTDOUT]
    call .closeHandle
    mov word [pipeSTDOUT], -1
    mov word [newPipeFlag], 0  ;Cover the pipe number too
    lea rdx, qword [pipe1Filespec]
    cmp byte [rdx], 0
    jz .checkOld
    mov eax, 4100h  ;Del File pointed to by rdx
    int 41h
.checkOld:
    lea rdx, qword [pipe2Filespec]
    cmp byte [rdx],0
    jz .pipeNamesComplete
    mov eax, 4100h  ;Del File pointed to by dl
    int 41h
.pipeNamesComplete:
    xor eax, eax
    ;Invalidate the pointers and the paths too
    mov qword [newPipe], rax
    mov qword [oldPipe], rax
    mov dword [pipe1Filespec], eax
    mov dword [pipe2Filespec], eax
    stc
    return
.closeHandle:
    cmp ebx, -1
    rete
    mov eax, 3E00h
    int 41h
    return

cleanUpRedir:
;Cleans up the redir stuff after we are done.
    test byte [redirIn], -1
    jnz .redirInClear
    test byte [redirOut], -1
    jnz .redirOutClear
    test byte [newPipeFlag], -1 ;New pipe active flag set?
    jnz .newPipe
    test byte [pipeNumber], -1
    retz
;Here for final pipe cleanup
    mov rdx, [oldPipe]
    mov eax, 4100h
    int 41h
    jc pipeFailure
    mov byte [rdx], 0   ;Invalidate the path too
;Now place STDIN handle back where it belongs
    xor ecx, ecx    ;Close STDIN and duplicate ebx in it
    movzx ebx, word [pipeSTDIN]
    mov eax, 4600h
    int 41h
    jc pipeFailure
    mov eax, 3E00h  ;Now close the duplicate in ebx
    int 41h
    jc pipeFailure
    mov word [pipeSTDIN], -1
    mov byte [pipeNumber], 0   ;Make the pipe number 0
    return
.newPipe:
    cmp byte [pipeNumber], 2 ;Do we have two pipes active?
    jne .noClose    ;If not, skip deleting old pipe
    ;Here to delete the old pipe file
    mov rdx, [oldPipe]
    mov eax, 4100h
    int 41h
    jc pipeFailure
    mov byte [rdx], 0   ;Overwrite the first byte of pathname with a zero
    dec byte [pipeNumber]   ;Decrement the number of active pipes
.noClose:
    mov rax, qword [newPipe]   ;Transfer the name pointer
    mov qword [oldPipe], rax
    mov ebx, 1  ;Now move STDOUT to STDIN
    xor ecx, ecx
    mov eax, 4600h
    int 41h
    jc pipeFailure
    mov eax, 3E00h  ;And CLOSE STDOUT as it stands
    int 41h
;Now we reset STDOUT back to what it was initially.
    movzx ebx, word [pipeSTDOUT]
    mov ecx, 1
    mov eax, 4600h
    int 41h
    jc pipeFailure
;And now close the copy
    mov eax, 3E00h
    int 41h
    jc pipeFailure
    mov byte [newPipeFlag], 0  ;Indicate we are done with pipe command
    mov word [pipeSTDOUT], -1
    return

.redirInClear:
    movzx ebx, word [redirSTDIN]    ;Put this file back to STDIN
    xor ecx, ecx    ;Duplicate original STDIN into CX (into STDIN position)
    mov eax, 4600h  ;This closes the redir file in the process
    int 41h
    jc redirFailure
    mov eax, 3E00h  ;Now close BX in the process too remove duplicates.
    int 41h
    jc redirFailure
    mov word [redirSTDIN], -1  ;Replace the file handle with -1
    mov byte [redirIn], 0   ;Clear the flag
    return
.redirOutClear:
    movzx ebx, word [redirSTDOUT]    ;Put this file back to STDOUT
    mov ecx, 1    ;Duplicate original STDOUT into CX (into STDOUT position)
    mov eax, 4600h  ;This closes the redir file in the process
    int 41h
    jc redirFailure
    mov eax, 3E00h  ;Now close BX in the process too remove duplicates.
    int 41h
    jc redirFailure
    mov word [redirSTDOUT], -1  ;Replace the file handle with -1
    mov byte [redirOut], 0   ;Clear the flag
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
    ;jc .redirExit
    dec rsi ;Ensure rsi points to the terminating char
    call skipSpaces
    ;Setup the redir here for STDIN
    xor ebx, ebx    ;DUP STDIN
    mov eax, 4500h
    int 41h
    jc .redirError
    mov word [redirSTDIN], ax   ;Save the handle in variable
    lea rdx, rdrInFilespec
    mov eax, 3D02h  ;Open file for read write access
    int 41h
    jc .redirError
    xor ecx, ecx    ;Close STDIN and duplicate bx into it
    movzx ebx, ax   ;Move the handle into bx to duplicate into cx (STDIN)
    mov eax, 4600h
    int 41h
    jc .redirError
    mov eax, 3E00h  ;Now close the original copy of the handle
    int 41h
    jc .redirError
    xor al, al
    jmp short .redirExit
.outputRedir:
    mov byte [redirOut], 1
    cmp byte [rsi], ">" ;Was this a > or a >>
    jne .notDouble
    inc byte [redirOut] ;Inc to make it 2
    inc rsi ;Go past it too
.notDouble:
    lea rdi, rdrOutFilespec
    call skipSpaces
    call copyCommandTailItem
    ;jc .redirExit
    dec rsi ;Ensure rsi points to the terminating char
    call skipSpaces
    ;Setup the redir here for STDOUT
    mov ebx, 1    ;DUP STDOUT
    mov eax, 4500h
    int 41h
    jc .redirError
    mov word [redirSTDOUT], ax   ;Save the handle in variable
    lea rdx, rdrOutFilespec
    mov eax, 3D02h  ;Open file for read write access
    int 41h
    jnc .fileExists
    mov eax, 3C00h
    mov ecx, 0  ;Make the file with no attributes
    int 41h
    jc .redirError
.fileExists:
    mov ecx, 1    ;Close STDOUT and duplicate bx into it
    movzx ebx, ax   ;AX has the new handle for output
    mov eax, 4600h  ;DUP2
    int 41h
    jc .redirError
    mov eax, 3E00h  ;Now close the original copy of the handle (in bx)
    int 41h
    jc .redirError
    cmp byte [redirOut], 1
    je .dontAppend
    ;Here we move the file pointer to the end of the file
    xor edx, edx    ;Low order 32 bits
    xor ecx, ecx    ;High order 32 bits
    mov ebx, 1  ;We seek STDOUT to the end
    mov eax, 4202h  ;Seek from end of file
    int 41h
    jc .redirError
.dontAppend:
    mov byte [redirOut], -1
    xor al, al
    jmp .redirExit
.pipeSetup:
    lea rdx, pipe1Filespec
    cmp byte [rdx], 0
    jz .pathFound
    lea rdx, pipe2Filespec
    cmp byte [rdx], 0
    jnz .pipeError
.pathFound:
    mov qword [newPipe], rdx    ;Use this as the newPipe path
    call getCurrentDrive    ;Get current drive in al (0 based number)
    add al, "A"
    mov ebx, 005C3A00h  ;0,"\:",0
    mov bl, al  ;Move the drive letter into low byte of ebx
    mov ecx, dirHidden  ;Hidden attributes
    mov eax, 5A00h  ;Create a temporary file
    int 41h
    jc .pipeError
    ;AX has the handle for this file now, this will become STDOUT
    ;If this is the first pipe, we want to save a copy of this handle
    test byte [pipeNumber], -1
    jnz .notFirstPipe
    movzx edx, ax    ;Save this handle for a minute in dx
    ;Now DUP STDOUT to save for later
    mov eax, 4500h
    mov ebx, 1  ;Duplicate STDOUT
    int 41h
    jc .pipeError
    ;Save this handle in the variable
    mov word [pipeSTDOUT], ax   ;Save this pipe number
    mov eax, edx    ;Get the temp file handle back in eax
.notFirstPipe:
    movzx ebx, ax
    mov ecx, 1  ;Close STDOUT and move bx into it
    mov eax, 4600h  ;DUPlicate temp file handle into STDOUT
    int 41h
    jc .pipeError
    mov byte [newPipeFlag], -1  ;Mark we have a new pipe active
    inc byte [pipeNumber]   ;Start a new pipe
    xor al, al
    stc
    pop rdi
    return
.pipeError:
    pop rdi 
    jmp pipeFailure
.redirError:
    pop rdi 
    jmp redirFailure

copyCommandTailItemProgram:
;Copies a program name from the command tail until a terminator is found.
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
    cmp al, byte [pathSep]
    je .exit
    cmp al, byte [switchChar]
    je .exit
    stosb
    jmp short copyCommandTailItemProgram
.endOfInput:
    call .exit
    stc 
    return
.exit:
    xor al, al
    stosb
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
    cmp al, "<"
    jz .exit
    cmp al, ">"
    jz .exit
    cmp al, byte [pathSep]
    je .pathSep
    cmp al, byte [switchChar]
    je .exit
    stosb
    jmp short copyCommandTailItem
.pathSep:
;We look ahead, if the last char is a pathsep, we ignore it
    lodsb   ;Get the next char, increment rsi by one
    call isALterminator
    jz .exit
    cmp al, CR
    je .endOfInput
    cmp al, byte [switchChar]
    je .exit
    mov al, byte [pathSep]
    stosb   ;Else store the pathsep
    dec rsi ;Move rsi back a piece
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