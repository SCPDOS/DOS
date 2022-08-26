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
    mov byte [r8 + cmdBuffer], 80h  ;Reset the processed buffer length
.inputMain:
    call printCRLF
    call printPrompt

    lea rdx, inBuffer
    mov eax, 0C0Ah  ;Do Buffered input
    int 41h
    call printCRLF  ;Note we have accepted input

    call parseInput
    jc .inputMain   ;If error in parsing/invalid data in cmd line reset
    call doCommandLine
    jmp short .inputMain

printPrompt:
    cmp word [promptPtr], -1
    jne .validPrompt
    ;Here we print the default prompt
    call putCWDInPrompt
    call putGTinPrompt
    return
.validPrompt:
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
parseInput:
;Will parse the input string 
    return
doCommandLine:    ;And this is how we enter it normally
;r8 must be pointing to COMMAND.COM's internal PSP
    lea rsi, qword [inBuffer + 1]   ;Point to count byte
    cmp byte [rsi], 0   ;If the string has length 0, empty string
    rete
    inc rsi ;Goto first char in string
    lea rdi, cmdFcb   ;Parse into fcb
    mov eax, 2901h ;Parse FCB and skip leading spaces
    int 41h
    cmp al, 1   ;Command cannot have wildcards in the name
    je .dfltErrExit
    cmp byte [rsi], CR  ;Are we at a carriage return, i.e. end of string?
    je .endOneField     ;After one iteration, if we reached CR, now process
    ;If the terminator is pathsep, build a pathstring
    mov al, byte [pathSep]
    cmp byte [rsi], al
    jne .commandCase
    ;Path resolving here
    ;Go to the end of the path to see if we have a .COM, .EXE or .BAT
.resolve:
    inc rsi ;Go past the terminator
    mov eax, 2901h ;Parse FCB and skip leading spaces
    int 41h
    mov al, byte [pathSep]
    cmp byte [rsi], al  ;Keep going if pathsep
    je .resolve
    ;We stop looping if we are at the end of the path.
    jmp .external
.dfltErrExit:
    lea rdx, badCmd
    mov ah, 09h
    int 41h
    return
.endOneField:
;rsi points to the CR
    cmp byte [rdi + fcb.filename], " "  ;Filename cannot begin with space
    jne .commandCase
    ;Here we have an empty filename, but a drive letter may have been specified
    cmp al, -1
    je .badDrive
    mov dl, byte [rdi + fcb.driveNum]  ;1 based drive letter
    dec dl  ;Convert to 0 based drive letter
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
    return
.commandCase:
;Here we check if the word was an installed or internal command before 
; attempting to launching it as an external command
    ;breakpoint
    push rsi    ;Save rsi's current position in the command tail on stack
    ;Move the name from the FCB to the buffer
    xor ecx, ecx
    lea rbx, cmdFcb ;Find char count, scan for the first space
    lea rdi, qword [rbx + 1]    ;Skip byte 0 for scan
    mov al, " " ;Scan for a space
    xor ecx, ecx
    dec ecx ;Make ecx = -1 in three bytes
    repne scasb
    not ecx ;Now ecx has the count of chars including the space
    dec ecx ;so remove the terminating space from the count
    mov byte [rbx], cl  ;Store the byte count here
    lea rsi, inBuffer ;Point to our internal buffer
    mov eax, 0AE00h ;Installable command check
    mov edx, 0FFFFh
    mov ch, -1
    int 4Fh ;Return: al = -1 if this command a extension to COMMAND.COM
            ;        al = 0  if the command should be executed as usual
    mov eax, 0AE00h ;Installable command check
    mov edx, 0FFFFh
    xor ch, ch  ;Second call uses ch = 0
    int 4Fh
    pop rsi ;Get back rsi's position in command tail (after command name)
    test al, al
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
    cmp byte [cmdFcb], cl  ;Is command length the same as the tbl entry length?
    jnz .gotoNextEntry  ;If not, goto next entry
    ;Here they have the same length so lets see if the name is the same
    push rsi
    ;ecx has the length to compare
    push rcx
    lea rsi, qword [rbx + 1]
    lea rdi, qword [cmdFcb + 1]   ;Go to the name portion
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
    call checkExtensionExec ;ZF=ZE => Executable
    jnz .dfltErrExit
    ;!!!!!!!!!!!TEMPORARY MEASURE TO AVOID LAUNCHING BAT FILES!!!!!!!!!!!
    jc .dfltErrExit ;Remove this when ready to launch batch files
    ;!!!!!!!!!!!TEMPORARY MEASURE TO AVOID LAUNCHING BAT FILES!!!!!!!!!!!
    ;So it is a com or exe that we are searching for
    jmp .dfltErrExit


skipSpaces:
;Input: rsi must point to the start of the data string
;Output: rsi points to the first non-space char
    cmp byte [rsi], " "
    retne
    inc rsi
    jmp short skipSpaces

checkExtensionExec:
;Checks the extension field of cmdFcb is .COM, .EXE, .BAT in that order
;Returns: ZF=ZE if executable. ZF=NZ if not executable.
;         If ZF=ZE and CF=CY => Batch file
    cmp byte [cmdFcb + fcb.fileext], "C"
    jne .notCOM
    cmp word [cmdFcb + fcb.fileext + 1], "OM"
    return
.notCOM:
    cmp byte [cmdFcb + fcb.fileext], "E"
    jne .batFile
    cmp word [cmdFcb + fcb.fileext + 1], "XE"
    return
.batFile:
    cmp byte [cmdFcb + fcb.fileext], "B"
    retne
    cmp word [cmdFcb + fcb.fileext + 1], "AT"
    retne
    stc
    return

resetCommandLineState:
;Clears the vars that track the state of a command line
;Called every time we enter command line processor for a NEW command line
;Not called when returning from a pipe.
; In that case only clear redir vars when returning from executing command
    push rax
    push rcx
    push rdi
    lea rdi, cmdLineState
    mov ecx, cmdLineStateL
    xor eax, eax
    rep stosb
    pop rax
    return


