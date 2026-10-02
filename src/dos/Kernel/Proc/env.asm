

;Environment and process information functions go here :)
;
;An environment (ENV) is a list of strings of the form:
; var1 = <value>,<NUL>
;      .
;      .
;      .
; varN = <value>,<NUL>
; <NUL>
;
;Argument block (ARGS) is a list of strings of the form:
; wArgc (argc)
; <string1>,<NUL> (argv[0])
;       .
;       .
;       .
; <stringN>,<NUL> (argv[N-1])
; <NUL>
;
; where wArgc has a maximum value of (ENV_MAX-2)/2
; Subtract 2 for the mandatory argc count and divide by 2 as the minimum
; non-empty string length is 2.
;An empty string in ARGS is given by the string <NUL>.
;The end of the ARGS block is denoted by a <NUL>.
;Both ENV and ARGS have a maximum size defined by ENV_MAX.
;
;Together, an environment list and an argv list forms the 
; full environment (FENV) of an application. 
;Each application has a field in the PSP called the envPtr. This 
; contains a pointer to a special memory block called the full 
; environment memory block. The FENV memory block contains both the 
; ENV and the ARGS for the application. Note, this pointer can point 
; to an ENV without a following ARGS. This will only happen if someone 
; attempts to manually twiddle with the environment pointer in the PSP. 
;In such cases, all documented functions will fail. The only caveat is if
; the pointer is the null pointer, in which case, function calls to 
; GetEnvironmentStrings will always return an empty opaque environment.

systemServices: ;ah = 61h, this is so named as it forms the core of
;   the system services endpoint.
;Available subfunctions:
;-------------------------------- UNDOCUMENTED --------------------------------
;al = 0 -> Get Environment pointer in rdx                                     :
;   Output: rdx -> Environment Pointer. May be a null pointer. Caller checks! :
;al = 1 -> Get Command Line Arguments Pointer in rdx                          :
;   Output: rdx -> Pointer to whatever was passed as a CR terminated          :
;                   command line.                                             :
;al = 2 -> Get ptr to ASCIIZ name for program in rdx. Might not be FQ.        :
;   Output: CF=NC: rdx -> Filename                                            :
;           CF=CY: eax = Error code (errAccDen) if no filename ptr.           :
;                   The error case should only happen for programms           :
;                   which have twiddled with their own environments.          :
;                               DON'T DO THAT!!!                              :
;-------------------------------- UNDOCUMENTED --------------------------------
;al = 3 -> GetEnvironmentStrings, gets an opaque copy of the environment
;al = 4 -> GetEnvironmentVariable, gets this var's value from the environment
;al = 5 -> SetEnvironmentStrings, copies these strings into a new env, if valid
;al = 6 -> SetEnvironmentVariable, sets this var value in the environment
;al = 7 -> FreeEnvironmentStrings, frees a Get Env Strings block
;al = 8 -> ExpandEnvironmentStrings, expands a string which has a string
;           of the form %var% in it, where var is an environment variable.
;al = 9 -> GetCommandLine, get lpcsz to command line.
;al = 0Ah -> GetLaunchParameters, copy the PSP from cmdLineArgPtr down.
;               However, .fcb1 and .fcb2 are reserved!!!!
;
;al > 0Bh: Returns CF=CY and eax = Error code (errInvFnc).
;Subfunctions 0-2 remain undocumented for use!

    mov r8, qword [currentPSP]  ;Use r8 as the PSP pointer
    cmp al, 01h ;Get RAW CmdLine arguments
    je .getPSPCmdLineArgs
    cmp al, 02h ;Get RAW env pointer (al=0 and al=2)
    jbe .getPSPEnvPtr
;Functions below this are documented for users.
    cmp al, 03h ;GetEnvironmentStrings
    je .getEnvStrings
    cmp al, 04h ;GetEnvironmentVariable
    je .getEnvVar
    cmp al, 05h ;SetEnvironmentStrings
    je .setEnvStrings
    cmp al, 06h ;SetEnvironmentVariable
    je .setEnvVar
    cmp al, 07h ;FreeEnvironmentStrings
    je .freeEnvStrings
    cmp al, 08h ;ExpandEnvironmentStrings
    je .expEnvVar
    cmp al, 09h ;GetCommandLine
    je .getCmdLine
    cmp al, 0Ah ;GetLaunchParameters
    je .getLaunchParams
.exitBadFunc:
    mov byte [errorLocus], eLocUnk  
    mov eax, errInvFnc  ;Error, with invalid function number error
.exitBad:
    jmp extErrExit
.exitBadParam:
    mov eax, errBadParam
    jmp short .exitBad
.exitBadEnv:
    mov eax, errBadEnv
    jmp short .exitBad
.exitAccDen:
    mov eax, errAccDen  ;Set error code here
    jmp short .exitBad  ;Return setting CF=CY and errAccDen (no pointer)
;------------------
; Undoc functions
;------------------
.getPSPCmdLineArgs:
    lea rdx, qword [r8 + psp.cmdLineArgPtr]   ;Get the cmdArgs pointer
    jmp short .exitOk
.getPSPEnvPtr:
;Gets the environment pointer in rdx
    mov rdx, qword [r8 + psp.envPtr]   ;Get the environment pointer
    test al, al     ;Was al=0?
    jz .exitOk   ;Exit if al = 0 since we have the pointer we need!
    test rdx, rdx   ;Check if the env pointer is ok to use
    jz .exitAccDen
    cmp rdx, -1
    je .exitAccDen
;Here we search for the double 00 and then check if it is 0001 and
; pass the ptr to the word after.
    push rcx
    xor ecx, ecx
    mov ecx, ENV_MAX  ;Max environment size
.gep0:
    cmp word [rdx], 0   ;Zero word?
    je short .gep1
    inc rdx         ;Go to the next byte
    dec ecx
    jnz short .gep0
.gep00:
;Failure here if we haven't hit the double null by the end of 32Kb
    pop rcx
    jmp short .exitAccDen
.gep1:
    add rdx, 2          ;Skip the double null
    cmp word [rdx], 1   ;Check the count. Must be 1 for argv[0].
    jb .gep00
    add rdx, 2          ;Skip the argc count.
    pop rcx
.exitOk:
    call getUserRegs
    mov qword [rsi + callerFrame.rdx], rdx
    jmp extGoodExit

;----------------------
; Get Env String Block
;----------------------
.getEnvStrings:
;Returns a valid, opaque copy of the environment.
;
;Output: CF=NC: rdx -> Environment pointer
;        CF=CY: eax = Error code (errMCBbad, errNoMem)
    call dosCrit1Enter
    mov rsi, qword [r8 + psp.envPtr]    ;Get the raw ptr!
    call checkEnvGood
    jz .gesDefault
;Here rsi -> points to raw environment
    call getSzOfEnv ;Get number of bytes in ecx
    push rcx
    add ecx, 0Fh
    shr ecx, 4      ;Turn into paragraphs to allocate
    call allocEnvSig   ;Get the block ptr in rax (preserve rsi, rdi)
    pop rcx         ;Get the byte count back
    jc .gesExitBad
;rsi -> Raw environment
    mov rdi, rax
    rep movsb
.gesPreExit:
    mov rdx, rax    ;Return the pointer to memory in rdx
    call dosCrit1Exit
    jmp short .exitOk
.gesDefault:
    mov ebx, 6
    call allocEnvSig   ;Get in rax the allocated pointer
    jnc .gesPreExit
.gesExitBad:
    call dosCrit1Exit
    jmp .exitBad
;----------------------
; Set Env String Block
;----------------------
.setEnvStrings:
;Gets a block of environment strings. This will replace the current
; environment. Also copies and maintains the additional strings after 
; the environment, if present. 
;The environment strings DONT have to be from a get strings block 
; that we provide, that would be limiting and absurd.
;
;Input: rdx -> Environment block to set
;Output: CF=NC: Environment updated with new set of strings
;        CF=CY: Error in update (errMCBbad, errMemAddr, errNoMem, errBadFmt)
    call dosCrit1Enter
    mov rsi, rdx
    call checkEnvGood   ;This needs input in rsi and preserves it
    jz .sesExitBadEnv
    call getSzOfEnv ;Get in ecx the length of the env. Preserves rsi
    push rcx        ;Save the new env block size
    mov ebx, ecx    ;Tmp save count in eax
    call getSzOfStrings ;Get old block args size. 
    test ecx, ecx
    jz .sesExitBadEnv  ;If the strings have length 0, there was a booboo. Fail!
    push rcx        ;Save the args size
    add ebx, ecx    ;Sum them for the full size to allocate
    add ebx, 0Fh    ;Round and turn to paras
    shr ebx, 4
    call allocEnv   ;Get the block ptr in rax (preserve rsi, rdi)
    pop rbx         ;Get the args size back
    pop rcx         ;Get the env size back
    jc .gesExitBad
    mov rdi, rax    ;Write to the newly allocated block 
    rep movsb       ;Copy the environment over
;rdi points to the destination for the args copy now
    mov rsi, rdi    ;Save where to write the args here
    mov rdi, qword [r8 + psp.envPtr]
    call getPtrToEndOfEnvBlk    ;Get ptr to the end of the block in rdi
    inc rdi         ;Go past the terminating null
    mov ecx, ebx    ;ebx has the args size
    xchg rsi, rdi   ;Swap em
    rep movsb       ;And copy!
;We swap the environment pointer after freeing so that if the free of the
; old block fails we don't fail and also update/succeed. If we fail here, 
; we will try and free our newly allocated block too and if that fails as 
; well we just stop trying. 
    push rax        ;Save the new block
    push r8         ;Save the psp pointer
    mov r8, qword [r8 + psp.envPtr]     ;Free the old block :)
    call freeMemory ;Free the raw environment block, whoever owns it
    pop r8
    pop rbx         ;Put the new block ptr in rbx to preserve error code
    jc .sesBadFree
    mov qword [r8 + psp.envPtr], rax    ;Finally, set the new env in the psp!
    call dosCrit1Exit
    jmp extGoodExit
.sesBadFree:
    push rax        ;Save the originally returned error code
    mov r8, rbx     ;Try and free the newly allocated block. This should be ok.
    call freeMemory
    pop rax
    jmp .gesExitBad    ;Exit, bubbling the old error code.
.sesExitBadEnv:
    call dosCrit1Exit
    jmp .exitBadEnv
;-----------------------
; Free Env String Block
;-----------------------
.freeEnvStrings:
;This frees an opaque environment copy. 
; Only frees the block if WE allocated it.
;
;Input: rdx -> Envblock to free
;Output: CF=NC: Block freed
;        CF=CY: Block not freed (errMCBbad, errMemAddr, errBadParam)
    cmp byte [rdx - mcb_size + mcb.subSysMark], mcbSubEnv ;Correct type?
    jne .exitBadParam
    cmp qword [rdx - mcb_size + mcb.owner], 0
    je .exitBadParam
    mov r8, rdx     ;Set the register correctly
    jmp freeMemory  ;Exit through freeMemory
;------------------
;   Get Env Var
;------------------
.getEnvVar:
;Returns the value of an environment variable.
;Input: rsi -> [In, optional]  LPCSTR lpName
;       rdi -> [Out, optional] LPSTR lpBuffer
;       ecx =  [In]            DWORD dSize
;Output: Status returned in eax and CF=NC. 
;        If ecx = ecx, then call successful.
;        If ecx < eax, then the buffer needs to be eax in size.
;        If eax = 0, then get last error. Something went wrong!
;
;lpName = The name of the environment variable to search for as a null
;           terminated string
;lpBuffer = A pointer to a buffer that receives the contents of the specified
;            environment variable as a null-terminated string. 
;dSize = The size of the buffer pointed to by the lpBuffer parameter, including 
;         the null-terminating character, in characters.
;
;Return value (eax):
;If the function succeeds, the return value is the number of characters stored 
; in the buffer pointed to by lpBuffer, not including the terminating null 
; character. If lpBuffer is not large enough to hold the data, the return 
; value is the buffer size, in characters, required to hold the string and its
; terminating null character and the contents of lpBuffer are undefined.
;
;If the function fails, the return value is zero. If the specified environment
; variable was not found in the environment block, GetExtendedError returns 
; errEnvVarNotFnd.

;Put the input args on stack-based locals :)
    %push
    %stacksize flat64
    %assign %$localsize 0
    %local lpBuffer:qword, dSize:dword, dLenVal:dword

    call dosCrit1Enter
    enter %$localsize, 0
    mov qword [lpBuffer], rdi
    mov dword [dSize], ecx
    mov dword [dLenVal], 0   ;Length of the value of the envvar
;First check the environment we are working on is good
    call checkPSPEnvGood
    jnz .gevEnvOk
    mov word [errorExCde], errBadEnv
    jmp short .gevExitBad
.gevEnvOk:
;Start by just making sure the input pointer is not bad.
    test rsi, rsi   ;Is this null input string?
    jz .getVarExitNoEnv
    call searchForEnvVar    ;Search for the string in rsi
    jc .getVarExitNoEnv     ;Var doesn't exist!
;Here rsi -> string in the env and ecx has length of the var name and the = 
    add rsi, rcx    ;Go to the value portion of the env var
    call strlen2    ;Get the length of the value string
    dec ecx         ;Drop the final null from the count
    mov dword [dLenVal], ecx    ;This is the len of the value of the envvar
    cmp dword [dSize], ecx      ;Do we have enough space to store the length?
    jb .getVarExit      ;If not, inform the caller how much space is needed
    mov rdi, qword [lpBuffer]   ;Get buffer pointer back
    test rdi, rdi   ;Ensure it is a valid buffer ptr
    jz .getVarExit
    rep movsb       ;Now write the value into the buffer
.getVarExit:
    mov eax, dword [dLenVal]  ;Set this as the copy count
    leave
    call dosCrit1Exit
    jmp extGoodExit2
.getVarExitNoEnv:
;Exit through here if desired environment variable is not found.
;We setup the extended error codes explicitly as we are returning through ok
; to report the buffer size!
    mov word [errorExCde], errEnvVarNotFnd
.gevExitBad:
    call checkFail.skipFail ;Setup the other extended error vars
    jmp short .getVarExit
    %pop
;------------------
;   Set Env Var
;------------------
.setEnvVar:
;Sets an environment variable.
;Input: rsi -> [In]             LPCSTR lpName
;       rdx -> [In, optional]   LPCSTR lpValue
;lpName = The name of the environment variable. DOS creates the 
; environment variable if it does not exist and lpValue is not NULL.
;lpValue = The contents of the environment variable. If the pointer
; is null or the string is empty, we delete the variable.
;
;Output: CF=NC: eax = Number of chars written to environment. 
;               If eax = 0, error, no chars written. Get error code.

    %push
    %stacksize flat64
    %assign %$localsize 0
    %local lpName:qword, lpValue:qword, bAction:byte, dVarLen:dword
;Get the PSP lock. No other threads of this process 
; can enter this critical section.
    call dosCrit1Enter
    enter %$localsize, 0
;First check the default environment is good
    call checkPSPEnvGood
    jnz .sevEnvOk
    mov eax, errBadEnv
    jmp .sevBadExit
.sevEnvOk:
;Start by verifying the input args are good!
    test rsi, rsi
    jnz .sevGo
.sevBadParam:
    mov eax, errBadParam
    jmp .sevBadExit
.sevGo:
    cmp byte [rsi], 0       ;Can't create/delete a null string var!
    je .sevBadParam
;Name cannot contain the equals sign unless it is the first character
    lea rdi, qword [rsi + 1]    ;This char is part of the string
    call strlen
    mov al, "="
    repne scasb
    je .sevBadParam ;If we find an = in the name, fail!
;Now figure out what the user wanted to do with this variable
    xor eax, eax    ;0 means Update
    mov ebx, eax
    inc ebx         ;1 means Delete
    test rdx, rdx   ;Is the pointer null?
    cmovz eax, ebx
    jz .sev1        ;And skip the next check as ptr is invalid!
    cmp byte [rdx], 0   ;Is the string empty?
    cmove eax, ebx
.sev1:
    mov byte [bAction], al  ;Save the action flag
    mov qword [lpName], rsi
    mov qword [lpValue], rdx
;Now we get the full length of the string we're gonna write into the env
    call strlen2    ;Get the length of the name portion
    mov dword [dVarLen], ecx   ;Setup the return value size
    mov rdi, rdx
    call strlen     ;Get the length of the value portion
    add dword [dVarLen], ecx
    call searchForEnvVar    ;Search for var pointed to by rsi
    jc .sevVarNotFnd
;Here, rsi -> Env var that we are to update or delete
    call freeEnvVar ;Start by freeing this variable
    test byte [bAction], -1 ;Now check if we need to recreate this var.
    jnz .sevExitOk  ;Exit if not!
    jmp short .sevCreate    ;Else, lets build it afresh!
.sevVarNotFnd:
;Here the var doesn't exist. If we are to delete, then we return 
; error envvar not found. Else, we create!
    mov eax, errEnvVarNotFnd
    test byte [bAction], -1 ;Couldn't find the var to delete!
    jnz .sevBadExit
.sevCreate:  
    call getFreeSpaceInEnvBlk
    mov eax, dword [dVarLen]
    cmp eax, ecx ;Do we need to reallocate?
    ja .sevGetMoreMem
;No need, we have enough space in the current memory block. We shift
; the strings down far enough to make space for the var.
.sevDoMake:
;Here we create the environment variable. We start by moving the 
; args block down by one dVarLen amount. We know we have this space
; free to us.
    mov rdi, qword [r8 + psp.envPtr]
    push rdi
    call getPtrToEndOfEnvBlk    ;Get in rdi the ptr to the end of env block!
    call getSzOfStrings ;Get the size of the strings block plus terminating 0
    pop rax
    mov rsi, rdi    ;Make the source the end byte of the env
    sub rax, rdi    ;Get the difference from the start of the environment
    cmp rax, -1     ;Are we an empty environment? (Difference of 1)
    jnz .sevNotEmpty
    dec rdi         ;Point to the first byte instead now
.sevNotEmpty:
    mov rdx, rdi    ;Save ptr to the var start location
    mov eax, dword [dVarLen]
    add rdi, rax    ;Make space here
    add rsi, rcx    ;Point to the last byte of the args
    add rdi, rcx
    inc ecx         ;Add one for the terminating null of the env block
    std
    rep movsb   ;Copy the strings down backwards
    cld
    mov rdi, rdx
    mov rsi, qword [lpName]
;Rather than doing a char by char and UC copy we just copy the name 
; directly as it is, since we search for vars independently of case.
;Thus var1 is the same as VAR1
    call strlen2    ;Get the name length in ecx
    dec ecx         ;Drop the terminating null
    rep movsb
    mov al, "="     ;Now write the equals inbetween name
    stosb
    mov rsi, qword [lpValue]
    call strcpy
    jmp .sevExitOk
.sevGetMoreMem:
;Try reallocating, see if that works.
;Here eax = Number of bytes to allocate for the new var
    call growEnvBlk
    jnc .sevDoMake
    xor eax, eax    ;The error code has been set so we return 0 bytes
    jmp short .sevExit
.sevExitOk:
    mov eax, dword [dVarLen] ;Get the length we wrote/freed
.sevExit:
    leave
    call dosCrit1Exit
    jmp extGoodExit2
.sevBadExit:
;Input: ax = Error code to report
    mov word [errorExCde], ax
    call checkFail.skipFail
    xor eax, eax
    jmp short .sevExit
    %pop

;------------------
;  Expand Env Var
;------------------
.expEnvVar:
;Expands a string with envvar values.
;Input: rsi -> [In]            LPCSTR lpSrc
;       rdi -> [Out, optional] LPSTR lpDst
;       ecx =  [In]            DWORD dSize
;Output: Status returned in eax and CF=NC. 
;        If ecx = ecx, then call successful.
;        If ecx < eax, then the buffer needs to be eax in size.
;        If eax = 0, then get last error. Something went wrong!
;
;Copies lpSrc into lpDst replacing each %var% with the value of the variable
; found in the environment. Skips all leading % so if a variable has a % in its 
; name then this call will not work as intended. If no variable found for a 
; particular declaration, then the var name declaration is left unexpanded.
;Caveats:
;rdi cannot be the same as rsi. If rdi is null, we return the length of the 
; buffer needed. If ecx is null, we return bad parameter.

    %push
    %stacksize flat64
    %assign %$localsize 0
    %local lpSrc:qword, lpDst:qword, dSize:dword, dToWrite:dword

    call dosCrit1Enter
    enter %$localsize, 0

    mov qword [lpSrc], rsi
    mov qword [lpDst], rdi
    mov dword [dSize], ecx
    mov dword [dToWrite], 0

    call checkPSPEnvGood
    jnz .eevEnvOk
    mov eax, errBadEnv
    jmp .eevBadExit
.eevEnvOk:
;Now test the required parameters make sense...
    test ecx, ecx
    jz .eevBadParm
    test rsi, rsi
    jz .eevBadParm
;So we can use them. Now we get our count of chars.
.eevLp1:
    lodsb
    inc dword [dToWrite]    ;Add one more char to the count
    test al, al
    jz .eevLp1End
    call .eevChkVar
    jnz .eevLp1
;Here:
;rbx -> First char past the terminating %
;rsi -> First char past the leading %
    dec dword [dToWrite]    ;Drop this char from the count
    call .eevGetVarLen
    add dword [dToWrite], ecx   ;Add that to the length
    mov rsi, rbx    ;Move rsi up now
    jmp short .eevLp1
.eevLp1End:
;Now we know how much buffer space we need, we check that our buffer has
; the right size! If it is too small, report the size needed!
    mov ecx, dword [dToWrite]
    cmp dword [dSize], ecx
    jb .eevExit
    mov rdi, qword [lpDst]
    test rdi, rdi   ;Ensure if lpDst is null, we report count!
    jz .eevExit
;Now we do the actual expansion!
    mov rsi, qword [lpSrc]
.eevLp2:
    lodsb
    test al, al
    jz .eevLp2End
    call .eevChkVar
    jz .eevExpand
    stosb
    jmp short .eevLp2
.eevExpand:
;rbx -> Char past the % in the source string
    call .eevGetVar ;Move rsi to the var to source from. Get len in ecx
    rep movsb
    mov rsi, rbx
    jmp short .eevLp2
.eevLp2End:
    stosb   ;Store the terminating null!
.eevExit:
    mov eax, dword [dToWrite]
    leave
    call dosCrit1Exit
    jmp extGoodExit2
.eevBadParm:
    mov eax, errBadParam
.eevBadExit:
;Input: ax = Error code to report
    mov word [errorExCde], ax
    call checkFail.skipFail
    jmp short .eevExit
.eevGetVarLen:
;Get the count of a variable's length if it exists
;Input: rsi -> Variable to search for
;       CF=NC: ecx = Length of the variable in the environment
;       CF=CY: Var not in environment. ecx = Length of %var% string
    push rsi
    call .eevGetVar
    pop rsi
    return
.eevGetVar:
;Point rsi to the variable in the environment!
;Input: rsi -> Variable to search for. rbx -> Char past last % in src string
;       CF=NC: ecx = Length of the var in env. rsi -> Var value in env
;       CF=CY: Var not in environment. ecx = Length of %var% string.
    push rax
    neg rsi
    lea ecx, dword [rbx + rsi + 1]  ;Plus 1 to add the 2 % signs to count!
    neg rsi

    push rsi    ;Save source position if var not found
    push rcx    ;Save count if var not found
    mov byte [rbx - 1], 0   ;To make the search below work... its crap, I know
    call searchForEnvVar
    pop rax
    jc .eevgvlNoFnd
    pop rax
    add rsi, rcx            ;Move past the <varname>= portion
    call strlen2            ;Get the length of the value of the variable
    dec ecx                 ;Drop null-terminator from count
.eevgvlExit:
    mov byte [rbx - 1], "%" ;Return to normal.
    pop rax
    return
.eevgvlNoFnd:
    mov ecx, eax    ;Move varlength into ecx if var not found!
    pop rsi         ;Get source back
    dec rsi         ;Preserves CF :) Point back to the % sign
    jmp short .eevgvlExit

.eevChkVar:
;Checks if we are at a variable name. Does so by checking if the char
; is a %. If it is, it searches for the terminating %. If none found,
; we are not a variable name.
;
;Input: al = Char just read, rsi -> Next char in string
;Ouput: ZF=NZ: Not a variable, rsi -> Next char in string
;       ZF=ZE: A var, rbx -> First char past var, rsi -> First var char
    cmp al, "%"
    retne
    push rsi    ;Save the ptr to the first char past the %
.eevcvlp:
    lodsb
    test al, al
    jz .eevcvNok
    cmp al, "%"
    jne .eevcvlp
.eevcvOk:
    mov rbx, rsi
    pop rsi
    return
.eevcvNok:
    inc eax
    pop rsi ;Put rsi back where it was, this isn't a variable
    return

;-----------------------
; Get Command Line Copy
;-----------------------
;Input: Nothing
;Output: rdx -> Command line string 
;
;Checks the args block to see if there are 2 strings there. 
; If so, returns the pointer to the second string.
;Else, enlarges the block (if needed by reallocating and freeing)
; copies the environment and args as necessary and adds the new string.
;The new string is the execution command (if one exists), in quotation marks,
; followed by the unquoted args string, 
; i.e. "C:\Myprog.exe" /p /a:TEST.
.getCmdLine:
    call dosCrit1Enter
    call checkPSPEnvGood
    jnz .gclOk
.gclBadExit:
    call dosCrit1Exit
    jmp .exitBadEnv
.gclOk:
;To be here is to say, we have the args block!
    mov rsi, qword [r8 + psp.envPtr]
    call getSzOfEnv
    add rsi, rcx
    xor eax, eax    ;Zero the upper part (should be already...)
    lodsw   ;Get the word and adv rsi to the first string
    call strlen2    ;Get the length of string 1 (exists here) in ecx
    cmp eax, 1
    je .gclMakeCmdline
;Else, 2 or above so the string we want is here! Lets get it!
    lea rdx, qword [rsi + rcx]  ;And go past it!
.gclExit:
    call getUserRegs
    mov qword [rsi + callerFrame.rdx], rdx
    call dosCrit1Exit
    jmp extGoodExit
.gclMakeCmdline:
;rsi -> First string of the args block!
;ecx = Length of first string in args block.
    mov edx, ecx    ;Save the length of the first string in edx
    add ecx, 2      ;Add space for the quotation marks and a trailing space
    movzx eax, byte [r8 + psp.parmList] ;Get count w/o terminating CR
    lea eax, [eax + ecx + 1]    ;This is how much space we will need.
    call getFreeSpaceInEnvBlk   ;Get free space count in ecx
    cmp ecx, eax    
    jb .gclNeedMore
;Here we have enough space. Length of string 1 in edx
    inc word [rsi - 2]  ;Increment the word behind us
    lea rdi, qword [rsi + rdx]
    push rdi    ;Save the address of the string on the stack!
    mov al, '"'
    stosb
    mov ecx, edx
    rep movsb   ;Move now everything
    dec rdi
    mov al, '"'
    stosb
    mov al, SPC
    stosb
    lea rsi, qword [r8 + psp.progTail]
    movzx ecx, byte [r8 + psp.parmList]
    rep movsb
    xor eax, eax
    stosw       ;Null terminate the string AND the args block
    pop rdx     ;Get back the string address!!
    jmp short .gclExit
.gclNeedMore:
    call growEnvBlk
    jc .gclBadExit
    mov rsi, qword [r8 + psp.envPtr]
    call getSzOfEnv ;Gets size in ecx
    lea rsi, qword [rsi + rcx + 2]  ;Go past strings and the arg count
    call strlen2    ;Get the length of the first string in ecx
    jmp short .gclMakeCmdline
;-------------------
; Get Launch Params
;-------------------
.getLaunchParams:
;Copies the data in cmdLineArgPtr to the location specified by them and 
; also parses the argument list into an .
;This allows me to move it out of the PSP if needed later.
;Input: ecx = Length of buffer
;        If ecx = 0, return in eax the size of the struct necessary
;        Else, ecx = Size of the allocation provided.
;              rdx -> Buffer to write to
;Output: CF=NC: Data copied.
;        CF=CY: Error, if buffer too small, access denied!
;        
    test ecx, ecx
    jz .glpGetBlk
.glpGetLen:
    movzx eax, byte [r8 + psp.parmList]   ;Get the length in bytes w/o CR
    add eax, launchParams_size + 1   ;Add 1 for terminating CR
    jmp extGoodExit
.glpGetBlk:
    call .glpGetLen     ;Get the size to allocate
    cmp ecx, eax
    jae .glpGoGet
    mov eax, errAccDen  ;Access is denied because of wrong input val
    jmp extErrExit
.glpGoGet:
    mov rdi, rdx
    lea rsi, qword [r8 + psp.cmdLineArgPtr]
    rep movsb
    jmp extGoodExit

;----------------------------------------------------------------------------
;     Environment local functions
;----------------------------------------------------------------------------
;List of functions:
; growEnvBlk -> Reallocates or allocates/frees a new block of memory
; freeEnvVar -> Deletes an environment variable and compactifies
;               the environment.
; searchForEnvVar -> Looks in the raw environment for a variable
; allocEnvSig -> Same as below but adds a signature to the block
; allocEnv -> Returns an allocation 
; checkPSPEnvGood -> Check if the env in the psp is good.
; checkEnvGood -> Check if an env is good.
; getPtrToEndOfEnvBlk -> Returns in rdi ptr to second null
; getSzOfEnv     -> Return in ecx the number of bytes used in the environment
; getSzOfStrings -> Return in ecx the number of bytes used in the optional 
;                   strings after the environment, if present
; getSzOfEnvAndStrings -> Sums the output of both
; getFreeSpaceInEnvBlk -> Gets the number of bytes free in the environment block
;----------------------------------------------------------------------------

growEnvBlk:
;Reallocates or allocates/frees if necessary the main environment
; to fit the needs of our new action.
;Destroys all regs.
;Input: eax = Number of bytes for the new string to be added
;Output: CF=NC: All ok!
;        CF=CY: Error code in eax. Something went wrong!
;               Error code set in DOS vars.
    call getSzOfEnvAndStrings   ;Get the current size of strings and args
    lea ebx, dword [eax + ecx + 0Fh]  ;Get the amount of space we need
    shr ebx, 4  ;Turn into paragraphs
    push rbx    ;Save count of paras needed to store all strings + new string
    push rcx    ;Save the current env and args size
    push r8     ;Save the psp pointer
    mov r8, qword [r8 + psp.envPtr] ;Get ptr to block to reallocate
;Save caller rbx if there is not enough memory...
    call getUserRegs
    push qword [rsi + callerFrame.rbx]  ;Preserve, if an error occurs
    push rsi
    call reallocMemory
    pop rsi
    pop qword [rsi + callerFrame.rbx]
    pop r8
    pop rcx     ;Get back the raw count of chars in the env and args
    pop rbx     ;Get back the number of paragraphs allocated
    jc .getNewBlk
;Now we have reallocated our block! We should null the end portion.
    mov rdi, qword [r8 + psp.envPtr]
    mov ebx, dword [rdi - mcb_size + mcb.blockSize]  ;Get new sz of the mcb
    shl ebx, 4      ;Turn into bytes
    add rdi, rcx    ;Move rdi to the end of the blocks.
    sub ebx, ecx    ;Drop what is already allocated from the count
    mov ecx, ebx
    xor eax, eax    
    rep stosb   ;Zero the newly allocated stuff
    return
.getNewBlk:
    cmp eax, errNoMem   ;We had no memory to grow?
    jne .badMem      ;No? Must've been something more sinister.
;Now we allocate and free instead.
    call allocEnv   ;Zeros the block for us so all is good
    jc .badMem
;rax -> new env block
;ecx = Number of chars in the env totally
    push rax    ;Save the new psp
    mov rsi, qword [r8 + psp.envPtr]
    mov rdi, rax    ;This is where we will write to
    rep movsb       ;And move it over :)
    push r8         ;Save ptr to the psp on the stack
    mov r8, qword [r8 + psp.envPtr]
    call freeMemory ;Now we free r8. Ignore errors from this call.
    pop r8
    pop qword [r8 + psp.envPtr] ;Get the new environment pointer in its place
    clc
    return
.badMem:
    stc
    return

freeEnvVar:
;Frees a variable from the environment, pulls the strings behind it up
; zeros the rest of the environment, and returns a pointer to the first
; free byte of the environment!
;Input: rsi -> Variable to free in the environment.
    call getSzOfStrings ;Get the size of the strings
    test ecx, ecx
    retz
    mov rdi, qword [r8 + psp.envPtr]
    call getPtrToEndOfEnvBlk    ;Save the ptr and the size of the strings
    push rcx    ;Size of args block including null terminator
;Workout if we are the last envvar left
    mov rax, qword [r8 + psp.envPtr]
    cmp rax, rsi
    jne .notFirst
    call strlen2
    lea rax, qword [rsi + rcx]
    cmp rdi, rax
    jne .notFirst
;Here we are the first var so we place a null terminator and pull strings
;rdi -> Second null of env, rsi -> Byte to put empty string for empty env
    xchg rsi, rdi   ;Swap em!
    xor eax, eax
    stosb   ;Move rdi past the empty string terminator
    pop rcx ;Get args block size back now
    inc ecx ;Add the terminating null of the env block to the count too!
.pullUp:
    rep movsb
    call getFreeSpaceInEnvBlk
    xor eax, eax
    rep stosb
    return
.notFirst:
    lea rax, qword [rdi + 1]    ;Point rax to the last byte of the env
    call strlen2
    lea rdi, qword [rsi + rcx]  ;Point rdi to the next variable in env
    sub rax, rdi    ;Get in rax cnt of bytes from start of next var
    pop rcx         ;Get the size of the args block
    add ecx, eax    ;Sum the two
;rsi -> Head of the variable we are deleting, rdi -> Start of next var
    xchg rsi, rdi   ;Swap em!
    jmp short .pullUp

searchForEnvVar:
;Gets the environment, and scans it for a string with the var specified.
;Input: rsi -> Null terminated var name to look for.
;Returns: CF=NC: rsi -> Env var in env. ecx = len of "<varName>=" string
;         CF=CY: Var not found.
    push rdi
    mov rdx, rsi        ;Save the search pointer!
    mov rsi, qword [r8 + psp.envPtr]
.varLp:
    mov rdi, rdx        ;Reset the pointer for searching
    call .cmpEnvVar      ;Checks if these two environment vars are equal
    je .varFound
    xor eax, eax        ;Search for a null
    mov rdi, rsi        ;Scan the environment
    mov ecx, ENV_MAX    ;Just keep searching
    repne scasb         ;Now scan for the terminating null
    cmp byte [rdi], al  ;Now check the second char
    je .varNotFound     ;If second null, no more env to search!
    mov rsi, rdi        ;Now move to rsi the start of the next env var
    jmp short .varLp    ;And scan again!
.varNotFound:
    stc
.varFound:
    pop rdi
    return
.cmpEnvVar:
;Checks that we have found the environment variable we are looking for.
;Input: rsi -> Candidate var in the environment
;       rdi -> Null terminated supplied var name to compare against
;Output: ZF=ZE: Equal. ecx = Length of var name
;        ZF=NZ: Not equal.
    push rbx
    push rsi
    push rdi
    xchg rsi, rdi       ;Swap Env and user ptrs. 
;rdi -> env ptr. rsi -> lpcstr, given by caller and null terminated.
    xor ecx, ecx        ;Clear the counter
.cevLp:
    lodsb               ;Pick up from caller string
    test al, al
    jz .cevFinalCheck   ;End of string reached. Check if envvar is over too.
    call .ucChar        ;Upper case it!
    movzx ebx, al
    movzx eax, byte [rdi]   ;Compare al to *rdi
    call .ucChar
    inc rdi                 ;Go to next char      
    cmp eax, ebx
    jne .cevExit        ;If this char isn't equal, clearly not the same var
    inc ecx             ;Else, one more char in the name!
    jmp short .cevLp
.cevFinalCheck:
;We just read a null, meaning the env equivalent string must be an =
    inc ecx             ;Add terminator to the count
    cmp byte [rdi], "=" ;Now check if env var is ended too
.cevExit:
    pop rdi
    pop rsi
    pop rbx
    return
.ucChar:
    push rbx
    lea rbx, ucTbl  ;Use the non-filename table!
    jmp uppercaseCharWithTable  ;Return through here


allocEnvSig:
    call allocEnv
    retc
;Set the marker to allow us to manage this block
    mov byte [rax - mcb_size + mcb.subSysMark], mcbSubEnv
    return

allocEnv:
;Wraps the allocation call, preserving registers and rax on caller stack.
;It also adds a signature in the reserved bytes of the MCB header and
; memsets the block to 0.
;Input: ebx = Number of paragraphs to allocate
;Return: CF=NC: rax -> Memory block to use.
;        CF=CY: eax has error code (errMCBbad, errNoMem).

    push rcx
    push rsi
    push rdi
    call getUserRegs
    push qword [rsi + callerFrame.rax]  ;Save caller rax
    push qword [rsi + callerFrame.rbx]  ;Save caller rbx
    push rsi    ;Save pointer to the frame
    push rbp
    call allocateMemory
    pop rbp
    jc .exit
;Zero the newly allocate memory block.
    push rax
    mov rdi, rax    ;Point to the block
    mov ecx, dword [rax - mcb_size + mcb.blockSize]    ;Get number of paragraphs
    shl ecx, 4      ;Turn to bytes
    xor eax, eax    ;Store zeros
    rep stosb
    pop rax
.exit:
    pop rsi     ;Get back pointer frame
    pop qword [rsi + callerFrame.rbx]  ;Return caller original rbx 
    pop qword [rsi + callerFrame.rax]  ;Return caller original rax 
    pop rdi
    pop rsi
    pop rcx
    return

checkPSPEnvGood:
;Checks the PSP environment is good.
;Input: Nothing.
;Output: CF=NC: Ok!
;        CF=CY: Either missing strings block or 
    push rsi
    mov rsi, qword [r8 + psp.envPtr]
    call checkEnvGood   ;Check's double null termination
    jz .exitBad
    call getSzOfStrings ;Checks existence of strings block
    test ecx, ecx
    jz .exitBad
    pop rsi
    return
.exitBad:
    stc
    pop rsi
    return

checkEnvGood:
;Given an environment pointer, checks if it is good.
;Input: rsi -> Environment to check.
;Output:
;   ZF=ZE: Environment is bad. Is not double null terminated.
;   ZF=NZ: Environment is good. Is double null terminated.
    test rsi, rsi   ;Null envs are possible. If it happens, just fail!
    retz
    push rdi
    mov rdi, rsi
    call getPtrToEndOfEnvBlk   ;Get the ptr to the end.
    pop rdi
    return

getPtrToEndOfEnvBlk:
;Gets ptr to end of an environment block by searching for the 
; double terminator in a region.
;Input: rdi -> Block to get the size of.
;Output:
;   ZF=ZE: Environment is bad. Is not double null terminated.
;   ZF=NZ: Environment is good. Is double null terminated.
;           rdi -> Second null byte of the terminator of the environment.
    push rax
    push rcx
    mov ecx, ENV_MAX
;Ensure we have a good environment, i.e. one that is double null terminated
; within the max range.
    xor eax, eax
.pathNulScan:
    repne scasb
    test ecx, ecx   ;If we are zero on first null, its an error
    jz .badExit
    cmp byte [rdi], al  ;Is char two null?
    jne .pathNulScan    ;If not, keep searching
    test r8, r8         ;Clear the ZF. PSP ptr can never be NULL :)
.badExit:
    pop rcx
    pop rax
    return

getSzOfEnv:
;Gets the number of bytes of all strings in the environment only.
;Input = rsi -> Environment block to count.
;Output ecx = Number of bytes from start to second null inclusive
    push rsi
    push rdi
    mov rdi, rsi
    call getPtrToEndOfEnvBlk
    neg rsi
    lea ecx, dword [edi + esi + 1]  ;Add 1 for the terminating null
    pop rdi
    pop rsi
    return

getSzOfStrings:
;Gets the number of bytes of all strings optional strings 
; after the raw environment.
;Output: ecx = Number of bytes of strings plus 2 bytes for argc count.
;        If ecx is zero it means there is no strings block!
    push rdx
    push rsi
    push rdi
    mov rdi, qword [r8 + psp.envPtr]
    call getPtrToEndOfEnvBlk   ;Move rdi to the second null
    inc rdi ;Go past the second null
    mov esi, 2              ;Set the counter to the argc default value
    movzx edx, word [rdi]   ;Get the number of additional strings to copy 
    test edx, edx
    cmovz ecx, esi          ;Set ecx with the return value and exit 
    jz .exit
    cmp edx, (ENV_MAX-2)/2  
    ja .err
    add rdi, rsi            ;Move rdi past the argc count
.lp:
    call strlen     ;Get the length of the string in ecx
    add esi, ecx    ;Add this string to the sum too
    cmp esi, ENV_MAX    ;The sum cannot grow so large
    ja .err
    add rdi, rcx    ;And move rdi to the next string
    dec edx         ;Drop one from count
    jnz .lp         ;Go again if still have strings to handle
    lea ecx, dword [esi + 1]    ;Return value in ecx plus terminating null
    test word [rdi - 1], -1 ;Check if the final word is zero...
    jnz .err        ;Set value to 0, if not (i.e. we have garbage)
.exit:
    pop rdi
    pop rsi
    pop rdx
    return
.err:
    xor ecx, ecx
    jmp short .exit

getSzOfEnvAndStrings:
;Gets the combined size of the environment and the strings.
;Output: ecx = Number of bytes allocated in the environment block
    push rax
    push rsi
    mov rsi, qword [r8 + psp.envPtr]
    call getSzOfEnv
    push rcx
    call getSzOfStrings
    pop rax
    add ecx, eax
    pop rsi
    pop rax
    return

getFreeSpaceInEnvBlk:
;Output: ecx = Number of free bytes in the environment memory block
    push rbx
    call getSzOfEnvAndStrings   ;Get in ecx the size allocated.
    mov rbx, qword [r8 + psp.envPtr]
    mov ebx, dword [rbx - mcb_size + mcb.blockSize]
    shl ebx, 4  ;Get total number of bytes in the environment
    sub ebx, ecx    ;Get difference!
    mov ecx, ebx
    pop rbx
    return