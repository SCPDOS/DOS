

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
; where wArgc is a binary value between 0 and 7FFFh.
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
    ;cmp al, 08h ;UNIMPLEMENTED: ExpandEnvironmentStrings
    ;je .expEnvVar
    ;cmp al, 09h ;UNIMPLEMENTED: GetCommandLine
    ;je .getCmdLine
    cmp al, 0Ah ;GetLaunchParameters
    je .getLaunchParams
.exitBadFunc:
    mov byte [errorLocus], eLocUnk  
    mov eax, errInvFnc  ;Error, with invalid function number error
.exitBad:
    jmp extErrExit
.exitBadFmt:
    mov eax, errBadFmt
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
    mov rsi, qword [r8 + psp.envPtr]    ;Get the raw ptr!
    call checkEnvGood
    jz .gesDefault
;Here rsi -> points to raw environment
    call getSzOfEnv ;Get number of bytes in ecx
    push rcx
    add ecx, 0Fh
    shr ecx, 4      ;Turn into paragraphs to allocate
    call allocEnv   ;Get the block ptr in rax (preserve rsi, rdi)
    pop rcx         ;Get the byte count back
    jc .exitBad
;rsi -> Raw environment
    mov rdi, rax
    rep movsb
    jmp short .exitOk
.gesDefault:
    mov ebx, 6
    call allocEnv   ;Get in rax the allocated pointer
    jc .exitBad
    jmp short .exitOk
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
    mov rsi, rdx
    call checkEnvGood   ;This needs input in rsi and preserves it
    jc .exitBadEnv
    call getSzOfEnv ;Get in ecx the length of the env. Preserves rsi
    push rcx        ;Save the env size
    mov eax, ecx    ;Tmp save count in eax
    call getSzOfStrings ;Get raw block strings size. 
    push rcx        ;Save the strings size
    add ecx, eax    ;Sum them for the full size to allocate
    add ecx, 0Fh    
    shr ecx, 4      ;Turn into paragraphs to allocate
    call allocEnv   ;Get the block ptr in rax (preserve rsi, rdi)
    pop rbx         ;Get the strings size back
    pop rcx         ;Get the env size back
    jc .exitBad
    mov byte [rax + mcb.subSysMark], SPC    ;Clear the dflt signature.
    mov rdi, rax    ;Write to the newly allocated block 
    rep movsb       ;Copy the environment over
;rdi points to the destination for the strings copy now
    mov ecx, ebx    ;ebx has the string size
    mov rsi, qword [r8 + psp.envPtr]    ;Source from the sole string block...
    add rsi, rcx    ;... after the raw environment we are discarding.
    rep movsb
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
    jnc extGoodExit
    push rax        ;Save the originally returned error code
    mov r8, rbx     ;Try and free the newly allocated block. This should be ok.
    call freeMemory
    pop rax
    jmp .exitBad    ;Exit, bubbling the old error code.
;-----------------------
; Free Env String Block
;-----------------------
.freeEnvStrings:
;This frees an opaque environment copy. 
; Only frees the block if WE allocated it.
;
;Input: rdx -> Envblock to free
;Output: CF=NC: Block freed
;        CF=CY: Block not freed (errMCBbad, errMemAddr, errBadFmt)
    cmp byte [rdx + mcb.subSysMark], mcbSubEnv ;Correct type?
    jne .exitBadFmt
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
;Output: CF=NC: eax = Number of chars written. Fail if returning eax = 0. 
;        CF=CY: eax = Error code, errEnvVarNotFnd.
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

    enter %$localsize, 0
    mov qword [lpBuffer], rdi
    mov dword [dSize], ecx
    mov dword [dLenVal], 0   ;Length of the value of the envvar
;Start by just making sure the input pointer is not bad.
    test rsi, rsi   ;Is this null input string?
    jz .getVarExitNoEnv
    call searchForEnvVar    ;Search for the string in rsi
    jc .getVarExitNoEnv     ;Var doesn't exist!
;Here rsi -> string in the env and ecx has length of the var name and the = 
    add rsi, rcx    ;Go to the value portion of the env var
    call strlen2    ;Get the length of the value string
    mov dword [dLenVal], ecx    ;This is the len of the value of the envvar
    cmp dword [dSize], ecx      ;Do we have enough space to store the length?
    jb .getVarExit      ;If not, inform the caller how much space is needed
    mov rdi, qword [lpBuffer]   ;Get buffer pointer back
    test rdi, rdi   ;Ensure it is a valid buffer ptr
    jz .getVarExit
    rep stosb       ;Now write the value into the buffer
.getVarExit:
    mov eax, dword [dLenVal]  ;Set this as the copy count
    leave
    jmp extGoodExit2
.getVarExitNoEnv:
;Exit through here if desired environment variable is not found.
;We setup the extended error codes explicitly as we are returning through ok
; to report the buffer size!
    mov word [errorExCde], errEnvVarNotFnd
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
;Output: CF=NC: Success!
;        CF=CY: Fail!

    test rsi, rsi
    jnz .sevGo
.sevBadParam:
    mov eax, errBadParam
    jmp extErrExit
.sevGo:
    cmp byte [rsi], 0   ;Can't create/delete a null string var!
    je .sevBadParam

    %push
    %stacksize flat64
    %assign %$localsize 0
    %local lpName:qword, lpValue:qword, bAction:byte
    
    enter %$localsize, 0
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
    call searchForEnvVar    ;Search for var pointed to by rsi
    jnc .sevProceed
;Here the var doesn't exist. If we are to delete, then we return 
; error envvar not found.
    test byte [bAction], -1
    jz .sevCreate  ;Jump if we are creating a new variable :)
    mov eax, errEnvVarNotFnd
    jmp extErrExit
.sevCreate:

.sevProceed:
;Here, rsi -> Env var that we are to update or delete
    call freeEnvVar ;Start by freeing this variable
    test byte [bAction], -1 ;Now check if we need to recreate this var.
    jz .sevCreate   ;Jump if so.
.sevExit:
    xor eax, eax
    jmp extGoodExit2
    %pop
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

;------------------
;  Expand Env Var
;------------------
.expEnvVar:

;-----------------------
; Get Command Line Copy
;-----------------------
;Checks the args to see if there are 2 strings there. 
; If so, returns the pointer to the second string.
;Else, enlarges the block (if needed by reallocating and freeing)
; copies the environment and args as necessary and adds the new string.
;The new string is the execution command (if one exists, else an empty 
; string is made for entry 0) in quotation marks, followed by the 
; unquoted args string, i.e. "C:\Myprog.exe" /p /a:TEST
.getCmdLine:
    mov eax, errInvFnc
    jmp extErrExit

;--------------------------------------
;     Environment local functions
;--------------------------------------
;List of functions:
; getFreeSpace -> Gets the number of bytes free in the environment block
; freeEnvVar -> Deletes an environment variable and compactifies
;               the environment.
; searchForEnvVar -> Looks in the raw environment for a variable
; allocEnv -> Returns an allocation 
; checkEnvGood -> Check if an env is good.
; getPtrToEndOfEnvBlk -> Returns in rdi ptr to second null
; getSzOfEnv     -> Return in ecx the number of bytes used in the environment
; getSzOfStrings -> Return in ecx the number of bytes used in the optional 
;                   strings after the environment, if present

getFreeSpace:
;Output: ecx = Number of free bytes in the environment block
    push rbx
    push rsi
    push rdi
    call getPtrToEndOfEnvBlk   ;Get ptr in rdi to end of alloc 
    mov rsi, qword [r8 + psp.envPtr]
    sub rdi, rsi    ;This gets number of bytes allocated
    call getEnvSize ;Get total block size
    mov rbx, qword [r8 + psp.envPtr]
    mov ecx, dword [rbx - mcb_size + mcb.blockSize]
    shl ecx, 4  ;Get number of bytes in the environment
    sub ecx, edi    ;Get difference!
    pop rdi
    pop rsi
    pop rbx
    return

freeEnvVar:
;Frees a variable from the environment, pulls the strings behind it up
; zeros the rest of the environment, and returns a pointer to the first
; free byte of the environment!
;Input: rsi -> Variable to free.
;Output: rdi -> First byte to write new env var in
;        ecx = Number of free bytes in env
    mov rdi, rsi
    xor eax, eax
.freeLp:
    cmp byte [rdi], 0
    je .exitLp
    stosb
    jmp short .freeLp
.exitLp:
;rdi points to the terminating null of the var we just deleted
;rsi points to the start of the free space
    xchg rsi, rdi   ;Swap em!
    cmp word [rsi], 0   ;If we are already at the terminating null, dont advance!
    jne .prepPullup
    xor eax, eax
    jmp short .cleanEnv
.prepPullup:
    inc rsi         ;Go past the terminating null!
.pullUp:
    lodsb
    stosb
    test al, al ;Did we pick up a zero
    jne .pullUp ;If not, keep copying
    cmp byte [rsi], 0   ;Is this the famous second byte?
    jne .pullUp
;We are at the end of the copy!
.cleanEnv:
    stosb   ;Store the famous second null
    dec rdi ;without incrementing it!!
    call getFreeSpace
    xor eax, eax
    push rcx
    rep stosb       ;Now zero the remaining space of the env!
    pop rcx
    return

searchForEnvVar:
;Gets the environment, and scans it for a string with the var specified.
;Input: rsi -> Null terminated var name to look for.
;Returns: CF=NC: rsi -> Env var in env. ecx = len of <varName>= string
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
    push rsi
    push rdi
    xchg rsi, rdi       ;Swap Env and user ptrs. 
;rdi -> env ptr. rsi -> lpcstr, given by caller and null terminated.
    xor ecx, ecx        ;Clear the counter
.cevLp:
    lodsb               ;Pick up from caller string
    test al, al
    jz .cevFinalCheck   ;End of string reached. Check if envvar is over too.
    call uppercaseChar  ;Upper case it!
    scasb               ;Compare al to *rdi and inc rdi
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
    push rsi    ;Save pointer to the frame
    call allocateMemory
    jc .exit
;Set the marker to allow us to manage this block
    mov byte [rax + mcb.subSysMark], mcbSubEnv
;Zero the newly allocate memory block.
    push rax
    mov rdi, rax    ;Point to the block
    mov ecx, ebx    ;Get number of paragraphs
    shl ecx, 4      ;Turn to bytes
    xor eax, eax    ;Store zeros
    rep stosb
    pop rax
.exit:
    pop rsi     ;Get back pointer frame
    pop qword [rsi + callerFrame.rax]  ;Return caller original rax 
    pop rdi
    pop rsi
    pop rcx
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
    call getPtrToEndOfEnvBlk
    neg rsi
    lea ecx, dword [edi + esi]
    pop rdi
    pop rsi
    return

getSzOfStrings:
;Gets the number of bytes of all strings optional strings 
; after the raw environment.
;Output: ecx = Number of bytes of strings only.
    push rdx
    push rsi
    push rdi
    mov rsi, qword [r8 + psp.envPtr]
    call getPtrToEndOfEnvBlk   ;Move rdi past the end of the environment
    xor esi, esi            ;Clear the counter
    movzx edx, word [rdi]   ;Get the number of additional strings to copy 
    test edx, edx
    jz .exit
.lp:
    call strlen     ;Get the length of the string in ecx
    add esi, ecx    ;Add this string to the sum too
    dec edx         ;Drop one from count
    jnz .lp         ;Go again if still have strings to handle
.exit:
    mov ecx, esi    ;Return value in ecx
    xor esi, 2      ;Setup a default 0 value
;Now we ensure that we are double null terminated. If not, we just
; computed the size of garbage :)
    test word [rdi + rcx - 1], -1   ;Should be double zero
    cmovnz ecx, esi ;Set value to 0, if 
    pop rdi
    pop rsi
    pop rdx
    return
