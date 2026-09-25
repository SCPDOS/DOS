

;Environment and process information functions go here :)

;Will add:
;
;GetEnvironmentStrings
;GetEnvironmentVariable
;SetEnvironmentStrings
;SetEnvironmentVariable
;
;Optionally may add:
;
;ExpandEnvironmentStrings
;FreeEnvironmentStrings
;GetCommandLine
;GetLaunchParameters

;Aim to depreciate the pointers to the PSP, instead copying to new memory
; blocks owned by the calling process.
;Of course, it can only get this information for itself.

systemServices: ;ah = 61h
;al = 0 -> Get Environment pointer in rdx
;   Output: rdx -> Environment Pointer. May be a null pointer. Caller checks!
;al = 1 -> Get Command Line Arguments Pointer in rdx
;   Output: rdx -> Pointer to whatever was passed as a CR terminated 
;                   command line.
;al = 2 -> Get ptr to ASCIIZ name for program in rdx. Might not be FQ.
;   Output: CF=NC: rdx -> Filename 
;           CF=CY: eax = Error code (errAccDen) if no filename ptr.
;                   The error case should only happen for special
;                   programs that are launched without an environment
;                   as DOS has nowhere to put the filename string.
;al > 2: Returns CF=CY and eax = Error code (errInvFnc).
;
;Will be:
;al = 3 -> GetEnvironmentStrings
;al = 4 -> GetEnvironmentVariable
;al = 5 -> SetEnvironmentStrings
;al = 6 -> SetEnvironmentVariable
;
;Optionally may add:
;
;al = 7 -> ExpandEnvironmentStrings
;al = 8 -> FreeEnvironmentStrings
;al = 9 -> GetCommandLine
;al = 0Ah -> GetLaunchParameters
;
;Very optionally:
;
;al = 0Bh -> CommandLineToArgv
;
;The end goal is to superceed the existing functions and depreciate them
; possibly renumbering and adjusting all utilities which use them and direct 
; PSP access, to use the new functions. Since we're not public (yet), we can 
; just hard restart like this. 
; Will it be painful? Yes. Will it be worth it in the long run? Yes.
;Probably it won't take more than a day to adjust the logic of all current 
; utilities.

    cmp al, 1
    je .getCmdLineArgs
    cmp al, 2
    jbe .getEnvPtr
    mov byte [errorLocus], eLocUnk  
    mov eax, errInvFnc  ;Error, with invalid function number error
.exitBad:
    jmp extErrExit
.getCmdLineArgs:
    mov rdx, qword [currentPSP]
    lea rdx, qword [rdx + psp.cmdLineArgPtr]   ;Get the cmdargs pointer
    jmp short .gepExitOk
.getEnvPtr:
;Gets the environment pointer in rdx
    mov rdx, qword [currentPSP]
    mov rdx, qword [rdx + psp.envPtr]   ;Get the environment pointer
    test al, al     ;Was al=0?
    jz .gepExitOk   ;Exit if al = 0 since we have the pointer we need!
    test rdx, rdx   ;Check if the env pointer is ok to use
    jz .gepFail
    cmp rdx, -1
    je .gepFail
;Here we search for the double 00 and then check if it is 0001 and
; pass the ptr to the word after.
    push rcx
    xor ecx, ecx
    mov ecx, 7FFFh  ;Max environment size
.gep0:
    cmp word [rdx], 0   ;Zero word?
    je short .gep1
    inc rdx         ;Go to the next byte
    dec ecx
    jnz short .gep0
.gep00:
;Failure here if we haven't hit the double null by the end of 32Kb
    pop rcx
.gepFail:
    xor edx, edx        ;Turn it into null pointer
    mov eax, errAccDen  ;Set error code here
    jmp short .exitBad  ;Return setting CF=CY and errAccDen (no pointer)
.gep1:
    add rdx, 2  ;Skip the double null
    cmp word [rdx], 1   ;Check if one more string in environment
    jne .gep00
    add rdx, 2  ;Skip the 0001 word.
    pop rcx
.gepExitOk:
    call getUserRegs
    mov qword [rsi + callerFrame.rdx], rdx
    jmp extGoodExit
