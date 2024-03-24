;------------------------------------------------;
;              Process CONFIG.SYS                ;
;------------------------------------------------;
;Create a stack frame with the following order.
;Values greater than max are set to max. Values less than min are set to min.
; New Buffers value.        Default = 30, Min = 1, Max = 99
; New SFT value.            Default = 20, Min = 8, Max = 254
; New FCBS value.           Default = 4,  Min = 4, Max = 254
; New protected FCBS value. Default = 0,  Min = 0, Max = New FCBS value
; New CDS value.            Default = 5,  Min = 5, Max = 26
;
;Remember to maintain the base of occupied memory on stack (endPtr)
;-------------------------------------------------------------------------;
; CONFIG.SYS processing pseudocode:-
;
; _START:
; Read file one byte at a time a until a EOF or CR encountered.
; If (EOF encountered)
;   Insert a terminating ^Z to the end of the line. 
;   Close handle.
; Parse the line from beginning looking for a DOS terminating char.
;   If (CR or EOF encoutered before terminating char)
;       Bad line error msg. 
;       If (CR encountered)
;           Goto _START
;       Else
;           Goto _EXIT
;   Else 
;       If (Keyword AND not DEVICE) 
;           Store it's value on stack or change internal variable value
;       If (DEVICE)
;           Move endPtr after end of line and try load the driver.
;           If (driver doesn't exist or fails to init)
;               print bad driver error msg.
;       Else 
;           Bad line error msg
;       If (line terminated by CR)
;           Goto _START
; _EXIT:
;-------------------------------------------------------------------------;
; Note:
; If driver a Block Device Driver, build all the DPB's for it (up until max)
;   directly after the driver pointer as returned by the driver. Then, 
;   adjust the memory pointer and start loading next line.
; Once EOF has been reached, we jmp to noCfg which configures the other
;   data structures according to the values on the stack frame.
;-------------------------------------------------------------------------;
;Start CONFIG.SYS parsing here
configParse:
    mov qword [rbp - cfgFrame.cfgHandle], rax
    mov qword [rbp - cfgFrame.lastLine], 0
    mov qword [rbp - cfgFrame.linePtr], -1   ;Default buffer
    mov eax, 4800h
    mov ebx, 10h    ;Request 16 paragraphs (256 bytes)
    int 21h
    jc .stopProcessError
    mov qword [rbp - cfgFrame.linePtr], rax
    mov rdx, rax    ;Move the pointer to rdx
    sub rax, mcb_size 
    mov qword [rax + mcb.owner], mcbOwnerDOS    ;Set owner to DOS
    xor eax, eax
.nextChar:
    mov rbx, qword [rbp - cfgFrame.cfgHandle]   ;Move the handle into ebx
    cmp bx, -1
    je .stopProcessError
    mov eax, 3F00h  ;Read handle
    mov ecx, 1  ;Read one byte
    int 21h
    jc .stopProcessError
    test eax, eax	;If this is zero, EOF reached, take command
    jz .endOfFile
.notEOF:
    movzx eax, byte [rdx]
    cmp al, CR
    je short .endOfLine
    cmp al, LF
    je short .endOfLineChange   ;Continue, but replace with standard EOL char (CR)
    cmp al, EOF
    je short .endOfFile
    push rax    ;Push rax on stack as the argument to normalise
    mov eax, 1213h  ;Uppercase the char if it is uppercasable
    int 2Fh
    mov byte [rdx], al  ;Replace the char with the capitalised form
    pop rax ;Pop into rax to renormalise the stack
.notChar:
    inc rdx ;Now move our local pointer to the next byte
    jmp short .nextChar
.endOfFile:
    mov qword [rbp - cfgFrame.lastLine], -1	;Mark EOF and End of line
.endOfLineChange:
    mov byte [rdx], CR  ;Store a terminating char at the end of the command
.endOfLine:
;rdx points to terminating char
;First find the length of the instruction word
    mov rsi, qword [rbp - cfgFrame.linePtr]
    xor ecx, ecx
.cmdNameLenGet:
    lodsb
    call .isCharTerminal
    jz short .endOfCommandFound
    inc ecx
    cmp ecx, 10 ;If shorter than longest command, keep looping
    jb short .cmdNameLenGet
;Else, fall through in error
.endOfCommandFound:
;ecx has the length of the command
    cmp ecx, 10
    je .stopProcessError
    lea rdi, .keyTbl ;Put rdi at the table to search for
.cmdSearch:
    cmp byte [rdi], -1
    je .stopProcessError
    cmp byte [rdi], cl
    jne short .gotoNextCmd
    ;Candidate command found, check said command is the command we want
    mov rsi, qword [rbp - cfgFrame.linePtr]
    cmp rsi, -1 ;Error?
    je .stopProcessError
    push rdi
    push rcx
    inc rdi ;Go to next char
    repe cmpsb  ;Compare whilst the strings are equal
    pop rcx
    pop rdi
    jne short .gotoNextCmd    ;If not equal, just goto next command
    ;Else, rdi points to the table entry from the head of the table
    ;      rcx has the length of the name field
    ;rdx points to the terminating char of the line 
    lea rsi, .keyTbl
    mov rax, rsi    ;Keep a copy in rax
    movzx rsi, word [rdi + rcx + 1]
    add rsi, rax    ;So add the EA of the head of the tbl before calling
    clc ;Ensure flags are happy before entering
    push rbp
    call rsi    ;Call this function
    pop rbp
    jc .stopProcessError    ;If the function returns CF=CY, error exit
    test qword [rbp - cfgFrame.lastLine], -1 ;If we concluded at EOF, exit
    jnz .cfgExit
    mov rdx, qword [rbp - cfgFrame.linePtr] ;Start reading afresh
    ;Read the next char. 
    ;If EOF, exit.
    ;Else if, LF, proceed to read line routine.
    ;Else, advance ptr by one and proceed to process char
.endCommandClear:
    mov rbx, qword [rbp - cfgFrame.cfgHandle]   ;Move the handle into rbx
    mov eax, 3F00h  ;Read handle
    mov ecx, 1  ;Read one byte to clear the LF from the file
    int 21h
    jc .stopProcessError
    test eax, eax   ;If no chars were read, exit!
    jz .cfgExit
    ;Do a trash check
    mov al, byte [rdx]
    cmp al, EOF
    je .cfgExit
    cmp al, LF
    je .nextChar
    jmp .notEOF
;CONFIG.SYS utility functions
.gotoNextCmd:
    movzx eax, byte [rdi]
    add eax, 3
    add rdi, rax
    jmp .cmdSearch
.isCharTerminal:
;Input: AL = Char to check
;Output: ZF=ZE -> Char terminal
;        ZF=NZ -> Char not terminal
    cmp al, "="
    rete
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, ";"
    return
.stopProcessError:
    call .badLineErrorMsg
    ;Reset all values to OEM defaults
    movzx eax, byte [BUFFERS]
    mov qword [rbp - cfgFrame.newBuffers], rax
    movzx eax, byte [FILES]
    mov qword [rbp - cfgFrame.newSFTVal], rax
    mov qword [rbp - cfgFrame.newFCBSVal], fcbsDefault
    mov qword [rbp - cfgFrame.newProtFCBSVal], safeFcbsDeflt
    movzx eax, byte [LASTDRIVE]
    mov qword [rbp - cfgFrame.newLastdrive], rax
    jmp .cfgExit
    
.badLineErrorMsg:
;Prints an error message BUT DOES NOT TERMINATE PARSING
    push rax
    push rdx
    lea rdx, .speLine
    mov eax, 0900h
    int 21h
    pop rdx
    pop rax
    return
.speLine:   db CR,LF,"Unrecognised command in CONFIG.SYS",CR,LF,"$"
.keyTbl: 
    db 5, "BREAK"           ;DONE
	dw .breakHandler - .keyTbl
    db 7, "BUFFERS"         ;DONE
	dw .bufHandler - .keyTbl
	db 7, "COUNTRY"         ;Ignored for now
	dw .countryScan - .keyTbl
	db 6, "DEVICE"          ;DONE
	dw .drvLoader - .keyTbl
	db 4, "FCBS"            ;Ignored for now
	dw .fcbHandler - .keyTbl
	db 5, "FILES"           ;DONE
	dw .sftHandler - .keyTbl
	db 9, "LASTDRIVE"       ;DONE
	dw .lastdriveHandler - .keyTbl
	db 5, "SHELL"           ;Ignored for now
	dw .shellHandler - .keyTbl
	db 6, "STACKS"          ;Ignored for now
	dw .stacksHandler - .keyTbl
    db 8, "DRIVPARM"
    dw .drivParm - .keyTbl  ;Ignored for now
	db -1	;End of table marker
.cfgSkipLeadingSpaces:
;Input: rsi -> Start of string to skip spaces/tabs of
;Output: rsi -> First non-space char in string
    push rax
.csls1:
    lodsb
    cmp al, SPC
    je .csls1
    cmp al, TAB
    je .csls1
    pop rax
    dec rsi
    return

.breakHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 6  ;Go past BREAK=
    ;This must be the word ON or OFF 
    call .cfgSkipLeadingSpaces
    xor edx, edx    ;Clear CF and default to OFF
    cmp word [rsi], "ON"
    je .breakOn
    cmp word [rsi], "OF"
    jne .breakBad
    cmp byte [rsi + 2], "F"
    je .breakCommon
.breakBad:
    call .badLineErrorMsg
    return
.breakOn:
    inc edx ;Go from OFF to ON  (keeps CF=NC)
.breakCommon:
    mov eax, 3301h  ;Set break to value in dl
    int 21h
    return

.bufHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 8  ;Go past BUFFERS=
    ;This must be at most three digits, anything else is a failure
    call .cfgSkipLeadingSpaces
    mov rdi, rsi    ;Save the start in rdi
    xor ecx, ecx
    lodsb   ;Get the first char. Must be between ASCII '0' and '9'
    cmp al, "0"
    jb .bufHandlerErr
    cmp al, "9"
    ja .bufHandlerErr
    inc ecx ;Increment char counter
    lodsb   ;Get second char
    call .bufHandlerTermCheck
    je .bufHandlerProcess   ;If it is a terminating char, exit
    cmp al, "0"
    jb .bufHandlerErr
    cmp al, "9"
    ja .bufHandlerErr
    lodsb   ;Check no more chars!
    call .bufHandlerTermCheck
    jne .bufHandlerErr
.bufHandlerProcess:
    xor edx, edx    ;Accumulate value in edx
    mov rsi, rdi    ;Go back to the first number
.bufHandlerLp:
    lodsb   ;Get the digit
    sub al, "0" ;Convert to ASCII
    movzx eax, al
    jecxz .bufHandlerPrepExit   ;Exit if this is the only digit
    shl eax, 1  ;Multiply by 2
    lea edx, dword [4*eax + eax]    ;Multiply (2*eax) by 5
    lodsb   ;Get the next digit
    sub al, "0"
    movzx eax, al
    add eax, edx    ;Add the tens to the unit
.bufHandlerPrepExit:
    movzx ecx, byte [BUFFERS]
    test eax, eax
    cmovz eax, ecx  ;Replace zero with default if the user specified 0 buffers
    mov qword [rbp - cfgFrame.newBuffers], rax
    clc
    return
.bufHandlerMul:
    sub al, "0" ;Convert to a binary value
    mul cl  ;Multiply al by cl, answer in ax
    movzx eax, ax
    add edx, eax
    return
.bufHandlerErr:
    call .badLineErrorMsg
    return
.bufHandlerTermCheck:
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, CR
    rete
    cmp al, LF
    rete
    return

;===============================
;   Device Driver Loader here  :
;===============================
.drvLoader:
;    mov rsi, rdx    ;Save the ptr to past the end of the line in rsi
    mov rdi, qword [rbp - cfgFrame.linePtr]
    add rdi, 7  ;Go past DEVICE= to the pathname
    mov rdx, rdi    ;Prepare rdx for the open
    mov eax, SPC
    push rcx
    xor ecx, ecx
    dec ecx
    repe scasb      ;Skip leading spaces for name (between = and first char)
    pop rcx
;Now search for the first char after pathname. 
    mov rsi, rdi
.drvFindEndOfFileName:
    lodsb ;Get char from string name
    ;Was the char a primitive string terminator?
    cmp al, SPC
    je short .fileNameFound
    cmp al, EOF
    je short .fileNameFound
    cmp al, CR
    je short .fileNameFound
    cmp al, LF
    jne short .drvFindEndOfFileName
.fileNameFound:
    dec rsi ;Point rdi to the space itself
    mov qword [rbp - cfgFrame.driverBreak], rsi
    movzx eax, byte [rsi]   ;Get the original breakchar
    mov qword [rbp - cfgFrame.breakChar], rax  ;And save it
    mov byte [rsi], 0   ;Null terminate the path to the file
    ;rdx -> Filename
    ;Here open the file to attempt to see how much space to 
    ; allocate to the file for loading. 
    ;Consider using the 4B01h loading mode instead and swapping back
    ; the current DTA and PSP to DOS default? This gives each driver a PSP
    ; which would allow for opening of files independently of calling programs'
    ; file table... maybe try it after getting 4B03h load to work first!
    mov eax, 3D00h  ;Read only file
    int 21h
    jc .drvBad
    movzx ebx, ax   ;Get the handle in ebx
    xor edx, edx    ;Move the handle to the end of the file
    mov eax, 4202h  ;LSEEK to SEEK_END
    int 21h
    mov esi, eax    ;Save the file size in esi
    xor edx, edx    ;Move the handle to the start of the file
    mov eax, 4200h  ;LSEEK to SEEK_SET (start of the file)
    int 21h
    push rbx        ;Push the file handle on the stack
    mov ebx, 6      ;6 paragraphs (96 bytes)
    mov eax, 4800h  ;Allocate this block of memory
    int 21h
    pop rbx         ;Get the handle back in rbx
    jc .drvMemClose
    mov rdx, rax    ;Get pointer to memory in rdx
    mov ecx, imageDosHdr_size
    mov eax, 3F00h  ;READ
    int 21h
    mov r8, rdx     ;Store the pointer to the memory block in r8 if need to free
    mov rdi, rdx    ;Get pointer to the EXE header
    jnc short .headerReadOK
.drvFreeMemAndHdl: ;Frees the block and then handle
    ;r8 must point to the block to free
    mov eax, 4900h  ;Free the block first!
    int 21h
    jmp .drvBadClose
.headerReadOK:
;Use register r10 as the indicator for .COM or .EXE. Set if COM.
    mov rdi, rdx    ;Save the pointer in rdi
    ;First check this file is MZ/ZM. If this is not, we assume its a .COM driver
    cmp word [rdi], dosMagicSignature
    je short .exeDrivers
    cmp word [rdi], dosMagicSignature2
    je short .exeDrivers
;.COM drivers come down here
    ;Get File Image Allocation Size in ecx here.
    ;Must be leq than 64Kb, rounded to nearest paragraph if .COM
    xor ecx, ecx
    xor edx, edx
    mov eax, 4202h  ;LSEEK from the end of the file
    int 21h
    ;eax now has the filesize. 
    mov ecx, eax
    and ecx, ~0Fh   ;Clear lower byte
    add ecx, 1h     ;... and round up!
    shr ecx, 4      ;Convert to paragraphs
    cmp ecx, 10000h ;Is it greater than 64k?
    jae .drvFreeMemAndHdl
    jmp .loadCont
.exeDrivers:
    ;Get the file pointer for file header
    mov edx, dword [rdi + imageDosHdr.e_lfanew] ;Get this file offset
    xor ecx, ecx
    mov eax, 4200h  ;LSEEK from the start of the file
    int 21h
    ;Now read in imageFileHeader here
    mov rdx, rdi    ;Overwrite the 16-bit header
    mov ecx, imageFileHeader_size   ;Read the header
    mov eax, 3F00h  ;READ
    int 21h
    jc short .drvFreeMemAndHdl
    cmp eax, imageFileHeader_size   ;If fewer bytes were read, fail
    jb short .drvFreeMemAndHdl
    cmp dword [rdi + imageFileHeader.dPESignature], imagePESignature
    jne .drvFreeMemAndHdl
    cmp word [rdi + imageFileHeader.wMachineType], imageFileMachineAMD64
    jne .drvFreeMemAndHdl
    cmp word [rdi + imageFileHeader.wSizeOfOptionalHdr], 60
    jb .drvFreeMemAndHdl ;We need section alignment info if a .EXE!
    ;Now read the first 60 bytes of the optional header here. rdx points to buffer
    mov ecx, 60     ;Read only 60 bytes
    mov eax, 3F00h  ;READ
    int 21h
    jc .drvFreeMemAndHdl   ;If something goes wrong, skip
    cmp eax, 56
    jb .drvFreeMemAndHdl   ;If fewer than 60 bytes read, skip
    ;Round up size requirement.
    ;If .EXE, round up to nearest section alignment
    mov ecx, dword [rdi + imageFileOptionalHeader.dSizeOfImage] ;Get mem alloc size
    mov eax, dword [rdi + imageFileOptionalHeader.dSectionAlignment]
    mov esi, eax    ;Save in esi the alignment requirement
    dec eax         ;Set bits to strip, clear all other bits
    not eax         ;Flip the set and clear bits
    and ecx, eax    ;Now clear the bits to clear from size, aligning downwards
    add ecx, esi    ;Now round upwards!
    shr ecx, 4      ;Convert to number of paragraphs.
    cmp ecx, 20000000h  ;Drivers cannot be more than 2Gb in size.
    jae .drvFreeMemAndHdl
.loadCont:
    mov eax, 4900h  ;FREE -> Free the 6 paragraph header buffer.
    int 21h ;r8 has the pointer to the block for freeing
    ;Now close the file
    mov eax, 3E00h  ;Close handle in ebx
    int 21h
    mov ebx, ecx    ;Put the number of paragraphs in ebx
    mov eax, 4800h  ;Allocate this block of memory
    int 21h         ;rax gets the pointer to load the program into
    jc .drvMemClose
    ;Now set the subsystem marker and the owner to DOS
    mov byte [rax - mcb_size + mcb.subSysMark], mcbSubDriver  ;Mark as occupied by driver
    mov qword [rax - mcb_size + mcb.owner], mcbOwnerNewDOS
    ;Build the overlay command block
    lea rbx, cmdBlock
    mov qword [rbx + loadOvly.pLoadLoc], rax
    mov qword [rbx + loadOvly.qRelocFct], rax
    mov rdx, qword [rbp - cfgFrame.linePtr] ;Get the pointer to the 
    add rdx, 7  ;Go past DEVICE= to the null terminated pathname
    mov eax, 4B03h  ;Load overlay!
    int 21h
    jnc short .loadOk   ;Driver loaded and unpacked. Now we get going...
.badDriverLoad:
    mov r8, qword [cmdBlock + loadOvly.pLoadLoc] ;Get the address of this 
    mov eax, 4900h  ;FREE -> Free the space where the program shouldve gone
    int 21h
    lea rdx, .drvMemMsg
    mov eax, 0900h
    int 21h
    return
.drvMemMsg: db CR,LF,"Not enough memory for driver",CR,LF,"$" 
.loadOk:
    ;Use driver load routines. Get the first byte of the MCB (where prog is loaded).
    mov rsi, qword [rbx + loadOvly.pLoadLoc]
    mov r8, rsi  ;Get the pointer to the MCB arena in r8 for later!
    ;Reset the command line to have a space at the null terminator
    mov rax, qword [rbp - cfgFrame.driverBreak]
    push rbx
    mov bl, byte [rbp - cfgFrame.breakChar] ;Get the original breakchar
    mov byte [rax], bl  ;and replace the null terminator
    pop rbx
    ;Remember, the first byte of the overlay is the driver header. 
    ;Hence, rsi points to that byte!
    ;Pointers of each header need adjustment relative to their load address,
    ; and linking into the main driver chain after NUL.
    ;r11 = Local var, if no drivers in file passed init, free allocation.
    ;                 Else, free using kernel eject routine.
    push rsi    ;Save the pointer to the first pointer to adjust
.driverPtrAdjustment:
    add qword [rsi + drvHdr.strPtr], rsi
    add qword [rsi + drvHdr.intPtr], rsi
    cmp qword [rsi + drvHdr.nxtPtr], -1
    je short .driverPtrAdjustmentDone
    add qword [rsi + drvHdr.nxtPtr], rsi
    mov rsi, qword [rsi + drvHdr.nxtPtr]
    jmp short .driverPtrAdjustment
.driverPtrAdjustmentDone:
    pop rsi     ;Get back the pointer to the first driver header
    ;Prepare for initialising the drivers in the arena
    ;EXPERIMENT: USING R9-R11 UNTIL THE END OF THE FUNCTION
    mov r9, rsi     ;Save a copy of the driver pointer in r9
    mov r11, mcbOwnerNewDOS ;Set currentPSP for new dos object
    xchg r11, qword fs:[currentPSP] ;Save in r11 old owner
    lea rbx, initDrvBlk
    mov rax, qword [rbp - cfgFrame.linePtr] ;Get the line pointer
    mov qword [rbx + initReqPkt.optptr], rax ;and pass to driver!
    mov r12, qword [rbp - cfgFrame.oldRBP]  ;Get DOSSEG in r12
.driverInit:
    xchg r12, rbp
    call initDriver
    jc short .driverBadRbpAdjust
    call addDriverMarkers
    xchg r12, rbp
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz short .driverInitialised
    call buildDPBs          ;Preserves rbp, rsi and rbx
    jc short .driverBad
.driverInitialised:
    cmp qword [rsi + drvHdr.nxtPtr], -1     ;We at the end of the chain?
    cmovne rsi, qword [rsi + drvHdr.nxtPtr]    ;Walk rsi if not
    jne short .driverInit ;If not, goto next driver
;Now we eject the init routines for the driver
;r8 points to the MCB data area already
    xor ebx, ebx
    mov ebx, dword [r8 - mcb_size + mcb.blockSize] ;Get the size of the arena in paragraphs
    shl rbx, 4  ;Turn into number of bytes
    sub r8, mcb_size    ;Point to the mcb header proper
    lea rbx, qword [r8 + rbx + mcb.program] ;Get pointer to the end of the arena
    call ejectKernelInit    ;Ignore any errors in ejection.
    ;Link into main driver chain, 
    ;r9 points to first driver in block
    ;rsi points to last driver in block
    mov rdi, qword [rbp - cfgFrame.oldRBP]  ;Get DOSSEG ptr
    lea rdi, qword [rdi + nulDevHdr] ;Get ptr to first driver
    mov rax, qword [rdi + drvHdr.nxtPtr]    ;Get the link
    mov qword [rdi + drvHdr.nxtPtr], r9     ;Link new drivers in
    mov qword [rsi + drvHdr.nxtPtr], rax    ;Link end to old chain
.driverExit:
;Exit the init routine if it all works out, WOO!
;Return values to original registers/memory locations
    mov qword fs:[currentPSP], r11
    clc
    return
.driverBadRbpAdjust:
    mov rbp, r12
.driverBad:
    ;Form the string to print
    lea rdi, .driverBad2    ;Store the name here
    test word [rsi + drvHdr.attrib], devDrvChar ;Are we a char dev?
    jnz short .driverCharBad    ;If not, exit
    ;MSD devices need to have something placed in there
    mov rax, "MSD dev "
    stosq   ;Store the 8 chars here
.driverCharBad:
    lea rsi, qword [rsi + drvHdr.drvNam]    ;Copy the device driver name over
    movsq   ;Move all 8 chars over from device driver name
.driverBadPrint:
    lea rdx, .driverBad1
    mov eax, 0900h  ;Print the string!
    int 21h
    mov eax, 4900h  ;Attempt to deallocate the driver now
    int 21h
    jmp short .driverExit
.driverBad1 db CR,LF,"Error initialising driver: "
.driverBad2 db "        ",CR,LF,"$"
;------------------
;Bad exit cases
;------------------
.drvBadClose:
    mov eax, 3E00h  ;Close handle in ebx
    int 21h
.drvBad:
    lea rdx, .drvBadMsg
.drvBad2:
    mov eax, 0900h
    int 21h
    clc ;Never return with CF=CY
    return
.drvMemClose:
    mov eax, 3E00h  ;Close handle in ebx
    int 21h
    lea rdx, .drvMemMsg
    jmp short .drvBad2

.drvBadMsg: db CR,LF,"Bad or missing filename",CR,LF,"$"

.sftHandler:
;This reads the line to set the number of FILE to between 1 and 254
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 6  ;Go past FILES=
    call .cfgSkipLeadingSpaces
    ;This must be at most three digits, anything else is a failure
    mov rdi, rsi    ;Save the start in rdi
    xor ecx, ecx
    lodsb   ;Get the first char. Must be between ASCII '0' and '9'
    cmp al, "0"
    jb .sftHandlerErr
    cmp al, "9"
    ja .sftHandlerErr
    inc ecx ;Increment char counter
    lodsb   ;Get second char
    call .sftHandlerTermCheck
    je .sftHandlerProcess   ;If it is a terminating char, exit
    cmp al, "0"
    jb .sftHandlerErr
    cmp al, "9"
    ja .sftHandlerErr
    inc ecx ;Increment char counter
    lodsb   ;Get third char
    call .sftHandlerTermCheck
    je .sftHandlerProcess   ;If it is a terminating char, exit
    cmp al, "0"
    jb .sftHandlerErr
    cmp al, "9"
    ja .sftHandlerErr
    lodsb   ;Check no more chars!
    call .sftHandlerTermCheck
    jne .sftHandlerErr
    inc ecx ;Increment char counter
.sftHandlerProcess:
    xor edx, edx    ;Accumulate value in edx
    mov rsi, rdi    ;Go back to the first number
.sftHandlerLp:
    dec ecx
    lodsb   ;Get the digit
    call .sftHandlerMul
    jecxz .sftHandlerPrepExit
    jmp short .sftHandlerLp 
.sftHandlerPrepExit:
;edx has the value now, so place it in stack
    cmp edx, 254
    ja .sftHandlerErr       ;DOS maximum number of files
    movzx ecx, byte [FILES] ;Get default if the user specifies less than min
    cmp edx, 8              ;DOS minimum number of files
    cmovb edx, ecx
    mov qword [rbp - cfgFrame.newSFTVal], rdx
    clc
    return
.sftHandlerMul:
;Input:
;al = digit to add to result (ASCII digit)
;ecx = whether al is a unit (0), ten (1) or hundred (2)
;rdx = Accumulated sum
;Output:
;rdx = Accumulated sum with al added
;eax is destroyed
;All other registers preserved
    push rcx
    push rdx
    movzx eax, al
    sub eax, '0' ;Convert to a binary digit
    mov edx, 1    ;Get 1 in edx (multiplicative unit)
    cmp ecx, edx  ;Was cl a ten?
    mov ebx, 10     ;Default base offset to 10
    mov ecx, 100
    cmovb ebx, edx  ;If below, it was a unit
    cmova ebx, ecx  ;If above, it was a hundred
    mul ebx    ;Multiply the base offset to eax
    pop rdx ;Get the accumulated value back
    add edx, eax    ;Add this result over
    pop rcx
    return
.sftHandlerErr:
    call .badLineErrorMsg
    return
.sftHandlerTermCheck:
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, CR
    rete
    cmp al, LF
    rete
    return

.lastdriveHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 10  ;Go past LASTDRIVE=
    call .cfgSkipLeadingSpaces
    lodsb   ;Get this char
    movzx eax, al   ;Zero extend to eax
    push rax    ;Push on stack
    mov eax, 1213h  ;Uppercase the char
    int 2Fh
    pop rbx
    cmp al, "Z"
    ja .sftHandlerErr
    cmp al, "A"
    jb .sftHandlerErr
    cmp byte [rsi], CR
    je .ldProceed
    cmp byte [rsi], LF
    je .ldProceed
    cmp byte [rsi], TAB
    je .ldProceed
    cmp byte [rsi], SPC
    jne .sftHandlerErr
.ldProceed:
    sub al, "@"     ;Convert into a number 1-26 
    movzx eax, al   ;Zero extend in case DOS rets something dumb in upper bits
    movzx edx, byte [LASTDRIVE]
    cmp eax, edx
    cmovb eax, edx
    mov qword [rbp - cfgFrame.newLastdrive], rax
    clc
    return
.ldBad:
    stc
.countryScan:
.fcbHandler:
.shellHandler:
.stacksHandler:
.drivParm:
    return

.cfgExit:
    mov rbx, qword [rbp - cfgFrame.cfgHandle] ;Get the handle back
    mov eax, 3E00h    ;Close the handle
    int 21h ;bx already has the handle
    mov r8, qword [rbp - cfgFrame.linePtr]   ;Get the line buffer ptr back
    mov eax, 4900h  ;FREE
    int 21h
    return