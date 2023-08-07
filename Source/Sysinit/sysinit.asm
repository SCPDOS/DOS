
sysinit:    ;Control is passed here from OEMINIT
    call OEMINIT    ;First we call OEMINIT
    jc OEMHALT      ;If it returns CF=CY, assume halt boot
;First move the OEMINIT into fs 
    mov ecx, 0C0000100h ;Write FS MSR
    mov rdi, qword [FINALDOSPTR] ;Pointer of the address we loaded at
    mov eax, edi
    mov rdx, rdi
    shr rdx, 20h
    wrmsr   ;Write the new value to FS MSR
    mov rbp, rdi    ;Move the pointer into rbp as well
;------------------------------------------------;
;      Copy DOS to it's final resting ground     ;
;------------------------------------------------;  
dosReloc:
    lea rsi, section.resSeg.start   ;Get pointer to the start of resSeg
    mov rdi, qword [FINALDOSPTR]    ;Get ptr to where dSeg goes
    add rdi, dSegLen                ;Make this a pointer to the start of resSeg
    cmp rdi, rsi 
    je short skipDOSReloc   ;Skip relocating if DOS is at correct address
    mov ecx, (dosLen + 7)/8
    rep movsq
skipDOSReloc:
;------------------------------------------------;
;              Connect Debugger                  ;
;------------------------------------------------;
%if DEBUG
;Only connect if in debug mode
    mov eax, 0C501h ;Connect debugger
    int 35h
%endif
;------------------------------------------------;
;           Sanitise the data area               ;
;------------------------------------------------;
; This call initialises ALL fields in the DOS    ;
; data area with 0's. Thus any fields, which     ;
; need to be initialised with a 0 value, are     ;
; initialised for free.                          ;
;------------------------------------------------;
    mov rdi, qword [FINALDOSPTR]    ;Go back to the data area
    mov ecx, dSegLen
    xor al, al
    rep stosb   ;and sanitise away!
;------------------------------------------------;
;          Kernel inits and adjustments          ;
;------------------------------------------------;
;Adjust Interrupt Entries Int 00h-15h
;Assumes rbp points to DOSSEG
    sidt [localIDTpointer]   ;Get the idt pointer here
adjExceptions:
    lea rdi, exceptData
    xor eax, eax            ;Start with interrupt 0
    mov ecx, 21
    call setupInterruptBlock
;Adjust Interrupt Entries Int 40h-49h
adjInts:
    lea rdi, intData
    mov eax, 40h            ;Start with interrupt 40h
    mov ecx, 50h
    call setupInterruptBlock
;++++++++++++++++++++++++++++++++++++++++++++++++;
;    DOS INTERRUPTS CAN BE USED FROM HERE ON     ;
;++++++++++++++++++++++++++++++++++++++++++++++++;
;Now adjust int 42h and 44h correctly using DOS to get them low
    lea rdx, OEMHALT ;Get segment start address
    mov eax, 2542h  ;Int 42, set vector
    int 41h
    lea rdx, OEMHALT ;Get segment start address
    mov eax, 2544h
    int 41h
;------------------------------------------------;
;          Driver Adjustments and inits          ;
;------------------------------------------------;
;Modify the pointers in nData before putting them in the data area
    add qword [nData + drvHdr.strPtr], rbp
    add qword [nData + drvHdr.intPtr], rbp
;Copy the Null driver header to its location in Sysvars
    mov ecx, drvHdr_size
    lea rsi, qword [nData]
    lea rdi, qword [rbp + nulDevHdr]
    rep movsb   

;Adjust the addresses in the other driver headers 
    mov rsi, qword [OEMDRVCHAIN]
    mov qword [rbp + nulDevHdr + drvHdr.nxtPtr], rsi  ;Point NUL to the OEM driver chain
adjDrivers:
;Input: rsi = Effective address of driver in DOS segment
;       rbp = Ptr to the start of the DOS segment
;Output: rsi = EA of next header in DOS segment
    add qword [rsi + drvHdr.strPtr], rbp
    add qword [rsi + drvHdr.intPtr], rbp
    cmp qword [rsi + drvHdr.nxtPtr], -1 ;End of chain?
    je short .exit
    add qword [rsi + drvHdr.nxtPtr], rbp    ;Adjust address
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Dont "demand" ctguos headers... 
    ;add rsi, drvHdr_size   ;... but definitely suggest it for kernel drivers
    jmp short adjDrivers
.exit:
;------------------------------------------------;
;                   MCB inits                    ;
;------------------------------------------------;
makeMCBChain:
;Fill in Anchor MCB first
    lea rax, qword [rbp + anchorMcb + mcb.program]    ;Get first allocated byte
    lea rbx, qword [rbp + dosEnd] ;Get the end of the file
    sub rbx, rax    ;Number of bytes in rbx (ebx bzw.)
    add ebx, 0Fh    ;Round up if not para aligned
    shr ebx, 4      ;Get number of paragraphs
    mov dword [rbp + anchorMcb + mcb.blockSize], ebx
    mov qword [rbp + anchorMcb + mcb.owner], mcbOwnerDOS
    mov byte [rbp + anchorMcb + mcb.marker], mcbMarkEnd

    lea rax, qword [rbp + anchorMcb]    ;Now store the anchor pointer in var
    mov qword [MCBANCHOR], rax          ;Save in external var...
    mov qword fs:[mcbChainPtr], rax     ; and in internal DOS var 

    push rbp    ;Save the pointer to DOSSEG on the stack temporarily
    call OEMMCBINIT ;Build MCB chain
    pop rbp
    jc OEMHALT
;------------------------------------------------;
;              Kernel Driver inits               ;
;------------------------------------------------;
kernDrvInit:
    ;rbp and fs point to DOSSEG
    ;Set the CON pointer and the CLOCK pointers.
    ;The standard defines that kernel drivers are such that the 
    ;first driver MUST be CON and the fourth MUST be CLOCK$.
    ;This is done to allow the drivers to use DOS CHAR functions and 
    ;GET/SET TIME and GET/SET DATE
    mov rsi, qword [OEMDRVCHAIN]    ;Get the first driver in the chain
    mov rbx, rsi
    mov qword [rbp + vConPtr], rsi  ;Store default CON ptr
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Goto AUX
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Goto PRN
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Goto CLOCK$
    mov qword [rbp + clockPtr], rsi ;Store default CLOCK$ ptr
    mov rsi, rbx     ;Point rsi back to head of device chain
    lea rbx, initDrvBlk
    ;The following is to mark all kernel driver allocs as new DOS
    mov qword [rbp + currentPSP], mcbOwnerNewDOS
.init:
    call initDriver         ;Importantly preserves rbp, rsi and rbx
    jc OEMHALT
    call addDriverMarkers   ;Preserves all registers
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz short .notMSD
    call buildDPBs          ;Preserves rbp, rsi and rbx
    jc OEMHALT
.notMSD:
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Now point rsi to that header
    cmp rsi, -1     ;We at the end of the chain?
    jne short .init ;If not, goto next driver
;Finally, Eject the init routine if so desired by the implementers
    lea rbx, qword [rbp + dosEnd]   ;Get the original alloc end pointer (para aligned)
    lea r8, qword [rbp + anchorMcb]   ;Get pointer to mcb
    call ejectKernelInit
    jc OEMHALT
;----------------------------------------:
;           End of driver inits.         :
;----------------------------------------:
;
;----------------------------------------:
;END OF IMPLEMENTERS SYSINIT PORTION ^^^ :
;      So called the OEM BIOS INIT       :
;----------------------------------------:
;
;----------------------------------------:
;START OF COMMON DOS SYSINIT PORTION VVV :
;----------------------------------------:
;
;Setup internal DOS vars from OEM passed arguments.
    movzx eax, byte [DFLTDRIVE]
    xor ebx, ebx
    cmp eax, 25
    cmova eax, ebx
    mov byte [rbp + bootDrive], al

    movzx eax, byte [FILES]
    mov ebx, filesDefault
    cmp eax, 5
    cmovb eax, ebx
    mov byte [rbp + numFiles], al

    movzx eax, byte [BUFFERS]
    mov ebx, buffersDefault
    test eax, eax
    cmovz eax, ebx
    mov byte [BUFFERS], al

    movzx eax, byte [LASTDRIVE]
    mov ebx, lastDriveDeflt
    cmp eax, ebx
    cmovb eax, ebx
    mov byte [LASTDRIVE], al
    mov byte [rbp + lastdrvNum], al     ;Set for DOS to be usable

    mov word [rbp + shareCount], 3      ;Retry the repeat 3 times before failing
    mov word [rbp + shareDelay], 1      ;Go through one multiple of countdown loop
;------------------------------------------------;
;          Find largest sector size              ;
;------------------------------------------------;
sectorSizeSearch:
;Done by reading DPB's for each drive
    xor eax, eax
    mov rdx, qword fs:[dpbHeadPtr]  ;Get ptr to first DPB
    ;Go thru each block individually
.findLargest:
    cmp ax, word [rdx + dpb.wBytesPerSector]    ;Is current bigger than max?
    cmovb ax, word [rdx + dpb.wBytesPerSector]  ;Move if so
    mov rdx, qword [rdx + dpb.qNextDPBPtr]  ;Goto next DPB
    cmp rdx, -1 ;We at the end?
    jne short .findLargest  ;If not, keep checking
    mov word fs:[maxBytesSec], ax
;------------------------------------------------;
;                CDS array inits                 ;
;------------------------------------------------;
    movzx ecx, byte [rbp + lastdrvNum]     ;Use as a counter
    call makeCDSArray   ;Sets the CDS head pointer to rdi
    jmp initialCDSWritten ;Go past the function
makeCDSArray:
;Builds a new CDS array for ya and sets the sysvars var to point to it!
;Input: ecx = Size of array (number of CDS's in the array)
;Ouput: CF=CY: Abort operation. CF=NC: CDS Array allocated ok!
    mov eax, cds_size
    mul ecx ;eax has the size of the CDS array to make
    add eax, 0Fh    ;Round up if not on a para boundary
    shr eax, 4      ;Convert to paragraphs
    xor ebx, ebx
    mov ebx, eax
    mov eax, 4800h  ;ALLOC  (current owner is mcbOwnerNewDOS)
    int 41h
    retc    ;Return if Carry set
    mov rdi, rax            ;Save pointer to MCB in rdi
    sub rax, mcb_size       ;Move rax to point to MCB
    mov byte [rax + mcb.subSysMark], mcbSubCDS  ;Mark as a CDS array
    mov qword [rax + mcb.owner], mcbOwnerDOS    ;Mark as owned by DOS

    mov qword fs:[cdsHeadPtr], rdi
    push rdi
    push rcx
    mov eax, ecx
    mov ecx, cds_size
    mul ecx ;Multiply eax with ecx to get number of bytes to null out
    mov ecx, eax
    xor eax, eax
    rep stosb
    pop rcx
    pop rdi
    mov rbx, qword fs:[dpbHeadPtr]
    mov eax, 005C3A41h      ;"A:\"+NULL char
.tempCDS:
    mov dword [rdi + cds.sCurrentPath], eax
    mov qword [rdi + cds.qDPBPtr], rbx
    mov dword [rdi + cds.dStartCluster], 0  ;Root dir for all!
    mov word [rdi + cds.wBackslashOffset], 2    ;Skip the X:
    xor edx, edx    ;Use edx for flags
    cmp rbx, -1 ;Is rbx an invalid DPB ptr?
    je .skipValidCDS
    mov edx, cdsValidDrive  ;If not, set drive to valid and...
    mov rbx, qword [rbx + dpb.qNextDPBPtr]  ;... go to next DPB
.skipValidCDS:
    mov word [rdi + cds.wFlags], dx ;Store the flags now
    inc eax ;Increment the drive letter
    add rdi, cds_size   ;Goto next array entry
    dec ecx
    jnz .tempCDS
    ret
initialCDSWritten:
;------------------------------------------------;
;     Set up general PSP areas and DOS vars      ;
;------------------------------------------------;
;Ensure to link the default DOS vCON edit key
; controller routines before proceeding
    lea rax, qword [rbp + editKeys]
    mov qword fs:[extKeyFunc], rax

;Additional DOS Vars init and fixups
    mov byte fs:[errorDrv], -1   ;No error drive
    mov byte fs:[switchChar], "/"  ;Default switch char
    lea rdi, qword [rbp + extAsciiTbl]  ;Get the load of dflt extascii tbl
    mov qword fs:[ctryTbl + countryStruc.mapaddr], rdi ;Store in country table

;Server Table setup
    lea rdi, qword [rbp + serverDispTbl]  ;Get pointer to table
    mov qword fs:[serverDispTblPtr], rdi   ;Store to use

;Set network machine name to... nothing!
    lea rdi, qword [rbp + machineName]
    mov ecx, 10h    ;16 chars long
    mov al, SPC ;Space char
    rep stosb   ;Fill with space chars

;Patch Data Table init
    lea rdi, qword [rbp + critPtchTbl]
    lea rax, qword [rbp + dosCrit1Enter]
    stosq   ;Store this address and increment rdi by 8 to next tbl entry
    lea rax, qword [rbp + dosCrit1Exit]
    stosq
    lea rax, qword [rbp + dosCrit2Enter]
    stosq
    lea rax, qword [rbp + dosCrit2Exit]
    stosq

;Initial PSP pointer fields
    lea rbx, qword [tempPSP]
    mov qword fs:[currentPSP], rbx    ;Save current PSP
    push rbx
    add rbx, psp.dta
    mov qword fs:[currentDTA], rbx    ;Save current DTA
    pop rbx
    mov qword [rbx + psp.parentPtr], rbx ;Save self as parent Process
    mov qword [rbx + psp.prevPSP], rbx  ;Save self as previous PSP
    mov rdx, rbx
    mov eax, 3542h  ;Get pointer for Int 42h in rbx
    int 41h
    mov qword [rdx + psp.oldInt42h], rbx
    mov eax, 3543h
    int 41h
    mov qword [rdx + psp.oldInt43h], rbx
    mov eax, 3544h
    int 41h
    mov qword [rdx + psp.oldInt44h], rbx
;------------------------------------------------;
;              Setup DOSMGR Hooks                ;
;------------------------------------------------;   
    lea rdi, qword [rbp + dosMgrHooks + 1]  ;Skip the present flag
    lea rax, qword [rbp + goodDfltShareHook]    ;Return CF = NC
    stosq   ;Store ptr for LaunchTask
    stosq   ;Store ptr for TerminateTask
;------------------------------------------------;
;              Setup DLLMGR Hooks                ;
;------------------------------------------------;   
    lea rdi, qword [rbp + dllHooks]
    lea rax, qword [rbp + goodDfltShareHook]    ;Return CF = NC
    stosq   ;Store ptr for RegisterDLL
    stosq   ;Store ptr for UnloadDLLHook
;------------------------------------------------;
;               Setup Share Hooks                ;
;------------------------------------------------;
    lea rdi, qword [rbp + shareHooks]
    lea rbx, qword [rbp + goodDfltShareHook]
    lea rax, qword [rbp + badDfltShareHook]
    stosq   ;Store bad for openFileCheck
    xchg rax, rbx
    stosq   ;Store good for open
    stosq   ;Store good for close
    xchg rax, rbx
;Store bad for close for machine, task, name, lock and unlock file
    mov ecx, 5
    rep stosq
    xchg rax, rbx
    stosq   ;Store good for check file lock exists
    xchg rax, rbx
;Store bad for open file, update fcb from sft and get fst cluster of fcb
    mov ecx, 3
    rep stosq
    xchg rax, rbx
    stosq   ;Store good for close dup file share
    xchg rax, rbx
    stosq   ;Store bad for close handles for new file opened 
    stosq   ;Store bad for update dir information
;------------------------------------------------;
;        Create a Default Temporary Buffer       ;
;------------------------------------------------;
    movzx ebx, word fs:[maxBytesSec]    ;Get buffer size
    add ebx, bufferHdr_size             ;add header size for allocation size
    add ebx, 0Fh
    shr ebx, 4  ;Convert to number of paragraphs
    mov eax, 4800h
    int 41h
    jc OEMHALT
    mov qword fs:[bufHeadPtr], rax      ;Save pointer to buffer
    mov qword [rax + bufferHdr.nextBufPtr], -1 ;Point to no buffer
    mov word [rax + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags    
    sub rax, mcb_size   ;Now go back to the mcb itself
    mov qword [rax + mcb.owner], mcbOwnerDOS    ;Set DOS as owner of Buffer
    mov byte [rax + mcb.subSysMark], mcbSubBuffers  ;Set this arena as a buffer
;------------------------------------------------;
;          Default File Handle Creation          ;
;                                                ;
;   Note: Devices are opened AUX, CON then PRN   ;
;------------------------------------------------;
defaultFileHandles:
    lea rdx, qword [rbp + firstSftHeader]
    mov qword fs:[sftHeadPtr], rdx  ;Start from this SFT header
    mov qword [rdx + sfth.qNextSFTPtr], -1
    mov word [rdx + sfth.wNumFiles], 5  ;This SFTH has space for 5 SFTs

    call openStreams
    jc OEMHALT
;------------------------------------------------;
;             Print Welcome Message              ;
;------------------------------------------------;
    lea rdx, strtmsg
    mov ah, 09h
    int 41h    
;------------------------------------------------;
;               Load CONFIG.SYS                  ;
;------------------------------------------------;
;Setup stackframe, workout base 
setupFrame:
    push rbp
    mov rbp, rsp
    sub rsp, cfgFrame_size
    movzx eax, byte [BUFFERS]
    mov qword [rbp - cfgFrame.newBuffers], rax
    movzx eax, byte [FILES]
    mov qword [rbp - cfgFrame.newSFTVal], rax
    mov qword [rbp - cfgFrame.newFCBSVal], fcbsDefault
    mov qword [rbp - cfgFrame.newProtFCBSVal], safeFcbsDeflt
    movzx eax, byte [LASTDRIVE]
    mov qword [rbp - cfgFrame.newLastdrive], rax

    movzx edx, byte [DFLTDRIVE]    ;Get the default drive
    mov ah, 0Eh ;Select drive
    int 41h
    lea rdx, cfgspec    ;CONFIG.SYS, must be on bootdrive for now
    mov ah, 3Dh ;Open file for reading
    mov al, ReadAccess
    int 41h
    jc noCfg  ;If no CONFIG.SYS found, just use defaults that are already setup
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
    int 41h
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
    int 41h
    jc .stopProcessError
    test ecx, ecx	;If this is zero, EOF reached
    jnz short .notEOF
    mov qword [rbp - cfgFrame.lastLine], -1	;Note we are at EOF
.notEOF:
    movzx eax, byte [rdx]
    cmp al, CR
    je short .endOfLine
    cmp al, LF
    je short .endOfLine
    cmp al, EOF
    je short .endOfFileChar
    push rax    ;Push rax on stack as the argument to normalise
    mov eax, 1213h  ;Uppercase the char if it is uppercasable
    int 4fh
    mov byte [rdx], al  ;Replace the char with the capitalised form
    pop rax ;Pop into rax to renormalise the stack
.notChar:
    inc rdx ;Now move our local pointer to the next byte
    jmp short .nextChar
.endOfFileChar:
    mov qword [rbp - cfgFrame.lastLine], -1	;Mark EOF and End of line
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
    breakpoint
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
    int 41h
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
    lea rdx, .speLine
    mov eax, 0900h
    int 41h
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
.breakHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 6  ;Go past BREAK=
    ;This must be the word ON or OFF 
    xor edx, edx    ;Clear CF and default to OFF
    cmp word [rsi], "ON"
    je .breakOn
    cmp word [rsi], "OF"
    jne .breakBad
    cmp byte [rsi + 2], "F"
    je .breakCommon
.breakBad:
    stc
    return
.breakOn:
    inc edx ;Go from OFF to ON  (keeps CF=NC)
.breakCommon:
    mov eax, 3301h  ;Set break to value in dl
    int 41h
    return

.bufHandler:
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 8  ;Go past BUFFERS=
    ;This must be at most three digits, anything else is a failure
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
    call .bufHandlerMul
    jecxz .bufHandlerPrepExit
    dec ecx
    jmp short .bufHandlerLp 
.bufHandlerPrepExit:
;edx has the value now, so place it in stack
    movzx ecx, byte [BUFFERS]
    test edx, edx
    cmovz edx, ecx  ;Replace zero with default if the user specified 0 buffers
    mov qword [rbp - cfgFrame.newBuffers], rdx
    clc
    return
.bufHandlerMul:
    sub al, "0" ;Convert to a binary value
    mul cl  ;Multiply al by cl, answer in ax
    movzx eax, ax
    add edx, eax
    return
.bufHandlerErr:
    stc
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
    breakpoint
    mov rsi, rdx    ;Save the ptr to past the end of the line in rsi
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
    dec rdi ;Point rdi to the space itself
    mov qword [rbp - cfgFrame.driverBreak], rdi
    movzx eax, byte [rdi]   ;Get the original breakchar
    mov qword [rbp - cfgFrame.breakChar], rax  ;And save it
    mov byte [rdi], 0   ;Null terminate the path to the file
    ;rdx -> Filename
    ;Here open the file to attempt to see how much space to 
    ; allocate to the file for loading. 
    ;Consider using the 4B01h loading mode instead and swapping back
    ; the current DTA and PSP to DOS default? This gives each driver a PSP
    ; which would allow for opening of files independently of calling programs'
    ; file table... maybe try it after getting 4B03h load to work first!
    mov eax, 3D00h  ;Read only file
    int 41h
    jc .drvBad
    movzx ebx, ax   ;Get the handle in ebx
    xor edx, edx    ;Move the handle to the end of the file
    mov eax, 4202h  ;LSEEK to SEEK_END
    int 41h
    mov esi, eax    ;Save the file size in esi
    xor edx, edx    ;Move the handle to the start of the file
    mov eax, 4200h  ;LSEEK to SEEK_SET (start of the file)
    int 41h
    push rbx        ;Push the file handle on the stack
    mov ebx, 6      ;6 paragraphs (96 bytes)
    mov eax, 4800h  ;Allocate this block of memory
    int 41h
    pop rbx         ;Get the handle back in rbx
    jc .drvMemClose
    mov rdx, rax    ;Get pointer to memory in rdx
    mov ecx, imageDosHdr_size
    mov eax, 3F00h  ;READ
    int 41h
    mov r8, rdx     ;Store the pointer to the memory block in r8 if need to free
    mov rdi, rdx    ;Get pointer to the EXE header
    jnc short .headerReadOK
.drvFreeMemAndHdl: ;Frees the block and then handle
    ;r8 must point to the block to free
    mov eax, 4900h  ;Free the block first!
    int 41h
    jmp .drvBadClose
.headerReadOK:
;Use register r10 as the indicator for .COM or .EXE. Set if COM.
    xor r10, r10    ;Clear r10 i.e. EXE format
    mov rdi, rdx    ;Save the pointer in rdi
    ;First check this file is MZ/ZM. If this is not, we assume its a .COM driver
    cmp word [rdi], dosMagicSignature
    je short .exeDrivers
    cmp word [rdi], dosMagicSignature2
    je short .exeDrivers
;.COM drivers come down here
    dec r10 ;Set r10 to -1 i.e. COM format. Driver pointers need adjusting.
    ;Get File Image Allocation Size in ecx here.
    ;Must be leq than 64Kb, rounded to nearest paragraph if .COM
    xor ecx, ecx
    xor edx, edx
    mov eax, 4202h  ;LSEEK from the end of the file
    int 41h
    ;edx:eax now has the filesize. 
    test edx, edx   ;edx must be 0
    jnz .drvFreeMemAndHdl
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
    int 41h
    ;Now read in imageFileHeader here
    mov rdx, rdi    ;Overwrite the 16-bit header
    mov ecx, imageFileHeader_size   ;Read the header
    mov eax, 3F00h  ;READ
    int 41h
    jc short .drvFreeMemAndHdl
    cmp eax, imageFileHeader_size   ;If fewer bytes were read, fail
    jb short .drvFreeMemAndHdl
    cmp dword [rdi + imageFileHeader.dPESignature], imagePESignature
    jne .drvFreeMemAndHdl
    cmp word [rdi + imageFileHeader.wMachineType], imageFileMachineAMD64
    jne .drvFreeMemAndHdl
    cmp word [rdi + imageFileHeader.wSizeOfOptionalHdr], 56
    jb .drvFreeMemAndHdl ;We need section alignment info if a .EXE!
    ;Now read the first 56 bytes of the optional header here
    mov rdx, rdi    ;Overwrite the 16-bit header
    mov ecx, 56     ;Read only 56 bytes
    mov eax, 3F00h  ;READ
    int 41h
    jc .drvFreeMemAndHdl   ;If something goes wrong, skip
    cmp eax, 56
    jb .drvFreeMemAndHdl   ;If fewer than 56 bytes read, skip
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
    int 41h ;r8 has the pointer to the block for freeing
    ;Now close the file
    mov eax, 3E00h  ;Close handle in ebx
    int 41h
    mov ebx, ecx    ;Put the number of paragraphs in ebx
    mov eax, 4800h  ;Allocate this block of memory
    int 41h         ;rax gets the pointer to load the program into
    jc .drvMemClose
    ;Now set the subsystem marker and the owner to DOS
    mov qword [rax - mcb_size + mcb.subSysMark], mcbSubDriver  ;Mark as occupied by driver
    ;Build the overlay command block
    lea rbx, cmdBlock
    mov qword [rbx + loadOvly.pLoadLoc], rax
    mov qword [rbx + loadOvly.qRelocFct], rax
    mov rdx, qword [rbp - cfgFrame.linePtr] ;Get the pointer to the 
    add rdx, 7  ;Go past DEVICE= to the null terminated pathname
    mov eax, 4B03h  ;Load overlay!
    int 41h
    jnc short .loadOk   ;Driver loaded and unpacked. Now we get going...
.badDriverLoad:
    mov r8, qword [cmdBlock + loadOvly.pLoadLoc] ;Get the address of this 
    mov eax, 4900h  ;FREE -> Free the space where the program shouldve gone
    int 41h
    lea rdx, .drvMemMsg
    mov eax, 0900h
    int 41h
    return
.drvMemMsg: db "Not enough memory for driver",CR,LF,"$" 
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
    je short .driverPtrAdjustmentDone
    add qword [rsi + drvHdr.nxtPtr], rsi
    mov rsi, qword [rsi + drvHdr.nxtPtr]
    jmp short .driverPtrAdjustment
.driverPtrAdjustmentDone:
    pop rsi     ;Get back the pointer to the first driver header
    ;Prepare for initialising the drivers in the arena
    ;EXPERIMENT: USING R9-R11 UNTIL THE END OF THE FUNCTION
    mov r9, rsi     ;Save a copy of the driver pointer in r9
    mov r10, rbp    ;Save a copy of the cfg frame pointer
    mov r11, mcbOwnerNewDOS ;Set currentPSP for new dos object
    xchg r11, qword fs:[currentPSP] ;Save in r11 old owner
    lea rbx, initDrvBlk
    mov rax, qword [rbp - cfgFrame.linePtr] ;Get the line pointer
    mov qword [rbx + initReqPkt.optptr], rax ;and pass to driver!
.driverInit:
    call initDriver
    jc short .driverBad
    call addDriverMarkers
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz short .driverInitialised
    call buildDPBs          ;Preserves rbp, rsi and rbx
    jc short .driverBad
.driverInitialised:
    cmp rsi, -1     ;We at the end of the chain?
    cmovne rsi, qword [rsi + drvHdr.nxtPtr]    ;Walk rsi if not
    jne short .driverInit ;If not, goto next driver
;Now we eject the init routines for the driver
;r8 points to the MCB already
    xor ebx, ebx
    mov ebx, dword [r8 + mcb.blockSize] ;Get the size of the arena in paragraphs
    shl rbx, 4  ;Turn into number of bytes
    lea rbx, qword [r8 + rbx + mcb.program] ;Get pointer to the end of the arena
    call ejectKernelInit    ;Ignore any errors in ejection.
    ;Link into main driver chain, 
    ;r9 points to first driver in block
    ;rsi points to last driver in block
    mov rdi, qword [r10 - cfgFrame.oldRBP]  ;Get DOSSEG ptr
    lea rdi, qword [rdi + nulDevHdr] ;Get ptr to first driver
    mov rax, qword [rdi + drvHdr.nxtPtr]    ;Get the link
    mov qword [rdi + drvHdr.nxtPtr], r9     ;Link new drivers in
    mov qword [rsi + drvHdr.nxtPtr], rax    ;Link end to old chain
.driverExit:
;Exit the init routine if it all works out, WOO!
;Return values to original registers/memory locations
    mov qword fs:[currentPSP], r11
    mov rbp, r10    ;Return rbp to original cfg frame
    return

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
    int 41h
    mov eax, 4900h  ;Attempt to deallocate the driver now
    int 41h
    jmp short .driverExit
.driverBad1 db "Error initialising driver: "
.driverBad2 db "        ",CR,LF,"$"
;------------------
;Bad exit cases
;------------------
.drvBadClose:
    mov eax, 3E00h  ;Close handle in ebx
    int 41h
.drvBad:
    lea rdx, .drvBadMsg
.drvBad2:
    mov eax, 0900h
    int 41h
    clc ;Never return with CF=CY
    return
.drvMemClose:
    mov eax, 3E00h  ;Close handle in ebx
    int 41h
    lea rdx, .drvMemMsg
    jmp short .drvBad2

.drvBadMsg: db "Bad or missing filename",CR,LF,"$"

.sftHandler:
;This reads the line to set the number of FILE to between 1 and 254
    mov rsi, qword [rbp - cfgFrame.linePtr]
    add rsi, 6  ;Go past FILES=
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
    cmp al, "2" ;Max BUFFERS=254 soooo, sorry buddy!
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
    mov ecx, filesDefault  ;Get default if the user specifies less than min
    cmp edx, 8
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
    stc
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
    lodsb   ;Get this char
    movzx eax, al   ;Zero extend to eax
    push rax    ;Push on stack
    mov eax, 1213h  ;Uppercase the char
    int 4Fh
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
    sub al, "A" ;Convert into a number
    movzx eax, al   ;Zero extend in case DOS rets something dumb in upper bits
    mov edx, lastDriveDeflt
    cmp eax, lastDriveDeflt
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
    int 41h ;bx already has the handle
    mov r8, qword [rbp - cfgFrame.linePtr]   ;Get the line buffer ptr back
    mov eax, 4900h  ;FREE
    int 41h
;------------------------------------------------;
;   Setup Final Data Areas With Overrides from   ;
;                  CONFIG.SYS                    ;
;------------------------------------------------;
;Add additional buffers.
;Add additional SFT entries.
;Add additional FCBS.
;Create a larger CDS if needed.
noCfg:
;Start with buffers:
    mov rcx, qword [rbp - cfgFrame.newBuffers]    ;Get new number of buffers
    cmp ecx, 1   ;If its only one buffer, skip as we have one already
    je .skipBuffers
    dec ecx  ;Minus one now
    mov byte fs:[numBuffers], cl    ;Store this value in var
    ;Now do the allocation at rdi. Each buffer = maxSectorSize + bufferHdr_size
    movzx eax, word fs:[maxBytesSec]    ;Get buffer sector size
    add eax, bufferHdr_size ;eax has the size to add
    push rax    ;Save the total number of bytes for a buffer and its header
    mul ecx ;Get total size to allocate in eax
    pop rdx     ;and get the total value back in rdx
    mov ebx, eax    ;Move the total number of bytes into ebx
    add ebx, 0Fh
    shr ebx, 4      ;And convert it to paragraphs
    mov eax, 4800h  ;ALLOC
    int 41h
    jc short .skipBuffers   ;If it fails to allocate, default to one buffer
    ;Each buffer has no flags, drive number must be -1
    mov rbx, rdx    ;Put the total number of bytes per buffer in rbx
    mov rdi, rax    ;Point rdi to the new area 
    sub rax, mcb.program    ;Point rax to the head of the mcb
    mov qword [rax + mcb.owner], mcbOwnerDOS    ;Mark as owned by DOS
    mov byte [rax + mcb.subSysMark], mcbSubBuffers  ;Buffer buffer (funny)
    mov rax, qword fs:[bufHeadPtr]  ;Get the pointer to the first buffer
    mov qword [rax + bufferHdr.nextBufPtr], rdi ;And set the new next buffer to point to it
    mov rsi, rdi    ;Points rsi to first new buffer space
    xor eax, eax    ;Use for sanitising buffer headers
    dec ecx         ;Reduce to convert from 1 based count to 0 based
    jecxz .lastBuffer
.bufferLoop:
    add rdi, rbx    ;Goto next buffer space
    mov qword [rsi + bufferHdr.nextBufPtr], rdi ;Point to next buffer
    mov word [rsi + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags
    mov qword [rsi + bufferHdr.bufferLBA], rax
    mov byte [rsi + bufferHdr.bufFATcopy], al
    mov dword [rsi + bufferHdr.bufFATsize], eax
    mov qword [rsi + bufferHdr.driveDPBPtr], rax
    mov qword [rsi + bufferHdr.owningFile], rax
    mov rsi, rdi    ;Move rsi to next buffer position
    dec ecx
    jnz .bufferLoop
.lastBuffer:
    add rdi, rbx    ;Goto past the last buffer
    mov qword [rsi + bufferHdr.nextBufPtr], -1 ;Point to no buffer
    mov word [rsi + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags
    mov qword [rsi + bufferHdr.bufferLBA], rax
    mov byte [rsi + bufferHdr.bufFATcopy], al
    mov dword [rsi + bufferHdr.bufFATsize], eax
    mov qword [rsi + bufferHdr.driveDPBPtr], rax
    mov qword [rsi + bufferHdr.owningFile], rax
.skipBuffers:
;Now build a new SFT header for the number of files specified by user
    mov rcx, qword [rbp - cfgFrame.newSFTVal]
    cmp ecx, 5  ;If we are not adding anything, skip building SFT
    jbe short .skipSFT
    ;First compute how big this new arena needs to be
    sub ecx, 5   ;Remove the default five files that are *always* present!
    mov eax, sft_size
    mul ecx ;Get number of files*size of file in bytes in eax
    add eax, sfth_size  ;Add the size of one SFT header
    mov ebx, eax        ;And move into ebx for the syscall
    add ebx, 0Fh        ;Round up to nearest paragraph...
    shr ebx, 4          ;And convert to paragraphs
    mov eax, 4800h
    int 41h
    jc short .skipSFT   ;Skip adding files if this fails. Sorry end user!
    mov rsi, qword fs:[sftHeadPtr]
    mov qword [rsi + sfth.qNextSFTPtr], rax ;RAX points to the next sfth
    mov word [rax + sfth.wNumFiles], cx ;Move remaining files here
    mov qword [rax + sfth.qNextSFTPtr], -1  ;Last table in chain    
    sub rax, mcb.program    ;Point to MCB now
    mov qword [rax + mcb.owner], mcbOwnerDOS
    mov byte [rax + mcb.subSysMark], mcbSubFiles
.skipSFT:
;FCBS now
    mov rcx, qword [rbp - cfgFrame.newFCBSVal]
    jecxz .skipFCBS ;Skip if no FCBS requested
    mov eax, sft_size
    mul ecx ;Get number of files*size of file in bytes in eax
    add eax, sfth_size  ;Add the size of one SFT header
    mov ebx, eax        ;And move into ebx for the syscall
    add ebx, 0Fh        ;Round up to nearest paragraph...
    shr ebx, 4          ;And convert to paragraphs
    mov eax, 4800h
    int 41h
    jc short .skipFCBS   ;Skip adding files if this fails. Sorry end user!
    mov qword fs:[fcbsHeadPtr], rax ;This is the FCBS head now
    mov word [rax + sfth.wNumFiles], cx ;Move FCBS here
    mov qword [rax + sfth.qNextSFTPtr], -1  ;Last table in chain  
    sub rax, mcb.program    ;Point to MCB now
    mov qword [rax + mcb.owner], mcbOwnerDOS
    mov byte [rax + mcb.subSysMark], mcbSubFCBS
.skipFCBS:
;And CDS now
    mov rcx, qword [rbp - cfgFrame.newLastdrive]
    cmp byte fs:[lastdrvNum], cl
    jae .skipCDS    ;If user specifies less than 5 drives, dont reallocate
    ;Else, we first free the old CDS and then reallocate
    mov r8, qword fs:[cdsHeadPtr]
    mov eax, 4900h  ;FREE the old allocation.
    int 41h
    jc short .skipCDS
    mov byte fs:[lastdrvNum], cl ;Save this value
    call makeCDSArray
.skipCDS:
    mov rsp, rbp    ;Return stack pointer to original position
    pop rbp ;Stack frame no longer needed
;Now we close all five default handles and open AUX, CON and PRN
; and reopen the handles as user may have loaded new CON/AUX/PRN etc drivers
    xor ebx, ebx
closeHandlesLoop:
    mov eax, 3e00h  ;Close
    int 41h
    inc ebx ;Goto next handle
    cmp ebx, 6
    jne closeHandlesLoop
    call openStreams
l1:
    mov ebx, 1000h  ;Get a 64Kb block
    mov eax, 4800h  ;Allocate the memory block
    int 41h         ;Malloc and get pointer in rbx
    jc badMem
    mov rbx, rax    ;Get pointer to block header to set owner to DOS
    sub rbx, mcb_size
    mov qword [rbx + mcb.owner], mcbOwnerDOS
    mov qword [OEMMEMPTR], rax   ;Save the pointer here
    lea r8, tempPSP ;Get the DOS PSP pointer to r8
    mov r9, rax  ;Copy the Memory arena pointer to r9
    ;Input: r8 = PSP
    ;       r9 = Memory Arena Pointer
    ;All regs must be preserved (including r9, even if you free. Dont free!)
    call OEMCALLBK  ;Return CF=CY if OEM wants to keep the memory block
    jc short l2 
    mov r8, qword [OEMMEMPTR]
    mov eax, 4900h  ;Free the memory block
    int 41h
l2:
    ;Load COMMAND.COM
    ;Get currentPSP ptr
    mov ah, 62h ;Get current PSP ptr in rbx
    int 41h
    mov ah, 19h ;Get current Drive letter in al
    int 41h
    add al, "A"
    mov byte [cmdLine], al  ;Store drive letter at start of command line

    lea rbx, cmdBlock
    lea rsi, tempPSP
    lea rax, qword [rsi + psp.fcb1]
    mov qword [rbx + execProg.pfcb1], rax
    lea rax, qword [rsi + psp.fcb2]
    mov qword [rbx + execProg.pfcb2], rax
    lea rax, qword [rsi + psp.dta]  ;Get the dummy command line ptr
    mov qword [rbx + execProg.pCmdLine], rax    ;Store dummy command line here
    lea rdx, cmdLine
    mov eax, 4B00h  ;Exec Prog
    int 41h
    lea rdx, badCom
    mov ah, 09h ;Print message
    int 41h
hltLbl:
    hlt
    pause
    jmp short hltLbl
badMem:
    lea rdx, memErr
    mov eax, 0900h
    int 41h
    jmp short hltLbl
memErr  db "System Memory Error",0Ah,0Dh,"$"
;--------------------------------
;       DATA FOR SYSINIT        :
;--------------------------------
strtmsg db "Starting SCP/DOS...",0Ah,0Dh,"$"
badCom  db "Bad or missing Command interpreter",0Ah,0Dh,"$"
conName db "CON",0
auxName db "AUX",0
prnName db "PRN",0

cfgspec db "CONFIG.SYS",0 ;ASCIIZ for CONFIG
cmdLine db "_:\COMMAND.COM",0   ;ASCIIZ FOR COMMAND.COM

cmdBlock:   ;Used also for overlay block
    istruc execProg
    at execProg.pEnv,       dq 0    ;Is set to point at the above line
    at execProg.pCmdLine,   dq 0    ;Points to just a 0Dh
    at execProg.pfcb1,      dq 0    ;Set to DOS's fcb 1 and 2
    at execProg.pfcb2,      dq 0
    iend
exceptData:
    dq i0
    dq i1
    dq i2
    dq i3
    dq i4
    dq i5
    dq i6
    dq i7
    dq i8
    dq i9
    dq i10
    dq i11
    dq i12
    dq i13
    dq i14
    dq i15
    dq i16
    dq i17
    dq i18
    dq i19
    dq i20
    dq i21

intData:
    dq terminateProcess ;Int 40h
    dq functionDispatch ;Int 41h
    dq OEMHALT          ;Int 42h, If sysinit terminates, halt system
    dq defaultIretq     ;Int 43h, ignore any CTRL+C during init
    dq dosDefCritErrHdlr 
    dq absDiskRead      ;Int 45h
    dq absDiskWrite     ;Int 46h
    dq terminateRes     ;Int 47h
    dq defaultIretq     ;Int 48h
    dq defaultIretq     ;Int 49h
    dq defaultIretq     ;Int 4Ah
    dq defaultIretq     ;Int 4Bh
    dq defaultIretq     ;Int 4Ch
    dq defaultIretq     ;Int 4Dh
    dq defaultIretq     ;Int 4Eh
    dq multiplexHdlr    ;Int 4Fh, multiplex default handler
nData:
    dq 0    ;We link here to the head of the OEM driver chain
    dw 08004h
    dq nulStrat
    dq nulIntr
    db "NUL     " ;Default NUL data

openStreams:
;If this returns with CF=CY, an error occured. Halt boot if initial set of streams
    lea rdx, auxName
    mov eax, 3D02h   ;Open read/write
    int 41h
    retc
    mov ebx, eax
    mov ecx, 3  ;
    mov eax, 4600h  ;DUP2
    int 41h
    retc
    mov eax, 3e00h
    int 41h ;Close the original handle
    retc
    mov eax, 3D02h  ;Open read/write
    lea rdx, conName
    int 41h
    retc
    mov ebx, eax    ;Move file handle to ebx
    mov eax, 4500h  ;DUP
    int 41h
    retc
    mov eax, 4500h  ;DUP
    int 41h
    retc
    lea rdx, prnName
    mov eax, 3D02h
    int 41h       ;Open file
    return

addDriverMarkers:
;Traverses the MCB chain after a driver init to add the correct subsytem 
; information and owner to each memory block. Used for drivers that allocate
; their own memory using ALLOC.
;Input: qword [currentPSP] = Signature to search for (9 means kernel driver).
;       fs -> Dos Data Area
;Output: Sets the first occurrence to Driver, the rest to driver appendage,
;           unless the signature is 9 in which case, it is set to DOS owner.
;           In the event of a kernel driver then only mcbSubDrvExtra is used.
    push rax
    push rbx
    push rcx
    push rsi
    push rdi
    mov rdi, qword [rbp + currentPSP]
    mov rsi, qword [rbp + mcbChainPtr] ;Points to the kernel allocation
    mov eax, mcbSubDriver
    mov ebx, mcbSubDrvExtra
    cmp rdi, mcbOwnerNewDOS  ;If so, skip setting driver, only extra!
    cmove eax, ebx
    jmp short .gotoNextBlock    ;Skip the first alloc (the kernel)
.checkSubsystem:
    cmp qword [rsi + mcb.owner], rdi
    jne short .gotoNextBlock
    mov byte [rsi + mcb.subSysMark], al
    cmp eax, ebx
    cmovne eax, ebx
    cmp byte [rsi + mcb.owner], mcbOwnerNewDOS
    jne short .gotoNextBlock
    mov byte [rsi + mcb.owner], mcbOwnerDOS
.gotoNextBlock:
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    je short .exit
    xor ecx, ecx
    mov ecx, dword [rsi + mcb.blockSize]
    shl rcx, 4
    add rsi, mcb.program    
    add rsi, rcx
    jmp short .checkSubsystem
.exit:
    pop rdi
    pop rsi
    pop rcx
    pop rbx
    pop rax
    return


convertBPBArray:
;rsi -> BPB array
;rbp -> Space for cl consecutive DPB's
;rdi -> Driver header
;cl = Number of BPBs to process
;If ZF=NZ on entry, link the end of the last DPB array 
    call .findLastDPB
    movzx ecx, cl   ;Use ch as the unit number counter
.buildNext:
    push rsi
    mov rsi, qword [rsi]    ;Get the BPB pointer from the BPB array
    mov ah, 53h ;Build DPB
    int 41h
    pop rsi
    movzx eax, byte fs:[numPhysVol] ;Get current # drives
    mov byte [rbp + dpb.bDriveNumber], al   ;Set it as drvnum
    inc byte fs:[numPhysVol]    ;One more physical volume present!
    mov byte [rbp + dpb.bUnitNumber], ch    ;Set unit number
    mov qword [rbp + dpb.qDriverHeaderPtr], rdi ;Store ptr to driver
    inc ch  ;Goto next unit number
    cmp cl, ch  ;When equal, exit!
    jz short .exit
    lea rax, qword [rbp + dpb_size]
    mov qword [rbp + dpb.qNextDPBPtr], rax
    mov rbp, rax    ;Advance rbp by that amount
    add rsi, 8 ;Go to the next BPB in the BPB array
    jmp short .buildNext
.exit:
    ;Now set next DPB as -1 i.e end of chain!
    xor eax, eax
    dec rax
    mov qword [rbp + dpb.qNextDPBPtr], rax
    add rbp, dpb_size   ;Point rbp past the next DPB
    ret
.findLastDPB:
;Finds the last DPB and links the next DPB to it
    mov rax, qword fs:[dpbHeadPtr]
    test rax, rax
    jz short .first 
.lp:
    cmp qword [rax + dpb.qNextDPBPtr], -1   ;End of chain?
    je short .lastFound
    mov rax, qword [rax + dpb.qNextDPBPtr]  ;Get this pointer in rax
    jmp short .lp   ;And go again
.lastFound:
    mov qword [rax + dpb.qNextDPBPtr], rbp  ;The next dpb will go here
    ret
.first:
;If this is the first DPB array, set the dpbHeadPtr
    mov qword fs:[dpbHeadPtr], rbp
    ret

ejectKernelInit:
;Reallocates the space allocated to the driver file after 
; init was called.
;Input: rbx -> Pointer to the original end of the allocation (para aligned)
;       r8 -> Points to the mcb for reallocation
;Uses the sysinit init drive block. 
;rax, rbx, rflags trashed
;If returns CF=CY, error in reallocation.
    push rbx
    lea rbx, initDrvBlk
    mov rax, qword [rbx + initReqPkt.endptr]
    pop rbx
    ;If this endptr is zero or -1 ignore it. 
    ;If this endptr is greater than the end of alloc, ignore it.
    test rax, rax
    retz
    inc rax ;Carry over to 0 if this is -1
    retz
    dec rax ;Return to original value
    add rax, 0Fh    ;Paragraph align the endptr
    shr rax, 4
    shl rax, 4
    sub rbx, rax    ;If this is above zero then rbx > rax, which is valid
    jbe short .exit   ;If equal or below zero, dont reallocate
    mov eax, dword [r8 + anchorMcb + mcb.blockSize]    ;Get alloc size
    push r8 ;Save the pointer to the mcb before using syscall
    add r8, mcb.program ;Goto program
    shr ebx, 4  ;Now convert the difference into number of paragraphs
    sub eax, ebx
    mov ebx, eax
    mov eax, 4A00h  ;Reallocate space
    int 41h
    pop r8
    return
.exit:
    clc ;Make sure to clear the CF flag before returning
    return

initDriver:
;Initialises one driver and adjusts the DOS data appropriately
;If on return CF=CY then the driver didnt want to be loaded
;Preserves rbx (initReqPkt), rbp (DOSSEG ptr), rsi (driver pointer)
;initReqPkt.optptr must be set before calling this function if cmdline
; arguments are to be passed to the driver
;Input: rsi -> driver pointer
;       rbx -> sysinit request pointer
;       rbp -> DOSSEG pointer
    mov byte [rbx + initReqPkt.hdrlen], initReqPkt_size
    mov byte [rbx + initReqPkt.cmdcde], drvINIT
    mov word [rbx + initReqPkt.status], 0
    mov al, byte [rbp + numPhysVol]    ;Get current num of physical volumes
    mov byte [rbx + initReqPkt.drvnum], al
    ;Protect the important registers. All others trashable
    push rbx
    push rsi
    push rbp
    call qword [rsi + drvHdr.strPtr]
    call qword [rsi + drvHdr.intPtr]
    pop rbp
    pop rsi
    pop rbx
    ;Check if a driver wants to not load.
    ;If a kernel driver wants to stop, halt boot.
    test word [rbx + initReqPkt.status], drvErrStatus
    jnz short .errExit
    cmp byte [rbx + initReqPkt.numunt], 0
    jne short .notHalt
    cmp qword [rbx + initReqPkt.endptr], rsi    ;If endptr -> header, abort
    jne short .notHalt
.errExit:
    stc
    ret
.notHalt:
    ;Now check if the drivers were con/clock before exiting
    mov ax, word [rsi + drvHdr.attrib]
    and ax, devDrvConIn | devDrvConOut
    jz short .checkClock    ;If neither one of these bits are set, jmp
    cmp ax, devDrvConIn | devDrvConOut
    jne short .checkClock
    mov qword [rbp + vConPtr], rsi  ;Store the header ptr here
.checkClock:
    test word [rsi + drvHdr.attrib], devDrvClockDev
    jz short .notClock
    mov qword [rbp + clockPtr], rsi
.notClock:
;Now test if MSD driver. If so, store the number of units in the name field
    test word [rsi + drvHdr.attrib], devDrvChar
    retnz   ;Return if this is a char device
    ;Else, store the number of units as reported live by driver
    movzx eax, byte [rbx + initReqPkt.numunt] ;Get # units reported by driver
    mov byte [rsi + drvHdr.drvUnt], al ;Store this byte permanently here
    ret

buildDPBs:
    ;Here we specially handle MSD drivers, building DPBs
    ;If return with CF=CY, fail. Else, all done and setup
    ;Input: rbx -> Points to sysinit request packet
    ;       rsi -> Driver header
    ;       rbp -> DOSSEG pointer
    ;Preserves those registers
    push rbx
    push rbp
    push rsi
    push rdi
    mov rdi, rsi    ;SAVE THE DRIVER HEADER!
    movzx ecx, byte [rsi + drvHdr.drvUnt]  ;Get # of units reported by driver
    mov rsi, qword [rbx + initReqPkt.optptr]
    mov eax, dpb_size
    mul ecx         ;Get the number of bytes for all the dpb's into eax
    add eax, 0Fh    ;Round up if not precisely on para boundry
    shr eax, 4      ;Convert to paragraphs
    mov ebx, eax
    mov eax, 4800h  ;ALLOC (marked as owned by DOS for now)
    int 41h
    jc short .badExit
    mov rbp, rax    
    mov qword [rax + mcb.subSysMark], mcbSubDrvDPB  ;Set DPB marker here
    mov qword [rax + mcb.owner], mcbOwnerDOS    ;Set DOS owner here
    ;rsi -> Ptr to BPB
	;rbp -> Ptr to buffer to hold first DPB
    ;rdi -> Ptr to the driver header
    call convertBPBArray    ;Returns rbp -> past last DPB
.exit:
    pop rdi
    pop rsi
    pop rbp
    pop rbx
    return
.badExit:
    stc
    jmp short .exit

setupInterruptBlock:
;Sets up a block of interrupts with pointers provided in a table
;Input:
; al = Start interrupt
; rdi -> Start of pointer table
; cl = Last interrupt + 1
;Assumes rbp points to DOSSEG
    movzx eax, al
    movzx ecx, cl
.lp:
    mov rbx, qword [rdi]    ;Get address pointed to by rdi
    add rbx, rbp            ;Add the relocated base to rbx
    call writeIDTEntry
    add rdi, 8  ;Goto next interrupt handler
    inc eax     ;Goto next interrupt number
    cmp eax, ecx
    jne .lp
    return

writeIDTEntry:
;Writes the interrupt in the right place in the table
    ;al = Interrupt number
    ;rbx -> Handler to install
    push rax
    push rbx
    movzx eax, al
    xchg rbx, rax
    shl rbx, 4h     ;Multiply IDT entry number by 16
    add rbx, qword [localIDTpointer.Base]    ;rsx points to IDT entry
    mov word [rbx], ax  ;Get low word into offset 15...0
    shr rax, 10h    ;Bring next word low
    mov word [rbx + 6], ax  ;Get low word into offset 31...16
    shr rax, 10h    ;Bring last dword low
    mov dword [rbx + 8], eax
    pop rbx
    pop rax
    ret

; DATA AREA
localIDTpointer: ;Local IDT pointer
    .Limit  dw 0
    .Base   dq 0

FINALDOSPTR dq 0    ;Pointer to where dSeg should be loaded
DOSENDPTR   dq 0    ;Pointer to the first free byte AFTER DOS
MCBANCHOR   dq 0    ;Pointer to the Anchor MCB

;DOS Data given by OEM
FILES       db 0    ;Default number of FILES
BUFFERS     db 0    ;Default number of BUFFERS
DFLTDRIVE   db 0    ;Default drive number (0-25), this is the boot drive
LASTDRIVE   db 0    ;Default last drive number (0-25)
OEMBIOS     db 0    ;Set if to use IO.SYS or clear if to use SCPBIOS.SYS
OEMMEMPTR:  ;Used to save the allocated 64k block for OEMCALLBK
OEMDRVCHAIN dq 0    ;Pointer to the uninitialised device drivers

initDrvBlk  db initReqPkt_size dup (0)  ;Used for making driver init reqs
tempPSP: ;Points to a 256 byte space that is set up appropriately
    istruc psp
    at psp.return,      db 0CDh, 40h
    at psp.allocSize,   dd 0, 0 ;Second 0 is for the reserved dword
    at psp.oldInt42h,   dq 0
    at psp.oldInt43h,   dq 0
    at psp.oldInt44h,   dq 0
    at psp.parentPtr,   dq 0
    at psp.jobFileTbl,  db 20 dup (0FFh)
    at psp.envPtr,      dq 0
    at psp.rspPtr,      dq 0
    at psp.jftSize,     dw 20 
    at psp.unixEntry,   db 0CDh, 41h, 0CBh
    at psp.prevPSP,     dq 0
    at psp.fcb1,        db 16 dup (0)
    at psp.fcb2,        db 20 dup (0)
    at psp.dta,         db 0, CR, 126 dup (0)   ;Dummy empty command line
    iend