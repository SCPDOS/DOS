
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

    lea rax, qword [rbp + dosEnd] ;Get the end of the file
    mov qword [DOSENDPTR], rax ;Store the current end of memory here
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
;Copy the Null driver to its location in Sysvars
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
    add rsi, drvHdr_size
    jmp short adjDrivers
.exit:
;------------------------------------------------;
;              Kernel Driver inits               ;
;------------------------------------------------;
kernDrvInit:
    ;rbp and fs point to DOSSEG
    lea rbx, initDrvBlk
    mov rsi, qword [OEMDRVCHAIN]    ;Get the first driver in the chain
.init:
    call initDriver
    jc OEMHALT
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz .notMSD
    ;Here we specially handle MSD drivers, building DPBs
    push rbp
    push rsi
    push rdi
    mov rdi, rsi    ;SAVE THE DRIVER HEADER!
    mov cl, byte [rbx + initReqPkt.numunt]
    mov rsi, qword [rbx + initReqPkt.optptr]
    mov rbp, qword [DOSENDPTR]  ;Get the current end pointer
    ;rsi -> Ptr to BPB
	;rbp -> Ptr to buffer to hold first DPB
    ;rdi -> Ptr to the driver header
    call convertBPBArray    ;Returns rbp -> past last DPB
    mov qword [DOSENDPTR], rbp  ;Now point to the new space
    pop rdi
    pop rsi
    pop rbp
.notMSD:
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Now point rsi to that header
    cmp rsi, -1     ;We at the end of the chain?
    jne short .init ;If not, goto next driver
;------------------------------------------------;
;                   DPB Reloc                    ;
;------------------------------------------------;    
;We relocate the DPB chain to the space provided 
; by the drivers "eject" vector. If the eject
; vector is 0, -1 or equal to the end of the 
; driver file (i.e. no ejection at all) skip 
; this. If the address is above the current 
; DOSEND also ignore.
;------------------------------------------------;
dpbReloc:
    lea rbx, initDrvBlk ;Get the init drive block
    mov rax, qword [rbx + initReqPkt.endptr]
    test rax, rax   ;Is it zero?
    jz short .exit
    inc rax         ;Is it zero now (i.e. was -1)?
    jz short .exit
    dec rax
    cmp qword [DOSENDPTR], rax
    jbe short .exit     ;If we relocating, gotta be below the current addr
    ;Here we relocate 
    mov rsi, qword fs:[dpbHeadPtr]
    mov rdi, rax
    mov qword fs:[dpbHeadPtr], rdi   ;This is the new DPB head pointer
    xor ecx, ecx
    jmp short .lpstart
.lp:
    mov qword [rbx + dpb.qNextDPBPtr], rdi  ;Set the new next DPB
.lpstart:
    mov rbx, rdi    ;Save a pointer to where the DPB will be
    mov ecx, dpb_size    ;Set the counter
    rep movsb   ;Move the DPB over
    cmp qword [rbx + dpb.qNextDPBPtr], -1   ;Are we at the last DPB?
    jne short .lp   ;If not, jump again!
.exit1:
    mov qword [DOSENDPTR], rdi  ;Now rdi points to the new end of alloc
.exit:
    mov rax, qword [DOSENDPTR]  ;Round now to nearest paragraph
    shr rax, 4
    shl rax, 4
    add rax, 10h
    mov qword [DOSENDPTR], rax   ;Put it back now
;------------------------------------------------;
;                   MCB inits                    ;
;------------------------------------------------;
makeMCBChain:
    push rbp    ;Save the pointer to DOSSEG on the stack temporarily
    call OEMMCBINIT
    pop rbp
    jc OEMHALT
    mov rax, qword [DOSENDPTR]
    cmp rax, qword [OEMMCBANCHR]  ;If the ptrs are equal, shift to 
    jne short .end  ; make space for the MCB itself
    add qword [DOSENDPTR], mcb_size ;Shift this pointer to make space
.end:
;----------------------------------------:
;           End of MSD driver init.      :
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
    mov rax, qword [OEMMCBANCHR]
    mov qword fs:[mcbChainPtr], rax

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
    mov rdi, qword [DOSENDPTR]      ;Setup array
    call makeCDSArray   ;Sets the CDS head pointer
    mov qword [DOSENDPTR], rdi      ;Push the pointer further past
    jmp short initialCDSWritten ;Go past the function
makeCDSArray:
;Input: ecx = Size of array
;       rdi = Pointer to the start of the CDS array
;Ouput: rdi = first byte past the end of the table
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
    mov rdi, qword [DOSENDPTR]
    mov qword fs:[bufHeadPtr], rdi
    movzx eax, word fs:[maxBytesSec] ;Get the buffer size
    lea rbx, qword [rdi + bufferHdr_size + rax] 
    mov qword [DOSENDPTR], rbx  ;New end pointer goes here
    mov qword [rdi + bufferHdr.nextBufPtr], -1 ;Point to no buffer
    mov word [rdi + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags
    
;------------------------------------------------;
;          Default File Handle Creation          ;
;                                                ;
;   Note: Devices are opened AUX, CON then PRN   ;
;------------------------------------------------;
defaultFileHandles:

    lea rdx, qword [rbp + firstSftHeader]
    mov qword fs:[sftHeadPtr], rdx  ;Start from this SFT header
    mov rsi, rdx
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
    mov rdi, qword [DOSENDPTR]
    push rbp
    mov rbp, rsp
    sub rsp, cfgFrame_size
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Store the end pointer here
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
;   The space marked as "endPtr" can be used as a buffer by the disk buffers.
; Once EOF has been reached, we jmp to noCfg which configures the other
;   data structures according to the values on the stack frame.
;-------------------------------------------------------------------------;
;Start CONFIG.SYS parsing here
configParse:
    mov qword [rbp - cfgFrame.cfgHandle], rax
    mov qword [rbp - cfgFrame.lastLine], 0
.newLine:
;Keeps the new line unless a DEVICE= command read it, which adjusts endPtr
    mov rdx, qword [rbp - cfgFrame.endPtr]  ;Start reading into here
    mov qword [rbp - cfgFrame.linePtr], rdx	;Use var for start of line ptr
.nextChar:
    mov rbx, qword [rbp - cfgFrame.cfgHandle]   ;Move the handle into ebx
    cmp bx, -1
    je .stopProcessError
    mov eax, 3F00h  ;Read handle
    mov ecx, 1  ;Read one byte
    int 41h
    jc .stopProcessError
    test ecx, ecx	;If this is zero, EOF reached
    jnz .notEOF
    mov qword [rbp - cfgFrame.lastLine], -1	;Note we are at EOF
.notEOF:
    inc qword [rbp - cfgFrame.endPtr]	;Goto next byte
    movzx eax, byte [rdx]
    cmp al, CR
    je .endOfLine
    cmp al, LF
    je .endOfLine
    cmp al, "a"
    jb .notChar
    cmp al, "z"
    ja .notChar
    push rax    ;Push rax on stack as the argument to normalise
    mov eax, 1213h  ;Uppercase the char
    int 4fh
    mov byte [rdx], al  ;Replace the char with the capitalised form
    pop rax ;Pop into rax to renormalise the stack
.notChar:
    inc rdx ;Now move our local pointer to the next byte
    jmp short .nextChar
.endOfLine:
;rdx points to terminating char
;First find the length of the instruction word
    xor ecx, ecx
.cmdNameLenGet:
    lodsb
    call .isCharTerminal
    jz .endOfCommandFound
    inc ecx
    cmp ecx, 10 ;If shorter than longest command, keep looping
    jb .cmdNameLenGet
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
    jne .gotoNextCmd
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
    jne .gotoNextCmd    ;If not equal, just goto next command
    ;Else, rdi + rcx points to the word ptr of the function
    ;rdx points to the terminating char of the line 
    push rdx    ;This is to know whether we continue processing or end now
    lea rsi, .keyTbl
    mov rax, rsi    ;Keep a copy in rax
    add rsi, qword [rdi + rcx + 1]  ;This is the offset from .keyTbl
    add rsi, rax    ;So add the EA of the head of the tbl before calling
    clc ;Esure flags are happy before entering
    call rsi    ;Call this function
    pop rdx
    jc .stopProcessError    ;If the function returns CF=CY, error exit
    test qword [rbp - cfgFrame.lastLine], -1 ;If we concluded at EOF, exit
    jnz .cfgExit
    jmp .newLine
.gotoNextCmd:
    movzx eax, byte [rdi]
    add eax, 3
    add rdi, rax
    jmp short .cmdSearch
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
    rete
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
	db 4, "FCBS"            ;DONE (to be ignored for a while now)
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
.countryScan:
    return
.drvLoader:
;We first try to read the driver into the byte after rdx.
;If we cannot open the file, or we can open but not read the whole file
; we error with Bad or missing filename msg, and proceed as if nothing happened 
; (CF=NC). 
; Thus we DO NOT adjust .endPtr or .linePtr and recycle that space for the 
; next line.
;If the open succeeded and we were able to read the whole driver into memory, 
; we pass the lineptr to the driver and call init for the driver.
; Once the driver returns, if the DONE bit is set, we read the offset of 
; free memory above the driver and add that to the endPtr. If the driver
; is a block driver, we add to the endPtr the space for "Units supported" 
; number of DPBs.
    mov rsi, rdx    ;Save the ptr to past the end of the line in rsi
    mov rdi, qword [rbp - cfgFrame.linePtr]
    add rdi, 7  ;Go past DEVICE= to the pathname
    mov rdx, rdi    ;Prepare rdx for the open
    mov eax, SPC
.drvFindEndOfFileName:
    scasb  ;Is this char the space?
    je .fileNameFound
    ;Was the char terminal?
    cmp byte [rdi - 1], CR
    je .drvBad
    cmp byte [rdi - 1], LF
    je .drvBad
    jmp short .drvFindEndOfFileName
.fileNameFound:
    mov byte [rdi - 1], 0   ;Null terminate the path to the file
    mov eax, 3D00h  ;Read only file
    int 41h
    jc .drvBad
    mov byte [rdi - 1], " " ;Replace the null with a space now again
    movzx ebx, ax   ;Get the handle in ebx
    mov word [.drvHandle], ax   ;Save the handle in variable
    xor edx, edx    ;Move the handle to the end of the file
    mov eax, 4202h  ;LSEEK to SEEK_END
    int 41h
    mov ecx, eax    ;Get the file size in ecx
    xor edx, edx    ;Move the handle to the start of the file
    mov eax, 4200h  ;LSEEK to SEEK_SET (start of the file)
    int 41h
    ;Now we read ecx bytes to rsi as rsi points to first byte past the end
    ; of the DEVICE= line 
    mov rdx, rsi    ;Point to first byte past the end of DEVICE= line
    mov esi, ecx    ;Save the number of bytes to read in esi
    mov eax, 3F00h  ;Read handle    
    int 41h
    jc .drvBadClose
    cmp esi, ecx    ;Were all bytes read in?
    jne .drvBadClose
    ;Ok, full file read in, now prepare to call driver init routine
    mov rsi, rdx    ;Move ptr to driver header to rsi
    lea rbx, .drvInitStruc
    mov byte [rbx + initReqPkt.hdrlen], initReqPkt_size
    mov byte [rbx + initReqPkt.cmdcde], drvINIT
    mov word [rbx + initReqPkt.status], 0
    mov byte [rbx + initReqPkt.numunt], 0
    mov rax, qword [rbp - cfgFrame.linePtr] ;Get the line pointer
    add rax, 7  ;Goto the first byte past DEVICE=
    mov qword [rbx + initReqPkt.endptr], rax
    mov qword [rbx + initReqPkt.optptr], 0
    movzx eax, byte fs:[numPhysVol]
    dec eax ;Get a 0 based count
    mov byte [rbx + initReqPkt.drvnum], al
    ;Gotta relocate the two pointers to make them 
    ;relative to DOS
    lea rax, dosDataArea   ;Get DOS ptr back
    add qword [rsi + drvHdr.strPtr], rax    ;Reloc drvrs rel. DOS
    add qword [rsi + drvHdr.intPtr], rax
    call qword [rsi + drvHdr.strPtr]  ;Passing rbx through here
    call qword [rsi + drvHdr.intPtr]
    test word [rbx + initReqPkt.status], drvDonStatus
    jz .drvBadClose
    test word [rbx + initReqPkt.status], drvErrStatus
    jnz .drvBadClose
    ;Now check that the driver wants to be installed
    cmp rsi, qword [rbx + initReqPkt.endptr]    ;This is for char and blk devs
    je .drvWantsClose
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz .drvChar
    cmp byte [rbx + initReqPkt.numunt], 0
    je .drvWantsClose
.drvChar:
    ;Otherwise, this init passed, now build the structures we need.
    ;First adjust .endPtr
    mov rax, qword [rbx + initReqPkt.endptr]    ;Get the end pointer
    mov qword [rbp - cfgFrame.endPtr], rax  ;Move it here
    ;Now we link the driver into the driver chain
    mov rdi, qword [nulDevHdr + drvHdr.nxtPtr]  ;Get next ptr from nul drvr
    mov qword [rsi + drvHdr.nxtPtr], rdi    ;And store it here
    mov qword [nulDevHdr + drvHdr.nxtPtr], rsi  ;And link nul to this driver
    ;Now if we are a char device, we are done so check here
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz .drvWantsCloseChar  ;We are complete
    ;Now for block devices, we get the BPB ptr array and numUnits supported
    movzx ecx, byte [rbx + initReqPkt.numunt]
    mov rbx, qword [rbx + initReqPkt.optptr]    ;Get the BPB array pointer

    mov rdx, rsi    ;Move the driver pointer to rdx
    mov rsi, qword [rbp - cfgFrame.endPtr]  ;Build DPB array here
    mov rdi, rsi    ;Move rdi here too, to point to first new DPB later
    push rcx
    push rdx
    xor edx, edx
    mov eax, dpb_size
    mul ecx ;Multiply the number of DPB's needed with the size of a dpb
    add qword [rbp - cfgFrame.endPtr], rax  ;Add this value to endPtr
    pop rdx ;Get back the driver ptr in rdx
    pop rcx ;Get back the number of units count
    
    xchg rbp, rbx   ;Swap stack frame ptr and BPB array ptr
    xchg rsi, rbp   ;Swap BPB array and DPB space ptrs
.drvBuildDPB:
    mov eax, 5300h
    int 41h
    add rsi, bpbEx_size ;Goto next bpb in array
    ;Adjust fields in DPB
    inc byte fs:[numPhysVol] 
    mov al, byte fs:[numPhysVol]
    mov byte [rbp + dpb.bDriveNumber], al
    mov byte [rbp + dpb.bUnitNumber], ch
    mov qword [rbp + dpb.qDriverHeaderPtr], rdx
    lea rax, qword [rbp + dpb_size] ;Point to next DPB
    mov qword [rbp + dpb.qNextDPBPtr], rax
    inc ch  ;Increment unit number 
    cmp cl, ch  ;Are we done?
    je .dpbInitDone
    add rbp, dpb_size   ;Go to space for next DPB
    jmp short .drvBuildDPB
.dpbInitDone:
;Make sure we now make the last qNextDPBPtr = -1
    mov qword [rbp + dpb.qNextDPBPtr], -1
    ;Now we set the old last dpb to point to the first one
    mov rsi, qword fs:[dpbHeadPtr]
.drvDPBLp:
    cmp byte [rsi + dpb.qNextDPBPtr], -1
    je .drvLastDPBFound
    mov rsi, qword [rsi + dpb.qNextDPBPtr]  ;Goto next DPB
    jmp short .drvDPBLp
.drvLastDPBFound:
    mov qword [rsi], rdi    ;Chain this dpb now to the first new dpb
    mov rbp, rbx    ;Return the stack frame ptr to rbp
;And we are done!
.drvWantsClose:
;If the driver wants to not install silently, it can here
    movzx ebx, word [.drvHandle] ;Get the handle back, close it and proceed
    mov eax, 3E00h  
    int 41h 
    clc ;Never return with CF=CY
    return  
.drvBadClose:
    movzx ebx, word [.drvHandle]    ;Get back handle to close
    mov eax, 3E00h  ;Close handle in ebx
    int 41h
.drvBad:
    lea rdx, .drvBadMsg
    mov eax, 0900h
    int 41h
    clc ;Never return with CF=CY
    return
.drvWantsCloseChar:
;Final checks, to see if we are CLOCK$ or CON
    test word [rsi + drvHdr.attrib], devDrvConIn
    jz .dwccClock
    mov qword [vConPtr], rsi
.dwccClock:
    test word [rsi + drvHdr.attrib], devDrvClockDev
    jz .drvWantsClose
    mov qword [clockPtr], rsi
    jmp short .drvWantsClose
.drvBadMsg: db "Bad or missing filename",CR,LF,"$"
.drvInitStruc: db initReqPkt_size dup (0)  
.drvHandle: dw -1

.fcbHandler:
    return
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
    cmp al, "0"
    jb .sftHandlerErr
    cmp al, "2" ;Max BUFFERS=254 soooo, sorry buddy!
    ja .sftHandlerErr
    lodsb   ;Check no more chars!
    call .sftHandlerTermCheck
    jne .sftHandlerErr
.sftHandlerProcess:
    xor edx, edx    ;Accumulate value in edx
    mov rsi, rdi    ;Go back to the first number
.sftHandlerLp:
    lodsb   ;Get the digit
    call .sftHandlerMul
    jecxz .sftHandlerPrepExit
    dec ecx
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
    sub al, "0" ;Convert to a binary value
    mul cl  ;Multiply al by cl, answer in ax
    movzx eax, ax
    add edx, eax
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
    return
.shellHandler:
    return
.stacksHandler:
    return
.drivParm:
    return

.cfgExit:
    mov rbx, qword [rbp - cfgFrame.cfgHandle] ;Get the handle back
    mov eax, 3E00h    ;Close the handle
    int 41h ;bx already has the handle
;------------------------------------------------;
;   Setup Final Data Areas With Overrides from   ;
;                  CONFIG.SYS                    ;
;------------------------------------------------;
;Add additional buffers.
;Add additional SFT entries.
;Add additional FCBS.
;Create a larger CDS if needed.
noCfg:
;Begin by aligning the endPtr on a paragraph boundary
    mov rdi, qword [rbp - cfgFrame.endPtr]
    shr rdi, 4
    shl rdi, 4
    add rdi, 10h
    mov qword [rbp - cfgFrame.endPtr], rdi
;Start with buffers:
    mov rcx, qword [rbp - cfgFrame.newBuffers]    ;Get new buffers size
    mov byte fs:[numBuffers], cl    ;Store this value in var
    ;Now do the allocation at rdi. Each buffer = maxSectorSize + bufferHdr_size
    movzx ebx, word fs:[maxBytesSec]    ;Get buffer sector size
    add ebx, bufferHdr_size ;rbx has the size to add
    ;Each buffer has no flags, drive number must be -1
    ;rdi has the current allocation end pointer
    mov qword fs:[bufHeadPtr], rdi  ;Reset the var here
    mov rsi, rdi    ;Points rsi to first new buffer space
    xor eax, eax    ;Use for sanitising buffer headers
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
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here

;Now build a new SFT header for the number of files specified by user
    mov rcx, qword [rbp - cfgFrame.newSFTVal]
    cmp ecx, 5  ;If we are not adding anything, skip building SFT
    je .skipSFT
    mov rsi, qword fs:[sftHeadPtr]  ;Get the current only SFT head pointer
    mov qword [rsi + sfth.qNextSFTPtr], rdi ;Move rdi as new SFT pointer
    sub cx, word [rsi + sfth.wNumFiles] ;Remove the number of files we already have
    mov word [rdi + sfth.wNumFiles], cx ;Move remaining files here
    mov qword [rdi + sfth.qNextSFTPtr], -1  ;Last table in chain
    add rdi, sfth_size  ;Goto sft area, now need to compute size
    mov eax, sft_size
    mul ecx ;Multiply number of sft with their size to get allocation
    add rdi, rax    ;Add that many bytes to rdi
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here
.skipSFT:
;FCBS at rdi
    mov qword fs:[fcbsHeadPtr], rdi ;Setup the fcbs var here
    mov qword [rdi + sfth.qNextSFTPtr], -1  ;No more FCBS headers for now
    mov rcx, qword [rbp - cfgFrame.newFCBSVal]
    mov word [rdi + sfth.wNumFiles], cx ;Move this value here
    mov eax, sft_size
    mul ecx ;Multiply number of sft with their size to get allocation
    add rdi, rax    ;Add that many bytes to rdi
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here
    mov rcx, qword [rbp - cfgFrame.newProtFCBSVal] ;Get number of safe FCBs
    mov word fs:[numSafeSFCB], cx   ;And save that there
;And CDS now
    mov rcx, qword [rbp - cfgFrame.newLastdrive]
    mov byte fs:[lastdrvNum], cl ;Save this value
    call makeCDSArray
    mov qword [rbp - cfgFrame.endPtr], rdi  ;Save this new position here
;Computation of new space is complete, now work out how many bytes this is
    mov rbx, qword fs:[mcbChainPtr]
    add rbx, mcb_size
    sub rdi, rbx    ;Gives difference now
    lea rbx, dword [rdi + 11h]  ;Add 11 to round up a paragraph
    shr rbx, 4  ;Convert to paragraphs
;Resize DOS allocation before loading COMMAND.COM
    mov r8, qword fs:[mcbChainPtr] ;Get ptr
    add r8, mcb.program
    mov ah, 4Ah
    int 41h

    mov rsp, rbp    ;Return stack pointer to original position
    pop rbp ;Stack frame no longer needed
;Now we close all five default handles and open AUX, CON and PRN.
    xor ebx, ebx
closeHandlesLoop:
    mov eax, 3e00h  ;Close
    int 41h
    inc ebx ;Goto next handle
    cmp ebx, 6
    jne closeHandlesLoop
    call openStreams
l1:
    ;Load COMMAND.COM
    ;Get currentPSP ptr
    mov ah, 62h ;Get current PSP ptr in rdx
    int 41h
    mov ah, 19h ;Get current Drive letter in al
    int 41h
    add al, "A"
    mov byte [cmdLine], al  ;Store drive letter at start of command line

    lea rbx, cmdBlock
    lea rax, qword [rdx + psp.fcb1]
    mov qword [rbx + execProg.pfcb1], rax
    lea rax, qword [rdx + psp.fcb2]
    mov qword [rbx + execProg.pfcb2], rax
    lea rdx, cmdLine
    mov qword [rbx + execProg.pCmdLine], rdx    ;Store command line here
    mov eax, 4B00h  ;Exec Prog
    int 41h
    lea rdx, badCom
    mov ah, 09h ;Print message
    int 41h
hltLbl:
    hlt
    pause
    jmp short hltLbl
    
;--------------------------------
;       DATA FOR SYSINIT        :
;--------------------------------
strtmsg db "Starting SCP/DOS...",0Ah,0Dh,"$"
badCom  db "Bad or missing Command interpreter",0Ah,0Dh,"$"
conName db "CON",0
auxName db "AUX",0
prnName db "PRN",0

cfgspec db "CONFIG.SYS",0 ;ASCIIZ for CONFIG
cmdLine db "_:\COMMAND.COM",0
cmdBlock:
    istruc execProg
    at execProg.pEnv,       dq 0    ;Keep at 0 to "copy" DOS's environment ptr
    at execProg.pCmdLine,   dq 0
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

initDriver:
;Initialises one driver and adjusts the DOS data appropriately
;If on return CF=CY then the driver didnt want to be loaded
;Preserves rbx (initReqPkt), rbp (DOSSEG ptr), rsi (driver pointer)
    mov byte [rbx + initReqPkt.hdrlen], initReqPkt_size
    mov byte [rbx + initReqPkt.cmdcde], drvINIT
    mov word [rbx + initReqPkt.status], 0
    mov al, byte [rbp + numPhysVol]    ;Get current num of physical volumes
    mov byte [rbx + initReqPkt.drvnum], al
    ;Protect the two important registers. All others trashable
    push rbx
    push rsi
    push rbp
    call qword [rsi + drvHdr.strPtr]
    call qword [rsi + drvHdr.intPtr]
    pop rbp
    pop rsi
    pop rbx
    ;If a kernel driver wants to stop, halt boot.
    cmp byte [rbx + initReqPkt.numunt], 0
    jne short .notHalt
    cmp qword [rbx + initReqPkt.endptr], 0
    jne short .notHalt
    stc
    ret
.notHalt:
    ;Now check if the drivers were con/clock/msd before exiting
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
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz short .initComplete
    ;Here we get the data from the request block
    ;mov al, byte [rbx + initReqPkt.numunt]  ;Get the number of units here
    ;add byte [rbp + numPhysVol], al     ;Add the number of volumes here
.initComplete:
    ret


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
OEMMCBANCHR dq 0    ;Pointer to the Anchor MCB

;DOS Data given by OEM
FILES       db 0    ;Default number of FILES
BUFFERS     db 0    ;Default number of BUFFERS
DFLTDRIVE   db 0    ;Default drive number (0-25), this is the boot drive
LASTDRIVE   db 0    ;Default last drive number (0-25)
OEMBIOS     db 0    ;Set if to use IO.SYS or clear if to use SCPBIOS.SYS
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
    at psp.dta,         db 128 dup (0)
    iend