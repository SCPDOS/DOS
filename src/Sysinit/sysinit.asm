
SYSENTRY:    ;Control is passed here from OEMINIT module
;First move the load address into fs 
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
    lea rsi, dosResStart    ;Get pointer to the start of dText
    mov rdi, qword [FINALDOSPTR]    ;Get ptr to where dSeg goes
    add rdi, dSegLen                ;Make this a pointer to the start of dText
    cmp rdi, rsi 
    je short skipDOSReloc   ;Skip relocating if DOS is at correct address
    mov ecx, dosResLen      ;Get length of file portion for copy
    add ecx, 7              ;Round up
    shr ecx, 3              ;Divide by 8 to get number of QWORDS for copy
    rep movsq
skipDOSReloc:
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
    xor eax, eax             ;Start with interrupt 0
    mov ecx, 21
    call setupInterruptBlock
;Adjust Interrupt Entries Int 20h-2Fh
adjInts:
    lea rdi, intData
    lea rbx, OEMHALT    ;Add base address of sysinit to the symbol
    sub rbx, rbp        ;Subtract so that the add in setupInterruptBlock is Ok
    mov qword [rdi + 2*8], rbx  ;Store this address for Int 22h
    mov eax, 20h            ;Start with interrupt 20h
    mov ecx, 30h
    call setupInterruptBlock
;------------------------------------------------;
;              Setup PSP for DOS                 ;
;------------------------------------------------;
    lea rbx, qword [tempPSP]
    mov qword [rbp + currentPSP], rbx
;++++++++++++++++++++++++++++++++++++++++++++++++;
;    DOS INTERRUPTS CAN BE USED FROM HERE ON     ;
;++++++++++++++++++++++++++++++++++++++++++++++++;
;
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
    mov rsi, drv$_start
    add rsi, qword [FINALDOSPTR]    ;Offset this correctly
    mov r14, rsi    ;Save this pointer in r14
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
    call OEMMCBINIT ;Build MCB chain, kernel drvs can allocate... undoc :)
    pop rbp
    jc OEMHALT
;------------------------------------------------;
;             Set Default Time/Date              ;
;------------------------------------------------;
    mov byte [rbp + dayOfMonth], 01     ;Set 1st of Jan
    mov byte [rbp + monthOfYear], 01
;------------------------------------------------;
;     Set OEM ID number and ptr for drivers      ;
;------------------------------------------------;
    mov eax, dword [OEMVERSION]
    mov dword [rbp + biosVers], eax

    mov rax, qword [OEMPTR]
    mov qword [rbp + biosPtr], rax
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
    mov rsi, r14    ;Get back the pointer to the copied drivers
    mov rbx, rsi
    mov qword [rbp + vConPtr], rsi  ;Store default CON ptr
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Goto AUX
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Goto PRN
    mov rsi, qword [rsi + drvHdr.nxtPtr]    ;Goto CLOCK$
    mov qword [rbp + clockPtr], rsi ;Store default CLOCK$ ptr
    mov rsi, rbx     ;Point rsi back to head of device chain
    lea rbx, initDrvBlk
.init:
    call initDriver         ;Importantly preserves rbp, rsi and rbx
    jc OEMHALT
    call addDriverMarkers   ;Preserves all registers
    test word [rsi + drvHdr.attrib], devDrvChar
    jnz short .notMSD
    call buildKernDPBs          ;Preserves rbp, rsi and rbx
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
;------------------------------------------------;
;     Set up general PSP areas and DOS vars      ;
;------------------------------------------------;
;Ensure to link the default DOS vCON edit key
; controller routines before proceeding
    lea rax, qword [rbp + editKeys]
    mov qword fs:[extKeyFunc], rax

;Additional DOS Vars init and fixups
    mov byte fs:[errorDrv], -1   ;No error drive
    mov word fs:[currentNdx], -1    ;Has to be -1 initially
    mov word [rbp + shareCount], 3      ;Retry the repeat 3 times before failing
    mov word [rbp + shareDelay], 1      ;Go through one multiple of countdown loop
    mov byte fs:[switchChar], "/"  ;Default switch char
    lea rdi, qword [rbp + caseMapFunc]  ;Get the function pointer
    mov qword fs:[ctryTbl + countryStruc.mapptr], rdi ;Store in country table
    add qword [rbp + charTableArray.ucTable + 1], rbp ;Fixup stored address
    add qword [rbp + charTableArray.filenameUCTable + 1], rbp 
    add qword [rbp + charTableArray.filenameTerminatingTable + 1], rbp 
    add qword [rbp + charTableArray.collatingTable + 1], rbp 

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
    mov qword fs:[currentPSP], rbx    ;Reset current PSP
    push rbx
    add rbx, psp.dta
    mov qword fs:[currentDTA], rbx    ;Save current DTA
    pop rbx
    mov qword [rbx + psp.parentPtr], rbx ;Save self as parent Process
    mov qword [rbx + psp.prevPSP], rbx  ;Save self as previous PSP
    mov rdx, rbx
    mov eax, 3522h  ;Get pointer for Int 22h in rbx
    int 21h
    mov qword [rdx + psp.oldInt22h], rbx
    mov eax, 3523h
    int 21h
    mov qword [rdx + psp.oldInt23h], rbx
    mov eax, 3524h
    int 21h
    mov qword [rdx + psp.oldInt24h], rbx
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
    lea rdi, qword [rbp + shareHooks + 8]   ;Start from openShare
    lea rax, qword [rbp + goodDfltShareHook]
    lea rbx, qword [rbp + badDfltShareHook]
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

;Finish by setting up internal DOS vars from OEM passed arguments.
    movzx eax, byte [OEMBIOS]
    test eax, eax
    jz short skipOEMName
    lea rsi, qword [rbp + dosBIOSName]
    mov rax, "IO      "
    mov qword [rsi], rax
    mov dword [rsi + 8], ".SYS"
skipOEMName:
    movzx eax, byte [DFLTDRIVE]
    xor ebx, ebx
    cmp eax, 25
    cmova eax, ebx
    mov byte [rbp + bootDrive], al

    movzx eax, byte [FILES]
    mov ebx, filesDefault
    cmp eax, 5
    cmovb eax, ebx
    cmp eax, 254
    cmova eax, ebx
    mov byte [rbp + numFiles], al

    movzx eax, byte [BUFFERS]
    mov ebx, buffersDefault
    test eax, eax
    cmovz eax, ebx
    cmp eax, 99
    cmova eax, ebx
    mov byte [BUFFERS], al

    movzx eax, byte [LASTDRIVE]
    mov ebx, lastDriveDeflt
    cmp eax, ebx
    cmovb eax, ebx
    cmp eax, 25
    cmova eax, ebx
    mov byte [LASTDRIVE], al
    mov byte [rbp + lastdrvNum], al     ;Set for DOS to be usable

;------------------------------------------------;
;          Find largest sector size              ;
;------------------------------------------------;
    call sectorSizeSet
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
    mov eax, 4800h  ;ALLOC, set the owner below
    int 21h
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
;        Create a Default Temporary Buffer       ;
;------------------------------------------------;
    movzx ebx, word fs:[maxBytesSec]    ;Get buffer size
    add ebx, bufferHdr_size             ;add header size for allocation size
    add ebx, 0Fh
    shr ebx, 4  ;Convert to number of paragraphs
    mov eax, 4800h  ;ALLOC, set the owner below
    int 21h
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
;Select default drive here so openStreams doesnt fail!
    movzx edx, byte [rbp + bootDrive]    ;Get the default drive
    mov ah, 0Eh ;Select drive
    int 21h

    call openStreams
    jc OEMHALT
;------------------------------------------------;
;             Print Welcome Message              ;
;------------------------------------------------;
    lea rdx, strtmsg
    mov ah, 09h
    int 21h    
;------------------------------------------------;
;               Load CONFIG.SYS                  ;
;------------------------------------------------;
;Setup stackframe, workout base 
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

    lea rdx, cfgspec    ;CONFIG.SYS, must be on bootdrive for now
    mov ah, 3Dh ;Open file for reading
    mov al, openRdAcc
    int 21h
    jc noCfg  ;If no CONFIG.SYS found, just use defaults that are already setup
    call configParse ;Else, parse the config file
;Config.sys mightve made changes to files. Update DOS!
    movzx eax, byte [rbp - cfgFrame.newSFTVal]
    mov byte fs:[numFiles], al
;If a driver installed a new DPB device, check if it's sector size is bigger.
    call sectorSizeSet  ;This is needed for setting the buffer sizes
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
    mov eax, 4800h  ;ALLOC, set the owner below
    int 21h
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
    mov eax, 4800h      ;ALLOC, set the owner below
    int 21h
    jc short .skipSFT   ;Skip adding files if this fails. Sorry end user!
    mov rsi, qword fs:[sftHeadPtr]
    mov qword [rsi + sfth.qNextSFTPtr], rax ;RAX points to the next sfth
    mov word [rax + sfth.wNumFiles], cx ;Move remaining files here
    mov qword [rax + sfth.qNextSFTPtr], -1  ;Last table in chain    
    sub rax, mcb.program    ;Point to MCB now
    mov qword [rax + mcb.owner], mcbOwnerDOS
    mov byte [rax + mcb.subSysMark], mcbSubFiles
    ;Point rdi to first sft in this arena
    lea rdi, qword [rax + sfth_size + mcb_size]
.initExtraSFTs:
    mov word [rdi], 0
    add rdi, sft_size   ;Goto next SFT
    dec ecx
    jnz .initExtraSFTs  ;Remember uop hybridisation (don't use loop)
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
    mov eax, 4800h      ;ALLOC, set the owner below
    int 21h
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
    ;First free the old CDS and then reallocate. If nothing changed, DOS
    ; will reallocate this space. This is done to create CDS's for any newly
    ; installed drives from CONFIG.SYS
    mov r8, qword fs:[cdsHeadPtr]
    mov eax, 4900h  ;FREE the old allocation, owned by mcbOwnerDOS.
    int 21h
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
    int 21h
    inc ebx ;Goto next handle
    cmp ebx, 6
    jne closeHandlesLoop
    call openStreams
l1:
    mov ebx, 1000h  ;Get a 64Kb block
    mov eax, 4800h  ;Allocate the memory block
    int 21h         ;Malloc and get pointer in rbx
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
    int 21h
l2:
;Load Shell now
    lea rdx, initBadRet
    mov eax, 2522h  ;Setup the return address if the top level process dies
    int 21h
    lea rbx, cmdBlock
    lea rsi, tempPSP
    lea rax, qword [rsi + psp.fcb1]
    mov qword [rbx + execProg.pfcb1], rax
    lea rax, qword [rsi + psp.fcb2]
    mov qword [rbx + execProg.pfcb2], rax
    lea rax, qword [rsi + psp.dta]  ;Get the dummy command line ptr
    mov qword [rbx + execProg.pCmdLine], rax    ;Store dummy command line here
    mov qword [rbx + execProg.pEnv], 0  ;Pass a zero to indicate initial load!
    lea rdx, cmdSpec
    mov eax, 4B00h  ;Exec Prog
    int 21h
initBadRet:
    lea rdx, badCom
    mov ah, 09h ;Print message
    int 21h
hltLbl:
    hlt
    pause
    jmp short hltLbl
badMem:
    lea rdx, memErr
    mov eax, 0900h
    int 21h
    jmp short hltLbl
memErr  db "System Memory Error",0Ah,0Dh,"$"

;--------------------------------------------------------;
;--------------------------------------------------------;
;                Procedures for SYSINIT                  ;
;--------------------------------------------------------;
;--------------------------------------------------------;

sectorSizeSet:
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
    return

openStreams:
;If this returns with CF=CY, an error occured.
    lea rdx, auxName
    mov eax, 3D02h   ;Open read/write
    int 21h
    retc
    mov ebx, eax
    mov ecx, 3  ;
    mov eax, 4600h  ;DUP2
    int 21h
    retc
    mov eax, 3e00h
    int 21h ;Close the original handle
    retc
    mov eax, 3D02h  ;Open read/write
    lea rdx, conName
    int 21h
    retc
    mov ebx, eax    ;Move file handle to ebx
    mov eax, 4500h  ;DUP
    int 21h
    retc
    mov eax, 4500h  ;DUP
    int 21h
    retc
    lea rdx, prnName
    mov eax, 3D02h
    int 21h       ;Open file
    return

addDriverMarkers:
;Traverses the MCB chain after a driver init to add the correct subsytem 
; information and owner to each memory block. Used for drivers that allocate
; their own memory using ALLOC.
;All drivers (kernel and config) initialise using sysinit's own PSP. Thus
; any allocations made during this time result in arenas with owner 
; of qword [currentPSP]. We replace each such owner with mcbOwnerDOS and
; set the subsytem mark to driver extra. This works because in config.sys
; we load the driver into its own arena first and modify the MCB manually.
;Kernel drivers dont have a separate allocation so any allocation after 
; init must be an extra block.
;Input: Nothing
;Output: Sets all MCBs with currentPSP owner to mcbOwnerDOS and mcbSubDrvExtra
    push rax
    push rsi
    push rdi
    mov rdi, qword [rbp + currentPSP]
    mov rsi, qword [rbp + mcbChainPtr] ;Points to the kernel allocation
    jmp short .gotoNextBlock    ;Skip the first alloc (the kernel)
.checkSubsystem:
    cmp qword [rsi + mcb.owner], rdi
    jne short .gotoNextBlock
;Twiddle this newly allocated block!
    mov byte [rsi + mcb.subSysMark], mcbSubDrvExtra
    mov byte [rsi + mcb.owner], mcbOwnerDOS
.gotoNextBlock:
    cmp byte [rsi + mcb.marker], mcbMarkEnd
    je short .exit
    mov eax, dword [rsi + mcb.blockSize]
    shl rax, 4
    add rsi, mcb.program    
    add rsi, rax
    jmp short .checkSubsystem
.exit:
    pop rdi
    pop rsi
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
    movzx eax, byte fs:[numPhysVol] ;Get current # drives
    mov byte [rbp + dpb.bDriveNumber], al   ;Set it as drvnum
    inc byte fs:[numPhysVol]    ;One more physical volume present!
    mov byte [rbp + dpb.bUnitNumber], ch    ;Set unit number
    mov qword [rbp + dpb.qDriverHeaderPtr], rdi ;Store ptr to driver

    push rsi
    mov rsi, qword [rsi]    ;Get the BPB pointer from the BPB array
    mov eax, 5300h ;Build DPB
    int 21h
    pop rsi

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
;       r8 -> Points to the mcb header for reallocation
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
    int 21h
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

buildKernDPBs:
;This is a wrapper for the below to be used ONLY FOR LOADING KERNEL DRIVERS. 
; If at boot time a FAT32 partition is detected, this otherwise would cause 
; the system to crash as build dpb requires at least one buffer in the buffer
; chain. So here we allocate one 4Kb buffer and place it in the chain. Once we 
; are done, we free this buffer.
    push rax
    push r8
    mov eax, 4800h  ;ALLOC, get ptr in rax
    push rbx
;100h for 4Kb + space for a buffer header!
    mov ebx, 100h + ((bufferHdr_size + 0Fh) >> 4)
    int 21h
    pop rbx
    jc .exit
    mov r8, rax
    ;Setup MCB metadata
    mov byte [r8 + mcb.subSysMark - mcb_size], mcbSubBuffers
    ;Setup Buffer
    mov qword [r8 + bufferHdr.nextBufPtr], -1
    mov qword [r8 + bufferHdr.wDrvNumFlg], freeBuffer
    mov qword [rbp + bufHeadPtr], r8
    call buildDPBs  ;Call function
    mov qword [rbp + bufHeadPtr], -1    ;Now empty the buffer pointer
    mov eax, 4900h  ;Free the ptr in r8
    int 21h
.exit:
    pop r8
    pop rax
    return
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
    int 21h
    jc short .exit
    mov rbp, rax    
    mov byte [rax + mcb.subSysMark - mcb_size], mcbSubDrvDPB  ;Set DPB marker
    mov qword [rax + mcb.owner - mcb_size], mcbOwnerDOS    ;Set DOS owner
    ;rsi -> Ptr to BPB array
	;rbp -> Ptr to buffer to hold first DPB
    ;rdi -> Ptr to the driver header
    call convertBPBArray    ;Returns rbp -> past last DPB
.exit:
    pop rdi
    pop rsi
    pop rbp
    pop rbx
    return

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
