

loadExecChild:     ;ah = 4Bh, EXEC
;Input: rdx = Ptr to the ASCIIZ string for the file to load. Must include ext.
;       rbx = Ptr to the parameter block used for loading
;       al = Subfunction:
;            00h = Load Program and transfer control to it
;            01h = Load Program but do not transfer ctrl to it
;            03h = Load overlay (no PSP) -> Blk copy file from filesystem 
;      Reserved for future expansion:
;            04h = Load in background execution mode (PE only)
;            If bit 7 of the subfunction byte is set, we load the corresponding
;               function but in 16-bit mode. 
;
;If the loaded file is not a PE it is assumed to be a COM/RFS file. 
; If not EXE, we read the filename extension. If it is RFS, we assign maximum 
; memory. If it is COM, we assign only 64Kb to the application.

;If AL = 0 :
;            Load Child Program and Xfr control to it
; rbx ------>   |-------------------------------|
;               |   Ptr to Environment Block    |
;               |                               |
;               | Can be 0 => Get pointer to    |
;               | parent Environment Block      |
;               |-------------------------------|
;               |   Ptr to Command Line to be   |
;               |   placed at PSP + 80h         |
;               |-------------------------------|
;               |   Ptr to first default FCB    |
;               |      to copy to PSP.fcb1      |
;               |                               |
;               | Should be used to parse the   |
;               |  first two command line args  |
;               |-------------------------------|
;               |   Ptr to second default FCB   |
;               |      to copy to PSP.fcb2      |
;               |-------------------------------|
;
;If AL = 1 :
;           Load Child Program BUT DO NOT Xfr control to it
; rbx ------>   |-------------------------------|
;               |         AS IN AL=00H          |
;               |-------------------------------|
;               |     Initial Value of RSP      |
;               |       on program launch       |
;               |                               |
;               |     This is returned to the   |
;               |         caller by DOS         |
;               |-------------------------------|
;               |     Initial Value of RIP      |
;               |       on program launch       |
;               |                               |
;               |     This is returned to the   |
;               |         caller by DOS         |
;               |-------------------------------|
;
;                      !!Important Note!!
;   The block pointed to by rbx must reserve the space for 
;        those last two qwords on a AL=1 call to EXEC.
;
;If AL = 3 :
;                      Load program overlay
; rbx ------>   |-------------------------------|
;               |  Pointer to the byte in the   |
;               |  prog space to start writing  |
;               |        the overlay at         |
;               |-------------------------------|
;               | DWORD offset from the base of |
;               |  the program to the location  |
;               |  the overlay is being loaded  |
;               |              in               |
;               |  (Called a Relocation Factor) |
;               |     Only FOR EXE Overlays     |
;               |        for CODE fixups        |
;               |  For COM, Current PSP + 100h  |
;               |      assumed to suffice       |
;               |-------------------------------|
;
; All three will setup both COM and EXE files for whatever purpose. 
;           AL = 3 DOES NOT BUILD THE PROGRAM A PSP.
;
;Start by setting up a stack frame of local vars to keep track of vars in call
    push rbp
    mov rbp, rsp
    sub rsp, execFrame_size   ;Make the space pointing at rbp
    cmp al, execOverlay
    jbe .validSubfunction
.badSubFunction:
    mov eax, errInvFnc
    mov byte [errorLocus], eLocUnk
.badExit:
    mov rsp, rbp
    pop rbp
    jmp extErrExit

.validSubfunction:
    cmp al, execInvld
    je .badSubFunction
    ;Save registers for each function call
    mov qword [rbp - execFrame.pParam], rbx
    mov qword [rbp - execFrame.pProgname], rdx
    movzx eax, al
    mov qword [rbp - execFrame.bSubFunc], rax   ;clear alignment and progHdl
    mov rdi, rdx
    call strlen ;Get string length in cx
    mov word [rbp - execFrame.wNameLen], cx   ;Get the string length  
    ;Now open the file we wanna yeet to
    xor eax, eax    ;al = 0 => Normal program attributes to search for
    push rbp    ;Preserve local frame ptr
    call openFileHdl
    pop rbp
    jc .badExit ;Exit preserving error code in al
    ;Now ax has the file handle
    mov word [rbp - execFrame.wProgHdl], ax
    movzx ebx, ax   ;Move file handle into bx
    call derefSFTPtr    ;And deref it into rdi
    movzx edx, word [rdi + sft.wDeviceInfo] ;Get device word
    test edx, devCharDev
    jz .validDiskFile    ;We cannot have a char device
    mov al, errFnf
    jmp .cleanAndFail
.validDiskFile:
    xor eax, eax
    mov qword [rbp - execFrame.pEnvBase], rax   ;Clear pEnv and pProg Bases
    mov qword [rbp - execFrame.pProgBase], rax
    cmp byte [rbp - execFrame.bSubFunc], execOverlay
    je .loadProgram ;If overlay, skip making an environment block
    mov rdi, qword [rbp - execFrame.pParam] ;Get params ptr in rdi
    mov rax, qword [rdi + execProg.pEnv]
    test rax, rax   ;Is this 0? (i.e. inherit parent env ptr)
    jnz .copyEnvironmentBlock
    mov rsi, qword [currentPSP] ;Get current PSP address in rsi
    mov rax, qword [rsi + psp.envPtr]   ;Get the environment ptr
    mov qword [rbp - execFrame.pEnvBase], rax   ;Store the parent ptr
    test rax, rax   ;Was the parent pointer 0? If so, make a new block
    jnz .loadProgram
.copyEnvironmentBlock:
    mov rdi, rax    ;Point rdi to the source of the environment
    ;Get the length of the environment
    mov ecx, 7fffh  ;Arbitrary 32kb DOS limit, consider increasing to 64Kb
    xor eax, eax
    mov rbx, rdi    ;Use rbx as the base ptr of the scan
.envVerifyLp:
    rep scasb   ;Scan for a terminating word of nulls
    jnz .invalidEnvironmentError
    jecxz .invalidEnvironmentError  ;Error if no space for a second null
    dec ecx
    scasb   ;Check if we have a second byte of 00 (i.e. end of environment)
    jnz .envVerifyLp

    sub rdi, rbx ;Get offset into block, gives a result less than 7FFFh
    push rdi     ;Save the length of the environment block
    add edi, 11h    ;Add 11 to round up when converting to paragraphs
    movzx ebx, word [rbp - execFrame.wNameLen]  ;Get name length
    add edi, ebx    ;edi has number of bytes to allocate for environment blk
    mov ebx, edi
    shr ebx, 4  ;Turn bytes needed into paragrapsh
    call allocateMemory
    pop rcx ;Pop the length of the environment block into rcx
    jnc .copyEnvironment
    ;Fall thru if not enuff memory
.insufficientMemory:
    mov eax, errNoMem
    jmp .cleanAndFail
.invalidEnvironmentError:   ;DO NOT MOVE THIS TO USE JECXZ ABOVE
    mov eax, errBadEnv
    jmp .cleanAndFail

.copyEnvironment:
    ;rax has the ptr to allocated memory block
    ;rcx has the number of chars to copy from the source env block
    mov rdi, rax    ;This is the destination of the copy
    mov qword [rbp - execFrame.pEnvBase], rax   ;Save the env block in frame
    mov rsi, qword [rbp - execFrame.pParam]
    mov rsi, qword [rsi + execProg.pEnv]    ;Get in rsi the src of the env
    rep movsb   ;Copy from rsi to rdi
    mov eax, 1  ;One additional string and a second null char!
    stosw       ;Away you go!
    mov rsi, qword [rbp - execFrame.pProgname]  ;Get ASCIIZ string for filespec
    movzx ecx, word [rbp - execFrame.wNameLen]
    rep movsb   ;Move the bytes to rdi
;Done with the environment... more or less
.loadProgram:
    mov ecx, imageDosHdr_size   ;Read the DOS header for the exe file
    lea rdx, exeHdrSpace    ;Read into the SDA area
    call .readDataFromHdl
    jc .badFmtErr
    test eax, eax   ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, imageDosHdr_size
    jb .loadCom

    cmp word [rdx + imageDosHdr.e_magic], dosMagicSignature
    je .proceedEXE
    cmp word [rdx + imageDosHdr.e_magic], dosMagicSignature2
    jne .loadCom    ;If not equal to ZM or MZ, must be a COM file
.proceedEXE:
    ;Now we need to read e_lfanew
    push rdx    ;Save exeHdrSpace addr on stack
    mov edx, dword [rdx + imageDosHdr.e_lfanew]
    mov dword [rbp - execFrame.dCOFFhdr], edx   ;Save this for later
    xor ecx, ecx    ;Officially, need to set ecx to 0
    xor al, al  ;Set file pointer from start of file
    movzx ebx, word [rbp - execFrame.wProgHdl]  ;Get handle
    call lseekHdl   ;Move to that position in the file

    pop rdx ;Get exeHdrSpace address back
    mov ecx, imageFileHeader_size
    call .readDataFromHdl       ;Now read the COFF header
    test eax, eax   ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, imageFileHeader_size
    jb .badFmtErr
    ;So now rdx points to the imageFileHeader
    cmp word [rdx + imageFileHeader.wMachineType], imageFileMachineAMD64
    jne .badFmtErr
    cmp word [rdx + imageFileHeader.wSizeOfOptionalHdr], imageFileOptionalHeader_size
    jb .badFmtErr   ;We need the full optional header (as normal)
    ;Now save the number of sections in the the file
    movzx eax, word [rdx + imageFileHeader.wNumberOfSections]
    test eax, eax   ;If this is 0, what?
    jz .badFmtErr
    mov word [rbp - execFrame.wNumSeg], ax  ;Save this value for later!

    ;Now load Optional header, file ptr points to it so all good!
    mov ecx, imageFileOptionalHeader_size
    ;rdx points to exeHdrSpace
    call .readDataFromHdl
    test eax, eax   ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, imageFileOptionalHeader_size
    jb .badFmtErr
    ;Now File Ptr points to data dirs, not an issue
    add dword [rbp - execFrame.dFilePtr], imageFileOptionalHeader_size
    ;We load the program in, one section at a time, reading section headers in
    ; one at a time to the section header internal buffer.
    cmp byte [rbp - execFrame.bSubFunc], execOverlay
    je .exeOvlySkipAlloc    ;DONT allocate memory if loading an overlay
    mov ebx, dword [exeHdrSpace + imageFileOptionalHeader.dSizeOfImage]
    mov rax, qword [exeHdrSpace + imageFileOptionalHeader.qSizeOfStackReserve]
    test rax, rax
    jnz .notDefaultStackAlloc
    mov rax, 40000h ;256Kb stack default
.notDefaultStackAlloc:
    add ebx, eax    ;Add stack allocation
    mov dword [rbp - execFrame.dProgSize], ebx  ;Save the program size
    add ebx, 11
    shr ebx, 4  ;Turn into paragraphs
    call allocateMemory ;Get in rax, ptr to memory block
    jc .insufficientMemory  ;Unless not enough, sorry buddy!
    mov qword [rbp - execFrame.pPSPBase], rax  ;Save ptr here, psp will go here
    add rax, 100h
    mov qword [rbp - execFrame.pProgBase], rax  ;First byte of code goes here
    jmp short .exeProceed1
.exeOvlySkipAlloc:
    mov rbx, qword [rbp - execFrame.pParam]
    mov rax, qword [rbx + loadOvly.pLoadLoc]    ;Get the load addr
    mov qword [rbp - execFrame.pProgBase], rax
.exeProceed1:
    ;So now copy one section at a time, read section header in
    ;File pointer points to the directory table, so skip that
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dNumberOfRvaAndSizes]
    cmp edx, 6  ;Does .reloc exist (6th directory entry)
    jb .badFmtErr
    dec edx ;Decrement by 1 to make into a 0 based offset
    ;Each directory is 8 bytes, so multiply edx by 8
    shl edx, 3  ;edx has number of bytes to move file pointer forwards by
    xor ecx, ecx
    movzx ebx, word [rbp - execFrame.wProgHdl]
    mov al, 1   ;Move handle forwards from current position
    call lseekHdl   ;Move the handle forwards by that many bytes
    ;eax has pointer location after adjustment
    mov dword [rbp - execFrame.dFilePtr], eax   ;We have moved to section table
    ;File now points to start of Section headers. Read first header in.
    ;USE ECX AS COUNTER FOR HEADERS LEFT TO PROCESS
    mov rdi, qword [rbp - execFrame.pProgBase]  ;Move prog base in rdi
    movzx ecx, word [rbp - execFrame.wNumSeg]   ;Get number of segments in ecx
.loadLp:
    push rcx    ;Save # of segments left on stack
    push rdi    ;Save the current buffer ptr here
    mov ecx, imageSectionHdr_size
    lea rdx, sectHdr    ;Read section headers here
    call .readDataFromHdl
    pop rdi
    pop rcx
    test eax, eax
    jz .badFmtErr
    cmp eax, imageSectionHdr_size
    jne .badFmtErr
    ;File ptr moved forwards to next section header
    add dword [rbp - execFrame.dFilePtr], imageSectionHdr_size
    ;Section header read, now we load section into memory
    ;Move file ptr to data location
    mov edx, dword [sectHdr + imageSectionHdr.dPointerToRawData] ;Data File ptr
    movzx ebx, word [rbp - execFrame.wProgHdl]  ;Get the handle
    xor eax, eax    ;Seek from start of file
    push rcx
    push rdi
    call lseekHdl
    pop rdi
    pop rcx

    push rcx
    push rdi    ;Save current buffer offset
    mov ecx, dword [sectHdr + imageSectionHdr.dSizeOfRawData]   ;Get # of bytes
    mov rdx, rdi    ;Get ptr to buffer in rdx
    call .readDataFromHdl
    pop rdi
    pop rcx
    jc .badFmtErr
    test eax, eax
    jz .badFmtErr
    cmp eax, dword [sectHdr + imageSectionHdr.dSizeOfRawData]
    jne .badFmtErr
    ;Data read ok, now fill in any zeros needed
    mov eax, dword [sectHdr + imageSectionHdr.dSizeOfRawData]
    add rdi, rax    ;Move rdi forwards by that amount at least
    cmp eax, dword [sectHdr + imageSectionHdr.dVirtualSize]
    jae .skipPadding
    push rcx
    mov ecx, dword [sectHdr + imageSectionHdr.dVirtualSize]
    sub ecx, eax    ;Get number of bytes to pad with in ecx
    ;rdi points to pad space
    xor eax, eax
    rep stosb   ;Pad that many zeros
    pop rcx
.skipPadding:
    dec ecx ;Decrement our section counter
    jz .doExeFixups
    ;Read next section header in here
    push rcx
    push rdi
    xor al, al  ;Move rel start of file
    mov edx, dword [rbp - execFrame.dFilePtr]
    movzx ebx, word [rbp - execFrame.wProgHdl] ;Get the file handle
    xor ecx, ecx
    call lseekHdl
    pop rdi
    pop rcx
    jmp .loadLp
.doExeFixups:
;Here we fixup addresses as needed
;Program Entrypoint is saved in the header structure in the SDA
;Move File pointer to COFF header Coff + optional header sizes
;We look only for .reloc segment. We have it in memory too so use it to make 
; fixups. Zero the in memory image of reloc segment once we are done with it. 
;We checked that .reloc exists so all ok
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dNumberOfRvaAndSizes]
    mov edx, dword [rbp - execFrame.dCOFFhdr]
    add edx, imageFileHeader_size + imageFileOptionalHeader_size + 5*8
    ;eax now points to position in file of direcotry entry for reloc
    movzx ebx, word [rbp - execFrame.wProgHdl]  ;Get handle in bx
    xor eax, eax
    call lseekHdl   ;Move handle there in file
    mov dword [rbp - execFrame.dFilePtr], eax   ;Save table offset here
    mov ecx, imageDataDirectory_size
    ;Read 8 bytes into sectHdr space
    lea rdx, sectHdr
    call .readDataFromHdl   ;Read this directory entry in
    ;Now we have the offset in memory if the file was loaded at imageBase
    mov esi, dword [sectHdr + imageDataDirectory.virtualAddress]
    test esi, esi   ;If there are no relocations, skip this...
    jz .buildChildPSP   ;... including if overlay
    add rsi, qword [rbp - execFrame.pProgBase]
    ;Now rsi points to where in memory the relocation data table is
    ;Now compute the relocation factor 
    mov rax, qword [rbp - execFrame.pProgBase]
    sub rax, qword [exeHdrSpace + imageFileOptionalHeader.qImageBase]
    ;The value in rax gives how much to subtract by
    cmp byte [rbp - execFrame.bSubFunc], execOverlay
    jne .notOverlayReloc
    mov rbx, qword [rbp - execFrame.pParam]
    add eax, dword [rbx + loadOvly.dRelocFct]   ;Add the overlay reloc factor
.notOverlayReloc:
    mov qword [rbp - execFrame.qRelocVal], rax  ;Save relocation value
    mov rbx, rax    ;Save this relocation factor in rbx
    ;rsi points to relocation data table in memory
    mov ecx, dword [sectHdr + imageDataDirectory.size]  ;Get number of words
    cmp ecx, 0    ;If no relocations, skip
    jz .buildChildPSP
    mov rdi, qword [rbp - execFrame.pProgBase]  ;Point to start of program
    ;rsi points to the first base relocation block. The relocations begin
    ; after the first block
    ;ecx has the number of base relocation blocks to process.
.nextBlock:
    push rcx    ;Reuse rcx as a counter for the current page
    mov eax, dword [rsi + baseRelocBlock.pageRVA]   ;Get the page rva
    add rdi, rax    ;Add this page offset to rdi to goto correct page for reloc
    mov ecx, dword [rsi + baseRelocBlock.size]  ;Get number of bytes
    shr ecx, 1  ;Divide by 2 to get number of words = # of relocs to do
    jecxz .blockDone
.blockNotDone:
    lodsw   ;Get the next page offset word
    and eax, 00000FFFh  ;Save bottom 12 bits
    add rdi, rax    ;Add this offset to rdi, the pointer to program image
    ;rdi points to qword to rebase
    add qword [rdi], rbx    ;Relocation factor was saved in rbx
    dec ecx
    jnz .blockNotDone
.blockDone:
    pop rcx
    dec ecx
    jnz .nextBlock
    mov eax, dword [exeHdrSpace + imageFileOptionalHeader.dAddressOfEntryPoint]
    add rax, qword [rbp - execFrame.pProgBase]
    mov qword [rbp - execFrame.pProgEP], rax
    jmp .buildChildPSP
.loadCom:
    ;File is open here, so just read the file into memory. 
    ;The file cannot exceed 64Kb in size. COM ONLY for small files!!!!
    ;Allocate 64Kb of memory, or as much as we can
    cmp byte [rbp - execFrame.bSubFunc], execOverlay
    je .comOverlay
    mov ebx, 0FFFFh ;64Kb pls
    mov dword [rbp - execFrame.dProgSize], ebx
    call allocateMemory
    jnc .comallocOk
    cmp al, errNoMem
    jne .cleanAndFail   ;Propagate the proper error if not a lack of memory
    ;rbx should have the amount available
    ;We check if this value is 100h than filesize
    push rbx    ;Save new minimum size
    mov eax, 2    ;Reposition to end of file
    movzx ebx, word [rbp - execFrame.wProgHdl]
    xor edx, edx    ;Go to end of file
    call lseekHdl
    ;eax has file size
    pop rbx ;Get back max alloc size
    mov edx, ebx    ;Save in max alloc in edx temporarily
    sub edx, eax
    cmp edx, 100h   ;If the difference of filesize and memory space is < 100h
    jb .insufficientMemory   ;Fail
    mov dword [rbp - execFrame.dProgSize], ebx  ;Store progsize
    call allocateMemory
    jc .cleanAndFail
    jmp .comallocOk
.comOverlay:
    ;Here we simply read the file into the buffer provided
    mov rbx, qword [rbp - execFrame.pParam]
    mov rax, qword [rbx + loadOvly.pLoadLoc]
    mov qword [rbp - execFrame.pProgBase], rax
    jmp short .comRead
.comallocOk:
    ;rax should point to the first byte
    mov qword [rbp - execFrame.pPSPBase], rax
    sub rax, 100h
.comRead:
    mov qword [rbp - execFrame.pProgBase], rax

    mov eax, 2    ;Reposition to end of file
    movzx ebx, word [rbp - execFrame.wProgHdl]
    xor edx, edx    ;Go to end of file
    call lseekHdl
    ;eax has filesize now
    push rax    ;Save filesize
    xor eax, eax    ;Reposition to start of file
    movzx ebx, word [rbp - execFrame.wProgHdl]
    xor edx, edx    ;Go to start of file
    call lseekHdl
    pop rcx ;Get the filesize in rcx (# of bytes to read)
    mov rdx, qword [rbp - execFrame.pProgBase]  ;Buffer to read into
    call .readDataFromHdl   ;Read from the file handle
    mov rax, qword [rbp - execFrame.pProgBase]
    mov qword [rbp - execFrame.pProgEP], rax
.buildChildPSP:
    ;We can close handle now
    movzx ebx, word [rbp - execFrame.wProgHdl]
    push rbp
    call closeFileHdl   ;Close the file
    pop rbp

    ;Only build a PSP if not in overlay mode. If in overlay mode skip
    cmp byte [rbp - execFrame.bSubFunc], execOverlay
    je .overlayExit
    ;Now build the PSP
    mov esi, dword [rbp - execFrame.dProgSize]
    mov rdx, qword [rbp - execFrame.pPSPBase]
    push rdx
    push rbp
    call createPSP
    pop rbp
    pop rdx
    ;Now set Current PSP to our PSP and set current DTA to command line
    mov qword [currentPSP], rdx
    lea rdi, qword [rdx + psp.parmList]
    mov qword [currentDTA], rdi
    ;Now We need to copy over the command line and fcbs to the PSP
    ; and set FS to point to the PSP
    ;rdx points to PSP segment  
    ;rdi points to commandline in PSP anyway
    mov rbx, qword [rbp - execFrame.pParam] ;Get the paramter block ptr in rbx
    mov ecx, 80h    ;copy all 128 chars in command tail
    mov rsi, qword [rbx + execProg.pCmdLine]
    rep movsb   ;Copy the string over
    lea rdi, qword [rdx + psp.fcb1]
    mov ecx, fcb_size
    mov rsi, qword [rbx + execProg.pfcb1]
    mov al, byte [rsi + fcb.driveNum]   ;Get FCB1's drive number in al
    rep movsb   ;Copy fcb 1 over
    lea rdi, qword [rdx + psp.fcb2]
    mov ecx, fcb_size
    mov rsi, qword [rbx + execProg.pfcb2]
    mov ah, byte [rsi + fcb.driveNum]   ;Get FCB2's drive number in ah
    rep movsb   ;Copy fcb 2 over

    mov ebx, eax  ;Save the fcb drive numbers in bx

    ;Put PSP base value in edx:eax to place in FS
    mov ecx, 0C0000100h ;Read FS MSR
    mov rax, qword [rbp - execFrame.pPSPBase]
    mov rdx, rax
    shr rdx, 20h    ;Shift high dword in low dword
    or eax, eax ;Clear upper bits
    wrmsr   ;Write the new value to FS MSR

    call getUserRegs    ;Need to get Int 42h address from stack
    mov rdx, qword [rbp - execFrame.pPSPBase]
    mov qword [rdx + psp.oldInt42h], rdx
    push rdx    ;Save PSPBase on stack
    push rbx    ;Save BX drive numbers
    mov rdx, qword [rax + callerFrame.rip]
    mov al, 42h
    call setIntVector   ;Set interrupt vector and write it to PSP manually
    pop rbx
    pop rdx

    ;Check FCB drive numbers are valid. Return FFh if not
    mov al, bl
    xor bl, bl
    call setDrive
    jnc .drive1Ok
    mov bl, -1
.drive1Ok:
    mov al, bh
    xor bh, bh
    call setDrive
    jnc .drive2Ok
    mov bh, -1
.drive2Ok:
    ;bx has validity flags for the two fcb drives, undocumented!!

    mov esi, dword [rbp - execFrame.dProgSize]  ;Get program size
    lea rsi, qword [rsi + rdx - 8]    ;Get Stack Ptr in rsi
    cmp byte [rbp - execFrame.bSubFunc], execLoadGo
    je .xfrProgram
    mov rax, qword [rbp - execFrame.pProgEP]
    mov rbx, qword [rbp - execFrame.pParam]
    mov qword [rbx + loadProg.initRIP], rax
    mov qword [rbx + loadProg.initRSP], rsi
    movzx eax, bx   ;Return fcb drive status
.overlayExit:
    mov rsp, rbp    ;Reset the stack to its position
    pop rbp ;Point rsp to the return address
    jmp extGoodExit ;And return!
.xfrProgram:
    cli
    mov rsp, rsi    ;Set rsp to initRSP value
    mov byte [inDOS], 0 ;Clear all inDosnessness
    sti
    push qword [rbp - execFrame.pProgEP]
    movzx eax, bx   ;ax must contain validity of the two FCB drives
    xor ebp, ebp
    mov esi, ebp
    mov ebx, ebp
    return  ;Return to child task

.badFmtErr:
    mov eax, errBadFmt  ;Fall thru with bad resource format error
.cleanAndFail:
;Close the open file and any open resources and fail
    call .clearArenaOwner   ;Enters level 1 critical section
    call dosCrit1Exit
    movzx ebx, word [rbp - execFrame.wProgHdl]
    push rax    ;Save error code
    push rbp
    call closeFileHdl
    pop rax
    pop rbp
    jmp .badExit

.readDataFromHdl:
;Input: bx = File Handle
;       ecx = Number of bytes to transfer
;       rdx = Ptr to the buffer to use
    call .clearArenaOwner   ;Entering critical section!
    movzx ebx, word [rbp - execFrame.wProgHdl]
    push rbp
    call readFileHdl
    pop rbp
    call .setPSPArenaOwner  ;Exiting critical section!
    return
.clearArenaOwner:
    push rbx
    xor ebx, ebx    ;Make owner null, ok to trash flags here
    call dosCrit1Enter
    call .setProgOrEnvArenaOwnerToRBX
    pop rbx
    return
.setPSPArenaOwner:
;Sets the current PSP as the arena owner
    push rbx
    mov rbx, qword [currentPSP]
    call .setProgOrEnvArenaOwnerToRBX
    call dosCrit1Exit
    pop rbx
    return
.setProgOrEnvArenaOwnerToRBX:
;Input: rbx = Owner ID  (Start of PSP address)
    pushfq
    push rax
    ;Only one of the two below addresses may be non zero at any one time!
    ;This is because they are set up at separate points in the routine!
    mov rax, qword [rbp - execFrame.pProgBase]
    call .writeArenaHeaderOwner
    mov rax, qword [rbp - execFrame.pEnvBase]
    call .writeArenaHeaderOwner
    pop rax
    popfq
    return
.writeArenaHeaderOwner:
;Input: rax = Ptr to arena (NOT HEADER)
;       rbx = Owner ID
    test rax, rax   ;Don't write if arena header null
    retz
    sub rax, mcb.program    ;Go to start of arena header
    mov qword [rax + 1], rbx
    return
