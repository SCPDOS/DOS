

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

;If AL = 0 and 4 (if DOSMGR present):
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
;               |        the overlay at.        |
;               |-------------------------------|
;               | QWORD value used as the base  |
;               |   address for relocation      |
;               |   computations. In most cases |
;               |   should be the same as the   |
;               |   load pointer. The desired   |
;               |   load address is subtracted  |
;               |   from this value to generate |
;               |   a valid relocation factor,  |
;               |   much like for normal load   |
;               |   but here we are controlling |
;               |    precisely the factor by    |
;               |     which we relocate the     |
;               |           symbols.            |
;               |  (Called a Relocation Factor) |
;               |     Only FOR EXE Overlays.    |
;               |    For COM, 0 is sufficient.  |
;               |-------------------------------|
; For AL = 3: Both pointers must be aligned to section alignment.
;   If this is not the case, DOS will round UP both pointers to the 
;   section alignment for the file being loaded. 
; The overlay will be loaded so that the first byte at the start of the memory
; block is the first byte of the first section of the executable. No header
; information will be retained. Thus, (for now) overlays cannot be used to export 
; functions or import functions from DLLs. Furthermore, it is HIGHLY recommended
; that you compile any .EXE overlays to have a section alignment of 1 byte.
;
; All three will setup both COM and EXE files for whatever purpose. 
;           AL = 3 DOES NOT BUILD THE PROGRAM A PSP.
;
;Start by setting up a stack frame of local vars to keep track of vars in call
    push rbp
    mov rbp, rsp
    sub rsp, execFrame_size   ;Make the space pointing at rbp
    ;Clear up the pointers on the stack frame
    xor ecx, ecx
    mov qword [rbp - execFrame.pPSPBase], rcx
    ;These two are cleared
    ;mov qword [rbp - execFrame.pEnvBase], rcx
    ;mov qword [rbp - execFrame.pProgBase], rcx
    mov qword [rbp - execFrame.pPSPBase], rcx
    mov qword [rbp - execFrame.pProgEP], rcx

    mov ah, execOverlay
    test byte [dosMgrPresent], -1 ;If bits set, change max to execBkgrnd
    jz short .noMulti
    mov ah, execBkgrnd
.noMulti:
    cmp al, ah
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
    cmp qword [rbp - execFrame.bSubFunc], execOverlay
    je .loadProgram ;If overlay, skip making an environment block
    mov rdi, qword [rbp - execFrame.pParam] ;Get params ptr in rdi
    mov rax, qword [rdi + execProg.pEnv]
    test rax, rax   ;Is this 0? (i.e. inherit parent env ptr)
    jnz short .copyEnvironmentBlock
    mov rsi, qword [currentPSP] ;Get current PSP address in rsi
    mov rax, qword [rsi + psp.envPtr]   ;Get the environment ptr
    mov qword [rbp - execFrame.pEnvBase], rax   ;Store the parent ptr
    test rax, rax   ;Was the parent pointer 0? If so, skip
    jz short .loadProgram ;This is used by the boot process!!
.copyEnvironmentBlock:
    mov rdi, rax    ;Point rdi to the source of the environment
    ;Get the length of the environment
    mov ecx, 7fffh  ;Arbitrary 32kb DOS limit, consider increasing to 64Kb
;    cmp byte [rdi - mcb_size + mcb.marker], mcbMarkCtn
;    je .envMCBFound
;    cmp byte [rdi - mcb_size + mcb.marker], mcbMarkEnd
;    jne .invalidEnvironmentError
;.envMCBFound:
    ;The env block mcb size must be between 160 and 32768 bytes.
    ;Get the length of the environment from the MCB itself!
;    xor ecx, ecx
;    mov ecx, dword [rdi - mcb_size + mcb.blockSize]
;    shl ecx, 4  ;Convert to bytes from paragraphs
;    cmp ecx, 8000h  ;Is it above 32Kb?
;    ja .invalidEnvironmentError ;Error out!
;    cmp ecx, 0A0h   ;Is the environment less than 160 bytes?
;    jb .invalidEnvironmentError ;Error out!
    breakpoint
    xor eax, eax
    mov rbx, rdi    ;Use rbx as the base ptr of the scan
.envVerifyLp:
    repne scasb   ;Scan for a terminating word of nulls
    jnz short .invalidEnvironmentError
    jecxz .invalidEnvironmentError  ;Error if no space for a second null
    dec ecx
    scasb   ;Check if we have a second byte of 00 (i.e. end of environment)
    jnz short .envVerifyLp

    sub rdi, rbx ;Get offset into block, gives a result less than 7FFFh
    push rdi     ;Save the length of the environment block
    add edi, 11h    ;Add 11 to round up when converting to paragraphs
    movzx ebx, word [rbp - execFrame.wNameLen]  ;Get name length
    add edi, ebx    ;edi has number of bytes to allocate for environment blk
    mov ebx, edi
    shr ebx, 4  ;Turn bytes needed into paragrapsh
    push rbp
    call allocateMemory
    pop rbp
    pop rcx ;Pop the length of the environment block into rcx
    jnc short .copyEnvironment
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
    cmp dword [rdx + imageFileHeader.dPESignature], imagePESignature
    jne .badFmtErr
    cmp word [rdx + imageFileHeader.wMachineType], imageFileMachineAMD64
    jne .badFmtErr
    ;Check the binary is executable
    movzx eax, word [rdx + imageFileHeader.wCharacteristics]
    test ax, imageFileExecutable
    jz .badFmtErr
    mov word [rbp - execFrame.wCOFFChars], ax   ;Save this for later!

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
    cmp qword [rbp - execFrame.bSubFunc], execOverlay
    je .exeOvlySkipAlloc    ;DONT allocate memory if loading an overlay
    mov ebx, dword [exeHdrSpace + imageFileOptionalHeader.dSizeOfImage]
    mov rax, qword [exeHdrSpace + imageFileOptionalHeader.qSizeOfStackReserve]
    test rax, rax
    jnz .notDefaultStackAlloc
    mov rax, 40000h ;256Kb stack default
.notDefaultStackAlloc:
    add ebx, eax    ;Add stack allocation
    add ebx, psp_size   ;Add space for the PSP to the allocation too
    ;Add also one section alignment as it is likely that our arena won't be 
    ; section aligned and so we will need to align before reading, to 
    ; guaranee that we will have space for the EXE header to be read in later.
    add ebx, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    mov dword [rbp - execFrame.dProgSize], ebx  ;Save the program size
    add ebx, 11h
    shr ebx, 4  ;Turn into paragraphs
    push rbp
    call allocateMemory ;Get in rax, ptr to memory block
    pop rbp
    jc .insufficientMemory  ;Unless not enough, sorry buddy!
    mov qword [rbp - execFrame.pPSPBase], rax  ;Save ptr here, psp will go here
    add rax, psp_size
    mov qword [rbp - execFrame.pProgBase], rax  ;First byte of exe hdr goes here
    ;Finally, just check that we have some code to execute. 
    ;Empty code sections are NOT allowed if executing. Only for overlays
    cmp dword [exeHdrSpace + imageFileOptionalHeader.dSizeOfCode], 0
    je .badFmtErr   ;If no bytes, exit error
    jmp short .exeProceed1
.exeOvlySkipAlloc:
    mov rbx, qword [rbp - execFrame.pParam]
    mov rax, qword [rbx + loadOvly.pLoadLoc]    ;Get the load addr
    mov qword [rbp - execFrame.pProgBase], rax
.exeProceed1:
;===========================================================================
    ;The below blocks are being kept because they can be turned on later
    ; to change this exe loader to force section alignment of the 
    ; base load address. It appears there is no need to enforce that the 
    ; base load address be section aligned, but individual sections need
    ; to adhere to the section alignment requirements thereafter. 
    ;Finally, the pProgBase gets rescaled so that the first byte of the 
    ; first section gets places at the load address and not at some
    ; offset from it (as is usually the case, offset 1000h).
;===========================================================================
    ;=======================================================================
    ;Now we align the progBase to full header size aligned to the next page
    ;mov ebx, dword [exeHdrSpace + imageFileOptionalHeader.dSizeOfHeaders]
    ;add rax, rbx    ;Add this offset where the header should go in future
    ;=======================================================================
    ;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    ;It appears that the load address does NOT need to be aligned at all xD
    ;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    ;Now we section pad. Once aligned, that is the program base address!
    ;push rax
    ;mov ecx, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    ;dec ecx ;Turn into a mask
    ;and rax, rcx    ;Compute ptr modulo mask
    ;inc ecx
    ;sub rcx, rax
    ;pop rdi
    ;xor eax, eax
    ;rep stosb
    ;mov qword [rbp - execFrame.pProgBase], rdi
    ;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

    ;So now copy one section at a time, read section header in
    ;File pointer points to the directory table, so skip that
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dNumberOfRvaAndSizes]
    ;Load however many directories we have into place
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
    xor esi, esi    ;Use as an indicator for the first data segment. 
    mov qword [rbp - execFrame.bSegCount], rsi  ;Clear the segment counter
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
    test edx, edx
    jz short .skipRawPtrMove
    movzx ebx, word [rbp - execFrame.wProgHdl]  ;Get the handle
    xor eax, eax    ;Seek from start of file
    push rcx
    push rdi
    call lseekHdl
    pop rdi
    pop rcx
    ;Is this a overlay load?
    cmp qword [rbp - execFrame.bSubFunc], execOverlay
    jne short .skipRawPtrMove
    ;Here we rescale to put the first byte at pLoadLoc and use the 
    ; rescale value against RelocFct to compute the qRelocVal for later
    ;Is this is the first segment with data being read into memory?
    inc qword [rbp - execFrame.bSegCount]
    cmp qword [rbp - execFrame.bSegCount], 1 
    jne short .skipRawPtrMove   ;If not, skip
    ;Now rebase the program to point the first byte of the first
    ; section at the ProgBase.
    push rcx
    push rdi
    mov rdi, qword [rbp - execFrame.pProgBase]  ;Get the load address
    mov ecx, dword [sectHdr + imageSectionHdr.dVirtualAddress]
    sub rdi, rcx    ;Rebase by offset of the first section
    mov qword [rbp - execFrame.pProgBase], rdi 
    mov rdi, qword [rbp - execFrame.pParam]
    mov rdi, qword [rdi + loadOvly.qRelocFct]   ;Get the reload factor
    sub rdi, rcx    ;Now rescale the relocation factor by the same amount
    mov qword [rbp - execFrame.qRelocVal], rdi  ;Now store this value for later
    pop rdi
    pop rcx
.skipRawPtrMove:
    push rcx
    xor edi, edi
    mov edi, dword [sectHdr + imageSectionHdr.dVirtualAddress]  ;Get where it should go in memory, offset from image base
    add rdi, qword [rbp - execFrame.pProgBase]  ;Turn into offset from progbase
    ;If a section has a virtual address outside of the allocation arena
    ; refuse to load it IF it contains no BSS, Data or Code and skip to the 
    ; next section.
    mov rdx, qword [rbp - execFrame.pPSPBase]
    test rdx, rdx   ;If this is 0 (as in the case of overlay)...
    jz short .okToLoad  ;skip this as it is assumed there is enough space!
    sub rdx, mcb_size   ;Go back a unit of mcb
    xor ecx, ecx
    mov ecx, dword [rdx + mcb.blockSize]
    shl rcx, 4  ;Convert to bytes (multiply by 16)
    add rdx, mcb_size   ;Go to the first byte of the mcb
    add rdx, rcx    ;Now rdx points to the first byte outside the arena
    cmp rdx, rdi    ;If rdx > rdi, we are ok
    ja short .okToLoad
    ;Now check if this is a useless section. If so, we don't load it at all

    ;V-0000000000-EARMARK FOR REMOVAL-0000000000-V
    test dword [sectHdr + imageSectionHdr.dCharacteristics], imgScnCntBSS | imgScnCntCode | imgScnCntData
    jnz .badFmtErr  ;If any of these bits set, error out
    ;^-0000000000-EARMARK FOR REMOVAL-0000000000-^

    ;Else, just skip this section, goto next section
    pop rcx
    jmp short .gotoNextSection
.okToLoad:
    mov ecx, dword [sectHdr + imageSectionHdr.dVirtualSize]   ;Get # of bytes to read
    mov rdx, rdi    ;Get ptr to buffer in rdx
    push rdi    ;Save section in memory locally
    call .readDataFromHdl
    pop rdi
    pop rcx     ; Matches push after skipRawPtrMove
    jc .badFmtErr
    ;Don't check for a zero section read as empty sections may be present!
    cmp eax, dword [sectHdr + imageSectionHdr.dVirtualSize]
    jne .badFmtErr
    ;Data read ok, now fill in any zeros needed
    add rdi, rax    ;Move rdi forwards by that amount at least

    push rcx
    ;Here do section padding
    mov rax, rdi    ;Get the current address
    mov ecx, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    dec ecx ;Turn into a mask
    and rax, rcx    ;Compute ptr modulo mask
    inc ecx
    sub rcx, rax
    xor eax, eax
    rep stosb
    pop rcx
.gotoNextSection:
    dec ecx ;Decrement our section counter
    jz short .doExeFixups
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
;Here we fixup addresses if needed
;If the program is loaded at its desired location never need to relocate.
;Else we need to have a .reloc section to see if we need to do relocations. 
;   If the program has had it's .reloc section stripped by the linker
;       or has no .reloc directory, we fail
;   Else, we read the .reloc directory in. It most likely will have no 
;       relocations anyway.

;If program base = desired load, skip relocs
    mov rdx, qword [rbp - execFrame.pProgBase]
    cmp rdx, qword [exeHdrSpace + imageFileOptionalHeader.qImageBase]
    je .exeComplete
;If program has had relocs stripped, fail
    movzx eax, word [rbp - execFrame.wCOFFChars]
    test ax, imageFileRelocsStripped
    jnz .badFmtErr
;If program has no .reloc section, fail
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dNumberOfRvaAndSizes]
    cmp edx, 6  ;Does .reloc exist (6th directory entry)
    jb .badFmtErr ;Need relocs but no .reloc directory exists
;Now we get the reloc section
    mov edx, dword [rbp - execFrame.dCOFFhdr]
    add edx, imageFileHeader_size + imageFileOptionalHeader_size + 5*8
    ;eax now points to position in file of directory entry for reloc
    movzx ebx, word [rbp - execFrame.wProgHdl]  ;Get handle in bx
    xor eax, eax
    call lseekHdl   ;Move handle there in file
    mov dword [rbp - execFrame.dFilePtr], eax   ;Save table offset here
    mov ecx, imageDataDirectory_size
    ;Read 8 bytes into sectHdr space
    lea rdx, sectHdr
    call .readDataFromHdl   ;Read this directory entry in
    test eax, eax
    jz .badFmtErr
    cmp eax, ecx
    jne .badFmtErr
    ;Now we have the offset in memory if the file was loaded at imageBase
    mov esi, dword [sectHdr + imageDataDirectory.virtualAddress]
    test esi, esi   ;If there are no relocations, skip this...
    jz .exeComplete   ;... including if overlay
    add rsi, qword [rbp - execFrame.pProgBase]
    ;Now rsi points to where in memory the relocation data table is
    ;Now compute the relocation factor =
    ;   Difference from the load address and prefered
    mov rax, qword [rbp - execFrame.pProgBase]
    sub rax, qword [exeHdrSpace + imageFileOptionalHeader.qImageBase] 
    cmp qword [rbp - execFrame.bSubFunc], execOverlay
    jne short .notOverlayReloc
    ;For overlays, we use the relocation factor as the base of computation.
    ;Thus now the relocation factor becomes the ProgBase.
    ;This should be the same as ProgBase anyway for overlays.
    mov rax, qword [rbp - execFrame.qRelocVal]   ;Get the overlay reloc factor
    sub rax, qword [exeHdrSpace + imageFileOptionalHeader.qImageBase]
    ;Store this as the overlay program base
    mov qword [rbp - execFrame.pProgBase], rax
.notOverlayReloc:
    mov rbx, rax    ;Save this relocation factor in rbx
    ;rsi points to relocation data table in memory
    mov ecx, dword [sectHdr + imageDataDirectory.size]  ;Get number of words
    test ecx, ecx    ;If no relocations, skip
    jz .exeComplete
    ;rsi points to the first base relocation block. The relocations begin
    ; after the first block
    ;ecx has the number of base relocation blocks to process.
.nextBlock:
    push rcx    ;Reuse rcx as a counter for the current page
    mov eax, dword [rsi + baseRelocBlock.pageRVA]   ;Get the page rva
    mov rdi, qword [rbp - execFrame.pProgBase]  ;Point to start of program
    add rdi, rax    ;Add this page offset to rdi to goto correct page for reloc
    mov ecx, dword [rsi + baseRelocBlock.size]  ;Get number of bytes in block
    jecxz .blockDone    
    sub ecx, 8
    add rsi, 8  ;Go to the start of the directory data
    shr ecx, 1  ;Get number of directories = # of relocs to do
.blockNotDone:
    lodsw   ;Get the next page offset word
    and eax, 00000FFFh  ;Save bottom 12 bits
    ;rdi points to base, rax give offset into 4Kb page
    add qword [rdi + rax], rbx    ;Relocation factor was saved in rbx
    dec ecx
    jnz .blockNotDone
.blockDone:
    pop rcx
    dec ecx
    jnz .nextBlock
.exeComplete:
    mov eax, dword [exeHdrSpace + imageFileOptionalHeader.dAddressOfEntryPoint]
    ;Now get EP relative to the (rescaled) load address.
    add rax, qword [rbp - execFrame.pProgBase]
    mov qword [rbp - execFrame.pProgEP], rax
    call qword [registerDLL]    ;Now we register the DLL and any import/exports
    jc .badFmtErr   ;If this errors out for some reason, quit loading EXE
    jmp .buildChildPSP
.loadCom:
    ;File is open here, so just read the file into memory. 
    ;The file cannot exceed 64Kb in size. COM ONLY for small files!!!!
    ;Allocate 64Kb of memory, or as much as we can
    cmp qword [rbp - execFrame.bSubFunc], execOverlay
    je .comOverlay
    mov ebx, 0FFFFh ;64Kb pls
    mov dword [rbp - execFrame.dProgSize], ebx
    push rbp
    call allocateMemory
    pop rbp
    jnc .comallocOk
    cmp al, errNoMem
    jne .cleanAndFail   ;Propagate the proper error if not a lack of memory
    ;rbx should have the amount available
    ;We check if this value is psp_size more than filesize
    push rbx    ;Save new minimum size
    mov eax, 2    ;Reposition to end of file
    movzx ebx, word [rbp - execFrame.wProgHdl]
    xor edx, edx    ;Go to end of file
    call lseekHdl
    ;eax has file size
    pop rbx ;Get back max alloc size
    mov edx, ebx    ;Save in max alloc in edx temporarily
    sub edx, eax
    cmp edx, psp_size   ;If filesize - memory space is < psp_size...
    jb .insufficientMemory   ;Fail
    mov dword [rbp - execFrame.dProgSize], ebx  ;Store progsize
    push rbp
    call allocateMemory
    pop rbp
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
    add rax, psp_size
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
    cmp qword [rbp - execFrame.bSubFunc], execOverlay
    je .overlayExit
    ;Now build the PSP
    mov esi, dword [rbp - execFrame.dProgSize]
    mov rdx, qword [rbp - execFrame.pPSPBase]
    push rdx
    push rbp
    call createPSP
    pop rbp
    pop rdx

    ;Now copy the environment block over if rax != 0
    mov rbx, qword [rbp - execFrame.pEnvBase]
    test rbx, rbx
    jz short .skipEnvCopy
    mov qword [rdx + psp.envPtr], rbx
.skipEnvCopy:
    ;Now set Current PSP to our PSP and set current DTA to command line
    mov qword [currentPSP], rdx
    call dosCrit1Enter
    call .setPSPArenaOwner  ;Set the new PSP as the owner of the arenas 

    lea rdi, qword [rdx + psp.dta] ;Point to default dta...
    mov qword [currentDTA], rdi ;and set it!

    ;Now We need to copy over the command line and fcbs to the PSP
    ; and set FS to point to the PSP
    mov rbx, qword [rbp - execFrame.pParam] ;Get the paramter block ptr in rbx

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
    mov rsi, qword [rbx + execProg.pCmdLine]
    lea rdi, qword [rdx + psp.dta]
    mov ecx, 80h
    rep movsb   ;Copy the command line over (terminated by 0Dh)

    mov ebx, eax  ;Save the fcb drive numbers in bx
    mov rdi, rdx  ;Point RDI to PSP

    call getUserRegs    ;Need to get Int 22h address from stack
    mov rax, qword [rsi + callerFrame.rip]  ;Get parent return address
    mov qword [rdi + psp.oldInt22h], rax    ;and save it in PSP
    mov rdx, rax    ;Move return address to rdx to set up the Interrupt Vector
    mov al, 22h
    call setIntVector   ;bx preserved by this call

    ;Check FCB drive numbers are valid. Return FFh if not
    mov al, bl
    xor bl, bl
    call getCDSNotJoin
    jnc .drive1Ok
    mov bl, -1
.drive1Ok:
    mov al, bh
    xor bh, bh
    call getCDSNotJoin
    jnc .drive2Ok
    mov bh, -1
.drive2Ok:
    ;bx has validity flags for the two fcb drives, undocumented!!
    ;rdi has pointer to psp
    mov esi, dword [rbp - execFrame.dProgSize]  ;Get program size
    ;Add psp base (rdi) to prog size to get the last byte of the allocation
    lea rsi, qword [rsi + rdi - 8]    ;Get new rsp in rsi (last qword of alloc)
    mov rax, ~7     ;Clear the bottom 3 bits
    and rsi, rax    ;To align downwards
    ;We align stack to qword. x64 ABI requires paragraph alignment.
    ;That is the job of the runtime to handle.
;Registers carrying data at this point:
;bx = FCB drive statuses
;rsi = Stack Base
;rbp = execFrame
    cmp byte [rbp - execFrame.bSubFunc], execBkgrnd
    jne short .noBg
    ;Get termination mode in ecx before xfring control to dosmgr
    push rsi
    call getUserRegs
    mov rcx, qword [rsi + callerFrame.rcx]  ;Get termination mode
    pop rsi
.noBg:
    call qword [launchTask]
    jc short .cleanAndFail
;Final step: Transfer control
    cmp byte [rbp - execFrame.bSubFunc], execLoadGo
    je .xfrProgram
    cmp byte [rbp - execFrame.bSubFunc], execBkgrnd
    je .overlayExit ;Skip the below for background tasks
    mov rax, qword [rbp - execFrame.pProgEP]
    mov rdx, qword [rbp - execFrame.pParam]
    mov qword [rdx + loadProg.initRIP], rax
    movzx eax, bx   ;Return fcb drive status
    mov qword [rsi], rax    ;Store the FCB status on the top of stack for AH=01h
    sub rsi, 8  ;Now go down one so that we can pop the AX value from the stack
    mov qword [rdx + loadProg.initRSP], rsi
.overlayExit:
    mov rsp, rbp    ;Reset the stack to its position
    pop rbp ;Point rsp to the return address
    jmp extGoodExit ;And return!
.xfrProgram:
    cli
    mov rsp, rsi    ;Set rsp to initRSP value
    mov byte [inDOS], 0 ;Clear all inDosnessness
    sti

    push rdi    ;Push &psp[0] onto the stack to allow for ret exit
    push qword [rbp - execFrame.pProgEP]
    mov r8, rdi ;Move psp base into r8 and r9
    mov r9, rdi
    movzx eax, bx   ;ax must contain validity of the two FCB drives
    return  ;Return to child task
;r8 is guaranteed to have a copy of the PSP.
;ax is guaranteed to have the validity signatures in AH and AL.
;Everything else is optional and potentially changable at a later stage

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
    pop rbp
    pop rax
    jmp .badExit

.readDataFromHdl:
;Input: ecx = Number of bytes to transfer
;       rdx = Ptr to the buffer to use
    push rdx
    call .clearArenaOwner   ;Entering critical section!
    movzx ebx, word [rbp - execFrame.wProgHdl]
    push rbp
    call readFileHdl
    pop rbp
    call .setPSPArenaOwner  ;Exiting critical section!
    pop rdx
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
    mov rax, qword [rbp - execFrame.pPSPBase]
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
