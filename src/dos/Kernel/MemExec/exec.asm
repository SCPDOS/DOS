

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
;If the loaded file is not a PE it is assumed to be a COM file. 
; If not EXE, we read the filename extension.

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
;               |  prog space to start loading  |
;               |       the PE overlay at.      |
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
;               |     Only for PE Overlays.     |
;               |        Ignored For COM.       |
;               |-------------------------------|
;NOTES:
;
;For AL = 0 or 1: All pointers that we generate wrt the program are checked to 
;  ensure that no reads or writes go past the allocated block or above the 
;  end of the PE header. This is done to ensure that a corrupt PE header 
;  won't cause a system crash or corrupt the header copy in memory. 
;  This is practically not possible if loading a COM file.
;
;For AL = 3: Both provided pointers must be aligned to section alignment 
;  for PE files. If this is not the case, DOS will fail to load the executable.
;  The overlay will be loaded so that the first byte at the start of the memory
;  block is the first byte of the MZ header in the PE header. You will have to
;  extract from the header where the entry point is which isn't too bad since
;  the byte you pass to me will point to the ``M'' in the MZ header.
;   
;  Note: The relocation factor is useful for loading the executable as if it 
;  is loaded at a different address than it actually is being loaded at. 
;  This will be useful when introducing per-session page tables.
;
;TODO: 
;  -ADD MEMORY BOUNDS CHECKING FOR PE LOADING AKIN TO THAT FOR AL=0 AND 1 WHEN
;       LOADING PE OVERLAYS.
;       
;All three will setup both COM and PE files for whatever purpose. 
;          AL = 3 DOES NOT BUILD THE PROGRAM A PSP.
;
;Start by setting up a stack frame of local vars to keep track of vars in call
    %push
    %stacksize flat64
;The below is a local (wrt to preprocessor context) variable for 
; NASM preprocessor. DO NOT REMOVE! It keeps track of the # of local variables
; I declare on the stack, multiplied by 8, i.e. the number of bytes allocated.
    %assign %$localsize 0
    %local bSubFunc:byte, wCOFFChars:word, wNameLen:word, dProgHdl:dword, dAllocPSz:dword, pParam:qword, pProgname:qword, pEnvBase:qword, pPSPBase:qword, pLoadAddr:qword, qRelocFct:qword, pProgEP:qword, pSectionBase:qword, pAllocEnd:qword
;---------------------------------------------------
;Guide to local vars (that are not obvious)
;dAllocPSz = DWORD, number of paragraphs allocated in memory block.
;pParam -> PTR, pointer to the struct passed by caller
;---------------------------------------------------
    enter %$localsize, 0
;Clean up local pointer vars which are used in the default cleanup routines
    xor ecx, ecx
    mov qword [pPSPBase], rcx
    mov qword [pEnvBase], rcx

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
    leave
    jmp extErrExit

.validSubfunction:
    cmp al, execInvld
    je .badSubFunction
    ;Save registers for each function call
    mov qword [pParam], rbx
    mov qword [pProgname], rdx
    movzx eax, al
    mov qword [bSubFunc], rax   ;clear alignment and progHdl
    mov rdi, rdx
    call strlen ;Get string length in cx
    mov word [wNameLen], cx   ;Get the string length  
;Now open the file we wanna launch
    xor eax, eax    ;al = 0 => Normal program attributes to search for
    push rbp    ;Preserve local frame ptr
    mov ecx, 23h    ;Tell server to open for exec. No writing or sharing!
    call openFileHdl
    pop rbp
    jc .badExit ;Exit preserving error code in al
    ;Now eax has the file handle
    mov dword [dProgHdl], eax
    movzx ebx, ax   ;Move file handle into bx
    call derefSFTPtr    ;And deref it into rdi
    movzx edx, word [rdi + sft.wDeviceInfo] ;Get device word
    test edx, devCharDev
    jz .validDiskFile    ;We cannot have a char device
    mov al, errFnf
    jmp .cleanAndFail
.validDiskFile:
    cmp qword [bSubFunc], execOverlay
    je .loadProgram ;If overlay, skip making an environment block
;If we get an instruction to copy parent env, we do that. If the 
; parent ptr is a special NULL value, then we keep the NULL value
; in place in the execFrame envPtr var. Else, we use the parent
; env pointer as the source of our copy.
    mov rdi, qword [pParam] ;Get params ptr in rdi
    mov rax, qword [rdi + execProg.pEnv]
    test rax, rax   ;Is this 0? (i.e. copy parent env)
    jnz short .copyEnvironmentBlock
    mov rsi, qword [currentPSP] ;Get current PSP address in rsi
    mov rax, qword [rsi + psp.envPtr]   ;Get ptr to env src from parent env
    test rax, rax   ;Was parent ptr 0? If so, skip allocing new env.
    jz short .loadProgram ;This is used by the boot process!!
.copyEnvironmentBlock:
    mov rdi, rax    ;Point rdi to the source of the environment
;Get the length of the environment
    mov ecx, 7fffh  ;32kb limit for env size
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
    movzx ebx, word [wNameLen]  ;Get name length
    add edi, ebx    ;edi has number of bytes to allocate for environment blk
    mov ebx, edi
    shr ebx, 4  ;Turn bytes needed into paragrapsh
    call .execAlloc
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
    mov qword [pEnvBase], rax   ;Save the env block in frame
    mov rsi, qword [pParam]
    mov rsi, qword [rsi + execProg.pEnv]    ;Get in rsi the src of the env
    rep movsb   ;Copy from rsi to rdi
    mov eax, 1  ;One additional string and a second null char!
    stosw       ;Away you go!
    mov rsi, qword [pProgname]  ;Get ASCIIZ string for filespec
    movzx ecx, word [wNameLen]
    rep movsb   ;Move the bytes to rdi
;Done with the environment... more or less
.loadProgram:
    mov ecx, imageDosHdr_size   ;Read the DOS header for the exe file
    lea rdx, exeHdrSpace        ;Read into the SDA area
    call .readDataFromHdl
    jc .cleanAndFail
    test eax, eax               ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, ecx
    jb .loadCom

    cmp word [rdx + imageDosHdr.e_magic], dosMagicSignature
    je .proceedPE
    cmp word [rdx + imageDosHdr.e_magic], dosMagicSignature2
    jne .loadCom    ;If not equal to ZM or MZ, must be a COM file
.proceedPE:
;Now we need to read e_lfanew
    push rdx
    mov ebx, dword [dProgHdl]  ;Get handle
    mov edx, dword [rdx + imageDosHdr.e_lfanew]
    call .lSeekStart            ;Move to that position in the file
    pop rdx
    mov ecx, coffHdr_size
    call .readDataFromHdl       ;Now read the COFF header into inner buffer
    jc .cleanAndFail
    test eax, eax   ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, ecx
    jne .badFmtErr
;So now rdx points to the imageFileHeader
    cmp dword [rdx + imageFileHeader.dPESignature], imagePESignature
    jne .badFmtErr
    cmp word [rdx + imageFileHeader.wMachineType], imageFileMachineAMD64
    jne .badFmtErr
;Check the binary is executable
    movzx eax, word [rdx + imageFileHeader.wCharacteristics]
    test ax, imageFileExecutable
    jz .badFmtErr
    mov word [wCOFFChars], ax   ;Save this for later!

    cmp word [rdx + imageFileHeader.wSizeOfOptionalHdr], optCoffHdr_size
    jb .badFmtErr   ;We need the full optional header (as normal)
;Now check we have some sections in the file
    test word [rdx + imageFileHeader.wNumberOfSections], -1
    jz .badFmtErr   ;If this is 0, what?
;Now load Optional header, file ptr points to it so all good!
    mov ecx, optCoffHdr_size
;rdx points to exeHdrSpace
    call .readDataFromHdl
    jc .cleanAndFail
    test eax, eax   ;Were zero bytes read?
    jz .badFmtErr
    cmp eax, ecx
    jb .badFmtErr
    cmp qword [bSubFunc], execOverlay
    je .exeOverlay  ;DONT allocate memory if loading an overlay
;Now we work out the alloc size. We accumulate in rbx. All values unsigned.
;Max total allocation size is ~64Gb (64Gb - 10h bytes). 
;Anything larger and we fail! 
;
;We add the following values in the following order:
;   1) Stack size from PE OptHdr. If value in PE OptHdr is 0, allocate 256K.
;   2) dSizeOfImage from PE OptHdr. This is the in-memory alloc requirement.
;   3) Add one PSP size.
;   4) If the PE in memory alignement requirement is not a paragraph: 
;       A whole section alignment page, to ensure our PE load arena within 
;       the allocated block will be correctly section aligned, as our DOS 
;       mem alloc function will not put the code at the right alignment. 
;
;Start by getting stack size.
    mov ecx, 40000h ;256Kb stack default
    mov rbx, qword [exeHdrSpace + imageFileOptionalHeader.qSizeOfStackCommit]
    test rbx, rbx   ;If PE value is 0, use the default in rcx.
    cmovz rbx, rcx
;Get base memory alloc size.
    mov eax, dword [exeHdrSpace + imageFileOptionalHeader.dSizeOfImage]
    add rbx, rax
;Add PSP header size (add 0Fh for rounding to nearest paragraph)
    mov eax, psp_size + 0Fh ;Needed for if we ever grow the PSP.
    add rbx, rax
;Add final section alignment if needed. Else, skip it!
    mov eax, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    test eax, eax   ;A value of 0 is invalid. Error out!
    jz .badFmtErr
    cmp eax, 10h
    jbe .skipSecAlignAlloc
    add rbx, rax
.skipSecAlignAlloc:
;Here, rbx has the total number of bytes to allocate. Convert to paragraphs.
    shr rbx, 4      ;Turn into paragraphs
    cmp ebx, -1     ;Check paragraph count. If above (dword)-1, fail.
    ja .badFmtErr
    mov dword [dAllocPSz], ebx  ;Save the program size in paragraphs
    call .execAlloc
    jc .insufficientMemory
    mov qword [pPSPBase], rax   ;Save ptr here, psp will go here
;Now we work out a ptr to the first byte past our allocation for 
; bounds checking. rbx has the number of bytes we allocated.
    mov ebx, dword [dAllocPSz]
    shl rbx, 4                  ;Turn the count into a byte count
    add rbx, rax                ;Get ptr to first byte past alloc
    mov qword [pAllocEnd], rbx  ;Stuff it for later 
;Now make space for the PSP at the head of the alloc.
    add rax, psp_size   ;Make space for the psp
;Now we check if we need to round up the pointer in rax 
; which is paragraph aligned. Thus, it is ok for any alignment up to 16 bytes.
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    mov ecx, edx    ;Save a copy of the alignment
    dec edx
    test rax, rdx   ;If the allocated address is on section boundary, leave it
    jz .allocDone
    not edx         ;Swap the bits that are not set
    and rax, rdx    ;Clear the lower bits to round down
    add rax, rcx    ;Add one alignment requirement.
.allocDone:
    mov qword [pLoadAddr], rax  ;First byte of PE hdr goes here
    mov qword [qRelocFct], rax  ;This is the relocation base
;Finally, just check that we have some code to execute since here we are
; executing. Only allowed for overlays.
    cmp dword [exeHdrSpace + imageFileOptionalHeader.dSizeOfCode], 0
    je .badFmtErr   ;If no bytes, exit error
    jmp short .readPEHdr
.exeOverlay:
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    test edx, edx   ;A value of 0 is invalid here.
    jz .badFmtErr
;Now check the overlay pointers are correctly section aligned!
    dec edx         ;Convert into a mask
    mov rbx, qword [pParam]
    test qword [rbx + loadOvly.pLoadLoc], rdx
    jnz .badFmtErr
    test qword [rbx + loadOvly.qRelocFct], rdx
    jnz .badFmtErr
    call .overlayInit   ;Now do the common overlay code.
;The below makes the code work. Replace eventually with a 
; search for the real end of memory block in which we are being loaded.
    mov qword [pAllocEnd], -1
.readPEHdr:
;Before we start the read in, we adjust the relocation factor based
; on the image base address vs the desired relocation factor (which should
; be the same as the in memory load address).
    mov rax, qword [qRelocFct]
    sub rax, qword [exeHdrSpace + imageFileOptionalHeader.qImageBase]
    mov qword [qRelocFct], rax  ;If they are equal, reloc fct is now 0.
    ;We use this to indicate we don't need any fix ups :)
;We now read the full PE header into the allocated memory block.
    mov ebx, dword [dProgHdl]
    xor edx, edx
    call .lSeekStart 
    mov rdx, qword [pLoadAddr]
    mov ecx, dword [exeHdrSpace + imageFileOptionalHeader.dSizeOfHeaders]
    call .readDataFromHdl       ;Now read the count in
    jc .cleanAndFail
    cmp eax, ecx                ;If the count read in isnt full, we fail.
    jnz .badFmtErr
    lea rdi, qword [rdx + rcx]  ;Point to first byte past the header
    call .doSectionPad  ;Point rdi to the start of the section load area
    mov qword [pSectionBase], rdi   ;And save it!
;rdx points to the MZ header in the PE header. Move r8 to the COFF header
    mov r8, rdx
    mov edx, dword [r8 + imageDosHdr.e_lfanew]  ;Get len to COFF header.
    add r8, rdx
;Now point r9 to the section tables
    mov eax, dword [r8 + coffHdr_size + imageFileOptionalHeader.dNumberOfRvaAndSizes]
    lea r9, qword [r8 + 8*rax + coffHdrs_size]  ;Point r9 to the section tbl
;Now:
;   r8  -> Start of the PE Header
;   r9  -> Section Table 
;   esi = Number of sections left to process
;   ebx = Handle to the file
;   rbp -> Frame pointer for local variables
    movzx esi, word [r8 + imageFileHeader.wNumberOfSections]   ;Get # of segs.
.loadLp:
;Start by ensuring the section's load address isn't out of the allocation or 
; in the header space. Then ensure that the payload we will read into 
; memory + alignment, won't go out of the allocation. 
    mov edi, dword [r9 + imageSectionHdr.dVirtualAddress]   ;Get memory offset
    add rdi, qword [pLoadAddr]  ;Turn into offset from progbase
    mov ecx, dword [r9 + imageSectionHdr.dVirtualSize]  ;This is what we read!
    cmp rdi, qword [pSectionBase]
    jb .badFmtErr   ;If this is below, error out
    ;test byte [bSubFunc], execOverlay
    ;je .okToLoad
    mov rdx, qword [pAllocEnd]  ;Get the end pointer
    cmp rdx, rdi    ;If rdx > rdi, we are ok
    jbe .badFmtErr
;Now we ensure that the read + section align wont go past our allocation
    push rdx        ;Save the ptr to the end of allocation
    mov eax, ecx    ;Get read count in eax
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    dec edx
    test eax, edx   ;If this is 0, means eax is already section aligned
    jz .skipAlignCheck
    not edx         ;Flip the bits to align downwards
    and eax, edx    ;Round down by one section alignment
    add eax, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
.skipAlignCheck:
    add rax, rdi    ;Goto first byte past read
    pop rdx
    cmp rdx, rdi    ;If rdx > rdi, we are ok
    jbe .badFmtErr
.okToLoad:
;Now, we check if the section contains uninitialised data
    mov eax, dword [r9 + imageSectionHdr.dCharacteristics]
    test dword [r9 + imageSectionHdr.dCharacteristics], imgScnCntBSS
    jz .readSection
.nullPad:
;Just zero the number of bytes in ecx space.
    push rcx
    xor eax, eax
    rep stosb
    pop rax
    jmp short .gotoNextSection
.readSection:
;Do additional bss sanity checks.
    mov edx, dword [r9 + imageSectionHdr.dPointerToRawData]
    test edx, edx   ;If offset is 0, this is illegal so must be BSS.
    jz .nullPad
;If raw data on disk is zero, the virtual size might be non-zero. Thus,
; we simply null pad, as per the specification. 
    test dword [r9 + imageSectionHdr.dSizeOfRawData], -1
    jz .nullPad
    mov ebx, dword [dProgHdl]   ;Ensure ebx has the handle
    push rsi
    call .lSeekStart        ;Move to this part in the file. Preserves ecx
    mov rdx, rdi            ;Get ptr to buffer in rdx
    push rdi                ;Save section in memory locally
    call .readDataFromHdl   ;ecx already has count in it
    pop rdi
    pop rsi
    jc .cleanAndFail
;If fewer bytes read than requested, this is concerning so error out!
    cmp eax, ecx
    jne .badFmtErr
;Data read ok, now fill in any zeros needed.
    add rdi, rax            ;Move rdi forwards by that amount at least
.gotoNextSection:
    call .doSectionPad      ;Now we additionally do a section pad, if needed
    add r9, imageSectionHdr_size    ;Go to next section header
    dec esi                         ;Decrement our section counter
    jnz .loadLp
;Here we fixup addresses if needed.
;All fixups happen relative to the relocation factor, not the load address.
    test qword [qRelocFct], -1  ;If this is 0, we are done!
    jz .exeComplete
;If program has had relocs stripped, fail
    movzx eax, word [wCOFFChars]
    test ax, imageFileRelocsStripped
    jnz .badFmtErr
;If program has no .reloc section, fail
    mov edx, dword [exeHdrSpace + imageFileOptionalHeader.dNumberOfRvaAndSizes]
    cmp edx, dataDir_reloc  ;Does .reloc exist (6th directory entry)
    jb .badFmtErr           ;Need relocs but no .reloc directory exists
;Now we point r9 to the get the reloc directory
    lea r9, qword [r8 + coffHdrs_size + (dataDir_reloc-1)*imageDataDirectory_size]
;Now get offset in memory image to data directory
    mov esi, dword [r9 + imageDataDirectory.dVAddr]
    test esi, esi   ;If there are no relocations, skip this...
    jz .exeComplete   ;... including if overlay
    add rsi, qword [pLoadAddr]  ;And shift it to offset in loaded memory
    mov rbx, qword [qRelocFct]  ;Save the relocation factor in rbx
;rsi points to relocation data table in memory
    mov ecx, dword [r9 + imageDataDirectory.dSize]  ;Get reloc section size
    test ecx, ecx    ;If no relocations, skip
    jz .exeComplete
;rsi points to the first base relocation block. The relocations begin
; after the first block
;ecx has the number of base relocation blocks to process.
.nextBlock:
    sub ecx, dword [rsi + baseRelocBlock.dSize] ;Drop the block from the cnt
    jb .badFmtErr   ;If this goes below zero, we must fail!
    push rcx        ;Save number of bytes left to process.
    mov eax, dword [rsi + baseRelocBlock.dPageRVA]   ;Get the page rva
    mov rdi, qword [pLoadAddr]  ;Point to start of program
    add rdi, rax    ;Add this page offset to rdi to goto correct page for reloc
    mov ecx, dword [rsi + baseRelocBlock.dSize]  ;Get number of bytes in block
    jecxz .blockDone    
    sub ecx, baseRelocBlock_size
    add rsi, baseRelocBlock_size  ;Go to the start of the directory data
    shr ecx, 1  ;Get number of words = # of relocs to do
.blockNotDone:
    lodsw               ;Get the next page offset word
    mov edx, eax
    and edx, 0000F000h  ;Get the word's top four bits
    test edx, edx       ;IMAGE_REL_BASED_ABSOLUTE. Means skip this entry.
    jz .skipEntry
    cmp edx, 0A000h     ;IMAGE_REL_BASED_DIR64. If not this, fail to load
    jne .badFmtErr
    and eax, 00000FFFh  ;Save bottom 12 bits
;rdi points to base of 4kb page, rax give offset into 4Kb page
;Check the resultant pointer is within our allocation.
    lea rdx, qword [rdi + rax]  ;Get the pointer for adjustment
    cmp qword [pAllocEnd], rdx
    jbe .badFmtErr
    add qword [rdx], rbx    ;Relocation factor was saved in rbx
.skipEntry:
    dec ecx
    jnz .blockNotDone
.blockDone:
    pop rcx ;Get back the length of the reloc left to process
    test ecx, ecx 
    jnz .nextBlock
.exeComplete:
;Finally, fix up the entry point.
    mov eax, dword [exeHdrSpace + imageFileOptionalHeader.dAddressOfEntryPoint]
    add rax, qword [pLoadAddr]  ;EP is rel load address!
    mov qword [pProgEP], rax
    call qword [registerDLL]    ;Now we register the DLL and any import/exports
    jc .badFmtErr   ;If this errors out for some reason, quit loading PE
    jmp .buildChildPSP
.loadCom:
;File is open here, so just read the file into memory. 
;The file cannot exceed 64Kb in size.
;Allocate 64Kb of memory. If not enough, we fail as COM files
; expect that space and stack assumptions are prima-facie based on this.
    cmp qword [bSubFunc], execOverlay
    je .comOverlay
    mov ebx, 0FFFh ;64Kb - 16 bytes, give me FFF0h bytes.
    mov dword [dAllocPSz], ebx
    call .execAlloc
    jc .cleanAndFail
    jmp short .comallocOk
.comOverlay:
;Here we simply read the file into the buffer provided  
    call .overlayInit
    jmp short .comPrepRead
.comallocOk:
;rax should point to the first byte
    mov qword [pPSPBase], rax
    add rax, psp_size
.comPrepRead:
    mov qword [pLoadAddr], rax
    mov eax, 2    ;Reposition to end of file
    mov ebx, dword [dProgHdl]
    xor edx, edx    ;Go to end of file
    call lseekHdl
;eax has filesize now
    push rax    ;Save filesize
    xor edx, edx    ;Go to start of file
    call .lSeekStart
    pop rcx ;Get the filesize in rcx (# of bytes to read)
    cmp qword [bSubFunc], execOverlay
    je .comRead
;Now we check if 0FFF0h is enough space to hold the COM file. We know
; the allocation is 0FFF0h bytes. Subtract the mandatory PSP size.
;Skipped if an overlay being loaded. 
;ecx = # File size, in bytes.
    mov edx, 0FFF0h - psp_size  ;Get alloc size minus PSP size. 
    cmp edx, ecx    ;Do we have space for the PSP and program?
    jb .insufficientMemory
.comRead:
    mov rdx, qword [pLoadAddr]  ;Buffer to read into
    call .readDataFromHdl       ;Read from the file handle
    jc .cleanAndFail
    mov rax, qword [pLoadAddr]
    mov qword [pProgEP], rax
.buildChildPSP:
;We can close handle now
    mov ebx, dword [dProgHdl]
    push rbp
    call closeFileHdl   ;Rest now source file. You have made me proud.
    pop rbp
;Only build a PSP if not in overlay mode. If in overlay mode skip
    cmp qword [bSubFunc], execOverlay
    je .overlayExit
;Now build the PSP
    mov esi, dword [dAllocPSz]
    mov rdx, qword [pPSPBase]
    push rdx
    push rbp
    call createPSP
    pop rbp
    pop rdx
;Now copy the environment block ptr over. 
;Stores the null ptr that is our pointer (special init case)
    mov rbx, qword [pEnvBase]
;    test rbx, rbx
;    jz short .skipEnvCopy
    mov qword [rdx + psp.envPtr], rbx
;.skipEnvCopy:
;Now set Current PSP to our PSP and set current DTA to command line
    mov qword [currentPSP], rdx
    call dosCrit1Enter
    call .setPSPArenaOwner  ;Set the new PSP as the owner of the arenas 

    lea rdi, qword [rdx + psp.dta] ;Point to default dta...
    mov qword [currentDTA], rdi ;and set it!

;Now We need to copy over the command line and fcbs to the PSP
; and set FS to point to the PSP
    mov rbx, qword [pParam] ;Get the paramter block ptr in rbx

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
    mov esi, dword [dAllocPSz]  ;Get program size in paragraphs
    shl rsi, 4                  ;Convert into bytes.
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
    cmp byte [bSubFunc], execBkgrnd
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
    cmp byte [bSubFunc], execLoadGo
    je .xfrProgram
    cmp byte [bSubFunc], execBkgrnd
    je .overlayExit ;Skip the below for background tasks
    mov rax, qword [pProgEP]
    mov rdx, qword [pParam]
    mov qword [rdx + loadProg.initRIP], rax
    movzx eax, bx   ;Return fcb drive status
    sub rsi, 8  ;Now go down one so that we can pop the AX value from the stack
    mov qword [rsi], rax    ;Store the FCB status on the top of stack for AH=01h
    mov qword [rdx + loadProg.initRSP], rsi
.overlayExit:
    leave
    jmp extGoodExit ;And return!
.xfrProgram:
;No need to leave here as we swap stacks and dont return to this stack again
    cli
    mov rsp, rsi    ;Set rsp to initRSP value
    mov byte [inDOS], 0 ;Clear all inDosnessness
    sti

    push rdi    ;Push &psp[0] onto the stack to allow for ret exit
    push qword [pProgEP]
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
    mov ebx, dword [dProgHdl]
    push rax    ;Save error code
    push rbp
    call closeFileHdl
    pop rbp
    pop rax
    jmp .badExit

.overlayInit:
;Does the overlay common initialisation
;Output: Sets up pLoadAddr and qRelocFct!
;        rax -> pLoadAddr
    mov rbx, qword [pParam]
    mov rax, qword [rbx + loadOvly.qRelocFct]
    mov qword [qRelocFct], rax
    mov rax, qword [rbx + loadOvly.pLoadLoc]    ;Get the load addr
    mov qword [pLoadAddr], rax
    return

.execAlloc:
;Wraps the allocation to save the frame pointer in rbp.
;Input: ebx = Number of paragraphs to allocate
;Output: CF=NC: rax -> Ptr to allocated memory block
;        CF=CY: eax = Error code.
    push rbp
    call allocateMemory
    pop rbp
    return

.lSeekStart:
;Does seeking for us from start of file. Preserves all regs.
;Input: ebx = Handle to use
;       ecx = Offset into file.
;Return: eax = Offset set.
    push rax
    push rcx
    push rdi
    xor eax, eax
    xor ecx, ecx
    call lseekHdl
    pop rdi
    pop rcx
    pop rax
    return

.doSectionPad:
;Pads from the input address to the next section alignment.
;Preserves all regs except rdi.
;Input:  rdi -> Address to start pad from
;Output: rdi -> Section aligned address 
    push rax
    push rcx
    mov rax, rdi    ;Get the current address
    mov ecx, dword [exeHdrSpace + imageFileOptionalHeader.dSectionAlignment]
    dec ecx         ;Turn into a mask
    and rax, rcx    ;Compute ptr modulo mask. Catchs alignments of 1 and exit.
    jz .dspExit
    inc ecx
    sub rcx, rax
    xor eax, eax
    rep stosb
.dspExit:
    pop rcx
    pop rax
    return

.readDataFromHdl:
;Input: ecx = Number of bytes to transfer
;       rdx = Ptr to the buffer to use
    push rdx
    call .clearArenaOwner   ;Entering critical section!
    mov ebx, dword [dProgHdl]
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
    mov rax, qword [pPSPBase]
    call .writeArenaHeaderOwner
    mov rax, qword [pEnvBase]
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
;Pop the local context stack off now!
    %pop    