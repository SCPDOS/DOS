;All utility functions go here
;-----------------------------------------------------------------------------
; critEnter -> Enters a DOS 1 critical section
; critExit -> Exits a DOS 1 critical section
; addSFTtoMFT -> Adds an SFT to its MFT chain.
; getMFT -> Gets an MFT for a given filename. Creates if it has to.
; defragMFTArena -> Defragments the MFT arena
; findFreeMFT -> Finds a free MFT for a given size
; findMFT -> Find an MFT for a given filename
; errPrintAndHalt -> Prints a formatted error message and halts the machine
;-----------------------------------------------------------------------------


critEnter:
    mov eax, 8001h  ;Enter DOS Level 1 critical section
    int 2ah
    return
critExit:
    mov eax, 8101h  ;Exit DOS Level 1 critical section
    int 2ah
    return

addSFTtoMFT:
;Adds an SFT to the MFT chain and finishes filling in it's share fields. 
;Checks for sharing conflicts before adding.
;Input: rsi -> SFT to add
;       rbx -> MFT to add SFT to
;       r8 -> DOSSEG
    cmp qword [rsi + sft.pMFT], 0   ;This SFT better not already be on a chain
    jne .crash
;Add the networking ids now as we need them for permissions checking
    mov rax, qword [r8 + qPID]
    mov qword [rsi + sft.qPID], rax
    mov eax, dword [r8 + dMID]
    mov dword [rsi + sft.dMID], eax
;Now we check if there is any sharing conflicts
    call checkPermissions
    mov qword [rsi + sft.pMFT], rbx



.crash:
    call errPrintAndHalt
    db "SFT ALREADY IN USE",CR,LF,NUL

checkPermissions:
    return


getMFT:
;Search for an MFT for a file. If one is not found, it creates it.
;Input: rsi -> Filename to search for
;Output: CF=NC: rbx -> MFT for this file.
;        CF=CY: eax = Error code.
;Start by working out the string length and checksum values
    push rsi
    xor ecx, ecx    ;Use for string length (zero inclusive)
    xor edx, edx    ;Use for checksum value (dl)
.metalp:
    lodsb
    add dl, al
    adc dl, 0       ;Add 1 if this rolls over
    inc ecx
    test al, al
    jnz .metalp
    pop rsi
;Hereon: ecx = String length, dl = Checksum, rsi -> String 
    call findMFT        ;Preserves dl and ecx
    retnc
;We get here if this file has no MFT.
;Create an MFT entry. dl and ecx are always preserved in below
    call findFreeMFT
    jnc .buildMFT
    call defragMFTArena ;Trashes eax only (returns eax = free space)
    call findFreeMFT
    jnc .buildMFT
;Here if CF=CY. Return Sharing buffer full error!
    mov eax, errShrFul
    return
.buildMFT:
;Here we have the following values:
;rsi -> String to copy in
;rbx -> Space to allocate the MFT in. rbx points to a free MFT.
;ecx = String length 
;dl = String checksum 
;Start by doing the easy part of the allocation
    mov byte [rbx + mft.bSig], mftAlloc
    mov qword [rbx + mft.pLock], 0
    mov qword [rbx + mft.pSFT], 0
    mov byte [rbx + mft.bCheckSum], dl  ;Set the checksum immediately.
;Now we work out if we want to split space into two MFTs or keep it as one.
;Check if the MFT we are pointing at has enough space for a minimum mft and
; our MFT. If there is more than enough space, we split into two MFTs. 
;Else, we just allocate the whole space and waste the few extra bytes.
    lea edx, dword [rcx + mft_size] ;Get the min size of our MFT allocation
    mov eax, dword [rbx + mft.dLen] ;Get the current size of the free MFT
    sub eax, edx        ;Remove our own allocation from the free MFT size
    cmp eax, MIN_MFT_SIZE
    jbe .alloc   ;If this is beq, we just use this mft entry as is.
;eax has the size of the new free block
    lea rdi, qword [rbx + rdx]  ;Point rdi to where the new free block will go
    mov byte [rdi + mft.bSig], mftFree  ;Make the new free block
    mov dword [rdi + mft.dLen], eax     ;Set the new free block's size
    mov dword [rbx + mft.dLen], edx     ;Set the newly allocated block's size
.alloc:
;Finally, allocate the MFT. The size, if modified, is done already.
    lea rdi, qword [rbx + mft.name] 
    rep movsb   ;Move the string. rdi points to start of new MFT
    clc
    return

defragMFTArena:
;Defragments the MFT arena. Moves all allocated MFTs to the start.
;Creates a single large free MFT from all the free space.
;Input: Nothing
;Output: MFT defragged. eax = Max MFT free space
; All regs preserved.
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
;rsi = Read pointer
;rdi = Write pointer
;edx = Free space accumulator
    mov rsi, qword [pMftArena]
    mov rdi, rsi
    xor edx, edx
.lp:
;If we encounter an allocated mft, we move it over, moving the 
; the read and write pointers. If we encounter a free mft, we add its
; size and move the read pointer.
    cmp byte [rsi + mft.bSig], mftFree
    js .exit    ;At this point, all compression is done. Goto end
    je .free
;Here we copy over. Before we do, we walk down the SFT chain
; updating the MFT pointer in each SFT.
;rsi -> Allocated MFT
;rdi -> Where this MFT will be moved to
    mov rbx, qword [rsi + mft.pSFT]     ;Point rbx to the first SFT in chain.
.sftLp:
    test rbx, rbx   ;End of SFT chain?
    jz .sftEnd
    mov qword [rbx + sft.pMFT], rdi     ;Update the SFT's MFT pointer
    mov rbx, qword [rbx + sft.pNextSFT]
    jmp short .sftLp
.sftEnd:
;All SFTs updated, now we copy the MFT up.
    mov ecx, dword [rsi + mft.dLen] ;Get the length of this MFT
    rep movsb   ;Moves both pointers to the end of the MFT
;Now rdi points to the next write space and rsi points to the next MFT
    jmp short .lp
.free:
;Dont worry about overflow. Max dLen is 1Mb, way less than 32 bit max.
    mov eax, dword [rsi + mft.dLen] ;Get the length
    add edx, eax    ;Add to the free space accumulator
    add rsi, rax    ;Move the read pointer past this free space
    jmp short .lp
.exit:
;Now we must make a single free MFT at the write pointer with the 
; correct size and assert it is correct
;rdi -> Where to make this MFT
;edx = Accumulated free space
    mov byte [rdi + mft.bSig], mftFree
    mov dword [rdi + mft.dLen], edx
    add rdi, rdx
    cmp byte [rdi + mft.bSig], mftEnd
    jne .crash
    mov eax, edx    ;Return in eax the accumulated free space
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    return
.crash:
    call errPrintAndHalt    ;No return. Use call to push string ptr to stack
    db "MFT DEFRAG FAILURE",CR,LF,NUL

findFreeMFT:
;Searches for a free MFT of a particular size or greater.
;Input: ecx = length of the string to add 
;Output: CF=NC: rbx -> MFT of appropriate size
;        CF=CY: No MFT found of this size. rbx -> End MFT
;Trashes rax. All other regs preserved.
    mov rbx, qword [pMftArena]
    mov eax, ecx    ;Use eax for the size we are looking for
    add eax, mft_size   ;eax thus has string length + mft_size
.lp:
    cmp byte [rbx + mft.bSig], mftFree
    js findMFT.noMFT    ;If sign bit set, must be mftEnd. Exit error!
    jne .next           ;If not equal, must be mftAlloc. Goto next MFT
    cmp dword [rbx + mft.dLen], eax ;Is this MFT ok sizewise?
    retnc   ;Not carry means dLen is geq eax, as required
.next:
    push rax
    mov eax, dword [rbx + mft.dLen]
    add rbx, rax    ;Point to the next MFT
    pop rax
    jmp short .lp

findMFT:
;Searches for an MFT for the filename in rsi 
;Input: rsi -> Filename to search for
;       dl = Filename checksum
;Output: CF=NC: rbx -> MFT for file
;        CF=CY: No MFT found for this file. rbx -> End MFT
;Trashes rax. All other regs preserved.
    mov rbx, qword [pMftArena]
.lp:
    cmp byte [rbx + mft.bSig], mftFree  
    js .noMFT   ;If sign bit is set, bSig must be -1. End of arena!
    je .next    ;If this MFT is free, goto next entry
    cmp byte [rbx + mft.bCheckSum], dl  ;Compare checksums
    jne .next   ;If not equal, skip entry
    push rdi
    lea rdi, qword [rbx + mft.name] ;Point to mft filename
    mov eax, 121Eh  ;Compare strings in rsi and rdi
    int 2Fh         ;Doesnt modify string pointers
    pop rdi
    retz    ;If ZF=ZE (also CF=NC), we have found the MFT! Return to caller.
.next:
    mov eax, dword [rbx + mft.dLen]
    add rbx, rax    ;Point to the next MFT
    jmp short .lp
.noMFT:
    stc
    return


;Critical error handling
errPrintAndHalt:
;This function is called such that the return address points to the 
; null terminated string to print. We never return from this.
;********************************************
;Consider requesting all disk buffers flush before entering infinite loop.
;********************************************
;Input: TOS -> Ptr to string to print after header.
;Output: Never return.
    lea rdi, .sHdr
    call .doWrite
    pop rdi         ;Get the passed in string
    call .doWrite   ;Write the passed in ASCIIZ string.
.halt:
    pause           ;Hint the CPU to power down
    jmp short .halt ;Infinite loop, await CTRL+ALT+DEL
.doWrite:
;We write directly to the console driver completely bypassing any 
;redirection, without resorting to using BIOS functions.
;We use the PSP as the buffer for the ioReqPkt.
;We obviously do no error checking or Int 24 invokation if things
; go wrong with this. 
;Input: rdi -> String to print
;Output: Hopefully, string printed.
    mov eax, 1212h  ;Do dos strlen
    int 2Fh         ;Get length of string pointed to by rdi in ecx
;1) Build request packet
    mov rbx, qword [pPSP]   ;Get the ptr to our PSP
    mov byte [rbx + ioReqPkt.cmdcde], drvWRITE
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov word [rbx + ioReqPkt.status], 0
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    mov qword [rbx + ioReqPkt.bufptr], rdi
;2) Get the driver pointer
    mov rsi, qword [pDosseg]    
    mov rsi, qword [rsi + vConPtr]  ;Get the console driver ptr
;Go driver routine here
    mov eax, 8002h  ;SPECIAL: Enter DOS Level 2 critical section
    int 2ah
    call qword [rsi + drvHdr.strPtr]  ;Passing rbx through here
    call qword [rsi + drvHdr.intPtr]
    mov eax, 8102h  ;SPECIAL: Exit DOS Level 2 critical section
    int 2ah
    return
.sHdr:
    db CR,LF,LF
    db "        SCP/DOS EXCEPTION DETECTED!",CR,LF,LF
    db "    SCP/DOS SYSTEM STOP: SHARE INTERNAL ERROR : ",NUL