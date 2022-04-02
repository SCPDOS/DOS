;-----------------------------------:
;    File Handle Kernel routines    :
;-----------------------------------:

createFileHdl:     ;ah = 3Ch, handle function
openFileHdl:       ;ah = 3Dh, handle function
closeFileHdl:      ;ah = 3Eh, handle function
    ret
readFileHdl:       ;ah = 3Fh, handle function
    mov rbp, qword [oldRSP]
    call getSFTPtr
    jc .rfhErrorHandle  ;Function can only fail for bad file handle
;Here rdi points to the correct SFT 
    mov ax, word [rdi + sft.wOpenMode]  ;Get open mode
    test al, RWAccess  ;Check r/w permission
    jnz .rfh2   ;Bit set, proceed
    test al, al  ;Check read access (it is 0)
    jnz .rfhNoPermission    ;If not zero then it is write only permission
.rfh2:
    ;So now read the number of bytes from the data buffers
    mov rbx, rdi    ;Move SFT pointer into rbx
    test byte [rbx + sft.wDeviceInfo], devBinary
    jz .rfhASCII    ;If not set, read in ASCII
    call readBinaryBytesFromFile
    jc .rfCriticalError
    jmp short .rfhExitOK
.rfhASCII:
    call readASCIIBytesFromFile
    jc .rfCriticalError
.rfhExitOK:
    mov al, cl  ;Get low byte in cl
    mov dword [rbp + callerFrame.rax], ecx  ;Save number of bytes transf.
    ret
.rfCriticalError:
    ;Fail due to driver error. Invoke Int 44h if set to in SFT
.rfhErrorHandle:
    ;Fail due to bad file handle provided
.rfhNoPermission:
    ;Fail due to bad permissions
writeFileHdl:      ;ah = 40h, handle function
    call getSFTPtr
    jc .wfhErrorHandle  ;Function can only fail for bad file handle
    mov ax, word [rdi + sft.wOpenMode]  ;Get open mode
    test ax, 3
    jz .wfhNoPermission ;Bad permissions! No r/w or w permissions
.wfCriticalError:
    ;Fail due to driver error. Invoke Int 44h if set to in SFT
.wfhErrorHandle:
    ;Fail due to bad file handle provided
.wfhNoPermission:
    ;Fail due to bad permissions

deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
movFileReadPtr:    ;ah = 42h, handle function, LSEEK
changeFileModeHdl: ;ah = 43h, handle function, CHMOD
ioctrl:            ;ah = 44h, handle function
duplicateHandle:   ;ah = 45h, handle function
forceDuplicateHdl: ;ah = 46h, handle function
findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
renameFile:        ;ah = 56h
createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
createNewFile:     ;ah = 5Bh
lockUnlockFile:    ;ah = 5Ch
setHandleCount:    ;ah = 67h
commitFile:        ;ah = 68h, flushes buffers for handle to disk 
    ret
;-----------------------------------:
;        File Handle routines       :
;-----------------------------------:
getSFTPtr:
;Gets the SFT pointer for a given file handle from the calling application
;On entry:
;   bl = File handle
;On exit:
;   rsi = currentPSP
;   rdi = SFT pointer
;   rax, rbx trashed
    mov rsi, qword [currentPSP]
    movzx rbx, bl
    mov bl, byte [rsi + psp.jobFileTbl + rbx]   ;Use jft entry to get sft num
    xor eax, eax
    mov rdi, qword [sftHeadPtr]
.gsp0:
    add ax, word [rdi + sfth.wNumFiles]
    cmp al, bl  ;Check if the file header block contains the entry
    jbe .gsp1   ;IF bl is below or equal to al then it does
    cmp rdi, -1 ;End of list
    je .gspFail   ;If we have a number greater than the last entry, fail
    mov rdi, qword [rdi + sfth.qNextSFTPtr] ;Walk the chain
    jmp short .gsp0 ;Search again
.gsp1: 
    ;Now point to the right entry
    sub al, bl  ;Subtract the number from the total so far to get offset
    movzx eax, al
    add rdi, sfth_size  ;Point to first file in table
    test al, al ;Check if rdi points to the first file in this block
    jz .gsp12   ;Skip walking down the sft blocks
.gsp11:
    add rdi, sft_size
    dec al
    jnz .gsp11  ;Keep adding one until al is zero
.gsp12:
    ret
.gspFail:
    stc
    ret

readASCIIBytesFromFile:
    ret
readBinaryBytesFromFile:
;Reads a byte from a SFT entry, does not translate it. 
;Read or RW permissions are checked at the INT 41h level
;Entry: rbp = Pointer to the caller stack
;       rbx = SFT entry pointer
;       On stack:
;           rdx = Address of the data buffer to read to
;           ecx = Number of bytes to read
;Exit: If CF = NC : All ok!
;       rbx = SFT entry pointer
;       ecx = Number of chars read/written
;      If CF = CY : Error!
;       rbx = SFT entry pointer
;       al = Error code to ret if user returns fail from int 44h or no int 44h

    test word [rbx + sft.wDeviceInfo], devCharDev
    jnz .readBinaryBytesFromCharDevice
.readBinaryBytesFromHardFile:
;Disk files are accessed from here
;Use the sector buffers if the data is already buffered,
; else use the dpb to fill a sector buffer
    push rbx
    mov r8, rbx                     ;Use r8 as sft pointer
    mov r9, qword [r8 + sft.qPtr]   ;Use r9 as dpb pointer
    ;First compute the number of bytes to read for the current sector
    ;Compute how many bytes in a sector
    mov cl, byte [r9 + dpb.bBytesPerSectorShift]
    mov eax, 1
    shl eax, cl
    mov ecx, eax    ;ecx has bytes per sector
    mov rax, qword [rbp + callerFrame.rcx]  ;Get total number of bytes
    xor edx, edx
    mov qword [rbp + callerFrame.rcx], rdx  ;Zero this field
    div ecx ;Divide by number of bytes per sector
    xchg ecx, edx    ;Get "in current sector remainder of bytes" in ecx
                     ;and bytes per sector in edx
    mov eax, dword [r8 + sft.dAbsClusr]  ;Get cluster number
    call getStartSectorOfCluster
    movzx ebx, word [r8 + sft.wRelSect] ;Add the offset into the cluster
    add eax, ebx    ;eax now has the sector number
    mov rsi, r9 ;Move dpb pointer into rsi
    xor rdi, rdi
    mov edi, ecx    ;Save in current sector remainder of bytes in edi
    mov cl, dataBuffer
    call readBuffer ;Read the data
    jc .readBinaryBytesFromHardFileError
    add rbx, bufferHdr.dataarea
    mov rsi, rbx
    mov rcx, rdi    ;Get the number of bytes to read in this sector to rbx
    mov rdi, qword [rbp + callerFrame.rdx]  ;Point to destination
    add qword [rbp + callerFrame.rcx], rcx
    rep movsb   ;Move the bytes from this sector

    pop rbx
.readBinaryBytesFromHardFileError:

.readBinaryBytesFromCharDevice:
;Devices are accessed from here
    mov rbp, qword [rbx + sft.qPtr] ;Get device driver header pointer
    push rbx
    lea rbx, charReqHdr
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], drvREAD
    mov word [rbx + ioReqPkt.status], 0
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], ecx

    call qword [rbp + drvHdr.strPtr]
    call qword [rbp + drvHdr.intPtr]
    mov eax, dword [rbx + ioReqPkt.tfrlen] ;Get number of bytes read
    test word [rbx + ioReqPkt.status], 8000h    ;Test the error bit is set
    pop rbx
    jz .readBinaryBytesExitGood  ;Error bit not set, all good!
.readBinaryBytesExitGood:
    ret

