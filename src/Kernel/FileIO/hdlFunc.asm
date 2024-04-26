;-----------------------------------:
;    File Handle Kernel routines    :
;-----------------------------------:

createFileHdl:     ;ah = 3Ch, handle function
;Input: cx = File attributes (same as search attributes for find first)
;       rdx = Ptr to ASCIZ filename to create
;Output: CF=CY => ax = File handle
;        CF=NC => al = Error code
    push rcx    ;Save file attributes on stack
    lea rcx, createMain
    mov byte [searchAttr], dirInclusive ;Inclusive with directory
    jmp short openFileHdl.openCommon
openFileHdl:       ;ah = 3Dh, handle function
;Input: al = Open mode, to open file with
;       rdx = Ptr to ASCIZ filename to open if it exists
;If called from 5D00h, cl = File attributes too
;Output: CF=CY => ax = File handle
;        CF=NC => al = Error code
    mov ebx, dirInclusive
    test byte [dosInvoke], -1
    cmovz ecx, ebx  ;If not server, store this value instead
    mov byte [searchAttr], cl
    lea rcx, openMain  
    push rax    ;Save open mode on stack
.openCommon:
    mov rsi, rdx
    call checkPathspecOK
    jnc .pathOk ;Path ok save for potentially having wildcards
.badPath: ;We cant have wildcards when creating or opening a file!
    pop rax
    mov eax, errAccDen
    jmp extErrExit
.pathOk:
    call dosCrit1Enter
    call findFreeSFT    ;Get free SFT in rdi or error exit
    call dosCrit1Exit
    jc .exitBad
    ;Now save variables
    call setCurrentSFT
    mov word [currentNdx], bx   ;Save a word, SFTNdx are bytes though
    call findFreeJFTEntry    ;Get a ptr to a free JFT entry in rdi
    jc .exitBad
    mov word [currentHdl], bx   ;Save handle number in var
    mov qword [curHdlPtr], rdi  ;Save ptr to this entry
    movzx ebx, word [currentNdx]    ;Get the current ndx 
    mov byte [rdi], bl  ;And open the file
    ;If the rest of open/create fails, be prepared to close this entry
    mov rsi, rdx    ;Ptr to ASCIIZ path string    
    lea rdi, buffer1    ;Build the full path here
    push rcx    ;Save the procedure to call on stack
    call getFilePath    ;Check path existance, updates DPB
    pop rbx     ;Get the procedure address back from stack
    lea rax, openMain   ;Get EA for open procedure
    mov rsi, qword [currentSFT] ;Get current SFT pointer in rsi
    jnc .proceedCall
;If CF=NC => Full path exists. For Open, Good. For Create, Good. 
;                              For Create New, Bad but handled later.
;If CF=CY => Path doesnt all exist:
;      If parDirExists = -1 => For Open, Bad.  For both Creates, Good. 
;Now we check if we are creating or opening.
    cmp rbx, rax    ;Are we trying to open a non-existant file?
    je .badFile ;Jmp to error if opening file that doesnt exist
    test byte [parDirExist], -1 ;If creating, check if parent path was found
    jz .badPathspec ;If not then exit bad path
    ;Now check the path is not X:\<NUL>
    mov ecx, dword [buffer1]    ;Get the first four chars for comparison
    xor cl, cl
    cmp ecx, 005C3A00h  ;If this is a null path, set file not found!
    jnz .proceedCall    ;Else, proceed.
.badFile:   ;If trying to open a file that doesnt exit, error so!
    pop rax
    mov eax, errFnf 
    jmp .exitBad2   ;Need to deallocate the SFT before returning
.badPathspec:   ;If the parent path doesnt exist, error thus.
    pop rax
    mov eax, errPnf
    jmp .exitBad2   ;Need to deallocate the SFT before returning
.proceedCall:
;If the pathspec exists, recall that for create, we truncate.
    xor ecx, ecx    ;Use ecx to carry device info word
    cmp rbx, rax    ;Are we opening or creating? (rax=opening)
    pop rax         ;Pop off openmode or attribute from the stack
    jne .callProc   ;Jump if we are creating
    ;al means openmode here
    test al, 80h    ;No Inherit bit set?
    jz .callProc
    and al, 7Fh     ;Clear this bit
    mov ecx, devNoInherit
.callProc:
    mov word [rsi + sft.wOpenMode], 0   ;Clear open mode bits
    mov word [rsi + sft.wShareRec], 0   ;Clear Share record pointer details
    push rcx    ;Save the device word 
    call rbx    ;Enter with open mode in 
    pop rcx
    mov rsi, qword [currentSFT] ;Get current SFT pointer in rsi
    jc .exitBad2
    mov word [rsi + sft.wNumHandles], 1 ;One handle will refer to this boyo
    or word [rsi + sft.wDeviceInfo], cx ;Add the inheritance bit to dev info
    movzx eax, word [currentHdl]
    call qword [closeDupFileShare]  ;Close Duplicate Handles if opened file! 
    mov word [currentNdx], -1       ;Now reset the index back to -1
    jmp extGoodExit ;Save ax and return OK
.exitBad:
    sti ;To prevent new net open/create reqs from crapping out a failed request
    pop rbx ;Pop the word from the stack
    jmp short .exitBadCommon
.exitBad2:
    ;Now we deallocate the SFT entry in the JFT and SFT block
    mov rsi, qword [curHdlPtr]
    mov byte [rsi], -1  ;Re-free the entry in the JFT
    mov rsi, qword [currentSFT]
    mov word [rsi], 0   ;Re-free the SFT 
.exitBadCommon:
    mov word [currentNdx], -1
    jmp extErrExit ;Propagate the error code that is in ax

closeFileHdl:      ;ah = 3Eh, handle function
;Input: bx = file handle to close
    call getSFTPtr  ;Get a pointer to the SFT in rdi
    jc extErrExit   ;If CF is set, al has error code, exit!
    call setCurrentSFT  ;Set this as the current SFT
    cmp word [rdi + sft.wNumHandles], 1 ;If this is last reference to sft
    je .killHdl ;always kill the handle!
    ;Else if network file opened as FCB, avoid nuking JFT!
    movzx eax, word [rdi + sft.wOpenMode]  ;Get the share mode bits
    and al, 0F0h    ;And wipe out the other bits
    cmp al, netFCBShare
    je .notNetFCB
.killHdl:
    call getJFTPtr  ;Remember, bx has handle number
    mov byte [rdi], -1  ;Now free the JFT entry
.notNetFCB:
    call closeMain  ;Call close main!
    jc extErrExit   ;If an error, exit through error exit
    mov eax, 3E00h    ;Return value
    jmp extGoodExit

readFileHdl:       ;ah = 3Fh, handle function
    lea rsi, readBytes
.common:
    call getSFTPtr  ;Get SFT ptr in rdi (if file is r/w-able from machine)
    jc extErrExit   ;Error code in al and exit
    call setCurrentSFT  ;Set the current SFT (from rdi)
    or ecx, ecx ;Clear upper bits of RCX if they are NOT clear just in case
    push qword [currentDTA] ;Save the current Disk Transfer Area
    mov qword [currentDTA], rdx ;Set the user buffer as the currentDTA
    call rsi    ;Get back in ecx the bytes transferred!
    pop qword [currentDTA]
    jc extErrExit   ;Error code in al and exit
    mov eax, ecx    ;Get actual number of bytes tfrd in eax 
    jmp extGoodExit2    ;and exit!

writeFileHdl:      ;ah = 40h, handle function
    lea rsi, writeBytes
    jmp readFileHdl.common

deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
;Here don't allow malformed chars unless it is a network CDS
;Allows deleting volume labels.
    mov ebx, dirInclusive    ;Search all files, dirs handled later
    test byte [dosInvoke], -1
    cmovz ecx, ebx  ;If not server invoke, store this value instead
    mov byte [searchAttr], cl
    mov rsi, rdx
    call checkPathspecOK
    jnc .pathOk ;Path ok save for potentially having wildcards
    jnz .badPath    ;If ZF=NZ, then the path was bad
    ;Here the path has wildcards in the last portion. Check for netInvoke
    test byte [dosInvoke], -1
    jnz .pathOk ;If this is -1, server invoke, wildcards are OK
.badPath:
    mov eax, errAccDen
    jmp extErrExit
.pathOk:
    lea rdi, buffer1
    push rdx
    call getFilePath    ;Get the path for the file to delete
    pop rdx
    jc extErrExit   ;If the file or path was not found or error, bye bye
    ;In the case of a wildcard, recall this will return the first file
    cmp byte [fileExist], -1
    jnz extErrExit
    call outerDeleteMain
    jc extErrExit
    jmp extGoodExit

lseekHdl:          ;ah = 42h, handle function, LSEEK
;Input: ecx=0, edx=Position to seek to
;ecx may be non-zero for 64-bit IFS but currently DOS does not natively
; support them. INT 2Fh filesystems may have a tough time if they wish to
; use a 64 bit file pointer... They can try though!
;Output: If CF=NC: edx=0, eax=New position of file handle
;           Again, if network file, edx = Upper bytes of file hdl!
    call getSFTPtr
    jc extErrExit ;al (eax) has error code for bad file handle
    cmp al, 3
    jb .validFunction
    mov eax, errInvFnc       ;Error code and exit
    jmp extErrExit
.validFunction:
    cmp al, 1
    ja .seekend
    jb .seekset
;Here we are at seekcur, seek from current (signed)
    add edx, dword [rdi + sft.dCurntOff]    ;Get offset from current
.seekset:
;Seek from the start (unsigned)
    mov dword [rdi + sft.dCurntOff], edx ;Store the new offset
    xor edx, edx    ;All FAT files have 0 upper 32 bits! Set to 0!
    ;We return the file position in eax anyway!
.seekExit:
    call getUserRegs    ;Get user regs in rsi
    mov dword [rsi + callerFrame.rdx], edx
    mov eax, dword [rdi + sft.dCurntOff]  ;Return current offset if all ok!
    jmp extGoodExit2    ;Return OK in eax 
.seekend:
;Here we are at seekend, seek from end (signed)
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jnz .netCheck
.proceedDisk:
    add edx, dword [rdi + sft.dFileSize]    ;Add to file size
    jmp short .seekset
.netCheck:
    test word [rdi + sft.wOpenMode], FCBopenedFile  ;Is this a FCB opened file?
    jnz .proceedDisk
    movzx eax, word [rdi + sft.wOpenMode]   ;Get the open mode
    ;Check it's share mode
    and eax, 0F0h    ;Isolate share bits
    cmp eax, denyNoneShare  ;Don't deny? Proceed
    je .netSeek
    cmp eax, denyReadShare
    jne .proceedDisk
.netSeek:
    mov eax, 1121h  ;Make net seek from end request
    int 2Fh
    jnc .seekExit ;If the request returns with CF clear, there was no error
    jmp extErrExit


changeFileModeHdl: ;ah = 43h, handle function, CHMOD
    cmp al, 1
    jbe .subFuncOk
    mov eax, errInvFnc
.chmodError:
;Call with ax = errorcode
    mov byte [errorLocus], eLocUnk
    mov byte [errorClass], eClsAppFlt
    mov byte [errorAction], eActRetUsr
    jmp extErrExit
.subFuncOk:
    mov rsi, rdx
    mov ebx, eax    ;Store function code in bl
    push rbx
    push rcx
    call checkPathspecOK
    jnc .pathOk ;Path ok save for potentially having wildcards
.badPath:
    pop rcx
    pop rbx
    mov eax, errAccDen
    jmp short .chmodError
.pathOk:
    call checkPathNet 
    jz .badPath ;Or Net paths
    call scanPathWC
    jc .badPath ;Dont allow wildcards
    lea rdi, buffer1
    push rdx
    mov byte [searchAttr], dirInclusive    ;Inclusive search
    call getFilePath    ;Get the path for the file to delete
    pop rdx
    pop rcx
    pop rbx
    jc extErrExit   ;If the file or path was not found or error, bye bye
    ;In the case of a wildcard, recall this will return the first file
    cmp byte [fileExist], -1
    jnz .chmodError

    call testCDSNet
    jnc .notNet
    jnz .notNet ;This type of net ok
    mov eax, errAccDen
    jmp extErrExit
.notNet:
    test bl, bl
    jnz .setAttribs
    ;Get attribs here
    test word [rdi + cds.wFlags], cdsRedirDrive
    jz .getDiskAttribs
    mov eax, 110Fh  ;Get attributes and size in edi
    int 2Fh
    jc extErrExit
    jmp extGoodExit
.getDiskAttribs:
    movzx eax, byte [curDirCopy + fatDirEntry.attribute]   ;Get disk attributes
    jmp extGoodExit

.setAttribs:
    ;Set attribs here
    test word [rdi + cds.wFlags], cdsRedirDrive
    jz .setDiskAttribs
    movzx ecx, cx
    push rcx    ;Push attributes on stack in zero extended qword
    mov eax, 110Eh
    int 2Fh
    pop rcx
    jc extErrExit
    jmp extGoodExit
.setDiskAttribs:
    call checkNoOpenHandlesForShareAction
    jnc .okToSet
    mov eax, errShrVio
    jmp extErrExit
.okToSet:
    call dosCrit1Enter
    call getDiskDirectoryEntry  ;Get ptr to entry in rsi
    jc .setErrorNoFlush
    test cl, volLabelFile | directoryFile
    jz .set
    mov eax, errAccDen
    jmp .setErrorNoFlush
.set:
    mov ch, byte [rsi + fatDirEntry.attribute]  ;Get attribs
    and ch, (volLabelFile | directoryFile)    ;Keep these two bits
    or cl, ch
    mov byte [rsi + fatDirEntry.attribute], cl  ;Set new bits
    call flushAllBuffersForDPB
    jc .setError
    call dosCrit1Exit
    xor eax, eax
    jmp extGoodExit
.setError:

.setErrorNoFlush:
    call dosCrit1Exit
    jmp extErrExit

duplicateHandle:   ;ah = 45h, handle function
;Input: bx = Handle to duplicate
;Output: If ok then ax = New handle
    movzx eax, bx    ;Move handle to ax
    call findFreeJFTEntry    ;First find a free space in the JFT in rdi
.duplicateCommon:
    jc extErrExit   ;Exit if no space
    mov rsi, rdi    ;Save the free space in rsi
    xchg eax, ebx
    call getSFTPtr    ;Get the pointer to the SFT in rdi
    jc extErrExit
    inc word [rdi + sft.wNumHandles]    ;Increase the number of handles in SFT
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jnz .netFile
    call openDriverMux
.netFile:
    call getJFTPtr
    mov bl, byte [rdi]
    mov byte [rsi], bl
    jmp extGoodExit     ;Return handle in ax the entry in the JFT 

forceDuplicateHdl: ;ah = 46h, handle function
;Input: bx = Handle to duplicate
;       cx = Handle to close and replace with a duplicate of bx
    ;First we close cx if it exists
    movzx ecx, cx
    push rbx
    push rcx
    mov ebx, ecx
    call closeFileHdl   ;Close handle 
    pop rbx
    pop rax
    call getJFTPtr  ;Get a pointer to bx in rdi, destination for copy
    jmp short duplicateHandle.duplicateCommon

findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
;Input: cx = Search Attributes, cl only used
;       rdx = Ptr to path to file to look for
;       al = Document as needing to be 0 for now
;Return:
;Data in the DTA or error through AX with CF=CY
;CAVEATS!
;DTA:attribFnd = 40h => Char dev
;DTA:driveNum = Bit 7 set => Network redir drive
    mov byte [searchAttr], cl
    mov rsi, rdx
    call checkPathspecOK
    jnc .pathspecOk ;Path ok save for potentially having wildcards
    jz .pathspecOk  ;If ZF=ZE, then we had wildcards in last part which is ok
.badPath:
    mov eax, errAccDen  ;Gets xlat to errNoFil
    jmp extErrExit
.pathspecOk:
    push qword [currentDTA]
    lea rdi, dosffblock ;Use the dosFFblock as the DTA
    mov qword [currentDTA], rdi
    lea rdi, buffer1    ;Build the full path here
    call getFilePath
.findfileExit:
    pop qword [currentDTA]
    jc extErrExit
    lea rdi, dosffblock
    push rdi
    call setupFFBlock
    pop rsi ;Copy the internal ffblock block to the user's DTA
    mov rdi, qword [currentDTA]
    mov ecx, ffBlock_size
    rep movsb   ;Copy the whole block. 
;Ensure ffblock's non-reserved fields are filled from dir entry before returning
    xor eax, eax    ;Return value
    jmp extGoodExit ;Exit well

findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
;Input: DTA has the find first block from the previous search
    mov rsi, qword [currentDTA]
    lea rdi, dosffblock ;Copy the ffblock from the current DTA into my copy
    mov ecx, ffBlock_size
    rep movsb
    push qword [currentDTA] ;Save the current DTA address
    lea rdi, dosffblock ;Use the dosFFblock as the DTA
    mov qword [currentDTA], rdi
    call findNextMain
    jmp short findFirstFileHdl.findfileExit

renameFile:        ;ah = 56h
;Input: rdx -> Filespec to rename
;       rdi -> New filespec
;Wildcards are permissiable in the last path componant IFF server invoke!
    mov rsi, rdx
    mov ebx, dirInclusive
    test byte [dosInvoke], -1
    cmovz ecx, ebx  ;If not server, store this value instead
    mov byte [searchAttr], cl
    ;Step 0, verify both paths provided are valid
    call .renamePathCheck   ;Preserves rsi and rdi, check rsi path
    jc .accDenError
    push rsi
    mov rsi, rdi    ;Now check rdi path
    call .renamePathCheck
    pop rsi
    jc .accDenError
    ;Now we canonicalise the filenames to make life easy
    push rsi
    mov rsi, rdi
    lea rdi, buffer2
    call canonicaliseFileName   ;Now canonicalise rdi path
    pop rsi
    jc .accDenError
    ;Now test if first file exists.
    push qword [fname1Ptr]  ;Move the pointer to its var position
    pop qword [fname2Ptr]
    lea rdi, buffer1
    call canonicaliseFileName ;rdi = Buffer to use, rsi = filename
    jc .accDenError 
    call renameMain ;Both pathnames made good and copied internally, lets go!!
    jc extErrExit
    jmp extGoodExit
.accDenError:
    mov eax, errAccDen
    jmp extErrExit
.renamePathCheck:
;Checks if the pathspec in rsi is OK
    push rsi
    push rdi
    call checkPathspecOK
    pop rdi
    pop rsi
    jnc .pathOk     ;Path ok 
    jnz .badPath    ;If ZF=NZ, then the path was bad
    ;Here the path has wildcards in the last portion. Check for netInvoke
    test byte [dosInvoke], -1
    jnz .pathOk ;If this is -1, server invoke, wildcards are OK
.badPath:
    stc
    return
.pathOk:
    clc
    return

getSetFileDateTime: ;ah = 57h
    cmp al, 1
    jbe .oksubfun
    mov eax, errInvFnc
    jmp extErrExit
.oksubfun:
    call getSFTPtr  ;Preserves al unless error returned
    jc extErrExit ;al (eax) has error code for bad file handle
    cmp al, 1
    je .setTimeDate
    ;Here we get the Time/Date
    call dosCrit1Enter
    movzx ecx, word [rdi + sft.wTime]
    movzx edx, word [rdi + sft.wDate]
    call dosCrit1Exit
    call getUserRegs
    mov word [rsi + callerFrame.rcx], cx
    mov word [rsi + callerFrame.rdx], dx
    xor eax, eax
    jmp extGoodExit
.setTimeDate:
    ;Here we set the Time/Date
    call dosCrit1Enter
    mov word [rdi + sft.wTime], cx
    mov word [rdi + sft.wDate], dx
    xor eax, eax
    call qword [updateDirShare]
    ;Clear the flag to indicate that the dir needs to be updated and dont 
    ; further change the file time since we have manually overridden it 
    ; with the time specified
    and word [rdi + sft.wDeviceInfo], ~blokFileNoFlush  ;Clear flag to sync
    or word [rdi + sft.wDeviceInfo], blokNoDTonClose    ;Force it to this time
    call dosCrit1Exit
    xor eax, eax
    jmp extGoodExit

createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
;Uses the clock to do it's bidding
;cx = file attribute 
;rdx -> ASCIZ path ending with a '\' + 13 zero bytes to receive the generated 
;       filename
    test cx, ~(archiveFile | systemFile | hiddenFile | readOnlyFile)
    jz .validAttribs
    mov eax, errAccDen
    jmp extErrExit
.validAttribs:
    movzx r8, cx ;Save attributes in r8
    mov r9, rdx  ;Save pointer to the path in r9
    mov ecx, 64-13  ;First null must be at furthest, this many chars from rdx
    xor eax, eax
    mov rdi, rdx
    repne scasb
    test ecx, ecx
    jnz .nullFnd
    ;Bad path
    mov eax, errPnf
    jmp extErrExit
.nullFnd:
    dec rdi
    movzx eax, byte [rdi - 1]
    call swapPathSeparator
    jz .pathsepFnd
    ;If no pathsep found, force one at rdi
    mov al, "\"
    stosb
.pathsepFnd:
    mov rbp, rdi    ;Save in rbx the position of the start of the filename
.fileCreateLoop:
    push rbp
    call .uniqueTimeGet ;Get time in CX:DX
    pop rbp
    movzx eax, cx
    movzx edx, dx
    shl eax, 10h
    or eax, edx  ;Add the bits of dx too
    call .writeNybbles
    xor eax, eax
    stosb   ;Store terminating null
    mov ecx, r8d    ;Get the saved attribute back
    mov rdx, r9     ;Get the pointer to the path
    push rbp
    call createNewFile
    pop rbp
    jnc extGoodExit ;If the create succeeded, exit directly!
    movzx eax, word [errorExCde] ;Get pre translated error code
    cmp eax, errAccDen
    je .fileCreateLoop
    cmp eax, errFilExist
    je .fileCreateLoop
    stc
    jmp extErrExit  ;Exit with the error from the ExCde
.writeNybbles:
;Write the nybbles of eax at rdi
    mov ecx, 8  ;8 nybbles per dword
.wnLp:
    rol eax, 4  ;Roll eax left by 4
    push rax
    and eax, 0Fh    ;Save low nybble only
    add eax, '0'    ;Convert to ascii digit
    cmp eax, '9'
    jbe .notExtDigit
    add eax, 'A' - '9' ;Convert to a letter
.notExtDigit:
    stosb   ;Store the digit
    pop rax
    dec ecx
    jnz .wnLp
    return

.uniqueTimeGet:
    call readDateTimeRecord ;Update date if necessary, time in CLOCKrecrd
    mov cx, word [CLOCKrecrd + clkStruc.minutes]
    mov dx, word [CLOCKrecrd + clkStruc.hseconds]
    return

createNewFile:     ;ah = 5Bh
    push rcx    ;Save file attributes on stack
    lea rcx, createNewMain
    mov byte [searchAttr], dirInclusive ;Inclusive with dir (handled later)
    jmp openFileHdl.openCommon

lockUnlockFile:    ;ah = 5Ch
;ah = 5Ch
;al = subfunction
;       00h lock region of file
;       01h unlock region of file
;ebx = file handle
;ecx = start offset of region within file
;edi = length of region in bytes
    cmp al, 1
    ja .badFunction
    push rdi
    call derefSFTPtr
    jnc .validHdl
    pop rax ;Discard the qword on the stack
    mov eax, errBadHdl
    jmp extErrExit
.validHdl:
    test al, al ;Check if al = 0
    pop rax ;Get the length of the file region in bytes in eax
    jz .lockFileRegion
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jz .unlockShare ;Jump if a local file only
    push rax
    mov eax, 110Bh     ;Unlock Net file region
    int 2Fh
    pop rbx
    jmp short .exitSelect
.unlockShare:
    call qword [unlockFileShare]    ;Call share hook
.exitSelect:
    jc extErrExit
    jmp extGoodExit
.lockFileRegion:
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jz .lockShare   ;Jump if a local file only
    push rax
    mov eax, 110Ah  ;Lock net file region
    int 2Fh
    pop rbx
    jmp short .exitSelect
.lockShare:
    call qword [lockFileShare]  ;Call share hook
    jmp short .exitSelect
.badFunction:
    mov eax, errInvFnc
    mov word [errorExCde], ax
    jmp extErrExit

setHandleCount:    ;ah = 67h
;Input: bx = Size of new file handle table for process
;Output: CF=NC -> Ok to make more handles
;        CF=CY -> ax = Error code
;Five cases to consider:
;       1) Allocating a new block of memory, copying PSP JFT to it, inc hdl cnt
;       2) Freeing a block and returning to the PSP JFT, dec hdl cnt x
;       3) Extending an external block, inc hdl cnt. If realloc fails, goto 5)
;       4) Reducing an external block, dec hdl cnt, no realloc.
;   Special case below, cannot be enacted directly by caller.
;       5) Freeing an external block for a bigger external block, inc hdl cnt
    movzx ebx, bx   ;Zero extend to use ebx/rbx
    mov rbp, qword [currentPSP] ;Get a ptr to the currentPSP
    cmp bx, word [rbp + psp.jftSize]    ;Requesting more handles than we have?
    ja short .moreHdlsReq
    cmp bx, dfltJFTsize ;Requesting more than the default JFT amount?
    ja short .reduceExternal
    ;Here if 20 handles or less requested
    cmp word [rbp + psp.jftSize], dfltJFTsize   ;If this is 20 or less, exit
    ja short .reduceFree  ;Copying back to the JFT
    je short .exitGood    ;Else we are already in the PSP
    mov word [rbp + psp.jftSize], dfltJFTsize   ;Else, replace with dflt
.exitGood:
    jmp extGoodExit
.exitBad:
    jmp extErrExit
.reduceExternal:
;We try to reallocate the block to be more appropriate for the new maxhdls.
;If it fails, no big deal since we manually prevent the user from using more
; files. If we then grow this block again, realloc will try to grow it again
; and failing that, it will free it and then allocate a new block.
    mov rsi, qword [rbp + psp.externalJFTPtr]   ;Get xtrnal pointer
    call .checkOpenHandles
    jc short .exitBad
    ;Recall ebx has the numebr of handles here
    call .reallocBlock  ;Try realloc size to be more ok. If it fails, no biggie
    mov word [rbp + psp.jftSize], bx    ;Store new handle cnt
    jmp short .exitGood
.reduceFree:
;Entered once we know that we have an external block
;Now we close all handles above JFT size
    lea rdi, qword [rbp + psp.externalJFTPtr]   ;Get destination
    mov rsi, qword [rdi]    ;Get source 
    call .checkOpenHandles
    jc short .exitBad
    mov ebx, dfltJFTsize
    mov ecx, ebx    ;Move count of handles into ecx
;No open handles in range to be freed, now we copy the first 20 handles over
    call .copyBlock
    ;Now we can free the old block
    mov r8, rsi
    push rbx
    call freeMemory
    pop rbx
    jc short .exitBad
    mov word [rbp + psp.jftSize], bx   ;Now we have dflt number of hdls
    xor eax, eax
.exitGood2:
    jmp short .exitGood
.moreHdlsReq:
    cmp ebx, 0FFFFh ;HARD LIMIT ON NUMBER OF HANDLES IS (WORD)-1
    jne short .okToExpand
    mov eax, errInvFnc
.exitBad2:
    jmp short .exitBad
.okToExpand:
;Need to check if we are external and reallocating. 
;   If we are, can we realloc or do we need to free and save?
    cmp word [rbp + psp.jftSize], dfltJFTsize   ;Are we in JFT?
    jbe short .moreFromJFT
    mov rsi, qword [rbp + psp.externalJFTPtr]   ;Get xtrnal pointer
    call .reallocBlock
    jnc short .exitGood
    call .getBlock  ;rsi is preserved across the call
    jc short .exitBad
    mov r8, rsi ;Free the source block
    push rbx
    push rdi    ;Save the new pointer here
    push rbp
    call freeMemory
    pop rbp
    pop rdi
    pop rbx
    jnc short .freeOk ;Free'd the original block
    push rax    ;Save error code on stack
    mov r8, rdi ;Free the new block
    call freeMemory
    pop rax
    jmp short .exitBad2
.moreFromJFT:
    lea rsi, qword [rbp + psp.jobFileTbl]   ;Get the ptr to the current JFT
    call .getBlock
    jc short .exitBad2
.freeOk:
    mov word [rbp + psp.jftSize], bx    ;Set the new count
    mov qword [rbp + psp.externalJFTPtr], rdi
    xor eax, eax
    jmp short .exitGood2
.reallocBlock:
;Input:
;   rsi -> Source block to reallocate
;   ebx = Number of handles
;   rbp -> Current PSP
;Output: CF=NC => rsi -> Source block reallocated in size
;                 ebx = Number of handles
;        CF=CY => Error, EAX has error code
    push rsi ;Save external pointer on stack
    push rbx    ;Save number of handles on stack
    push rbp
    add ebx, 11h    ;Round up into next paragraph
    shr ebx, 4      ;Get number of paragraphs
    mov r8, rsi
    call reallocMemory
    pop rbp
    pop rbx
    pop rsi ;Get external pointer back in rsi
    return
.getBlock:
;rsi -> Source block for copy 
;ebx = Number of new handles
;Output: rsi and ebx as before
;        rdi -> New block
;IF CF=CY, bad exit
    push rbx    ;bx has the number of handles we want
    push rsi
    push rbp
    add ebx, 11h    ;Round up into next paragraph
    shr ebx, 4      ;Get number of paragraphs
    mov ecx, ebx
    shl ecx, 4  ;Get bytes being allocated
    push rcx    ;Save the actual number of bytes in the alloc
    call allocateMemory ;Allocate memory 
    pop rcx ;Get back actual number of bytes allocated
    pop rbp ;Get the PSP pointer back
    pop rsi ;Get the source pointer back
    pop rbx ;Get the number of handles to allocate back
    retc
    mov rdi, rax    ;Move the ptr of the new block to rdi
    push rdi
    xor eax, eax
    dec eax
    rep stosb   ;Setup the new memory block with all -1's
    pop rdi
    mov ecx, ebx    ;Get the new number of handles to copy over
    call .copyBlock ;Copy all the handles over
    return
.copyBlock:
;Input: rsi -> Source block
;       rdi -> Destination block
;       ecx = Number of handles to copy
    push rsi
    push rdi
    push rcx
    rep movsb
    pop rcx
    pop rdi
    pop rsi
    return
.checkOpenHandles:
;Checks for open handles in the range that is to be freed.
;If any found, error exit
;Input: rsi -> Where to start searching
    push rsi
    movzx ecx, word [rbp + psp.jftSize] 
.cohLoop:
    cmp byte [rsi], -1
    jne .checkOpenHandlesBadExit
    inc rsi
    dec ecx
    jnz .cohLoop
    pop rsi
    return
.checkOpenHandlesBadExit:
    mov eax, errNhl
    stc
    pop rsi
    return

commitFile:        ;ah = 68h, flushes buffers for handle to disk 
    ;Input: bx = File to flush
    call getSFTPtr  ;Get sft pointer in rdi
    jc extErrExit
    call setCurrentSFT  ;Set as current SFT to ensure it is committed
    ;Now we check if the device is a char, disk or net file and commit
    call commitMain
    jc extErrExit   ;If an error occured, exit with error code in al
.exitOk:
    xor al, al
    call getUserRegs
    and byte [rsi + callerFrame], ~1    ;Clear CF
    return

;-----------------------------------:
;       Main File IO Routines       :
;-----------------------------------:
commitMain:
;Commits the current SFT 
    call getCurrentSFT  ;Gets currentSFT into rdi
    movzx ebx, word [rdi + sft.wDeviceInfo]
    test ebx, devCharDev | blokFileNoFlush
    retnz   ;Return if nothing has been written or a char dev
    test ebx, devRedirDev
    jz .notNet
    ;Commit file net redir call and exit
    mov eax, 1107h
    int 2Fh
    return  ;Propagate CF and AL if needed due to error
.notNet:
    call dosCrit1Enter
    call updateSFTDateTimeFields    ;Update the SFT Time fields
    mov eax, -1         ;Set a "large" count for open handles
    call flushFile      ;Now file gets flushed and exit critical section
    return  ;Propagate CF and AL if needed due to error
    
renameMain:
;Now, creates a special find first block for the source file
; that is in curDirCopy. Then we build a search pattern for the new name, 
; following wildcard rules sourcing from the filespec in the special find first 
; block. 
;Then we search for that file name specifically (again), replacing 
; the filename portion of the destination buffer. If it exists or the 
; filename is a char device, we crap out. If it doesnt exist, we create
; the new directory entry and delete the original file. 
;Input:
; filenamePtr1 -> Source path + filename pattern
; filenamePtr2 -> New path + filename pattern
; workingCDS -> CDS for drive we are considering (set by first pathspec)
; searchAttr = Set to search attributes (inclusive or by request if netinvoke)
    mov rdi, qword [workingCDS]
    call testCDSNet ;CF=NC => Not net
    jnc .notNet
    mov eax, 1111h
    int 2Fh
    return
.notNet:
;First check if both drives are on the same disk
    mov al, byte [buffer1]  ;Canonicalised pathspecs are uppercased
    mov ah, byte [buffer2]  ; so can directly compare pathspecs
    cmp al, ah
    je .sameDrive   ;Drives have to be the same if local file rename
    mov eax, errDevUnk
    stc
    return
.sameDrive:
;Now we check if either pathsepc is simply X:\,0
; If they are, return fail as we cannot rename the root dir
    call dosCrit1Enter
    mov eax, dword [buffer1]
    xor al, al
    cmp eax, 005C3A00h  ;0,:\,0, are we root?
    jne .checkpath2
.accDen:
    mov eax, errAccDen
.errorExit:
    stc
    jmp .exit2
.checkpath2:
    mov eax, dword [buffer2]
    xor al, al
    cmp eax, 005C3A00h
    je .accDen  ;If this is the root, exit access denied
    ;Now ensure hte two paths are not equal
    lea rsi, buffer1
    lea rdi, buffer2
    call compareFileNames
    jz .accDen
;Now we find first the source file
    mov rsi, qword [fname1Ptr]
    mov rdi, rsi
    call getFilePathNoCanon    ;Get the path for the file to delete
    jc .exit2    ;Return with CF=CY if file doesn't exist
    lea rsi, curDirCopy
    lea rdi, renameDir
    mov ecx, fatDirEntry_size/8
    rep movsq   ;Copy directory over
    lea rdi, renameFFBlk
    call setupFFBlock   ;Need this to save the dir entry cluster/sector/offset 
    ;Now we check this path, if it is a DIR, ensure it is not the current
    ; dir for any CDS.
    test byte [curDirCopy + fatDirEntry.attribute], dirCharDev
    jnz .accDen ;Cant rename a char file!
    test byte [curDirCopy + fatDirEntry.attribute], directoryFile
    jz .notDirCheck
    mov rdi, qword [fname1Ptr]
    push rdi
    call strlen ;Get asciiz length in ecx
    pop rbx
    dec ecx ;Get one less char to check, we check the last one manually
    mov rsi, qword [cdsHeadPtr]
    movzx edx, byte [lastdrvNum]
.dirCheck:
    mov rdi, rbx
    push rcx    ;Save the char count to check!
    push rsi    ;Save rsi pointing to the start of the CDS
    repe cmpsb  ;Compare while they are equal
    lodsb   ;Get the last char to check in al
    pop rsi ;Put rsi back to the start of the string
    pop rcx
    jne .neqDir
    ;Instead of failing, if not join, simply reset that CDS entry to root.
    cmp al, "\" ;Check the last char manually for pathend
    je .curDirChangeErr
    test al, al
    jne .neqDir ;Proceed as normal if not null
.curDirChangeErr:
    ;Here we are trying to change a current directory. Fail it!
    ;This is (DOS 5.0+/Windows)-like behaviour but its sensible and what
    ; we initially had programmed in (with access denied error instead).
    mov eax, errDelCD   ;This is a more descriptive error.
    jmp .errorExit
.neqDir:
    add rsi, cds_size   ;Goto next CDS
    dec edx
    jnz .dirCheck
.notDirCheck:
    ;Now use FFBlock to temp swap out the filename with the source pattern
    mov ecx, -1    ;Just a large number to search
    xor eax, eax
    mov rdi, qword [fname1Ptr]
    repne scasb   ;Search for terminating null
    std ;Go backwards now, to find the path sep
    mov al, "\"
    repne scasb   ;Now rsi points one before
    cld ;Go normal ways now
    add rdi, 2  ;Goto first char of pathname
    push rdi
    mov rsi, rdi
    lea rdi, wcdFcbName
    call asciiToFCB ;Copy the search pattern to wcdFcbName
    pop rdi
    push rdi    ;Save rdi on stack again
    lea rsi, qword [renameFFBlk + ffBlock.asciizName]
    ;Copy the asciiz name (including terminating null) to pathspec
    movsq
    movsd
    movsb   ;This is a terminating null if a full 8.3 filename
    pop rdi ;rdi points to first char position
    call checkNoOpenHandlesForShareAction   ;Now do this, all regs saved!
    jc .errorExit   ;Propagate the share error code
    ;Else return the pattern back to its original position
    lea rsi, wcdFcbName ;rsi -> FCBified pattern, rdi -> First char for name
    call FCBToAsciiz
    ;Delete the original directory entry
    movzx ecx, byte [rbp + dpb.bSectorsPerClusterShift] 
    inc ecx ;Get sectors/cluster in ecx
    movzx eax, word [rbp + dpb.wBytesPerSector]
    mul ecx ;Get bytes per cluster in eax
    mov ecx, dword [renameFFBlk + ffBlock.dirOffset] ;32 byte offset in cluster
    shl ecx, 5  ;Get byte offset in cluster
    xchg ecx, eax
    div ecx ;Get Sector in cluster in eax and sector offset in edx
    mov ebx, eax
    mov eax, dword [dirClustA]  ;Get the current dir cluster, not start cluster
    call getStartSectorOfCluster    ;Cluster number in eax, sector in rax
    add rax, rbx    ;Goto the sector for the cluster
    call getBufForDir ;Get buffer pointer in rbx
    jc .badExit
    lea rsi, qword [rbx + bufferHdr.dataarea + rdx] ;Goto byte offset in sector
    ;rsi points to the file entry
    mov al, byte [delChar]
    mov byte [rsi], al
    call markBufferDirty    ;Set this buffer as having been written to now

    ;Now we use the destination filename pattern to build the new filename
    mov rdi, qword [fname2Ptr]  ;Get the destination path ptr in rdi
    xor eax, eax
    mov ecx, 67
    repne scasb   ;Find the null terminator of the destination path
.findPattern:
    dec rdi
    cmp byte [rdi], "\" ;Is this a pathsep?
    jne .findPattern
;rdi points to the pathseparator, unless it is the root pathsep
    mov byte [fileFDflg], 0 ;Clear this byte
    cmp byte [rdi - 1], ":"
    jne .notRoot
    ;The root dir always exists on a volume so can skip the check.
    inc rdi ;Goto the first char of the filename
    push rdi
    or byte [fileFDflg], 2  ;Bit 2 means ROOT DIR FILE
    jmp short .skipCheck
.notRoot:
    stosb   ;Store a null over the "\" and inc rdi to char one of pattern
    push rdi    ;Push the new name pattern portion ptr in var
    ;Now check if the parent directory exists for the destination
    push rdi    ;Save the ptr to the first char of the pathsep
    push qword [fname1Ptr]  ;Preserve original source buffer
    lea rsi, buffer2
    mov rdi, rsi
    call getDirPathNoCanon ;We are searching for a directory ONLY
    pop qword [fname1Ptr]
    pop rdi
    jc .badExit    ;Error if the parent dir doesnt exist
    mov byte [rdi - 1], "\" ;Replace the pathseparator
.skipCheck:
    ;rdi now points to the first char of the pattern
    mov rsi, rdi
    lea rdi, wcdFcbName
    call asciiToFCB ;Convert the asciiz name to FCB format
    ;Ensure the destination pattern is not all question marks
    cmp dword [wcdFcbName], "????"
    jne .validRenPattern
    cmp dword [wcdFcbName + 4], "????"
    jne .validRenPattern
    cmp word [wcdFcbName + 8], "??"
    jne .validRenPattern
    cmp byte [wcdFcbName + 10], "?"
    je .bad ;Access denied buddy ol' pal
.validRenPattern:
    ;curDirCopy has information for the destination directory file we will 
    ; write to. The cluster points to the directory file itself to search in
    ;Each filename we create must be searched for to ensure it doesnt exist
    ;Make new fname in fcbName from asciizname in renameDir
    lea rsi, renameDir  ;Copy the source filename over
    lea rdi, fcbName
    push rdi    ;Preserve this as the destination for copy
    mov ecx, 11
    rep movsb
    pop rdi
    lea rsi, wcdFcbName ;Now source the chars from here
.mnnLp:
    lodsb
    cmp al, "?" ;Is it a wc?
    je .mnnWC   ;Skip overriding this char
    stosb   ;Store new char and go forwards by one
    dec rdi ;Now go back by one
.mnnWC:
    inc rdi ;Goto next letter
    inc ecx
    cmp ecx, 11
    jne .mnnLp
    lea rsi, fcbName
    lea rdi, renameDir
    mov ecx, 11
    rep movsb   ;Move the name over from fcbName to new dir entry name field
    pop rdi ;Get the first char of the filename place in destination pathspec
    push rdi    ;Push it on stack again to be reused later if a WC spec
    lea rsi, fcbName
    call FCBToAsciiz    ;Copy the name over to asciiz 
    mov rsi, qword [fname2Ptr]
    mov rdi, rsi
    call getFilePathNoCanon    ;This must be a non-existant file
    jnc .badExit   ;If the file exists, then error
    cmp al, errFnf ;If Fnf error then we may proceed
    jne .badExit
    ;Now we search the parent dir (the curDirCopy dir) for free space
    ;If the parent is root, deal with special case
    lea rbx, curDirCopy
    test byte [fileFDflg], 2
    mov byte [fileFDflg], 0 ;Clear the flag again
    jz .notRoot2
    lea rsi, fcbName
    lea rdi, curDirCopy
    movsq
    movsd
    movsb
    xor eax, eax    ;Dir to search
    jmp short .getSpace
.notRoot2:
    movzx edx, word [curDirCopy + fatDirEntry.fstClusHi]
    movzx eax, word [curDirCopy + fatDirEntry.fstClusLo]
    shl edx, 10h
    or eax, edx ;Get first cluster of dir file in eax
.getSpace:
    call .searchForDirSpace
    jc .bad
;dir Entry found, rsi points to space in buffer
    mov rdi, rsi
    lea rsi, renameDir
    mov ecx, fatDirEntry_size/8
    rep movsq   ;Copy dir over
    call markBufferDirty ;Mark buffer as written to now
    ;Now we check if source filename or wcdFcbname has a wildcard
    ;If it does, we find next. If not, we exit
    mov al, "?"
    mov rdi, qword [fname1Ptr]  ;Check filename
    call strlen ;Get in rcx to get the length of the string
    repne scasb
    jne .exit  ;If source name has no wildcards, exit
    mov ecx, 11
    lea rdi, wcdFcbName
    repne scasb
    jne .exit
    ;Here we gotta do a find next now!
    push qword [currentDTA]
    lea rdi, renameFFBlk
    mov qword [currentDTA], rdi ;Set renameFFBlk as currentDTA for operation
    call findNextMain
    pop qword [currentDTA]
    jnc .validRenPattern    ;If this is ok, now find next file
    ;Else propagate the CF if this ends with an error (inc no more files)
.exit:
    pop rdi ;Pop the ptr to the dest pathspec ptr off stack
    call flushAllBuffersForDPB
    jc .badExit
.exit2: ;Bad exit before we push qword on stack 
    call dosCrit1Exit
    return
.bad:
    mov eax, errAccDen  ;Temp return code
.badExit:
    stc
    pop rdi
    jmp short .exit2

.searchForDirSpace:
;Input: eax = First directory to search 
    mov dword [dirClustA], eax
    xor eax, eax    ;Reset the search to the start of the current directory
    mov word [dirSect], ax
    mov dword [dirEntry], eax
    call findFreeDiskDirEntry   ;rsi = ptr to a dir entry in a disk buffer
    jnc .dirEntryFnd
    cmp dword [dirClustPar], 0  ;If the parent = 0 => Root Dir Fat12/16
    je .searchBad ;Set CF and exit
    call growDirectory  ;Increase directory size by 1 cluster
    jc .searchBad
    cmp eax, -1 ;Disk Full?
    je .searchBad
    ;Else eax = Newly allocated cluster
    jmp short .searchForDirSpace
.dirEntryFnd:
    clc
    return
.searchBad:
    stc
    return


checkNoOpenHandlesForShareAction:
;We check that we can proceed. This function will fail IF there are handles
; open, and thus makes it an ideal candidate for checking in RENAME, for 
; both filenames, DELETE and SETFILEATTRIBS if we have any open files. If we
; do, then we should get a Share error and thus it would prevent us from 
; proceeding.
;
;If SHARE not loaded, ALL handles must be closed before Rename or Delete.
;Input: SDA fname1Ptr -> Filename we want to consider
;       currDirCopy = Directory entry for the file
    call dosPushRegs    ;Save the context completely
    cmp byte [openFileCheck], 0 ;Some 16-bit SHAREs set the marker to 0.
    jz .noOpenFileCheckHandler
    call qword [openFileCheck]
    jc .noOpenFileCheckHandler  ;If CF=CY, this function not defined
    jz .exit    ;If CF=NC && ZF=ZE, Function defined and No open files, proceed.
    jmp short .errorMain    ;If ZF=NZ -> Have some open files, error out!
.noOpenFileCheckHandler:
    ;The following closes most recent shared handles referencing it
    ;Only if sharePSP, shareMachineNumber are equal and openMode not Compat
    ; mode and if there is precisely 1  
    call qword [closeNewHdlShare]    
    ;The close of the handle will only happen if there is 1 file referring to it
    lea rdi, scratchSFT
    mov qword [currentSFT], rdi
    mov eax, RWAccess | CompatShare ;Set open mode
    mov byte [openCreate], 0    ;Make sure we are just opening the file
    ;This is to avoid needing to put the file attributes on the stack
    push rdi    ;Save the SFT ptr
    call buildSFTEntry
    pop rdi
    jc .errorMain
    mov word [rdi + sft.wNumHandles], 1   ;One "reference"
    mov word [rdi + sft.wOpenMode], denyRWShare ;Prevent everything temporarily
    call shareFile  ;Puts an sft handle in rdi
    jc .errorMain
    mov word [rdi + sft.wNumHandles], 0
    call closeShareCallWrapper
.exit:
    call dosPopRegs
    clc
    return
.errorMain:
    call dosPopRegs
    mov eax, errShrVio  ;Share issue so return failed with share
    stc
    return

outerDeleteMain:
;Internal current dir must be populated with dir entry
; and internal DOS directory variables now point to this entry.
;Checks if the file is a char device or read only.
;Input: curDirCopy must has dir for the file to be deleted
;       disk vars must be populated (i.e. getFilePath mustve been run)
;Returns: CF=CY => Error (including no files if wildcard) in eax
;         CF=NC => File deleted
    mov rdi, qword [workingCDS]
    call testCDSNet ;CF=NC => Not net
    jnc .notNet
    mov eax, 1113h  ;Allows wildcards, and will delete all which match
    int 2Fh
    return
.notNet:
    mov eax, errAccDen  
    test byte [curDirCopy + fatDirEntry.attribute], dirCharDev | dirDirectory | dirReadOnly
    jnz .exitBad  ;Can't delete char dev, dir or ro file
    call deleteMain
    jc .exitBad
    ;Check if the name has a wildcard in it, if so, keep searching
    mov al, "?"
    lea rdi, fcbName    ;This is the search pattern we used to find the file
    mov ecx, 11
    repne scasb   ;Scan for the wildcard char
    je .serverWCloop   ;This is not possible if entered via 21/41h
    clc
    return  ;Return ok!
.serverWCloop:
    push qword [currentDTA] ;Save the current DTA address
    lea rdi, dosffblock
    push rdi    ;Push this address onto the stack
    call setupFFBlock   ;Setup FFblock internally
    pop qword [currentDTA] ;And use the dosFFblock as the DTA
    call findNextMain   ;rdi gets reloaded with DTA in this call
    pop qword [currentDTA]
    retc    ;Return with no more files error now
    call deleteMain ;Whilst it keeps finding files that match, keep deleting
    jnc .serverWCloop     
;Stop as soon as an error occurs
.exitBad:
    stc
    return

deleteMain:
;Now unlink FAT chain and then clear directory entry
;Get the start cluster of this file
;Input:
; The file must have NOT be read-only.
; If the CDS is NOT a net cds then the following must hold:
;     - curDirCopy must be filled with the file directory information
;     - workingDPB must be disk dpb and dir variables must be set
;Output: CF=NC => Success, 
; If not net CDS:
;   rsi keeps pointing to directory entry.
;   al = First char of the file that was deleted.
;        CF=CY => Error
;The dir buffer must be marked as referenced once we are done with it
    call checkNoOpenHandlesForShareAction   ;Also cannot delete if open handle
    retc    ;Return immediately if CF=CY and propagate error code
    push rbp
    mov rbp, qword [workingDPB] ;Get the working DPB for the disk of this file
    push rdx
    movzx edx, word [curDirCopy + fatDirEntry.fstClusLo]
    movzx eax, word [curDirCopy + fatDirEntry.fstClusHi]
    shl eax, 10h
    or eax, edx ;Add low bits to eax
    pop rdx
    test eax, eax   ;Cluster value of 0 means no allocation
    jz .skipUnlink  ;If there is no FAT allocation for file, skip "dealloc"
    call unlinkFAT  ;Unlink the FAT entry
    jc .exitBad
.skipUnlink:
    ;Now replace the first char of the directory to 0E5h
    ;Get the disk directory in a buffer to manipulate the entry
    call getDiskDirectoryEntry
    jc .exitBad
    mov al, byte [delChar]
    xchg byte [rsi], al    ;Mark entry as free, get char in al
    call markBufferDirty
    ;CF must be clear
    call flushAllBuffersForDPB
    pop rbp
    retnc
.exitBad:
    pop rbp
    stc
    return

openMain:
;Input: ax (formally al) = Open mode
;       [currentSFT] = SFT we are building
;       [workingCDS] = CDS of drive to access
;       [workingDPB] = DPB of drive to access
;Ouput: CF=CY => Error, eax has error code
    call setOpenMode
    retc    ;Error Exit 
    mov rdi, qword [currentSFT]
    mov rsi, qword [workingCDS]
    xor ah, ah  ;al has the access mode
    cmp rsi, -1
    jne .notNet
.redirOpen:
    push rax    ;Push open mode onto stack
    mov eax, 1116h  ;Open remote file on "\\" pathspec drives
    int 2Fh
    pop rax
    return
.notNet:
    test word [rsi + cds.wFlags], cdsRedirDrive
    jnz .redirOpen  ;If redir drive, go via the redir interface
    call dosCrit1Enter
;Ensure our disk attributes permit opening
    mov dl, byte [curDirCopy + fatDirEntry.attribute]   ;Get the disk attrib
    test dl, dirVolumeID    ;Is the found file a volume label?
    jnz .accDenExit
    test dl, dirReadOnly    ;Is the found file marked as RO in the file system?
    jz short .openFile      ;If not, proceed.
;Else, we check if we are permitted to open this file.
    movzx ecx, word [rsi + sft.wOpenMode]   ;Get the user-set open mode
    test ecx, FCBopenedFile  ;We consider FCBs here for future net use 
    jnz .fcbOpen    ;If FCB open, intervene appropriately
    mov edx, ecx
    and edx, 070h   ;Isolate the share bits only
    cmp edx, netFCBShare ;Is this a net server FCB open?
    je .fcbOpen     ;If it is net fcb, similarly force to ro as before
    and ecx, 0Fh    ;Else, isolate the bottom nybble
    cmp cl, ReadAccess  ;Are we asking for more than read?
    je .openFile    ;If no, proceed, eax has openmode. Else, access denied!
.accDenExit:
    mov eax, errAccDen
    jmp short .errorExit
.fcbOpen:
    and cx, 0FFF0h  ;Set to read access open only. Preserve share/property bits
    mov word [rsi + sft.wOpenMode], cx
    mov eax, ecx    ;Move the modified open mode into eax for buildSFT
.openFile:
    mov byte [openCreate], 0   ;Opening file, set to 0
    mov byte [delChar], 0E5h
    call buildSFTEntry  ;ax must have the open mode
    jc .errorExit
    call shareFile      ;Puts an SFT handle in rdi
    jnc .fileSharedOk   ;If the file open doesnt violate share, jump!
.errorExit:
    call dosCrit1Exit   ;Else we error out with error code in al
    return
.fileSharedOk:
    mov eax, 3  ;Update date/time and everything in the share dir sync call
    call getCurrentSFT  ;Get SFT ptr in rdi
    call qword [updateDirShare] ;Now call the dir sync, this default sets CF 
    call dosCrit1Exit
openDriverMux:  ;Int 2Fh, AX=120Ch, jumped to by Create
    mov rdi, qword [currentSFT]
    call openSFT
    test word [rdi + sft.wOpenMode], FCBopenedFile
    jnz .netOpen
    return
.netOpen:
    mov rax, qword [currentPSP]
    mov qword [rdi + sft.qPSPOwner], rax
    return

setOpenMode:
;Input: al = Open mode for the file open
    mov byte [fileOpenMd], al
    push rbx
;Check we are not opening a directory. This is to prevent disk io with a dir
    test byte [curDirCopy + fatDirEntry.attribute], directoryFile
    jnz .somBad    ;Directories are not allowed to be opened
    mov bl, al
    and bl, 0F0h    ;Isolate upper nybble. Test share mode.
    cmp byte [dosInvoke], -1    
    jnz .s1 ;Skip this check if not server invoke
    cmp bl, netFCBShare ;Test share mode for netFCB
    je .s2
.s1:
    cmp bl, denyNoneShare
    ja .somBad
.s2:
    mov bl, al  ;Isolate lower nybble. Access mode.
    and bl, 0Fh
    cmp bl, RWAccess
    ja .somBad
    pop rbx
    clc
    return
.somBad:
    pop rbx
    mov eax, errAccCde
    stc
    return

createNewMain:
;Input: ax (formally al) = File attributes
;       [currentSFT] = SFT we are building
;       [workingCDS] = CDS of drive to access
;       [workingDPB] = DPB of drive to access
    movzx eax, al
    mov ah, 1    ;Tell redir to create new file
    test byte [fileExist], -1
    jz createMain.createNewEP    ;Create only if the file doesnt exist!
    mov eax, errFilExist    ;Else, return a file exists error!
    stc
    return
createMain:
;Input: ax (formally al) = File attributes
;       [currentSFT] = SFT we are building
;       [workingCDS] = CDS of drive to access
;       [workingDPB] = DPB of drive to access
    movzx eax, al
.createNewEP:
    test al, 80h    ; Is this invalid bit set?
    jnz .invalidAttrib
    test al, volLabelFile    ;Is this a volume label?
    jz .notVol
    mov al, volLabelFile ;If the vol bit is set, set the whole thing to volume only
    ;Set archive bit for new vol labels for incremental archivers to update
.notVol:
    or al, archiveFile   ;Set archive bit
    test al, directoryFile | charFile   ;Invalid bits?
    jz .validAttr   ;Creating directory with this function is forbidden also
.invalidAttrib:
    mov eax, errAccDen
    stc
    return
.validAttr:
;Check we are not creating a directory.
    mov rdi, qword [currentSFT]
    mov rsi, qword [workingCDS]
    cmp rsi, -1
    jne .diskFile
    push rax    ;Save the new attributes
    mov eax, 1118h  ;Create file w/o CDS
    int 2Fh
    pop rbx
    return
.diskFile:
    test word [rsi + cds.wFlags], cdsRedirDrive ;We a redir drv?
    jz .hardFile
    push rax    ;Save the new attributes
    mov eax, 1117h  ;Create file with CDS
    int 2Fh
    pop rbx
    return
.hardFile:
    or word [rdi + sft.wOpenMode], RWAccess ;Set R/W access when creating file
    mov byte [openCreate], -1   ;Creating file, set to FFh
    mov byte [delChar], 0E5h
    call dosCrit1Enter  ;Writing the SFT entry, must be in critical section
    push rdi    ;Save the sft handle
    push rax    ;Save the file attributes on stack
    mov eax, RWAccess | CompatShare ;Set open mode
    call buildSFTEntry
    pop rbx ;Pop the file attribute off
    pop rdi
    jc .errorExit
    call shareFile  ;Puts an sft handle in rdi, preserves rbx
    jc .errorExit
    test bl, volLabelFile    ;Was the attribute a volume label?
    jz .notVolLabel    ;If not vol label, skip.
; Treat volume label creation case here. Rebuild DPB.
    mov rdi, qword [workingCDS]    ;Get the CDS ptr for getDiskDPB
    mov al, byte [rdi]     ;Get the drive letter
    sub al, "A"            ;Convert to a 0 based number
    mov byte [rebuildDrv], al  ;Set the volid rebuild var
    call dosCrit1Enter
    call getDiskDPB        ;Rebuild DPB and clear var
    call dosCrit1Exit
.notVolLabel:
    mov eax, 2  ;Needed for the SHARE call
    call qword [updateDirShare]
    call dosCrit1Exit
    jmp openDriverMux
.errorExit:
    call dosCrit1Exit
    return
buildSFTEntry:
;Called in a critical section.
;Input: al = Open mode
;       STACK: File attributes if creating a file
;       [currentSFT] = SFT we are building
;       [workingCDS] = CDS of drive to access
;       [workingDPB] = DPB of drive to access
;     SDA curDirCopy = Copy of dir for file if found or parent dir if not.
;
;Output: If CF=NC: - CurrentSFT filled in except for wNumHandles and bFileAttrib
;                  - wDeviceInfo is set except for inherit bit
;        If CF=CY: - Return fail
;
;
;Check if file exists. If it does, build SFT and truncate if not char dev. 
;   If not, create disk entry.
;Check if the device was a char device by checking curDirCopy.
;If disk, get dpb. We check if the parent dir was found.

;First set the open mode, time and date, ownerPSP and file pointer
; to start of file fields of the SFT
    push rbp    ;file attribute is rbp + 10h
    mov rbp, rsp
    mov rsi, qword [currentSFT]
;Set the open mode
    mov word [rsi + sft.wOpenMode], ax
;Get current time
    call readDateTimeRecord ;Update DOS internal Time/Date variables
    call getDirDTwords  ;Get current D/T words packed in eax
    mov dword [rsi + sft.wTime], eax    ;Store time and date together
;Set current Owner
    mov rax, qword [currentPSP]
    mov qword [rsi + sft.qPSPOwner], rax ;Set who opened the file
;Set file pointer to first byte
    mov dword [rsi + sft.dCurntOff], 0  
;Common fields set
    test byte [openCreate], -1  ;Create = -1
    jz .openProc
    ;Here if Creating a file.
    ;First check if we are handling a volume label
    test qword [rbp + 10h], volLabelFile  ;Are we creating a volume label?
    jz .notVolLbl   ;Bit not set? Jump!
    push rsi
    push rdi
    push rbp
    mov byte [volIdFlag], -1    ;Set the volid search bit
    call searchDir  ;Searches the root dir
    mov byte [volIdFlag], 0     ;We are done searching for volid
    pop rbp
    pop rdi
    pop rsi
    jnc .bad ;If CF=NC, then we have found a vollbl, fail.
    cmp al, errNoFil
    jne .bad ;If not "no file found", error out
.notVolLbl:
    test byte [fileExist], -1   ;-1 => File exists
    jz .createFile
    test byte [curDirCopy + fatDirEntry.attribute], dirCharDev ;Char dev?
    jnz .charDev    ;If its valid, just reopens it!
    test byte [curDirCopy + fatDirEntry.attribute], directoryFile | dirReadOnly
    jnz .bad    ;Cant recreate a dir or ro file!
    ;Here disk file exists, so recreating the file.
    push rbp
    push qword [currentSFT]
    call deleteMain ;Returns rsi pointing to the directory entry in a dsk buffer
    pop qword [currentSFT]
    pop rbp
    jc .bad
    ;al has the char for the filename
    ;Sets vars for the sector/offset into the sector
    mov rdi, qword [currentSFT]
    mov byte [rsi], al  ;Replace the first char of the filename back
    mov rax, qword [rbp + 10h]  ;Skip ptr to old rbp and return address
    ;al has file attributes.
    mov byte [rsi + fatDirEntry.attribute], al
    xor eax, eax
    ;Clear all the fields south of ntRes (20 bytes)
    mov qword [rsi + fatDirEntry.ntRes], rax
    mov qword [rsi + fatDirEntry.fstClusHi], rax
    mov dword [rsi + fatDirEntry.fileSize], eax
    mov eax, dword [rdi + sft.wTime]    ;Get the SFT time to set as crt and wrt
    mov dword [rsi + fatDirEntry.crtTime], eax
    mov dword [rsi + fatDirEntry.wrtTime], eax
    push rdi    ;Save SFT pointer
    lea rdi, curDirCopy ;Copy this directory entry internally
    mov ecx, fatDirEntry_size
    rep movsb
    call markBufferDirty ;We wrote to this buffer
    pop rdi
.createCommon:  ;rdi must point to the current SFT 
    ;Now populate the remaining SFT fields 
    lea rsi, curDirCopy
    mov al, byte [rsi + fatDirEntry.attribute]
    mov byte [rdi + sft.bFileAttrib], al
    mov rax, qword [tempSect]   ;Get directory entry sector
    mov qword [rdi + sft.qDirSect], rax
    movzx eax, word [entry]     ;Get 32 byte offset into sector for directory
    shr eax, 5   ;Divide by 32 to get directory entry number
    mov byte [rdi + sft.bNumDirEnt], al
    mov eax, dword [rsi + fatDirEntry.fileSize] ;Get the filesize
    mov dword [rdi + sft.dFileSize], eax
    movzx eax, word [rsi + fatDirEntry.fstClusLo]   ;Get first cluster
    movzx edx, word [rsi + fatDirEntry.fstClusHi]
    shl edx, 10h
    or eax, edx
    mov dword [rdi + sft.dStartClust], eax

    xor eax, eax
    ;Now set DeviceInfo to drive number and get the dpb for this disk file
    mov al, byte [workingDrv]
    or al, blokFileNoFlush  ;Dont flush until it is accessed
    mov word [rdi + sft.wDeviceInfo], ax    ;AH already 0
    mov rax, qword [workingDPB]
    mov qword [rdi + sft.qPtr], rax
    ;Last thing, copy the filename over
    ;Now save the name
    ;Copy from curDirCopy as we have a copy of the dir now
    lea rdi, qword [rdi + sft.sFileName]
    lea rsi, curDirCopy
    movsq   ;Copy over the space padded name to the sft
    movsw
    movsb
    ;SFT filled
    jmp .exit
.createFile:
    ;Create a dummy dir entry in the SDA to swap into the disk buffer
    ;rsi points to current sft entry
    lea rdi, curDirCopy
    ;Clear out the dir entry
    push rdi
    mov ecx, 4
    xor eax, eax
    rep stosq   ;Store 32 bytes of 0
    pop rdi
    ;Copy the FCB name over    
    push rsi
    push rdi
    mov ecx, 11
    lea rsi, fcbName
    rep movsb   ;Move over the FCB name
    pop rdi
    pop rsi

    mov rax, qword [rbp + 10h]  ;Skip ptr to old rbp and return address
    ;al has file attributes.
    mov byte [rdi + fatDirEntry.attribute], al
    mov eax, dword [rsi + sft.wTime]    ;Get the SFT time to set as crt and wrt
    mov dword [rdi + fatDirEntry.crtTime], eax
    mov dword [rdi + fatDirEntry.wrtTime], eax
    mov eax, dword [dirClustPar]    ;Get the parent directory information
.searchForDirSpace:
    mov dword [dirClustA], eax
    xor eax, eax    ;Reset the search to the start of the current directory
    mov word [dirSect], ax
    mov dword [dirEntry], eax
    push rdi
    call findFreeDiskDirEntry   ;rsi = ptr to a dir entry in a disk buffer
    pop rdi ;Preserve rdi = curDirCopy
    jnc .dirEntryFnd
    cmp dword [dirClustPar], 0  ;If the parent = 0 => Root Dir Fat12/16
    je .bad ;Set CF and exit
    call growDirectory  ;Increase directory size by 1 cluster
    jc .exit
    cmp eax, -1 ;Disk Full?
    je .bad
    ;Else eax = Newly allocated cluster
    jmp short .searchForDirSpace
.dirEntryFnd:
    xchg rdi, rsi
    mov ecx, 4
    rep movsq   ;Copy over the buffered directory
    call markBufferDirty ;We wrote to this buffer
    mov rdi, qword [currentSFT]
    jmp .createCommon
.open:
;curdircopy has a copy of the disk file directory
;Disk vars are set, compute sector and 32 byte entry numbers
    mov rdi, qword [currentSFT]
    mov rbp, qword [workingDPB] ;Need it for the following proc
    ;Now we can jump to common. qword [tempSect] and byte [entry] setup
    call getDiskDirectoryEntry  ;And setup vars! rsi points to disk buffer
    jmp .createCommon
.openProc:
    ;Here if Opening a file. 
    ;Dirs cannot be opened through open, only for renaming.
    ;This is taken care of by openMain.
    test byte [curDirCopy + fatDirEntry.attribute],dirCharDev
    jz .open
.charDev:
    mov rax, qword [curDirCopy + fatDirEntry.name]  ;Get the name
    call getCharDevDriverPtr    ;Get in rdi device header ptr
    jnc .notBadCharDevName
    mov eax, errAccDen
    jmp short .exit ;CF already set
.notBadCharDevName:
    mov rsi, qword [currentSFT]
    mov qword [rsi + sft.qPtr], rdi ;Store the Device Driver Header pointer
    movzx ebx, byte [rdi + drvHdr.attrib]   ;Get the attribute word low byte
    and bl, 01Fh    ;Clear bits 5 6 and 7
    or bl, devCharDev | charDevNoEOF ;Set charDev & noEOF on read
    mov word [rsi + sft.wDeviceInfo], bx    ;Store word save for inherit bit
    mov dword [rsi + sft.dFileSize], 0  ;No size
    mov qword [rsi + sft.sFileName], rax
    mov eax, "    "
    mov word [rsi + sft.sFileName + 8], ax
    mov byte [rsi + sft.sFileName + 10], al
.exit:
    call writeThroughBuffersForHandle
    jc .bad2
    pop rbp
    return
.bad:   ;Set Access Denied
    mov eax, errAccDen
.bad2:  ;Error propagating error code
    stc
    pop rbp
    return
closeMain: ;Int 2Fh AX=1201h
;Gets the directory entry for a file
;Input: qword [currentSFT] = SFT to operate on (for FCB ops, use the SDA SFT)
;If CF=CY on return: Error, return error with al = error code
;Preserve all regs except eax and rdi
; If CF=NC on return: eax = Unknown
;                     rdi = current SFT ptr
    mov rdi, qword [currentSFT] ;Get the sft pointer
    movzx ebx, word [rdi + sft.wDeviceInfo]
    test ebx, devRedirDev ;Is this a network drive?
    jz .physical
    ;Here we beep out the request to the network redirector (Int 2Fh AX=1106h)
    mov eax, 1106h  ;Make request
    int 2Fh ;Beep!
    return  ;Returns with CF set or clear as appropriate
.physical:  
; We make a request to the dev dir to close the device
; If the device is disk, we then update the directory entry for the disk file
    call dosCrit1Enter  ;Enter critical section 1
    call updateSFTDateTimeFields
    call decrementOpenCount ;rdi = current SFT, returns (e)ax = old handle count
    push rax
    push rbx
    call closeShareCallWrapper  ;The SFT count has been decremented
    pop rbx
    pop rax
flushFile:  ;Make this non-local to be jumped to by commit too!
;Updates the Dir entry with info from SFT and flushes.
;Closes the handle properly if only one reference to file remains.
;Input: ax = Initial open handle count
;       bx = attribute byte from the SFT
;       rdi -> Current SFT
    push rax    ;Save the handle count for later
    test bx, blokFileNoFlush | devCharDev
    jnz .notDiskBitsSet
    call getAndUpdateDirSectorForFile   ;rsi -> Buffer dir entry
    mov eax, errAccDen
    jc .accDenExit
    push rsi    ; -> Buffer dir entry
    push rdi    ; -> SFT ptr
    lea rdi, qword [rdi + sft.sFileName]    ;Ensure this is the right file
    call findInBuffer.nameCompare
    pop rdi     ; -> SFT ptr
    pop rsi     ; -> Buffer dir entry
    jz .dirEntryForUs
.badFileFound:
    mov eax, errFnf ;Dir entry has changed, and now file not found on medium
    stc
    jmp short .accDenExit
.dirEntryForUs:
    movzx ecx, byte [rsi + fatDirEntry.attribute] ;Get dir file attrib
    movzx eax, byte [rdi + sft.bFileAttrib]   ;Get SFT file attrib
    not al  ;Reverse the bits
    and al, cl  ;These should be equal
    and al, dirInclusive ;And nothing outside of these should be set
    jnz .badFileFound
    
    or byte [rsi + fatDirEntry.attribute], dirArchive   ;File changed!
    mov eax, dword [rdi + sft.dFileSize]    ;Get the file size
    mov dword [rsi + fatDirEntry.fileSize], eax ;And update field
    movzx eax, word [rdi + sft.wTime]   ;Get the last write time
    mov word [rsi + fatDirEntry.wrtTime], ax    ;And update field
    movzx eax, word [rdi + sft.wDate]   ;Get the last write time
    mov word [rsi + fatDirEntry.wrtDate], ax    ;And update field
    mov word [rsi + fatDirEntry.lastAccDat], ax ;Partialy implemented
    mov eax, dword [rdi + sft.dStartClust]  ;Always update the start cluster
    mov word [rsi + fatDirEntry + fatDirEntry.fstClusLo], ax
    shr eax, 10h
    mov word [rsi + fatDirEntry + fatDirEntry.fstClusHi], ax
    call markBufferDirty
    movzx eax, byte [workingDrv]
    call flushAllBuffersForDrive
    mov eax, errAccDen
    jc .accDenExit
.notDiskBitsSet:
    clc
.accDenExit:
    pushfq
    call closeSFT   ;Called with rdi -> Current SFT
    popfq
    pop rcx ;Get back the initial open handle count
    movzx ecx, cx   ;Force upper bits clear
    pushfq
    dec ecx ;Decrement count
    jnz .exit   ;If our initial count was not 1, skip resetting the count since
    mov word [rdi], cx ; decrementOpenCount didnt set it to -1
.exit:
    call dosCrit1Exit
    popfq
    return

readBytes:
;Reads the bytes into the user buffer for the setup SFT (currentSFT)
;Input: ecx = Number of bytes to read
;Output: ecx = Number of bytes read
;Number of bytes read 
;If CF=CY, return with error code in ax
    call getCurrentSFT  ;Get current SFT in rdi
    movzx eax, word [rdi + sft.wOpenMode]
    and al, 0Fh ;Eliminate except access mode
    cmp al, WriteAccess
    jne .readable
    mov eax, errAccDen
    xor ecx, ecx    ;Zero chars tfrred
    stc
    return
.readable:
    call setupVarsForTransfer   ;Setup initial stuff only!
    jecxz .exitOk  ;If ecx = 0 (number of bytes to transfer = 0), exit
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jz .notRedir
    mov eax, 1108h  ;Call Redir Read Bytes function
    int 2Fh ;Call redir (tfr buffer in DTA var, ecx has bytes to tfr)
    return 
.exitOk:
    clc
    return 
.notRedir:
    test word [rdi + sft.wDeviceInfo], devCharDev
    jnz readCharDev
    call dosCrit1Enter
    call readDiskFile   ;Called with rbp = Working DPB and rdi = CurrentSFT
    call dosCrit1Exit
    return 
readCharDev:
;rdi points to sft for char dev to read
;ecx has the number of bytes to transfer
;Vars have been set up and DTA has the transfer address
;Returns in ecx, the actual bytes transferred
;If CF=CY, return with error code in ax
    mov byte [errorLocus], eLocChr  ;Error is with a char device operation
    mov bx, word [rdi + sft.wDeviceInfo]    ;Get dev info
    mov rdi, qword [currentDTA] ;Get the DTA for this transfer in rdi
    test bl, charDevNoEOF   ;Does our device NOT generate EOF's on reads?
    jz charReadExitOk    ;If it does, jump to exit as if EOF has been hit
    test bl, charDevNulDev  ;Is our device the NUL device?
    jz .notNul
    ;If it is a new NUL device hdl, we can simply return!
    ;NUL never transfers bytes and now clears this bit to indicate EOF
    xor eax, eax    ;Set ZF so the next read causes EOF!
    jmp charReadExitOk    ;Goto exit
.notNul:
    test bl, charDevBinary
    jnz .binary
    ;Here if the device is in ASCII mode
    test bl, charDevConIn   ;Is this device STDIN?
    jz .generalASCII    ;If not, goto generalASCII, else fallthru
.consoleInput:
    ;Console input here
    call vConSwapDriver    ;Prepare CON Useage!
    ;Get current offset into buffer (if one exists)
    mov rsi, qword [vConHdlOff]
    test rsi, rsi   ;Any chars in the buffer?
    jnz .tfrBuf ;If so, we want to keep tfring those chars to user DTA
    cmp byte [vConInBuf], 80h ;Is this buffer full?
    je .oldBuf  ;If so, we set up the buffer function to allow editing of buffer
    ;Else, reset the buffer
    mov word [vConInBuf], 0FF80h ;Byte 0=>length of buf, byte 1 => chars in buf
.oldBuf:
;Preserve the dta and number of chars to tfr
    push rcx
    push rdi
    lea rdx, vConInBuf
    call buffCharInput_BE   ;Get con buffered input
    pop rdi
    pop rcx
    lea rsi, qword [vConInBuf + 2]  ;Get the address of the data area of buffer
    cmp byte [rsi], EOF
    jne .tfrBuf ;If not equal, start copying over the buffer to the user DTA
    mov byte [rdi], EOF ;Store EOF at start of user DTA
    mov al, LF
    call charOut_B.in   ;Echo CRLF
    xor esi, esi    ;Set ZF = ZE
    jmp short .exit
.tfrBuf:
    lodsb   ;Get the char across from rsi to rdi with a copy in al
    stosb
    cmp al, CR 
    jne .noCRLF
    mov byte [rsi], LF  ;Store an LF in source to go one more time around
.noCRLF:
    cmp al, LF  ;Compare if al is LF
    loopne .tfrBuf  ;Copy the LF over if so and exit and dec ecx one more time
    jne .exit   ;If the reason for exiting loop was ecx = 0, skip the following
    ;This only applies if the reason for exiting the loop is al=LF
    call charOut_B.in   ;Echo CRLF
    xor esi, esi
    or al, 1    ;Set ZF = NZ
.exit:
    call vConRetDriver
    mov qword [vConHdlOff], rsi ;Store the offset (or 0 value)
    ;Only return with ZF=ZE if first char in buffer was EOF
    jmp charReadExitOk    ;Exit ok! ecx has # chars tfred

.binary:
    ;Setup registers for transfer
    mov rbx, rdi    ;Transfer the buffer pointer into rbx
    xor rbp, rbp    ;Indicate Char device to the function
    ;ecx has the number of bytes to transfer directly
    call primReqReadSetup   ;Setup req hdr for read and get hdr addr in rbx 
    mov rsi, qword [workingDD]  ;Get the working device driver
    call goDriver   ;Make the request
    mov rdx, rdi    ;Save transfer buffer in rdx
    movzx edi, word [primReqHdr + ioReqPkt.status] ;Get status word in di
    test edi, drvErrStatus  ;Did an error occur?
    jz .binNoError
    ;ERROR HERE! Prepare for Int 24h (if SFT allows us to issue Int 24h)
    mov ah, critCharDev | critData ;Char device, data error signature
    call charDevErr   ;ah = has part of the error 
    ;al now has the response
    cmp al, critIgnore
    je .binNoError ;Simply proceed as normal
    mov rdi, rdx    ;Get back the buffer if it is a retry operation
    cmp al, critFail
    jne .binary ;If not fail, re-try the operation (ecx isn't touched)
    ;Fallthrough here for fail!
.failExit:
    mov rdi, qword [currentSFT]
    xor ecx, ecx
    mov eax, errAccDen
    stc ;Set carry flag to get caught as a error by caller
    return
.binNoError:
    ;Get number of bytes transferred into 
    mov eax, dword [primReqHdr + ioReqPkt.tfrlen]   ;Get bytes transferred
    neg eax ;make it into -eax
    lea ecx, dword [ecx + eax]  ;ecx has bytes to transfer, -eax has bytes trfrd
    ;ecx now has bytes left to transfer
    push rax    ;Save value on stack
    xor eax, eax ;Set ZF
    inc eax ;Clear ZF
    pop rax ;Get back the original value
    jmp charReadExitOk    ;GoExit with ecx=Bytes left to read. ZF=NZ always
.generalASCII:
    ;ecx has bytes to transfer here
    ;Setup registers for transfer
    mov rbx, rdi    ;Move the DTA address into rbx for readSetup
    push rcx
    mov ecx, 1  ;Get one char
    xor rbp, rbp    ;Indicate a char device
    call primReqReadSetup   ;Setup request
    pop rcx
    ;rbx now has request header ptr
    mov rsi, qword [workingDD]  ;Get device driver header ptr in rsi
.asciiReadChar:
    mov rdx, rdi    ;Save the current buffer pointer position in rdx
    call checkBreak ;Check we don't have a ^C pending on CON
    call goDriver   ;If no ^C found (which exits DOS) Make request!
    movzx edi, word [primReqHdr + ioReqPkt.status] ;Get status word in di
    test edi, drvErrStatus  ;Did an error occur?
    jz .asciiNoError
    mov ah, critCharDev | critData
    call charDevErr    ;Call Int 24h, ecx preserved
    ;Now setup number of bytes to transfer to 1 if the user requests retry
    mov dword [primReqHdr + ioReqPkt.tfrlen], 1
    mov rdi, rdx    ;Get the buffer position back into rdi
    cmp al, critFail
    je .failExit
    cmp al, critRetry
    je .asciiReadChar
    ;Ignore here, pretend NULL CHAR was read
    xor al, al
    jmp short .asciiIgnoreEP
.asciiNoError:
;Now process the char, add 1 to the transfer buffer (and rdi->BufferPtr)
; and dec 1 from ecx (tfrCntr is dealt with later)
;Preserve RBX, RSI
;Check EXACTLY 1 char was transferred. Any other value => exit from request
    mov rdi, rdx    ;Get the buffer position back into rdi
    cmp dword [primReqHdr + ioReqPkt.tfrlen], 1
    jne charReadExitOk    ;Exit request if more than 1 char was tranferred (ZF=NZ)
    mov al, byte [rdi]  ;Get byte just input from driver in al
.asciiIgnoreEP:
    inc qword [primReqHdr + ioReqPkt.bufptr]   ;Goto next char position
    inc rdi ;Also advance register pointer
    cmp al, EOF ;Was the char just read EOF?
    je charReadExitOk   ;Exit if so!
    cmp al, CR  ;Was this char CR?
    loopne .asciiReadChar   ;dec rcx, jnz .asciiReadChar
    ;Fallthrough also if al = CR (i.e ZF=ZE)
    inc al  ;make ZF=NZ
    jmp charReadExitOk    ;Called with ecx = Number of bytes LEFT to transfer

readDiskFile:
;rdi = Current SFT
;rbp = WorkingDPB
;ecx = Bytes to transfer
    mov byte [errorLocus], eLocDsk  ;Error is with a disk device operation
    mov byte [rwFlag], 0    ;Read operation
    ;We have the following vars setup:
    ;tfrLen, tfrCntr, qPtr, workingDPB, workingDrv, currByteF/S, currSectF/C, 
    ;currClustF
    ;Now convert currSectC to disk sector by using currClustF
    ;Using currClustF as a counter, we walk the fat from startingCluster
    mov edx, dword [rdi + sft.dFileSize]  ;Check that the file size isn't zero
    test edx, edx
    jz readExitOk  ;Return with zero bytes transferred
    mov edx, dword [currClustF] ;Use edx as the counter reg
    mov eax, dword [rdi + sft.dStartClust]  ;Get starting cluster
    mov dword [currClustD], eax
    test eax, eax   ;If starting cluster is zero, exit no bytes read
    jz readExitOk
    cmp eax, -1 ;If left in an indeterminate state somehow, exit no bytes
    jz readExitOk   
    mov ecx, dword [tfrLen] ;Get the tfrlen if we are past the end of the file
    ;Check if we have opened a volume label (should never happen)
    test byte [rdi + sft.bFileAttrib], volLabelFile    ;If we try read from vollbl
    jz .shareCheck
    mov eax, errAccDen
    stc
    return
.shareCheck:
;Entered with rdi -> SFT and ecx=number of bytes to read (to check if possible)
    call retryShareIODelay
    jnc .shareOk
    call shareCheckReadLockViolation
    jnc .shareCheck ;IF the user selected retry, lets try again
    return  ;Otherwise, return with the share error code in eax and CF=CY
.shareOk:
    call markBuffersAsUnreferencedWrapper
    xor ebx, ebx    ;Use ebx to contain the old cluster number
    test edx, edx   ;Is the relative sector zero? (I.E start of file?)
    jz .skipWalk
.goToCurrentCluster:
    call readFAT    ;Get in eax the next cluster
    jc .badExit   ;This can only return Fail
    cmp eax, -1 ;Are we gonna go past the end of the file?
    je readExitOk ;Exit with no bytes transferred
    mov dword [currClustD], eax    ;Save eax as current cluster
    dec edx ;Decrement counter
    jnz .goToCurrentCluster
    mov eax, dword [currClustD]    ;Get the current cluster in eax
.skipWalk:
    call getStartSectorOfCluster    ;Get the start sector on the disk in rax
    ;Now we add the offset to this
    movzx ebx, byte [currSectC] ;Get the sector offset into the cluster
    add rax, rbx    ;And finally get the absolute cluster on the disk
    mov qword [currSectD], rax  ;Save the current Sector on Disk in var
;Main
.mainRead:
    test byte [breakFlag], -1   ;If break flag is set, 
    jz .mainReadNoBreak
    push rax
    call checkBreak
    pop rax
.mainReadNoBreak:
    call getBufForData  ;Get bufHdr ptr in rbx and currBuff var for sector in rax
    jc .badExit
    lea rsi, qword [rbx + bufferHdr.dataarea]    ;Move buffer data ptr to rsi
    movzx ebx, word [currByteS] ;Get the byte offset into the current sector
    add rsi, rbx    ;Shift rsi by that amount into the sector
    ;Now we read the smallest of the following from the sector buffer:
    ; 1) Bytes left in sector size, 2) Bytes left in File, 
    ; 3) Bytes left to read from Request

    mov ecx, dword [rdi + sft.dFileSize]
    sub ecx, dword [currByteF]  ;Get bytes left to read in file in ecx
    mov ebx, dword [tfrCntr]
    cmp ecx, ebx    ;Is bytes left to read in file > bytes user has left?
    cmova ecx, ebx  ;Move ebx into ecx if so
    movzx ebx, word [rbp + dpb.wBytesPerSector]  ;Compare to sector size
    sub bx, word [currByteS]    ;Remove the number of bytes into the sector we are
    cmp ecx, ebx  ;ecx > sector size?
    cmova ecx, ebx  ;Move it into ecx if so
    
    push rdi
    mov rdi, qword [currentDTA]
    push rcx
    rep movsb
    pop rcx
    add dword [currByteF], ecx ;Move file pointer by ecx bytes
    sub dword [tfrCntr], ecx   ;Subtract from the number of bytes left
    mov qword [currentDTA], rdi ;rdi has been shifted by ecx on entry amount
    mov eax, dword [currByteF]  ;Get current byte in file
    movzx ebx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    xor edx, edx    ;Zero rdx
    div ebx ;Divide current byte in file by bytes per sector
    mov word [currByteS], dx ;CurrbyteS is a word!
    pop rdi
    mov ecx, dword [tfrCntr]   ;Get number of bytes left to transfer in ecx
    test ecx, ecx  ;Are we at the end yet?
    jz readExitOk ;Exit if so!
    call getNextSectorOfFile    ;Get the next sector of the file
    jc .badExit
    ;If ZF=ZE then currClustF has last cluster
    jz readExitOk ;ecx has the number of bytes left to transfer. ZF=ZE => EOF
    ;Else repeat
    ;currSectD has been updated, we now set currByteS = 0
    mov word [currByteS], 0 ;We start reading now from the start of the sector
    mov rax, qword [currSectD]  ;Get the next sector to read from
    jmp .mainRead
.badExit:
    ;When a disk error occurs within the bit where vars have changed,
    ; we need to update the SFT before returning
    mov ecx, dword [tfrCntr]    ;Get the bytes left to transfer
    xor al, al  ;Set ZF flag
    call readExitOk   ;We call this
    stc ;All calls which end up here return Fail!
    ret
charReadExitOk:
;Input: ecx = Number of bytes left to transfer!
;       ZF=ZE => Ensure we reach "EOF" on char device!
;       ZF=NZ => preserve bit 6
    jnz readExitOk
    call getCurrentSFT  ;Get currentSFT in rdi
    and byte [rdi + sft.wDeviceInfo], ~charDevNoEOF
readExitOk: ;Disk xfrs always go here. Binary char too but by bouncing!
;Input: ecx = Number of bytes left to transfer! 
    mov dword [tfrCntr], ecx    ;Update bytes left to transfer
    call updateCurrentSFT   ;Return with CF=NC and ecx=Bytes transferred
    return 

writeBytes:
;Writes the bytes from the user buffer
;Input: ecx = Bytes to xfr
;Returns number of bytes written in ecx if CF=NC
    call getCurrentSFT  ;Get current SFT in rdi
    movzx eax, word [rdi + sft.wOpenMode]
    and al, 0Fh ;Eliminate except access mode
    cmp al, ReadAccess
    jne .writeable
.noWrite:
    mov eax, errAccDen
    xor ecx, ecx
    stc
    ret
.writeable:
    test word [rdi + sft.wOpenMode], FCBopenedFile
    jz .skipAttribCheck ;FCB files don't check file attributes
    cmp byte [rdi + sft.bFileAttrib], readOnlyFile
    je .noWrite ;If the file is read only, RIP
.skipAttribCheck:
    call setupVarsForTransfer   ;Returns bytes to transfer in ecx
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jz .notRedir
    mov eax, 1109h  ;Write to redir
    int 2Fh
    return
.notRedir:
    test word [rdi + sft.wDeviceInfo], devCharDev
    jnz writeCharDev
    call dosCrit1Enter
    call writeDiskFile
    call dosCrit1Exit
    return
writeCharDev:
    mov byte [errorLocus], eLocChr
    ;We are adding bytes to this file so no EOF when reading from it
    or word [rdi + sft.wDeviceInfo], charDevNoEOF
    movzx ebx, word [rdi + sft.wDeviceInfo]
    ;If ecx = 0, we exit
    xor eax, eax    ;If ecx = 0, set eax = 0 to indicate 0 bytes tfrred
    test ecx, ecx
    jz writeExitChar
    mov al, bl  ;Move the flags over
    mov rbx, qword [currentDTA] ;Get ptr to storage buffer in rbx
    mov rdi, rbx
    xor edx, edx    ;Set edx to keep track of how many bytes have been xfrd
    test al, charDevBinary
    jz .asciiDev
;Write binary transfer here
.binaryLp:
    xor eax, eax
    xor rbp, rbp    ;Indicate a char device
    call primReqWriteSetup   ;Setup request, rbx points to buffer
    mov rsi, qword [currentSFT]
    call goDriverChar
    mov rdx, rdi    ;Save buffer ptr in rdx
    mov ah, critCharDev | critData | critWrite
    movzx edi, word [primReqHdr + ioReqPkt.status]  ;Get status word
    test edi, drvErrStatus
    jz .binXfrOk
    call charDevErr ;Invoke Int 24h
    mov rbx, rdx    ;Return the buffer ptr in rbx
    cmp al, critIgnore
    je .binXfrOk
    cmp al, critRetry
    je .binaryLp
    jmp .exitFail
.binXfrOk:
    mov eax, dword [primReqHdr + ioReqPkt.tfrlen]
    jmp writeExitChar   ;Exit oki with # bytes xfrd in eax
.asciiDev:
    test al, charDevConOut
    jnz .conDev
    test al, charDevNulDev
    jnz .nulDev
    ;Here we transfer for a generic character device in ascii mode
    mov eax, edx    ;Move bytes transferred into eax
    cmp byte [rbx], EOF ;Is the string pointer at a EOF character?
    je writeExitChar
    push rcx
    mov ecx, 1  ;xfr 1 byte
    xor rbp, rbp    ;Indicate a char device
    call primReqWriteSetup   ;Setup request, rbx points to buffer
    pop rcx
    mov rsi, qword [currentSFT]
    mov rsi, qword [rsi + sft.qPtr] ;Get the dev drv pointer in rsi
.asciiLp:
    call checkBreak
    call goDriver
    push rdi
    mov ah, critCharDev | critData | critWrite
    movzx edi, word [primReqHdr + ioReqPkt.status]  ;Get status word
    test edi, drvErrStatus
    jz .asciiNoError
    call charDevErr ;Invoke Int 24h
    pop rdi
    mov dword [primReqHdr + ioReqPkt.tfrlen], 1 ;Set tfrlen to 1 byte
    cmp al, critRetry
    je .asciiLp
    cmp al, critIgnore
    je .ignoreEp
    jmp .exitFail
.asciiNoError:
    pop rdi
    cmp dword [primReqHdr + ioReqPkt.tfrlen], 0
    je .bytesXfrdOk
.ignoreEp:
    inc edx ;One more char has been xfrd
    inc dword [primReqHdr + ioReqPkt.bufptr]    ;Increment buffer ptr
    inc rdi ;And our copy... 
    cmp byte [rdi], EOF ;... to do this!
    je .bytesXfrdOk
    mov word [primReqHdr + ioReqPkt.status], 0
    dec ecx
    jnz .asciiLp
.bytesXfrdOk:
    mov eax, edx
    jmp writeExitChar
.nulDev:
    mov eax, ecx    ;Move bytes to transfer into eax (as if it happened)
    jmp writeExitChar
.conDev:
    call vConSwapDriver
    mov rsi, rbx    ;Move the buffer ptr into rsi
    push rcx
.conDevLp:
    lodsb
    cmp al, EOF
    je .conDevExit
    call charOut_B.in   ;Use internal ep to tfr byte out to CON
    dec ecx
    jnz .conDevLp
.conDevExit:
    pop rax ;Get initial ecx back into eax
    sub eax, ecx
    call vConRetDriver
    jmp writeExitChar   ;Input: eax = bytes xfrd
.exitFail:
    mov eax, errAccDen
    stc
    return
writeDiskFile:
    ;rdi has SFT ptr
    mov ecx, dword [tfrLen] ;Get the transfer length 
    mov byte [errorLocus], eLocDsk 
    mov byte [rwFlag], 1    ;Write operation
    test word [rdi + sft.wOpenMode], 08h    ;Bit 3 is a reserved field
    jnz .badExit
    test ecx, ecx
    jnz .nonZeroWrite
    mov ecx, -1 ;If write cnt is 0 (i.e. truncating file), check for NO locks
.nonZeroWrite:
    ;Now do share check here
    call retryShareIODelay
    jnc .proceedWithWrite   ;No lock for rdi and ecx, all good!
    call shareCheckWriteLockViolation
    jnc .nonZeroWrite   ;If returned retry, retry the request
    return  ;Else return with CF=CY
.proceedWithWrite:
;Ensure that all buffers are now unreferenced
    call markBuffersAsUnreferencedWrapper
    xor ebx, ebx
    mov dword [bytesAppend], ebx    ;Used for file extends (not writes!)
    mov byte [fileGrowing], bl   ;Reset the file growth flag!
    mov eax, dword [rdi + sft.dStartClust]    ;Get start cluster
    ;If the start cluster is 0, we create a new cluster chain
    test eax, eax
    jnz .notStart
    call startNewChain  ;Allocate a first cluster! 
    ;jc .exitPrepHardErr
    retc
    cmp eax, -1
    je writeExit
    ;Now eax has the first cluster of chain
    mov dword [rdi + sft.dStartClust], eax  ;Store the start cluster in the sft
    mov byte [fileGrowing], -1  ;Set to true as this only occurs for new files!
.notStart:
;eax has the start cluster of the file
;Now we go to CurntOff
    mov dword [currClustD], eax ;Store in var
    xor ebx, ebx
    mov edx, dword [currClustF] ;Use edx as the counter reg
    test edx, edx
    jz .skipWalk
.goToCurrentCluster:
    call readFAT    ;Get in eax the next cluster
    ;jc .exitPrepHardErr
    retc   ;This can only return Fail
    cmp eax, -1 ;Is this cluster the last cluster?
    jne .stillInFile
.addCluster:
    ;Here we extend by one cluster
    mov eax, dword [currClustD] ;Get the disk cluster 
    mov ebx, eax    ;Setup last cluster value in ebx
    mov ecx, 1  ;Allocate one more cluster
    call allocateClusters   ;ebx has last cluster value
    ;jc .exitPrepHardErr
    retc
    mov eax, ebx    ;Walk this next cluster value to get new cluster value
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    add dword [bytesAppend], ecx    ;Add a bytes per sector to filesize
    mov byte [fileGrowing], -1
    call readFAT    ;Get in eax the new cluster
    ;jc .exitPrepHardErr
    retc
.stillInFile:
    mov dword [currClustD], eax    ;Save eax as current cluster
    dec edx ;Decrement counter
    jnz .goToCurrentCluster
;Now we fall out
    mov eax, dword [currClustD]
.skipWalk:
    call getStartSectorOfCluster    ;Get the start sector on the disk in rax
    ;Now we add the offset to this
    movzx ebx, byte [currSectC] ;Get the sector offset into the cluster
    add rax, rbx    ;And finally get the absolute cluster on the disk
    mov qword [currSectD], rax  ;Save the current Sector on Disk in var
    mov ecx, dword [tfrLen]
    test ecx, ecx   ;If this is not zero, goto write
    jnz .mainWrite  
;Here we have a zero byte write, so either truncate or have an extend
    test byte [fileGrowing], -1
    jnz .extend
;Here we truncate where needed
    mov eax, dword [currClustD] ;We must free the chain from currClustD
    call truncateFAT    ;Truncate from the current cluster 
.extend:
    mov eax, dword [rdi + sft.dCurntOff]
    mov dword [rdi + sft.dFileSize], eax    ;This is the new filesize now
    jmp .noByteExit ;Exit ok!
.mainWrite:
    test byte [breakFlag], -1   ;If break flag is set, 
    jz .mainWriteNoBreak
    push rax
    call checkBreak
    pop rax
.mainWriteNoBreak:
;Must intervene here for direct writes (if the handle specifies no buffering)
    call getBufForData  ;Get bufHdr ptr in rbx and currBuff var for sector in rax
    jc .badExit
    lea rdi, qword [rbx + bufferHdr.dataarea]    ;Move buffer data ptr to rdi
    movzx ebx, word [currByteS] ;Get the byte offset into the current sector
    add rdi, rbx    ;Shift rdi by that amount into the sector
    ;Now we read the smallest of the following from the sector buffer:
    ; 1) Sector size, 2) Bytes left to read from Request, 
    ; 3) Number of bytes left free in the sector
    xor eax, eax
    movzx ebx, word [rbp + dpb.wBytesPerSector]
    mov eax, ebx
    sub ax, word [currByteS]   ;Get # of bytes in sector we are in
    mov ecx, dword [tfrCntr]

    cmp ecx, ebx    ;If tfrCntr - wBytesPerSector < 0
    cmova ecx, ebx
    cmp ecx, eax    ;If small - #bytesleft < 0 
    cmova ecx, eax

    push rsi
    mov rsi, qword [currentDTA]
    push rcx
    rep movsb
    pop rcx
    call markBufferDirty   ;Preserves all CPU state, this buffer is now dirty

    add dword [currByteF], ecx ;Move file pointer by ecx bytes
    sub dword [tfrCntr], ecx   ;Subtract from the number of bytes left
    mov qword [currentDTA], rsi ;rsi has been shifted by ecx on entry amount
    pop rsi

    mov eax, dword [tfrLen] ;Get total length
    mov ecx, dword [tfrCntr]   ;Get number of bytes left to transfer in ecx
    test ecx, ecx  ;Are we at the end yet?
    jz writeExit
    call getNextSectorOfFile    ;If ZF=ZE, then @ last sector of last cluster
    ;jc .exitPrepHardErr
    retc
    cmp eax, -1
    jne .noExtend
    ;Here we need to extend by a cluster
    mov eax, dword [currClustD] ;Get the disk cluster 
    mov ebx, eax    ;Setup last cluster value in ebx
    mov ecx, 1  ;Allocate one more cluster
    call allocateClusters   ;ebx has last cluster value
    ;jc .exitPrepHardErr
    retc
    mov eax, ebx    ;Walk this next cluster value to get new cluster value
    movzx ebx, word [rbp + dpb.wBytesPerSector]
    add dword [bytesAppend], ebx    ;Add a bytes per sector to filesize
    mov byte [fileGrowing], -1
    call getNextSectorOfFile    ;Now we walk to chain to the new cluster
    ;jc .exitPrepHardErr
    retc
    cmp eax, -1
    je .noMoreClusters
.noExtend:
    mov word [currByteS], 0 ;We start reading now from the start of the sector
    mov rax, qword [currSectD]  ;Get the next sector to read from
    jmp .mainWrite
.noMoreClusters:
    push rsi
    mov rsi, qword [currentSFT]
    test word [rsi + sft.wOpenMode], diskFullFail
    pop rsi
    jz .noExtend    ;If no trigger Int 24h, return success
    ;Here we future proof for triggering Int 24h.
.badExit:
    mov eax, errAccDen
;.exitPrepHardErr:
    stc
    return
.noByteExit:
    mov eax, 2  ;Update last accessed fields of SFT
    call qword [updateDirShare] ;Remember, CF=CY by default so keep xor after
writeExit:
;Advances the bytes on the file pointer
;Return: ecx = Number of bytes transferred
    mov rdi, qword [currentSFT]
    call updateCurrentSFT
    test word [rdi + sft.wDeviceInfo], devCharDev   ;Char dev?
    jnz .exit   ;These just exit as no filesize!
    test ecx, ecx   ;If no bytes transferred, dont flush
    jz .noFlush
    and word [rdi + sft.wDeviceInfo], ~blokFileNoFlush ;File has been accessed
.noFlush:
    mov eax, dword [rdi + sft.dFileSize]
    cmp dword [rdi + sft.dCurntOff], eax
    jbe .exit   ;Don't change filesize unless offset is past the Filesize
    mov eax, dword [rdi + sft.dCurntOff]
    mov dword [rdi + sft.dFileSize], eax
.exit:
    mov eax, 1  ;Give it one last update of the data in the directory!
    call qword [updateDirShare] ;Remember, CF=CY by default!
    clc
    return
writeExitChar:
;Input: eax = Number of chars transferred
    mov ecx, dword [tfrLen]
    sub ecx, eax    ;Get chars left to xfr
    mov dword [tfrCntr], ecx
    jmp short writeExit
;-----------------------------------:
;        File Handle routines       :
;-----------------------------------:

setCurrentSFT:
;Set the pointer in rdi as current SFT 
    mov qword [currentSFT], rdi
    return 
getCurrentSFT:
;Get the current SFT pointer in rdi
    mov rdi, qword [currentSFT]
    return 
updateCurrentSFT:
;Updates the Current SFT fields before returning from a file handle operation
;Return: ecx = Actual bytes transferred and CF=NC
    push rdi
    mov rdi, qword [currentSFT]
    call getBytesTransferred
    jecxz .exit ;Skip this if ecx = 0
    ;ecx has bytes transferred
    test word [rdi + sft.wDeviceInfo], devCharDev   ;Char dev?
    jnz .exit
    push rax
    mov eax, dword [currClustD]
    mov dword [rdi + sft.dAbsClusr], eax
    mov eax, dword [currClustF]
    mov dword [rdi + sft.dRelClust], eax
    pop rax
    push rcx
    mov ecx, dword [currByteF]
    mov dword [rdi + sft.dCurntOff], ecx    ;Add to the current offset in file
    pop rcx
.exit:
    pop rdi
    clc
    return 

setupVarsForTransfer:
;Computes the actual bytes to be transferred and 
; sets up internal variables for the transfer. 
;Works for both reading and writing
;Input: ecx = User desired Bytes to transfer
;       rdi = SFT pointer for the file
;Output: ecx = Actual Bytes that will be transferred 
;Setup BOTH: tfrLen, tfrCntr, qPtr 
;      DISK: workingDPB, workingDrv, currByteF/S, currSectF/C, currClustF
;
;Note: Does not account for a FULL disk. When writing,
; if the disk will get full as a result of the write,
; stop at the last byte before the transfer. If the 
; file pointer is past the last free byte, write 0
    mov rsi, qword [rdi + sft.qPtr] ;Get qPtr in rsi
    mov qword [qPtr], rsi ;Save whatever pointer here (workingDD OR workingDPB)
    mov eax, dword [rdi + sft.dCurntOff]    ;Get current offset into file
    mov dword [currByteF], eax  ;Save Current byte in the file
    mov dword [tfrLen], ecx ;Save the number of bytes to transfer
    mov dword [tfrCntr], ecx    ;Save the bytes left to transfer
    test word [rdi + sft.wDeviceInfo], devRedirDev | devCharDev
    jz setupVarsForDiskTransfer
    clc
    return
setupVarsForDiskTransfer:
;Extension of the above, but for Disk files only
;Input: ecx = User desired Bytes to transfer
;       rdi = SFT pointer for the file
;Output: CF=NC: ecx = Actual Bytes that will be transferred, if it is possible
;        CF=CY: Error exit
    mov eax, dword [rdi + sft.dCurntOff] ;Update cur. offset if it was changed
    mov dword [currByteF], eax
    mov rbp, qword [rdi + sft.qPtr] ;Get DPB ptr in rbp
    ;DPB will get updated by reading the disk, no need to force it here
    mov qword [workingDPB], rbp
    mov bl, byte [rbp + dpb.bDriveNumber]
    mov byte [workingDrv], bl   ;Set working drive number
    mov eax, dword [currByteF]  ;Get current byte in file
    movzx ebx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    xor edx, edx    ;Zero rdx
    div ebx ;Divide current byte in file by bytes per sector
    ;eax has 0 based file relative sector (cannot grow beyond 03FFFFFh)
    ;edx has the offset into that sector
    mov dword [currSectF], eax
    mov word [currByteS], dx ;CurrbyteS is a word!
    mov edx, eax    ;Save file relative sector in edx
    and al, byte [rbp + dpb.bMaxSectorInCluster]   ;Works with max 64k clusters
    mov byte [currSectC], al    ;Save sector in cluster value in var
    mov eax, ecx    ;Save bytes to tfr in eax
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shr edx, cl ;Convert file relative sector to file relative cluster
    mov dword [currClustF], edx ;Save in var
    mov ecx, eax    ;Return the bytes to tfr in ecx
.exit:
    clc
    return 
findFreeSFT:
;Returns a pointer to a free SFT if CF=NC. Else, no free SFTs.
;Modifies an SFT entry. Must be called in a critical section.
;Output: CF=NC => rdi = Points to a free SFT entry, bx = SFTndx
;        CF=CY => eax = errNhl, error exit
    xor ebx, ebx
.mainLp:
    push rbx    ;Save the sft ndx
    call getSFTPtrfromSFTNdx    ;Get ptr to SFT in rdi
    pop rbx
    jnc .sftExists
    mov eax, errNhl
    stc
    return
.sftExists:
    cmp word [rdi + sft.wNumHandles], 0
    je .sftFound
    cmp word [rdi + sft.wNumHandles], -1    ;Is SFT being alloc'd/free'd?
    jne .gotoNextNdx
    ;Here, check that if this sft is owned by the caller and repurpose it.
    push rbx
    mov rbx, qword [serverPSP]
    cmp qword [rdi + sft.qPSPOwner], rbx
    jne .netGoToNextNdx
    movzx ebx, word [machineNum]
    cmp word [rdi + sft.wMachNum], bx
.netGoToNextNdx:
    pop rbx
    je .sftFound
.gotoNextNdx:
    inc ebx
    jmp short .mainLp
.sftFound:
    push rbx
    mov word [rdi + sft.wNumHandles], -1    ;Mark as repurposing!
    mov rbx, qword [serverPSP]
    mov qword [rdi + sft.qPSPOwner], rbx
    movzx ebx, word [machineNum]
    mov word [rdi + sft.wMachNum], bx
    pop rbx
    clc
    return
getSFTPtrfromSFTNdx:    ;Int 2Fh AX=1216h
;Return a pointer to the SFT entry in rdi
;Input: rbx = Valid SFT ndx number (byte, zero extended)
;Output: rdi = SFT pointer
    mov rdi, qword [sftHeadPtr] ;Get head of SFT pointer
.walk:
    cmp bx, word [rdi + sfth.wNumFiles]
    jb .thisTable
    sub bx, word [rdi + sfth.wNumFiles] ;Subtract
    mov rdi, qword [rdi + sfth.qNextSFTPtr] ;Goto next table
    cmp rdi, -1
    jne .walk
    stc
    return
.thisTable:
    push rax
    push rdx
    mov eax, sft_size
    mul ebx
    add rdi, rax    ;Shift rdi to go to SFT entry in current table
    pop rdx
    pop rax
    add rdi, sfth_size  ;Go past the header
    return
getJFTPtr:    ;Int 2Fh AX=1220h
;Return a zero extended value in rdi for the SFT entry
;Input: bx = JFT handle (we zero extend)
;Output: CF=NC => rdi = Points to first SFT ndx or -1 => free space
;        CF=CY => al = Error code, Fail
    movzx ebx, bx   ;Ensure we zero extended
    mov rdi, qword [currentPSP]
    cmp bx, word [rdi + psp.jftSize] ;jftSize is the size of the JFT array
    jb .ok
    mov al, errBadHdl
    stc
    return
.ok:
    cmp word [rdi + psp.jftSize], dfltJFTsize   ;Are we in PSP JFT or external?
    je .pspJftOk    ;If dfltJFTsize, its a good PSP JFT.
    jb .pspJftBelow ;If < dfltJFTsize, in PSP and needs to be corrected
    mov rdi, qword [rdi + psp.externalJFTPtr]   ;Get the ptr to the external JFT
    lea rdi, qword [rdi + rbx]  ;Get pointer into JFT
    jmp short .pspOkExit
.pspJftBelow:
    mov word [rdi + psp.jftSize], dfltJFTsize  ;Reset to dfltJFTsize if needed!
.pspJftOk:
    lea rdi, qword [rdi + psp.jobFileTbl + rbx] ;Use rbx as index in tbl
.pspOkExit:
    clc
    return
findFreeJFTEntry:
;Finds a free JFT entry in the currentPSP.
;Output: CF=NC => rdi => Ptr to JFT entry, bx = File Handle
;        CF=CY => al=errNhl
    xor ebx, ebx    ;Start searching from offset 0 in the JFT
.searchLp:
    call getJFTPtr
    jc .badExit
    cmp byte [rdi], -1
    rete
    inc ebx
    jmp short .searchLp
.badExit:
    mov al, errNhl
    stc
    return
getSFTPtr:
;This gets the SFT pointer and checks it was opened by this machine
;Input: bx = JFT handle
;Output: CF=NC: rdi = SFT pointer
;        CF=CY: Error, ax=Error code
    call derefSFTPtr
    retc    ;Return if carry
    push rax
    movzx eax, word [machineNum]
    cmp ax, word [rdi + sft.wMachNum]
    pop rax
    rete
    mov al, errBadHdl   ;Error code
    stc         ;Reset CF
    return

derefSFTPtr:
;Walk the whole way from a handle to SFT pointer (for the current process)
;Input: bx = File handle (gets zero extended)
;Output: CF=NC: rdi = SFT pointer
;        CF=CY: Error, ax=Error code
    call getJFTPtr    ;Get the ptr to the value in rdi
    jb .fail
    cmp byte [rdi], -1  ;Is this JFT entry unassigned?
    jne .ok
.fail:
    mov al, errBadHdl
    stc
    return
.ok:
    push rbx    ;Preserve the JFT handle
    movzx ebx, byte [rdi]  ;Get byte entry into rbx
    call getSFTPtrfromSFTNdx    ;Get SFT pointer in rdi
    pop rbx 
    return

getBytesTransferred:
    mov ecx, dword [tfrCntr]   ;Get bytes left to transfer
    neg ecx ;Multiply by -1
    add ecx, dword [tfrLen]     ;Add total bytes to transfer
    return ;Return bytes transferred in ecx

getSFTndxInheritable:
;Given a SFTndx this function will verify if it is inheritable
;Input: ebx = SFTndx (word)
;Output: 
;   ZF=ZE => Inheritable
;   ZF=NZ => Not Inheritable or bad ndxNumber
    push rdi
    call getSFTPtrfromSFTNdx    ;SFT pointer in rdi
    jc .badNdx
    test word [rdi + sft.wDeviceInfo], devNoInherit
    pop rdi
    return
.badNdx:
    xor edi, edi
    inc edi ;Clear the ZF flag if it was set
    clc
    pop rdi
    return

incrementOpenCount:
;Given a SFTndx, this function will increment it's open count
;Output:
;   CF=NC => sftndx ok, count incremented
;   CF=CY => Bad ndx
    push rdi
    call getSFTPtrfromSFTNdx
    jc .exit
    inc word [rdi + sft.wNumHandles]    ;Add one to open count
.exit:
    pop rdi
    return

decrementOpenCount: ;Int 2Fh AX = 1208h
;Input: rdi = SFT pointer
;Output: ax = Original wNumHandles count (zero extended to eax)
    pushfq
    movzx eax, word [rdi + sft.wNumHandles]
    dec eax     ;Decrement count
    jnz .exit                           ;If the count is not zero, exit
    dec eax    ;If it is zero, now we make it -1
.exit:
    popfq
    xchg ax, word [rdi + sft.wNumHandles] ;RBIL says ax returns og num hdls
    return

;Buffer Wrapper
writeThroughBuffersForHandle:
;Input: qword [currentSFT] = Current SFT pointer
    push rdi
    push rbp
    mov rdi, qword [currentSFT]
    test word [rdi + sft.wDeviceInfo], devRedirDev | devCharDev
    jnz .exit
    push qword [workingDPB]
    mov rbp, qword [rdi + sft.qPtr] ;Ensure the rigth DPB is in
    mov qword [workingDPB], rbp
    call flushAllBuffersForDPB
    pop qword [workingDPB]
.exit:
    pop rbp
    pop rdi
    return

setDPBfromSFT:
;Sets and updates the DPB from an SFT ptr
;Input: rdi -> sft
;Output: CF=NC: workingDPB set
;        CF=CY: Error fail, exit
    mov rbp, qword [rdi + sft.qPtr] ;Get the DPB ptr in rbp
    movzx eax, byte [rbp + dpb.bDriveNumber]
    mov byte [workingDrv], al
    call setWorkingDPB
    call ensureDiskValid
    return