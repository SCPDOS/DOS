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
    mov byte [searchAttr], dirInclusive ;Inclusive w/ directory
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
    mov eax, errPnf
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
    je .badPathspec ;Jmp to error if opening file that doesnt exist
    test byte [parDirExist], -1 ;If creating, check if parent path was found
    jnz .proceedCall    ;If so, proceed.
.badPathspec:
    pop rax
    mov eax, errFnf
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
    jmp extGoodExit ;Save ax and return OK
.exitBad:
    sti ;To prevent new net open/create reqs from crapping out a failed request
    pop rbx ;Pop the word from the stack
    mov word [currentNdx], -1
    jmp extErrExit ;Propagate the error code that is in ax
.exitBad2:
    ;Now we deallocate the SFT entry in the JFT and SFT block
    mov rsi, qword [curHdlPtr]
    mov byte [rsi], -1  ;Re-free the entry in the JFT
    mov rsi, qword [currentSFT]
    mov word [rsi], 0   ;Re-free the SFT 
    mov word [currentNdx], -1
    jmp extErrExit ;Propagate the error code that is in ax

closeFileHdl:      ;ah = 3Eh, handle function
;Input: bx = file handle to close
    call getSFTPtr  ;Get a pointer to the SFT in rdi
    jc extErrExit   ;If CF is set, al has error code, exit!
    call setCurrentSFT  ;Set this as the current SFT
    ;Check count to see if we need to check share mode
    xor eax, eax    ;Preset ax to 0
    cmp word [rdi], 1   ;Opened once only, not shared, no need for share check
    je .skipShareCheck
    ;Now check sharing mode
    movzx eax, word [rdi + sft.wOpenMode]  ;Get the share mode bits
    and al, 0F0h    ;And wipe out the other bits
.skipShareCheck:
    push rax    ;Save the share mode on stack
    call closeMain  ;Call close main!
    pop rax 
    jc extErrExit   ;If an error, exit through error exit
    cmp al, netFCBShare ;Do NetFCB check (only if file opened more than once)
    je .exitOk  ;If sharing mode was net FCB, it had no JFT entry, skip nulling
    call getJFTPtr  ;Get the pointer to the JFT entry in rdi
    mov byte [rdi], -1  ;Free JFT entry
.exitOk:
    xor eax, eax    ;Return value
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
    mov ebx, dirIncFiles    ;Inclusive w/o dirs
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
    mov eax, errPnf
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
;New pointer passed in edx! ecx will be DOCUMENTED as having to be 0
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
    call getUserRegs    ;Get user regs in rsi
    mov dword [rsi + callerFrame.rdx], edx
.seekExit:
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
    int 4Fh
    jnc .seekExit ;If the request returns with CF clear, there was no error
    jmp extErrExit


changeFileModeHdl: ;ah = 43h, handle function, CHMOD
    cmp al, 1
    jbe .subFuncOk
    mov eax, errInvFnc
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
    mov eax, errPnf
    jmp extErrExit
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
    jnz extErrExit

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
    int 4Fh
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
    int 4Fh
    pop rcx
    jc extErrExit
    jmp extGoodExit
.setDiskAttribs:
    call getDiskDirectoryEntry  ;Get ptr to entry in rsi
    jc extErrExit
    test cl, dirVolumeID | dirDirectory
    jz .set
    mov eax, errAccDen
    jmp extErrExit
.set:
    mov ch, byte [rsi + fatDirEntry.attribute]  ;Get attribs
    and ch, (dirVolumeID | dirDirectory)    ;Keep these two bits
    or cl, ch
    mov byte [rsi + fatDirEntry.attribute], cl  ;Set new bits
    xor eax, eax
    jmp extGoodExit


duplicateHandle:   ;ah = 45h, handle function
;Input: bx = Handle to duplicate
;Output: If ok then ax = New handle
    call findFreeJFTSpace    ;First find a free space in the JFT
    jc extErrExit   ;Exit if no space
    ;rsi points to the free space
.duplicateCommon:
    call getJFTPtr  ;Get a pointer to the JFT entry in rdi for bx
    xchg rsi, rdi
    lodsb   ;Move over the SFT ndx from the old to the new position
    stosb
    dec rsi
    dec rdi
    ;rdi now points to new position
    ;rsi points to old position
    ;al has SFT ndx
    mov rsi, rdi    ;Move rsi to point to the new position jft position
    movzx ebx, al   ;Move SFTndx into ebx
    call getSFTPtrfromSFTNdx    ;Get the pointer to the SFT in rdi
    inc word [rdi + sft.wNumHandles]    ;Increase the number of handles in SFT
    ;Now we must return in ax the entry in the JFT 
    mov rdi, qword [currentPSP]
    lea rdi, qword [rdi + psp.jobFileTbl]   ;Point to head of table
    sub rsi, rdi    ;Get the difference of the two in si
    mov eax, esi    ;Get the difference as the return code
    jmp extGoodExit

forceDuplicateHdl: ;ah = 46h, handle function
;Input: bx = Handle to duplicate
;       cx = Handle to close and replace with a duplicate of bx
    ;First we close cx
    xchg ebx, ecx ;Swap cx and bx
    push rbx
    push rcx
    call closeFileHdl   ;Close handle 
    pop rcx
    pop rbx
    retc    ;The error code is set by errExtExit and CF is set on callerFrame
    ;Else, close was ok, lets duplicate now
    call getJFTPtr  ;Get a pointer to bx in rdi, destination for copy
    jc extErrExit   ;Return bad with error code in al
    xchg ebx, ecx   ;Now get source to duplicate in ebx
    mov rsi, rdi    ;Put the free space ptr in rsi
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
    mov eax, errPnf
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
;Input: rsi -> Filespec to rename
;       rdi -> New filespec
;Wildcards are permissiable in the last path componant IFF server invoke!
    mov ebx, dirInclusive
    test byte [dosInvoke], -1
    cmovz ecx, ebx  ;If not server, store this value instead
    mov byte [searchAttr], cl
    ;Step 0, verify both paths provided are valid
    call .renamePathCheck   ;Preserves rsi and rdi, check rsi path
    jc .pnfError
    push rsi
    mov rsi, rdi    ;Now check rdi path
    call .renamePathCheck
    pop rsi
    jc .pnfError
    ;Now we canonicalise the filenames to make life easy
    push rsi
    mov rsi, rdi
    lea rdi, buffer2
    call canonicaliseFileName   ;Now canonicalise rdi path
    pop rsi
    jc .pnfError
    ;Now test if first file exists.
    push qword [fname1Ptr]  ;Move the pointer to its var position
    pop qword [fname2Ptr]
    lea rdi, buffer1
    call canonicaliseFileName ;rdi = Buffer to use, rsi = filename
    jc .pnfError  

    call renameMain ;Both pathnames made good and copied internally, lets go!!
    jc extErrExit
    jmp extGoodExit
.pnfError:
    mov eax, errPnf
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

getSetFileDateTime:;ah = 57h
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
    movzx ecx, word [rdi + sft.wTime]
    movzx edx, word [rdi + sft.wDate]
    call getUserRegs
    mov word [rsi + callerFrame.rcx], cx
    mov word [rsi + callerFrame.rdx], dx
    xor eax, eax
    jmp extGoodExit
.setTimeDate:
    ;Here we set the Time/Date
    mov word [rdi + sft.wTime], cx
    mov word [rdi + sft.wDate], dx
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
    rep scasb
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
    mov byte [searchAttr], dirIncFiles ;Inclusive w/o directory
    jmp openFileHdl.openCommon

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
    int 4Fh
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
    int 4Fh
    pop rbx
    jmp short .exitSelect
.lockShare:
    call qword [lockFileShare]  ;Call share hook
    jmp short .exitSelect
.badFunction:
    mov eax, errInvFnc
    mov word [errorExCde], ax
    jmp extErrExit
;STUB FUNCTIONS
setHandleCount:    ;ah = 67h
    jmp extErrExit

;-----------------------------------:
;       Main File IO Routines       :
;-----------------------------------:
commitMain:
;Commits the current SFT 
    call getCurrentSFT  ;Gets currentSFT into rdi
    movzx eax, word [rdi + sft.wDeviceInfo]
    test eax, devCharDev | blokFileNoFlush
    retnz   ;Return if nothing has been written or a char dev
    test ax, devRedirDev
    jnz .notNet
    ;Commit file net redir call and exit
    mov eax, 1107h
    int 4Fh
    return  ;Propagate CF and AL if needed due to error
.notNet:
    call dosCrit1Enter
    mov rbp, qword [rdi +sft.qPtr]  ;Get DPB pointer in rbp
    call setWorkingDPB
    call updateDirectoryEntryForFile    ;Update the directory entry
    jc short .exit    ;Return in error if this fails, exit critical
    call flushFile  ;Now the file gets flushed
.exit:
;Propagate CF and AL if needed due to error
    call dosCrit1Exit
    return
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
; workingCDS -> CDS for drive we are considering (set by )
    mov rdi, qword [workingCDS]
    call testCDSNet ;CF=NC => Not net
    jnc .notNet
    mov eax, 1111h
    int 4Fh
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
;Now we find first the source file
    mov rsi, qword [fname1Ptr]
    mov rdi, rsi
    call getFilePath    ;Now hit the disk to search for the file
    jc .exit2    ;Return with CF=CY if file doesn't exist
    lea rsi, curDirCopy
    lea rdi, renameDir
    mov ecx, fatDirEntry_size/8
    rep movsq   ;Copy directory over
    lea rdi, renameFFBlk
    call setupFFBlock   ;Need this to save the dir entry cluster/sector/offset 
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
    ;Now we use the destination filename pattern to build the new filename
    mov rdi, qword [fname2Ptr]  ;Get the destination path ptr in rdi
    xor eax, eax
    mov ecx, 67
    repne scasb   ;Find the null terminator of the destination path
.findPattern:
    dec rdi
    cmp byte [rdi], "\" ;Is this a pathsep?
    jne .findPattern
;rdi points to the pathseparator
    stosb   ;Store a null over the "\" and inc rdi to char one of pattern
    push rdi    ;Push the new name pattern portion ptr in var
    ;Now check if the parent directory exists for the destination
    push rdi    ;Save the ptr to the first char of the pathsep
    push qword [fname1Ptr]  ;Preserve original source buffer
    lea rsi, buffer2
    mov rdi, rsi
    call getDirPath ;We are searching for a directory ONLY
    pop qword [fname1Ptr]
    pop rdi
    jc .badExit    ;Error if the parent dir doesnt exist
    mov byte [rdi - 1], "\" ;Replace the pathseparator
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
    call .makeNewName   ;Make new fname in fcbName from asciizname in renFFblk
    lea rsi, fcbName
    lea rdi, renameDir
    mov ecx, 11
    rep movsb   ;Move the name over from fcbName to new dir entry name field
    pop rdi ;Get the first char of the filename place in destination pathspec
    push rdi    ;Push it on stack again to be reused later if a WC spec
    mov rsi, fcbName
    call FCBToAsciiz    ;Copy the name over to asciiz 
    mov rsi, qword [fname2Ptr]
    mov rdi, rsi
    call getFilePath    ;This must be a non-existant file
    jnc .badExit   ;If the file exists, then error
    cmp eax, errFnf ;If Fnf error then we may proceed
    jne .badExit
    ;Now we search the parent dir (the curDirCopy dir) for free space
    movzx edx, word [curDirCopy + fatDirEntry.fstClusHi]
    movzx eax, word [curDirCopy + fatDirEntry.fstClusLo]
    shl edx, 10h
    or eax, edx ;Get first cluster of dir file in eax
    call .searchForDirSpace
    jc .bad
;dir Entry found, rsi points to space in buffer
    mov rdi, rsi
    lea rsi, renameDir
    mov ecx, fatDirEntry_size/8
    rep movsq   ;Copy dir over
    call markBufferDirty ;Mark buffer as written to now
;Now we delete the original directory entry
    mov eax, dword [renameFFBlk + ffBlock.dirOffset]
    shl eax, 5  ;Turn into byte offset
    mov rbp, qword [workingDPB]
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    div ecx ;Turn byte in clust offset into sec in clust offset and byte offset
    mov word [dirSect], ax
    shr edx, 5  ;Turn into 32 byte offset
    mov dword [dirEntry], edx
    mov eax, dword [renameFFBlk + ffBlock.parDirClus]
    call getStartSectorOfCluster    ;Cluster number in eax, sector in rax
    movzx edx, word [dirSect]
    add rax, rdx
    ;rax now has the sector number to read in
    call getBufForDirNoFile ;Get buffer pointer in rbx
    jc .bad
    mov edx, dword [dirEntry]   ;Get the dir entry
    shl edx, 5  ;Get as byte offset
    lea rsi, qword [rbx + bufferHdr.dataarea + rdx] ;rsi points to old dir
    movzx eax, byte [delChar]
    mov byte [rsi], al  ;Store the del char there
    call markBufferDirty ;Mark buffer as written to now
    ;Now we check if source filename or wcdFcbname has a wildcard
    ;If it does, we find next. If not, we exit
    mov al, "?"
    mov rdi, qword [fname1Ptr]  ;Check filename
    call strlen ;Get in rcx to get the length of the string
    repne scasb
    jne .exit  ;If source name has no wildcards, exit
    mov ecx, 11
    lea edi, wcdFcbName
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
    call writeThroughBuffers
    jc .badExit
.exit2: ;Bad exit before we push qword on stack 
    call dosCrit1Exit
    return
.bad:
    mov eax, errAccDen  ;Temp return code
.badExit:
    call cancelWriteThroughBuffers
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

.makeNewName:
;Copy old filename as initial pattern into fcbName
;Then copies wcfcb letter by letter unless a ? is encountered
    push rcx
    push rsi
    push rdi
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
    pop rdi
    pop rsi
    pop rcx
    return  ;Return new filename in fcbName

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
    push rdi
    call buildSFTEntry
    pop rdi
    jc .errorMain
    mov word [rdi + sft.wNumHandles], 1   ;One "reference"
    mov word [rdi + sft.wOpenMode], denyRWShare ;Prevent everything temporarily
    push rdi
    call shareFile
    pop rdi
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
    int 4Fh
    return
.notNet:
    mov eax, errAccDen  
    test byte [curDirCopy + fatDirEntry.attribute], dirCharDev
    jnz .exitBad  ;Can't delete a char dev
    test byte [curDirCopy + fatDirEntry.attribute], dirReadOnly
    jnz .exitBad  ;Can't delete a read only file
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
    push rdx
    movzx edx, word [curDirCopy + fatDirEntry.fstClusLo]
    movzx eax, word [curDirCopy + fatDirEntry.fstClusHi]
    shl eax, 10h
    or eax, edx ;Add low bits to eax
    pop rdx
    test eax, eax   ;Cluster value of 0 means no allocation
    jz .skipUnlink  ;If there is no FAT allocation for file, skip "dealloc"
    mov rbp, qword [workingDPB] ;Get the working DPB for the disk of this file
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
    call writeThroughBuffers
    retnc
.exitBad:
    call cancelWriteThroughBuffers
    return

openMain:
;Input: ax (formally al) = Open mode
;       [currentSFT] = SFT we are building
;       [workingCDS] = CDS of drive to access
;       [workingDPB] = DPB of drive to access
;Ouput: CF=CY => Error, eax has error code
    call .setOpenMode
    retc    ;Error Exit 
    mov rdi, qword [currentSFT]
    mov rsi, qword [workingCDS]
    xor ah, ah  ;al has the access mode
    mov word [rdi + sft.wOpenMode], ax  ;Set the SFT access mode
    cmp rsi, -1
    jne .notNet
.redirOpen:
    push rax    ;Push open mode onto stack
    mov eax, 1116h  ;Open remote file on \\ pathspec drives
    int 4Fh
    pop rax
    return
.notNet:
    test word [rsi + cds.wFlags], cdsRedirDrive
    jnz .redirOpen  ;If redir drive, go via the redir interface
    call dosCrit1Enter
    mov byte [openCreate], 0   ;Opening file, set to 0
    mov byte [delChar], 0E5h
    call buildSFTEntry  ;ax must have the open mode
    jc .errorExit
.openShareLoop:
;Now we attempt to register the file with SHARE
    movzx ecx, word [shareCount]    
.openShareTryAgain: 
    push rcx
    call openShareCallWrapper
    pop rcx
    jnc .fileSharedOk
    call shareRetryCountdown
    dec ecx
    jnz .openShareTryAgain
    mov rdi, qword [currentSFT]
    call shareCheckOpenViolation
    jnc .openShareLoop  ;If user selects retry, we retry!
.errorExit:
    call dosCrit1Exit   ;Else we error out
    return
.fileSharedOk:
    mov eax, 3  ;Update date/time and everything in the share dir sync call
    call getCurrentSFT  ;Get SFT ptr in rdi
    call qword [updateDirShare] ;Now call the dir sync, this default sets CF 
    call dosCrit1Exit
    test word [rdi + sft.wOpenMode], FCBopenedFile
    retz
    stc ;FCB opened files are not allowed anymore, this shouldnt exist anymore
    return
.setOpenMode:
;Input: al = Open mode for the file open
    mov byte [fileOpenMd], al
    push rbx
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
    test al, 80h | 40h   ;Invalid bits?
    jnz .invalidAttrib
    test al, dirVolumeID
    jnz .invalidAttrib  ;Creating volume label with this function is forbidden
    or al, dirArchive   ;Set archive bit
    test al, dirDirectory   
    jz .validAttr   ;Creating directory with this function is forbidden also
.invalidAttrib:
    mov eax, errAccDen
    stc
    return
.validAttr:
    mov rdi, qword [currentSFT]
    mov rsi, qword [workingCDS]
    cmp rsi, -1
    jne .diskFile
    push rax    ;Save the new attributes
    mov eax, 1118h  ;Create file w/o CDS
    int 4Fh
    pop rbx
    return
.diskFile:
    test word [rsi + cds.wFlags], cdsRedirDrive ;We a redir drv?
    jz .hardFile
    push rax    ;Save the new attributes
    mov eax, 1117h  ;Create file with CDS
    int 4Fh
    pop rbx
    return
.hardFile:
    or word [rdi + sft.wOpenMode], RWAccess ;Set R/W access when creating file
    mov byte [openCreate], -1   ;Creating file, set to FFh
    mov byte [delChar], 0E5h
    call dosCrit1Enter  ;Writing the SFT entry, must be in critical section
    push rdi
    push rax    ;Save the file attributes on stack
    mov eax, RWAccess | CompatShare ;Set open mode
    call buildSFTEntry
    pop rbx ;Pop the word off (though it has been used already!)
    pop rdi
    jc .errorExit
    mov eax, 2
    call qword [updateDirShare]
    clc ;Always clear the CF flag here updateDir defaults to CF=CY
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
    test byte [curDirCopy + fatDirEntry.attribute], dirCharDev ;Char dev?
    jnz .charDev
    test byte [fileExist], -1   ;-1 => File exists
    jz .createFile
    ;Here disk file exists, so recreating the file.
    ;If recreating, check we are not overwriting a Dir
    test byte [curDirCopy + fatDirEntry.attribute], dirDirectory
    jnz .bad    ;Directories are not allowed to be created
    call deleteMain ;Returns rsi pointing to the directory entry in a dsk buffer
    ;al has the char for the filename
    ;Sets vars for the sector/offset into the sector
    mov rdi, qword [currentSFT]
    mov byte [rsi], al  ;Replace the first char of the filename back
    mov rax, qword [rbp + 10h]  ;Skip ptr to old rbp and return address
    ;al has file attributes.
    and al, dirArchive | dirIncFiles | dirReadOnly ;Permissable bits only
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
    shr al, 5   ;Divide by 5 to get directory entry number
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
    ;SFT filled, now we open on the driver if it supports it
    jmp .openDriver
.createFile:
    ;Create a dummy dir entry in the SDA to swap into the disk buffer
    ;rsi points to current sda entry
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
    and al, dirArchive | dirIncFiles | dirReadOnly ;Permissable bits only
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
    test byte [curDirCopy + fatDirEntry.attribute], 40h ;Was this a char dev?
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
.openDriver:
    mov rdi, qword [currentSFT]
    mov rsi, qword [rdi + sft.qPtr] ;Get the ptr here
    test word [rdi + sft.wDeviceInfo], devCharDev
    jnz .charDevOpen
    movzx eax, byte [rsi + dpb.bUnitNumber]    ;Get the unit number in cl
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Get driver ptr
.charDevOpen:
    test word [rsi + drvHdr.attrib], devDrvHdlCTL   ;Support Close?
    jz .exit  ;If not, immediately jump to exit, all is well
    ;rsi has device driver ptr for device, make request
    push rbx
    call primReqOpenSetup  ;rbx gets header ptr, rsi has driver ptr
    call goDriver   ;Make request
    pop rbx
.exit:
    call writeThroughBuffers
    jc .bad2
    pop rbp
    return
.bad:   ;Set Access Denied
    mov eax, errAccDen
.bad2:  ;Error propagating error code
    call cancelWriteThroughBuffers
    stc
    pop rbp
    return
closeMain: ;Int 4Fh AX=1201h
;Gets the directory entry for a file
;Input: qword [currentSFT] = SFT to operate on (for FCB ops, use the SDA SFT)
;If CF=CY on return: Error, return error with al = error code
;Preserve all regs except eax and rdi
; If CF=NC on return: eax = Unknown
;                     rdi = current SFT ptr
    mov rdi, qword [currentSFT] ;Get the sft pointer
    test word [rdi + sft.wDeviceInfo], devRedirDev ;Is this a network drive?
    jz .physical
    ;Here we beep out the request to the network redirector (Int 4Fh AX=1106h)
    mov eax, 1106h  ;Make request
    int 4Fh ;Beep!
    return  ;Returns with CF set or clear as appropriate
.physical:  
; We make a request to the dev dir to close the device
; If the device is disk, we then update the directory entry for the disk file
    call dosCrit1Enter  ;Enter critical section 1
    push rbx
    push rsi
    mov rsi, qword [rdi + sft.qPtr] ;Get driver or DPB ptr in rsi
    test word [rdi + sft.wDeviceInfo], devCharDev
    jnz .charClose   ;Char devs aren't affected by directory work
    ;rsi has DPB pointer here
    ;rdi has the SFT pointer
    push rbp
    mov rbp, rsi ;Move the dpb pointer into rbp
    call setWorkingDPB  ;Set the working dpb to rbp
    call updateDirectoryEntryForFile
    pop rbp
    ;If CF is set, Fail was requested and ax has an error code
    jc .exit
    call flushFile
    jc .exit    ;If something went wrong, exit
    movzx ecx, byte [rsi + dpb.bUnitNumber]    ;Get the unit number in cl
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Get driver ptr
.charClose:
    ;Now rsi = Device Driver Header and rdi = Current SFT header
    ;We now decrement handle count in SFT structure
    call decrementOpenCount ;rdi = current SFT, returns ax = old handle count
    dec ax  ;If this is zero, then we need to set wNumHandles to zero
    jnz .driverClose
    inc word [rdi + sft.wNumHandles]    ;Now make it zero again as it is -1
.driverClose:
    push rax
    push rcx
    call closeShareCallWrapper  ;The SFT count has been decremented
    pop rcx
    pop rax
    xchg ecx, eax ;Now store this because DOS returns in cx (according to RBIL)
    ;and if the device is a disk device, cl will have the unit number
    ;We first check if the driver supports oper/close requests
    test word [rsi + drvHdr.attrib], devDrvHdlCTL   ;Support Close?
    jnz .exit  ;If not, immediately jump to exit, all is well
    ;rsi has device driver ptr for device, make request
    call primReqCloseSetup  ;rbx gets header ptr, rsi has driver ptr
    call goDriver   ;Make request
    ;Don't check the status here, as we are simply informing the driver 
    ; of an operation. Nothing should be able to go wrong. 
    ;Functionally, an ignore if anything does go wrong.
    call writeThroughBuffers
    jnc short .exitOk
.exit:
    call cancelWriteThroughBuffers
.exitOk:
    pop rsi
    pop rbx
    call dosCrit1Exit
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
    int 4Fh ;Call redir (tfr buffer in DTA var, ecx has bytes to tfr)
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
    jz readExitOk    ;If it does, jump to exit as if EOF has been hit
    test bl, charDevNulDev  ;Is our device the NUL device?
    jz .notNul
    ;If it is the NUL device, we can simply return unsucessfully!
    ;NUL never transfers bytes 
    xor eax, eax    ;Set ZF so the next read causes EOF!
    jmp readExitOk    ;Goto exit
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
    jmp readExitOk    ;Exit ok! ecx has # chars tfred and ZF=ZE if @ EOF

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
    ;ERROR HERE! Prepare for Int 44h (if SFT allows us to issue Int 44h)
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
    jmp readExitOk    ;GoExit with ecx=Bytes left to read
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
    call charDevErr    ;Call Int 44h
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
    jne readExitOk    ;Exit request if more than 1 char was tranferred (ZF=NZ)
    mov al, byte [rdi]  ;Get byte just input from driver in al
.asciiIgnoreEP:
    inc qword [primReqHdr + ioReqPkt.bufptr]   ;Goto next char position
    inc rdi ;Also advance register pointer
    cmp al, EOF ;Was this char EOF?
    je readExitOk
    cmp al, CR  ;Was this char CR?
    loopne .asciiReadChar   ;dec rcx, jnz .asciiReadChar
    ;Fallthrough also if al = CR (i.e ZF=ZE)
    inc al  ;make ZF=NZ
    jmp readExitOk    ;Called with ecx = Number of bytes LEFT to transfer

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
    mov ecx, dword [tfrLen] ;Get the tfrlen if we are past the end of the file
    ;Check if we have opened a volume label (should never happen)
    test word [rdi + sft.wOpenMode], volumeLabel    ;If we try read from vollbl
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
    xor ebx, ebx    ;Use ebx to contain the old cluster number
    test edx, edx   ;Is the relative sector zero? (I.E start of file?)
    jz .skipWalk
.goToCurrentCluster:
    cmp eax, -1 ;Are we gonna go past the end of the file?
    je readExitOk ;Exit with no bytes transferred
    mov ebx, eax    ;Save eax as current cluster
    call readFAT    ;Get in eax the next cluster
    jc .badExit   ;This can only return Fail
    dec edx ;Decrement counter
    jnz .goToCurrentCluster
;Now we fall out with ebx = Current cluster
    mov eax, ebx    ;Get the current cluster in eax
.skipWalk:
    call getStartSectorOfCluster    ;Get the start sector on the disk in rax
    ;Now we add the offset to this
    movzx ebx, byte [currSectC] ;Get the sector offset into the cluster
    add rax, rbx    ;And finally get the absolute cluster on the disk
    mov qword [currSectD], rax  ;Save the current Sector on Disk in var
;Main
.mainRead:
    call getBufForData  ;Get bufHdr ptr in rbx and currBuf var for sector in rax
    jc .badExit
    lea rsi, qword [rbx + bufferHdr.dataarea]    ;Move buffer data ptr to rsi
    movzx ebx, word [currByteS] ;Get the byte offset into the current sector
    add rsi, rbx    ;Shift rsi by that amount into the sector
    ;Now we read the smallest of the following from the sector buffer:
    ; 1) Sector size, 2) Bytes left in File, 3) Bytes left to read from Request
    mov ecx, dword [rdi + sft.dFileSize]
    sub ecx, dword [rdi + sft.dCurntOff] ;Get bytes left to read in file in ecx
    mov ebx, dword [tfrCntr]
    cmp ecx, ebx    ;Is bytes left to read in file > bytes user has left?
    cmova ecx, ebx  ;Move ebx into ecx if so
    movzx ebx, word [rbp + dpb.wBytesPerSector]  ;Compare to sector size
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
    pop rdi
    mov ecx, dword [tfrCntr]   ;Get number of bytes left to transfer in ecx
    test ecx, ecx  ;Are we at the end yet?
    jz readExitOk ;Exit if so!
    call getNextSectorOfFile    ;Get the next sector of the file
    jc .badExit
    ;If ZF=ZE then CurrClustF has last cluster
    jz readExitOk ;ecx has the number of bytes left to transfer. ZF=ZE => EOF
    ;Else repeat
    ;currSectD has been updated, we now set currByteS = 0
    mov word [currByteS], 0 ;We start reading now from the start of the sector
    mov rax, qword [currSectD]  ;Get the next sector to read from
    jmp short .mainRead
.badExit:
    ;When a disk error occurs within the bit where vars have changed,
    ; we need to update the SFT before returning
    mov ecx, dword [tfrCntr]    ;Get the bytes left to transfer
    xor al, al  ;Set ZF flag
    call readExitOk   ;We call this
    stc ;All calls which end up here return Fail!
    ret

readExitOk:
;Input: ecx = Number of bytes left to transfer!
;       ZF=ZE => clear bit 6 of deviceInfo Word ZF=NZ => preserve bit 6
    mov dword [tfrCntr], ecx    ;Update bytes left to transfer
    ;I argue as this is a simply read-only exit vector, this is unnecessary
    ;jnz .skipbitClear
    ;call getCurrentSFT  ;Get currentSFT in rdi
    ;The disk transfer must've flushed by now. 
    ;and byte [rdi + sft.wDeviceInfo], ~blokFileNoFlush ;File has been accessed
.skipbitClear:  ;Or skip that entirely
    call updateCurrentSFT   ;Return with CF=NC and ecx=Bytes transferred
    return 

writeBytes:
;Writes the bytes from the user buffer
;Input: ecx = Bytes to xfr
;Returns number of bytes written in ecx
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
    int 4Fh
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
    jz writeExit
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
    call charDevErr ;Invoke Int 44h
    mov rbx, rdx    ;Return the buffer ptr in rbx
    cmp al, critIgnore
    je .binXfrOk
    cmp al, critRetry
    je .binaryLp
    jmp .exitFail
.binXfrOk:
    mov eax, dword [primReqHdr + ioReqPkt.tfrlen]
    jmp writeExit   ;Exit oki with # bytes xfrd in eax
.asciiDev:
    test al, charDevConOut
    jnz .conDev
    test al, charDevNulDev
    jnz .nulDev
    ;Here we transfer for a generic character device in ascii mode
    mov eax, edx    ;Move bytes transferred into eax
    cmp byte [rbx], EOF ;Is the string pointer at a EOF character?
    je writeExit
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
    call charDevErr ;Invoke Int 44h
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
    jmp writeExit
.nulDev:
    mov eax, ecx    ;Move bytes to transfer into eax (as if it happened)
    jmp writeExit
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
    jmp writeExit   ;Input: eax = bytes xfrd
.exitFail:
    mov eax, errAccDen
    stc
    return
writeDiskFile:
    ;rdi has SFT ptr
    mov ecx, dword [tfrLen] ;Get the transfer length 
    mov byte [errorLocus], eLocDsk 
    mov byte [rwFlag], -1    ;Write operation
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
    xor ebx, ebx
    mov dword [bytesAppend], ebx ;Reset the appending counter
    mov eax, dword [rdi + sft.dStartClust]    ;Get start cluster
    ;If the start cluster is 0, we create a new cluster chain
    test eax, eax
    jnz .notStart
    call startNewChain  ;Allocate a first cluster! 
    jc .exitPrepHardErr
    cmp eax, -1
    je .exitPrep
    ;Now eax has the first cluster of chain
    mov dword [rdi + sft.dStartClust], eax  ;Store the start cluster in the sft
    mov byte [fileGrowing], -1  ;Set to true as this only occurs for new files!
.notStart:
    call getLastClusterInChain  ;to get the current last cluster in the file
    mov dword [lastClustA], eax
    call getNumberOfClustersInChain ;Gets number of clusters
    dec eax ;Turn into an offset of clusters in file
    mov dword [lastClust], eax
    ;Get the disk cluster of the file (currClustD)
    mov ecx, dword [currClustF]
    mov eax, dword [rdi + sft.dStartClust]
    call getClusterInChain  ;Returns in eax the disk cluster value
    jc .badExit
    ;ecx has the number of clusters we need to extend the allocation by.
    jecxz .skipExtension
    call findFreeClusterData    ;This updates the dpb to have free cluster data
    jc .badExit
    cmp eax, -1 ;No more free clusters
    je .noByteExit
    cmp dword [rbp + dpb.dNumberOfFreeClusters], ecx
    jb .noByteExit    ;If dNumberOfFreeClusters < ecx, exit
    ;Enough to extend by ecx amount
    mov ebx, dword [lastClustA] ;Get the last disk cluster of the file
    call allocateClusters   ;Extend by ecx clusters
    jc .badExit
    add dword [lastClust], ecx  
    mov eax, dword [rdi + sft.dStartClust]
    mov ecx, dword [lastClust]
    call getClusterInChain
    jc .badExit
    ;eax has the last cluster on disk
    mov dword [lastClustA], eax
    ;Now we must extend the filesize in the SFT
    mov eax, dword [rdi + sft.dCurntOff]
    mov dword [rdi + sft.dFileSize], eax    ;This is the new filesize now
    xor eax, eax
    test dword [tfrLen], eax
    jz writeExit    ;If we were extending the file, we are done
    mov eax, dword [lastClustA] ;Get the absolute last cluster
.skipExtension:
    mov dword [currClustD], eax ;Now eax has the currClustD value
    ;Get the disk sector too
    call getStartSectorOfCluster
    movzx ebx, byte [currSectC] ;Add the in cluster sector offset
    add eax, ebx    ;Add the offset to eax
    mov dword [currSectD], eax 
    ;If tfrLen = 0, we truncate to current file pointer position, rounding up
    ; clusterwise!
    mov ecx, dword [tfrLen] ;Get the number of bytes to transfer in ecx
    test ecx, ecx
    jz .truncate
    ;Here we write proper data to the disk file
.writeLoop:
    movzx eax, word [currByteS] ;Get bytewise sector offset
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    sub ecx, eax    ;Get bytes left to fill this sector in ecx
    mov eax, dword [tfrCntr] ;Get # bytes left to transfer
    cmp cx, ax  ;Is # of bytes leftto tfr less than bytes left in sector?
    cmova cx, ax    ;If yes, swap
    mov word [sectTfr], cx  ;Save this value in the var
    movzx eax, byte [currSectC] ;Get sector offset in cluster
    cmp al, byte [rbp + dpb.bMaxSectorInCluster]
    jbe .stayInCluster
    ;Get next Cluster information here
    mov eax, dword [currClustD] ;Get disk cluster
    cmp eax, dword [lastClustA] ;Is this the last sector?
    jne .nextCluster
    ;Growing the file
    mov byte [fileGrowing], -1  ;Set to true
    mov ecx, 1  ;Request 1 cluster
    mov ebx, eax    ;Save the last cluster number in eax
    call allocateClusters
    jc .exitPrepHardErr
    cmp eax, -1 ;If eax = -1 then disk full condition
    jc .exitPrep
    mov eax, ebx    ;ebx is preserved
    call readFAT    ;Goto next cluster now, return in eax next cluster
    jc .exitPrepHardErr
    inc dword [lastClust]
    mov dword [lastClustA], eax ;Now eax is the new last cluster
    mov eax, dword [currClustD] ;Get the old last cluster
    ;eax now has the old last sector
.nextCluster:
    ;eax has old disk cluster information
    call readFAT    ;Get the next disk cluster in eax
    jc .exitPrepHardErr
    mov dword [currClustD], eax
    inc dword [currClustF]
    call getStartSectorOfCluster
    mov qword [currSectD], rax
    inc dword [currSectF]
    xor eax, eax
    mov byte [currSectC], al  ;Sector zero in cluster
    mov word [currByteS], ax  ;And byte zero of this sector in the cluster
.stayInCluster:
    mov rax, qword [currSectD]  ;Get disk sector
    call getBufForData
    jc .exitPrepHardErr
    ;rbx points to disk buffer header
    movzx eax, word [currByteS] 
    lea rbx, qword [rbx + bufferHdr.dataarea + rax] ;In sector offset
    ;rbx points to the current byte to write at
    push rdi
    push rsi
    mov rdi, rbx    ;The sector is the destination of the write
    mov rsi, qword [currentDTA] ;Get the user buffer as the source
    movzx ecx, word [sectTfr]   ;Get # of bytes to write
    rep movsb   ;Move over cx number of bytes
    mov qword [currentDTA], rsi ;Update currentDTA
    pop rsi
    pop rdi
    call markBufferDirty
    movzx ecx, word [sectTfr]
    test byte [fileGrowing], -1
    jz .notGrowing
    add dword [bytesAppend], ecx
.notGrowing:
    sub dword [tfrCntr], ecx
    jz .exitPrep
    xor eax, eax
    mov word [currByteS], ax    ;Start of the next sector
    add dword [currByteF], ecx  ;Goto the next sector in the file
    inc byte [currSectC]    ;Increment sector in cluster now
    jmp .writeLoop

.truncate:
;We must free the chain from currClustD
    mov eax, dword [currClustD]
    call truncateFAT    ;Truncate from the current cluster
    mov eax, dword [rdi + sft.dCurntOff]
    mov dword [rdi + sft.dFileSize], eax    ;This is the new filesize now
    jmp .noByteExit ;Exit ok!
.exitPrepHardErr:
    push rax    ;Save error code
    call .exitPrep
    pop rax
.badExitHard:    ;AL has error code already
    call cancelWriteThroughBuffers
    mov eax, 1  ;Give it one last update of the data in the directory!
    call qword [updateDirShare]
    stc
    return
.badExit:
;Might need to do some weird stuff later. Leave for now
    mov eax, errAccDen
    jmp short .badExitHard

.exitPrep:
    mov ecx, dword [bytesAppend]
    add dword [rdi + sft.dFileSize], ecx    ;Add these bytes to the filesize
    mov eax, dword [tfrLen]
    sub eax, dword [tfrCntr]    ;Subtract by bytes left to tfr
    jmp writeExit

.noByteExit:
    mov eax, 2  ;Update last accessed fields of SFT
    call qword [updateDirShare] ;Remember, CF=CY by default so keep xor after
    xor eax, eax
writeExit:
;Advances the bytes on the file pointer
;eax = Number of bytes transferred  
    call writeThroughBuffers
    jc writeDiskFile.exitPrepHardErr
    mov rdi, qword [currentSFT]
    mov ecx, eax
    call .advPtr
    return  ;Return to caller, ecx = # bytes xfrd
.advPtr:
    jecxz .exit ;If no bytes written, skip updating anything
    and byte [rdi + sft.wDeviceInfo], ~blokFileNoFlush ;File has been accessed
    add dword [rdi + sft.dCurntOff], ecx
.exit:
    mov eax, 1  ;Give it one last update of the data in the directory!
    call qword [updateDirShare] ;Remember, CF=CY by default!
    clc
    return
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
    mov ecx, dword [tfrLen]     ;Get bytes to transfer
    sub ecx, dword [tfrCntr]    ;Subtract bytes left to transfer
    ;ecx has bytes transferred
    test word [rdi + sft.wDeviceInfo], devCharDev   ;Char dev?
    jnz .exit
    push rax
    mov eax, dword [currClustD]
    mov dword [rdi + sft.dAbsClusr], eax
    mov eax, dword [currClustF]
    mov dword [rdi + sft.dRelClust], eax
    pop rax
    jecxz .exit ;Skip this if ecx = 0
    add dword [rdi + sft.dCurntOff], ecx    ;Add to the current offset in file
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
getSFTPtrfromSFTNdx:    ;Int 4Fh AX=1216h
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
getJFTPtr:    ;Int 4Fh AX=1220h
;Return a zero extended value in rdi for the SFT entry
;Input: bx = JFT handle (we zero extend)
;Output: CF=NC => rdi = Points to an SFT ndx or -1 => free space
;        CF=CY => al = Error code, Fail
    movzx ebx, bx   ;Ensure we zero extended
    cmp bx, word [maxHndls] ;0-19 acceptable ONLY!
    jb .ok
    mov al, errBadHdl
    stc
    return
.ok:
    mov rdi, qword [currentPSP]
    lea rdi, qword [rdi + psp.jobFileTbl + rbx] ;Use rbx as index in tbl
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

findFreeJFTSpace:
;Input: [currentPSP] = Task whose PSP we will look through
;If there are no free spaces, then we return with al = errNhl and CF=CY
;Else, a pointer to the free space in rsi and al = -1
    push rcx
    mov rsi, qword [currentPSP]
    movzx ecx, word [maxHndls]
    lea rsi, qword [rsi + psp.jobFileTbl]   ;Point to start of table
.search:
    lodsb
    cmp al, -1
    je .exit
    dec ecx
    jnz .search
    mov al, errNhl  ;No free handles buddy, sorry
    stc ;Set error bit
.exit:
    pop rcx
    return

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

decrementOpenCount: ;Int 4Fh AX = 1208h
;Input: rdi = SFT pointer
;Output: ax = Original wNumHandles count
    pushfq
    movzx eax, word [rdi + sft.wNumHandles]
    dec eax     ;Decrement count
    jnz .exit                           ;If the count is not zero, exit
    dec eax    ;If it is zero, now we make it -1
.exit:
    popfq
    xchg ax, word [rdi + sft.wNumHandles] ;RBIL says ax returns og num hdls
    return
