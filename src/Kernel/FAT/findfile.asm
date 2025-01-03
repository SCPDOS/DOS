;Generic Find First and Find Next functions here
findNextMain:
    mov rdi, qword [currentDTA] ;Get the current DTA ptr in rdi
    test byte [rdi + ffBlock.driveNum], 80h ;Bit 7 set for network search
    jz .notNet
    mov eax, 0111Ch ;Network find next
    int 2Fh
    return  ;Return propagating the error code
.notNet:
    mov byte [errorLocus], eLocDsk
    call dosCrit1Enter
    lea rax, tmpCDS
    mov qword [workingCDS], rax ;Set the working CDS to be the tmp cds!
    mov al, byte [rdi + ffBlock.driveNum]   ;Get 1 based number back!
    add al, "@"  ;Convert into letter
    call buildNewCDS     ;Build a new CDS bypassing the real CDS!
    jc .critError   ;Return error if this fails
    mov rdi, qword [workingCDS] 
    mov rbp, qword [rdi + cds.qDPBPtr] 
    call setWorkingDPB
    movzx eax, byte [rbp + dpb.bDriveNumber]
    mov byte [workingDrv], al
    mov byte [delChar], 0E5h
    mov byte [fcbName + 11], 0  ;Set to this being a file we are searching for
    mov byte [fileDirFlag], -1  ;Set to searching for a file!
    call searchMoreDir  ;Now find the next one!
    jc .critError   ;Return error if this fails
.errExit:
    call dosCrit1Exit
    return
.critError:
    mov eax, errNoFil
    jmp short .errExit
searchMoreDir:
;The three directory variables are set up from the ffblock. 
; WorkingDPB is setup also (hence, level 1 critical section)
;Current DTA is also used to contain the ff block address
;All registers can be trashed
    mov rbp, qword [workingDPB]
    ;First copy the search template back then setup dirClustA and dirSect vars
    mov rdi, qword [currentDTA]
    push rdi
    lea rsi, qword [rdi + ffBlock.template]
    lea rdi, fcbName
    mov ecx, 11
    rep movsb   ;Get copy the search template!
    pop rdi
    test byte [rdi + ffBlock.attribFnd], dirCharDev  ;Did we find a char dev?
    jnz .errorExit                              ;No more files if so!!
    mov al, byte [rdi + ffBlock.attrib] ;Get search attrib...
    mov byte [searchAttr], al   ;... and save it!
    mov eax, dword [rdi + ffBlock.parDirClus]   ;Get the directory cluster
    mov dword [dirClustA], eax  ;... into dir vars
    mov dword [dirClustPar], eax
    ;Get number of 32 byte entries in a sector
    mov eax, dword [rdi + ffBlock.dirOffset]    ;Get the 32 byte entry
    cmp eax, -1 ;If this is an invalid value, exit error!
    je .errorExit
    mov dword [dirEntry], eax
    ;Multiply by 32 to get the byte offset into the directory file
    shl eax, 5  ;eax has byte offset into directory file
    ;Now get bytes per cluster
    mov esi, eax    ;Save bytewise file ptr in esi
    cmp dword [dirClustA], 0   ;If we at cluster 0, we are in old style root dir
    je .oldFat
    movzx eax, word [rbp + dpb.wBytesPerSector]
    movzx ecx, byte [rbp + dpb.bSecPerClustShift]
    shl eax, cl ;Shift to get bytes per cluster in eax
    mov ecx, eax    ;Move bytes per cluster into ecx
    mov eax, esi    ;Get bytewise file ptr back in eax
    xor edx, edx
    div ecx ;file ptr / bytes per cluster 
    ;eax now has which cluster rel file (need to walk FAT that many times)
    ;edx has byte offset into cluster (or Root dir)
    mov ebx, eax    ;Save cluster rel directory file in ebx
    ;Now compute sector offset into cluster
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    mov eax, edx    ;Move offset into cluster
    xor edx, edx
    div ecx 
    ;eax now has sector offset into cluster
    ;edx has byte offset into sector 
    mov word [dirSect], ax  ;Store the sector offset into var
    mov eax, dword [dirClustA]  ;Get disk cluster number
    ;Now walk the FAT ebx many times starting from dirClustA in eax
    mov ecx, ebx
    jecxz .skipFatWalk  ;IF ecx is 0, skip walking FAT
.fatlp:
    call readFAT
    cmp eax, -1
    je .errorExit
    dec ecx
    jnz .fatlp
    mov dword [dirClustA], eax  ;Store this cluster number in variable
.skipFatWalk:
    lea rax, searchDir.nextEp
    push rax    ;Push the return address onto stack
    mov eax, dword [dirClustA]
    call getStartSectorOfCluster    ;Get Start Sector of cluster
    movzx ebx, word [dirSect]   ;Get sector offset into the cluster
    add rax, rbx    ;Add the sector offset into the cluster
.common:
    call getBufForDir
    jc searchDir.fnfError
    call prepSectorSearch  ;rbx has the buffer ptr for this dir sector
    ;Gets also in ecx the # of 32 byte entries a sector
    mov eax, dword [dirEntry]   ;Get 32 byte offset into directory
    ;Need to convert into an offset into this sector
    push rdx
    xor edx, edx
    div ecx     ;Divide dirEntry by max entries in sector. Remainder in edx.
    sub ecx, edx ;Subtract the offset to get the number of entries left
    shl edx, 5  ;Multiply by 32 to turn into bytes to add to rsi
    add rsi, rdx    ;rsi points to current entry in the sector.
    pop rdx
    ;We continue AS IF this entry was bad
    ;Now setup al as upon normal entry 
    mov al, byte [searchAttr]  ;Get the search attrib
    jmp findInBuffer.nextEntry ;Proceed from within to resume search properly!
    ;The return address on the stack will return to the ep's pushed
.oldFat:
;Old FAT 12/16 root dirs fall thru here only
;esi = byte ptr in root dir of entry
    lea rax, searchDir.oldNextEP
    push rax    ;Push return address onto the stack
    movzx eax, word [rbp + dpb.wBytesPerSector]
    xor edx, edx
    xchg esi, eax
    div esi ;Divide dir file position by bytes per sector
    ;eax = Sector number 
    ;edx = Byte offset within the sector
    mov word [dirSect], ax ;Set the root directory sector offset
    add eax, dword [rbp + dpb.dFirstUnitOfRootDir] ;Add sector 0 of root dir
    jmp short .common
.errorExit:
    stc
    return
searchDir:
;Called in a level 1 critical section. 
;The directory variables are set and fcbName has the pattern to match
; WorkingDPB is setup also (hence, level 1 critical section)
;All registers can be trashed
;Return with CF=CY if no entry found in directory with al = errNoFil
;If CF=NC, then rsi also points to the directory entry in disk buffer.

    mov rdi, qword [workingCDS] ;Get current CDS
    mov rbp, qword [workingDPB] ;Get the working dpb for the transfer
    test byte [searchAttr], dirVolumeID  ;If the attr has volid, intervene
    jnz .volIdSearch
    test byte [volIdFlag], -1  ;Is volid search override flag set?
    jz .notVolSearch    ;Skip the intervention if not set
.volIdSearch:
;Here we do a volid search intervention. Always searches root dir.
    test byte [fileDirFlag], -1 ;If we are going down path, avoid invervention
    jz .notVolSearch
    xor eax, eax ;Search the root dir.
    call prepSetupDirSearchVars
.notVolSearch:
    mov eax, dword [dirClustA]  ;Get the cluster number to start searching at
    test eax, eax
    jz .oldRoot
    call getStartSectorOfCluster    ;Else, get the start sector in rax
.sectorLoop:
    call getBufForDir
    jc .fnfError
    call prepSectorSearch    ;rbx has the buffer pointer for this dir sector
.rmdirEP: ;Entry used by rmdir to jump into this routine
    call findInBuffer
.nextEp:
    retnc   ;If CF=NC, then the dir has been found and the DTA has been setup
    jz .chardev    ;CF=CY AND ZF=ZE => File not found
    ;If ZF=ZE then chardev (i.e. we hit an entry starting with 00)
    ;Else, we now have to get the next sector of the cluster or next cluster
    ;IF however, the next cluster is -1, then we return fail
    mov eax, dword [dirClustA]  ;Get disk relative cluster
    mov dword [currClustD], eax
    mov ax, word [dirSect]
    mov byte [currSectC], al    ;Cluster Relative sector
    mov rax, qword [rbx + bufferHdr.bufferLBA]
    mov qword [currSectD], rax  
    mov dword [currClustF], 0 ;Use as flag to tell us if cluster has changed
    call getNextSectorOfFile    ;If ZF=ZE, then @ last sector of last cluster
    jc .fnfError
    jz .chardev    ;We are at the end of the directory and didnt find the file
    mov qword [currSectD], rax  ;Store the sector number here
    inc word [dirSect]  ;Goto next sector in cluster
    push rax    ;Save the next sector we need to read
    mov eax, dword [dirClustA]  ;Get disk relative cluster
    cmp eax, dword [currClustD] ;Did it change?
    pop rax
    je .sectorLoop  ;If not, we advanced sectors only
    mov word [dirSect], 0   ;If we did, reset this counter
    push rax
    mov eax, dword [currClustD]
    mov dword [dirClustA], eax  ;Put new cluster number in var
    pop rax
    jmp .sectorLoop

.oldRoot:
;Different search for FAT 12/16 root directories. We assume we have 
; one large contiguous cluster.
.oldSectorLp:
    movzx eax, word [dirSect]    ;Move the sector number into eax
    add eax, dword [rbp + dpb.dFirstUnitOfRootDir] ;Get sector 0 of root dir
    call getBufForDir
    jc .fnfError
    call prepSectorSearch      ;rbx has the buffer pointer for this dir sector
    call findInBuffer
.oldNextEP:
    retnc   ;If CF=NC, then the dir has been found and the DTA has been setup 
    jz .chardev
    inc word [dirSect]  ;Goto next sector in directory
    movzx eax, word [rbp + dpb.wNumberRootDirEntries]
    cmp dword [dirEntry], eax ;Have we reached the last dir entry?
    jb .oldSectorLp    ;If equal, no more entries to search. Game over!
.chardev:
;Now check for a char device ONLY if we are checking for file or dir!
;If exclusive dir on, fail!
    test byte [fileDirFlag], -1 ;Set if file/dir. Clear if only dir!
    jz .fnfError    
    call checkIfCharDevice
    jc .fnfError    ;Not a char dev? Exit!
    call buildCharDir
    xor esi, esi        ;Clear the CF flag
    lea rsi, curDirCopy ;Return with rsi pointing to the dir copy!
    return
.fnfError:
    stc
    mov al, errNoFil
    return
prepSectorSearch:
;Input: rbx -> Buffer with Dir sector to search
;Output: rsi -> Start of the dir data area
;        ecx = Max number of 32 byte dir entries in sector buffer
    lea rsi, qword [rbx + bufferHdr.dataarea]   ;Set rsi to buffer data area
    movzx ecx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    shr ecx, 5  ;Divide by 32 to get # of entries in sector buffer
    return

findInBuffer:
;Input:  rsi = Sector buffer data area
;        cx = Number of entries to check in the sector
;Output: CF=CY => No entries found
;        ZF=NE => Keep searching in subsequent directories
;        ZF=ZE => End of directory reached early, stop
;        CF=NC => Entry found, directory data copied to SDA
;        rsi = Points to start of the disk buffer directory entry
    mov al, byte [searchAttr]  ;Get the search attrib
    call adjustSearchAttr   ;Adjust the search attributes, including volid
.searchMainLp:
;First check if rsi is pointing to a 00h or 0E5h
    mov ah, byte [delChar]
    cmp byte [rsi], 00h
    je .emptySlot   ;If so, check if we are looking for a free dir ptr
    cmp byte [rsi], ah  ;Is the first char the del char?
    jne .notLookingForEmpty
.emptySlot:
;Here we check if we are looking for an empty directory entry or
; we have reached the end of the file (if the first byte is 00h)
;If the first byte of the FCB name = delchar => searching for free dir entry
    cmp ah, byte [fcbName] 
    rete    ;Return if equal (CF=NC too)
    ;If we are not looking for an empty dir but rsi points to 00, exit bad
    cmp byte [rsi], 00h ;Minor optimisation for dir searches
    jne .nextEntry  ;If not, skip this entry as it starts with 0E5h (free)
    stc
    return
.notLookingForEmpty:
    mov ah, byte [rsi + fatDirEntry.attribute]  ;ah = File attributes
    and ah, ~(dirReadOnly | dirArchive) ;Avoid these two bits in search
    test byte [volIdFlag], -1   ;If this is set, intervene in search.
    jz .notVolIdExclusive
    test ah, dirVolumeID   ;If we are a volid, clear CF return
    jz .nextEntry
    return
.notVolIdExclusive:
;If any entry has the volid bit set, it is considered a VOL id only.
    cmp byte [fileDirFlag], 0   ;Are we in dir only mode?
    je .exclusiveDir
    cmp al, dirVolumeID ;Are WE searching for a volume label?
    je .volFile ;If so, go here
    test ah, dirVolumeID ;Is this file a vollbl that we are not looking for?
    jnz .nextEntry
    test ah, ah ;Regular files are always accepted at this point!
    jz .scanName
    cmp al, dirInclusive    ;Is this an inclusive search?
    je .scanName            ;If so, we accept this file!
    cmp ah, al              ;(ah=File bits)==(al=Search bits)?
    jne .nextEntry          ;If bits dont match now, we ignore entry
.scanName:
;rsi points to the start of the fatDirEntry in the Sector Buffer (fname)
    push rsi
    lea rdi, fcbName ;Goto name template to search for
    call .nameCompare
    pop rsi
    je .searchEntryFound
.nextEntry:
;Go to next entry
    add rsi, fatDirEntry_size    ;Goto next entry
    inc dword [dirEntry] ;And denote that in variable
    dec ecx
    jnz .searchMainLp
.badExit:
    xor eax, eax
    inc eax ;Clear ZF
    stc
    return
.exclusiveDir:
;Used when going down a path, dir bit simply need be set
    test ah, dirDirectory
    jnz .scanName
    jmp short .nextEntry

.volFile:
    cmp ah, al  ;Is the file indeed a Volume ID?
    je .scanName       ;If so, scan the name agrees
    jmp short .nextEntry    ;Else, goto next entry

.searchEntryFound:
;Here a good entry was found!
    push rsi
    push rdi
    mov bl, al  ;Save temporarily the search attributes
    lea rdi, curDirCopy
    mov ecx, 32/8
    rep movsq   ;Copy the directory to SDA
    pop rdi
    pop rsi ;Point rsi to the directory entry in the buffer
    clc
    return

.nameCompare:
;Input: rsi = source string
;       rdi = string template to compare against
;Output: ZF=ZE => Strings are ok
;        ZF=NZ => Strings not ok
;Accepts wildcards. Trashes al
    push rcx
    xor ecx, ecx    ;11 chars to compare
.ncLp:
    cmp ecx, 11
    je .ncExit
    inc ecx
    cmpsb   ;Compare the chars in the two strings and advance ptrs
    je .ncLp    ;If equal, keep going
    cmp byte [rdi - 1], "?" ;Was the char in the template a wildcard?
    je .ncLp
.ncExit:
    pop rcx
    return
adjustSearchAttr:
;Converts the byte to a system only if the bit is set
;Input: eax = User selected search mask
;Output: eax = Modified search mask
    and eax, 03Fh   ;Clear upper two bits of the search attributes
    test eax, dirVolumeID   ;Is the volume id bit set?
    retz
    mov eax, dirVolumeID
    return

asciiToFCB:
;Converts a filename in the form FILENAME.EXT,0 to FILENAMEEXT
;Will uppercase any lowercase chars as this could be used with user buffers.
;Names such as SYS.COM get converted to "SYS     COM"
;Name is space padded.
;Input: rsi = ASCII string buffer
;       rdi = FCB name buffer
;Output: al = Char that terminated the source string 
    push rbx    
    push rdi
    mov ecx, 11
    mov al, " "
    rep stosb   ;Fill the buffer with spaces (so we don't need to fill later)
    pop rdi
    mov rbx, rdi    ;Use rbx as the base pointer of this buffer
.processName:
    lodsb   ;Get the char in al
    call uppercaseChar  ;Just in ANY case, we will uppercase the cahar
    test al, al
    jz .exit
    cmp al, " " ;If space or a period, go to extension field. If null, exit
    je .extSpace
    cmp al, "."
    je .ext
    ;Test if the char is valid
    call checkCharValid ;ZF=ZE => Invalid char
    jz .exit    ;If the char invalid, consider it a terminator
    stosb   ;Store the char
    jmp short .processName
.extSpace:
;Now we scan for a period in the name
    lodsb   ;Get a char and increase rsi
    test al, al
    jz .exit
    cmp al, "."     ;If al is not a period...
    jne .extSpace   ; keep searching
.ext:
    lea rdi, qword [rbx + filename.fExt]    ;Put destination at the extension
.processExt:
    lodsb
    test al, al
    jz .exit
    cmp al, " "
    je .exit
    stosb
    jmp short .processExt
.exitBadChar:
    xor al, al  ;Return a null terminator
.exit:
    pop rbx
    return

FCBToAsciiz:
;Converts a filename in the form FILENAMEEXT to FILENAME.EXT,0
;Name is space padded too
;Input: rsi = FCB name buffer
;       rdi = ASCIIZ string buffer
    mov ecx, 8
    rep movsb   ;Move the name over
.scanNameSpace:
    cmp byte [rdi - 1], " " ;Is the previous char a space?
    jne .ext
    dec rdi
    inc ecx
    cmp ecx, 8
    jb .scanNameSpace
.ext:
    cmp word [rsi], "  "    ;Are the first two chars a space?
    jne .validExt
    cmp byte [rsi + 2], " " ;Is the final char a space?
    je .exit
.validExt:
    mov al, "." ;We have a valid extension, store a period
    stosb
    mov ecx, 3
    rep movsb   ;Move the three extension chars over
.scanExtSpace:
    cmp byte [rdi - 1], " " ;Is the previous char a space
    jne .exit
    dec rdi
    jmp short .scanExtSpace
.exit:
    xor eax, eax
    stosb   ;Store a null at the end
    return

setupFFBlock:
;Sets up the find first block for the search
;Uses workingDrv, fcbName, curDirCopy and rdi as the source of the FFBlock
    push rax
    push rbx
    push rsi
    push rdi
    mov rbx, rdi ;Get current DTA address into rbx
    mov al, byte [searchAttr]
    and al, 3Fh ;Clear upper two bits
    mov byte [rbx + ffBlock.attrib], al 
    movzx eax, byte [workingDrv]  ;Get the 0 based working drive number
    inc al  ;Convert into 1 based number to agree with DOS
    mov byte [rbx + ffBlock.driveNum], al
    lea rsi, fcbName
    lea rdi, qword [rbx + ffBlock.template]
    push rdi
    mov rax, "        "
    stosq
    stosw
    stosb
    pop rdi
    movsq   ;Move 8 chars
    movsw   ;Move 2 chars
    movsb   ;Move the final char

    mov eax, dword [dirEntry]
    mov dword [rbx + ffBlock.dirOffset], eax
    mov eax, dword [dirClustPar]
    mov dword [rbx + ffBlock.parDirClus], eax
    mov al, byte [curDirCopy + fatDirEntry.attribute]
    mov byte [rbx + ffBlock.attribFnd], al
    mov eax, dword [curDirCopy + fatDirEntry.wrtTime] ;Get time/date together
    mov dword [rbx + ffBlock.fileTime], eax
    mov eax, dword [curDirCopy + fatDirEntry.fileSize]
    mov dword [rbx + ffBlock.fileSize], eax
    lea rdi, qword [rbx + ffBlock.asciizName]   ;Goto the name field
    lea rsi, curDirCopy
    call FCBToAsciiz    ;Convert the filename in FCB format to asciiz

    pop rdi
    pop rsi
    pop rbx
    pop rax
    return

getDrvLetterFromPath:   ;Int 2Fh, AX=121Ah
;Gets the drive letter for the path in al
;Input: rsi = Buffer to process
;Output: If al = 0, rsi NOT incremented by 2. Else, rsi incremented by 2 
;       ZF=ZE and al = 0 => Null path
;       ZF=NZ and al = 0 => Relative path, splice flag = 0 or Net path
;       ZF=NZ and al = -1 => Bad drive number
;       ZF=NZ and al = 1 based drive number => All oki, but may be relative
    xor al, al
    cmp byte [rsi], 00h ;Is this a null path?
    retz    ;Return if a null path
    cmp byte [rsi + 1], ":" ;Path separator?
    retne   ;If not equal, Relative path or network path
    lodsw   ;Get first word, rsi += 2
    ;Make char lower case if its not and then convert to offset from "a" - 1
    or al, 20h  ;Set the bit for lowercase chars
    sub al, 60h
    retnz ;If the number is non-zero, then a potentially valid drive number
    mov al, -1  ;Else not a valid drive number
    return

getDirPath:
    xor al, al   ;Set to Directory
    jmp short getPath
getFilePath:
    mov al, -1  ;Set to File
getPath:
;Called with:
; rdi = SDA Buffer for filename
; rsi = Potentially unqualified filename
; al = 0 => Search for Dir only. al != 0 => Search for File (or dir)
    push rax
    push rdi
    call canonicaliseFileName   ;First canonicalise the pathspec presented
    pop rdi
    pop rax
    retc
    ;Now our name is FQ, we can search the for it!
    cmp qword [workingCDS], -1  ;If there is no CDS associated with this path
    je getPathNoCanon.netFFnoCDS ;It must be a net no CDS search!
    mov byte [fileDirFlag], al 
    mov al, -1
    mov byte [spliceFlag], al   ;Set splice for Full path by default
    mov byte [skipDisk], al     ;Store -1 to NOT skip checking the file on disk
    mov byte [parDirExist], 0   
    mov byte [fileExist], 0     
    mov al, byte [workingDrv]
    inc al
    lea rsi, qword [rbx + 1]    ;Move the pointers past the machine name pathsep
    cmp word [rdi], "\\"        ;Did we resolve to remote path?
    je .net
    mov rbx, qword [fname1Ptr]
    add rbx, 2  ;Now our path is completely normalised so this always works!
    lea rsi, qword [rbx + 1]
    push qword [workingCDS] ;Save the original CDS to allow for subst to work
    call pathWalk
    pop qword [workingCDS]
    return
.net:
    mov rdi, rsi                ;Else, skip setting up vars for drive access
    jmp pathWalk.netEp          ;Still do all drive access using CDS, but over redir
    ;The reason why this works is that DOS doesn't need to keep track of the 
    ; internal vars for accessing a net drive. All accesses still occur via the
    ; redir, and the redir can update its vars as it needs. The DOS pointers
    ; point as is necessary on the path (with machine name prefix), and in the SDA
    ; so the redir can do it's job.

canonicaliseFileName:
;Always trying to build and qualify a full pathname
;Does getPath without hitting the disk
    mov al, -1
    mov byte [fileDirFlag], al  
    mov byte [spliceFlag], al   ;Set splice for Full path by default
    mov qword [fname1Ptr], rdi  ;Save the SDA buffer we are using for this file
    inc al  ;make al = 0
    mov byte [skipDisk], al  ;Store 0 to skip checking the file exists
    call getPathNoCanon.epAlt
    retc    ;Error return
    ;Check the last two chars to see if we have a null terminator.
    ;If neither has a null terminator, add one
    xor eax, eax
    cmp byte [rdi], al
    retz                
    cmp byte [rdi - 1], al 
    retz
    stosb
    return

getDirPathNoCanon:
;FCB only
    xor eax, eax
    mov rsi, rdi
    jmp short getPathNoCanon
getFilePathNoCanon:
;Used when the path is constructed internally (as for FCB functions), and renameMain
;Input: rdi -> Buffer with qualified pathname for search
    mov al, -1
    mov rsi, rdi
getPathNoCanon:
;Called with:
; rdi = SDA Buffer for filename
; rsi = Potentially unqualified filename
; al = 0 => Search for Dir only. al != 0 => Search for File (or dir)
    mov byte [fileDirFlag], al  
    mov al, -1
    mov byte [spliceFlag], al   ;Set splice for Full path by default
    mov qword [fname1Ptr], rdi  ;Save the SDA buffer we are using for this file
    mov byte [skipDisk], al     ;Store -1 to NOT skip checking the file on disk
.epAlt:
    mov byte [parDirExist], 0  ;If parent dir exists, set to -1
    mov byte [fileExist], 0 ;If the file exists, set to -1
    test byte [dosInvoke], -1   ;Was it invoked via server? -1 = Server
    jz .notServer
    call getDrvLetterFromPath   ;rsi will point to the \ in "X:\"
    call getCDS ;Get the cds for the drive letter on the path
    inc al  ;Turn back into a 1 based drive number
    push rsi
    push rdi
    mov rdi, qword [workingCDS]
    push rax
    call dosCrit1Enter
    call getDiskDPB ;Force an initial update of the disk dpb. Get ptr in rbp
    call dosCrit1Exit
    pop rax
    pop rdi
    pop rsi
    jnc .driveOk
.serverExit:
    mov al, errPnf  ;If CF=CY, use this error code
    return
.notServer:
    ;Make Redir request to qualify the filename if NOT invoked by server call
    ; and only in truename mode
    test byte [skipDisk], -1    ;If set, we on disk hit mode
    jnz .skipRemoteQualify      ;So skip!
    mov qword [workingCDS], -1  ;Set workingCDS to unknown
    mov eax, 1123h  ;Net Qualify Path name
    int 2fh ;CF=CY if not resolved. CF=NC if resolved
    retnc  ;Return if resolved
.skipRemoteQualify:
    call getDrvLetterFromPath ;Get the drive letter in al (or -1)
    pushfq  ;Save the flag state on stack
    push rax    ;Save whether rsi is incremented by 2
    mov ax, word [rsi]   ;Get the word pointed to by rsi
    call swapPathSeparator  ;Convert al if it is a path separator
    xchg ah, al ;Now swap al into ah to check if we on a network path (i.e. UNC)
    call swapPathSeparator  ;Returns ZF=ZE if al = "/" or "\"
    jnz .notNet
    cmp ah, al  ;If they are equal, we have a net path
    jne .notNet
    pop rax ;We are in a net situation, so rsi is pointing at "\\"
    popfq
    lodsw
    mov ax, "\\"    ;Orient the path correctly
    stosw   ;Tfr the two chars rsi, rdi + 2
;For Net paths, if skipDisk is clear, we only copy and qualify the path
;If skipDisk is set, proceed as before, except we then pass the path to net
; redirector Find First Without CDS
    mov bl, byte [skipDisk] ;Save skipDisk state
    mov byte [skipDisk], 0  ;Set to copy and qualify name first
    push rbx
.moveNetName: ;This sets up the machine name portion of the UNC path
    lodsb   ;Get the third char into al and inc rsi
    call uppercaseChar  ;Make char in al uppercase
    test al, al
    jz .netEnd
    call swapPathSeparator  ;If path sep, swap it
    stosb
    jnz .moveNetName  ;If not a path separating char in al, keep looking
    ;Skip all following pathseps if there are any
.moveNetSharePtr:
    lodsb
    call swapPathSeparator
    jz .moveNetSharePtr
    mov byte [rdi], 0   ;Null terminate path to make ASCIIZ
    dec rsi ;More rsi back to the first char past the seps
    test al, al ;Was this char null?
    jz .netEnd  ;Skip processing if so!
    mov rbx, rdi
    dec rbx ;rbx points at the pathsep before the space for the first char
    call pathWalk.netEp     ;Now expand the pathspec portion
    jc .netExitBad
    ;Now if we have a trailing backslash, throw it away
    ;rdi points to the null char of the path
    mov al, byte [rdi - 1]  ;Get the char before the nullsep
    call swapPathSeparator  ;Is it a pathsep?
    jnz .netEnd
    mov byte [rdi - 1], 0   ;Write a null if it is
.netEnd:
    pop rbx
    test bl, bl ;If skip disk was zero, exit
    retz
.netFFnoCDS:
    mov eax, 1119h  ;Find First Without CDS
    int 2Fh
    return
.netExitBad:
    pop rbx
    return
.notNet:
;This is the normal case; paths can be relative, or absolute.
    pop rax ;Get the drive letter back
    popfq   ;Get the flag state back
    jnz .notNull    ;If ZF=ZE, the path is a null path, errExit
.pnfErr:
    mov al, errPnf  ;Null path error
    stc
    return
.notNull:
    cmp al, -1  ;Bad drive letter?
    jne .driveOk    ;Jump if ok drive letter
    mov al, errBadDrv   ;Bad drive letter specified
    stc
    return
.driveOk:
    test al, al
    jz .curPath ;If al = 0, the path is definitely relative to curr. drv.
    ;al now has 1-based drive number, rsi has been incremented by 2.
    cmp byte [rsi], 0   ;Is this pathspec "X",":",0?
    je .pnfErr  ;Throw error if it is a malformed path
    push rax    ;Save the drive letter
    lodsb   ;Move rsi to the third char, get char in al
    dec rsi ;Move rsi back to point to the previous char
    call swapPathSeparator  ;ZF=ZE if path separator
    ;If al is a path separator, then this path is absolute.
    pop rax ;Get back the drive letter
    jnz .relMain ;If relative, rsi points to first char in path
.absoPath:
    ;Here the path is absolute. Now point rsi to first char past "\"
    inc rsi
    jmp short .commonDir
.curPath:
;This is only jumped to if we are relative the current drive
;rsi points to first char in path
;If this char is \ or /, then it is an absolute path.
    mov al, byte [currentDrv]   ;Get current drive (0-based number)
    inc al  ;Turn it into a 1 based drive number
    push rax    ;Save the current drive number
    mov al, byte [rsi]  ;Get the first char w/o moving rsi ...
    call swapPathSeparator ;... to ensure rsi points at first char of string
    pop rax ;Get back the current drive number
    jz .absoPath    ;If first char was pathsep, it is abs. path for curr. drv
.relMain:
    mov byte [spliceFlag], 0    ;Set Splice flag to indicate Relative to CDS
.commonDir:
;rsi points to the start of the string we will be appending
;Fall through now
pathWalk:
;Input: rsi must point to source buffer for path to expand
;       rdi must point to a destination buffer
;       al must contain the drive 1 based number
    call prepareDir    ;Prepare the start of the path
    jc .badDriveExit 
.netEp:
;For net path resolution (resolution ONLY) ptrs must point past "\\".
;For subst, resolution cannot go past backslash offset.
;For join, it is transparent.
    test byte [skipDisk], -1    ;Clear if not hitting disk!
    jz .notNet                  ;Skip this if not hitting disk!
;Start diskhit by checking the cds here if hitting the disk. 
;If it is a network cds we make findfirst req. 
    push rdi
    mov rdi, qword [workingCDS] ;Get current CDS
    test word [rdi + cds.wFlags], cdsRedirDrive
    pop rdi
    jz .notNet
    mov rdi, qword [workingCDS] ;Get current CDS as in DOS, undocumented
    mov eax, 111Bh  ;Find First with CDS!
    int 2fh 
    return
.notNet:
    cmp byte [rsi], 0   ;If rsi at the end of the string, exit for ROOT dir
    jnz .mainlp
    ;Setup dummy dir data for Root directory
    xor eax, eax
    mov word [curDirCopy + fatDirEntry.fstClusHi], ax
    mov word [curDirCopy + fatDirEntry.fstClusLo], ax
    mov byte [curDirCopy + fatDirEntry.attribute], dirDirectory
    ;Now we check if we are on a JOIN drive. If so, we swap back to the 
    ; join host! Only when hitting the disk though.
    test byte [skipDisk], -1
    jz .exitGood
    push rdi
    mov rdi, qword [workingCDS]     ;Get the current CDS
    test word [rdi + cds.wFlags], cdsJoinDrive
    pop rdi
    jz .exitGood    ;If not a join drive, exit (this never happens?)
    mov rsi, qword [workingCDS] ;Use the join client CDS as source string!
    mov al, -1  ;Set to search for a dir as only dir can be mntpoint
    jmp getPathNoCanon
.mainlp:
    ;rbx must remain constant in this portion,
    ; and is used to signify the first writable byte in the path
    push rbx
    call copyPathspec  ;Now setup the filename in the FCB name field
    pop rbx
    jc .exit    ;If this errors (bad chars in filename portion), exit bad 
    test al, al
    jnz .notFile
    mov byte [parDirExist], -1  ;Set byte to -1 to indicate parent dir exists!
.notFile:
    push rbx                ;The below messes up rbx
    call checkDevPath       ;Silently intervene DEVs if in truename mode!
    pop rbx
    call searchForPathspec  ;Now search the directory
    jc .exit    ;If CF=CY, error exit. al has error code
    call addPathspecToBuffer    ;Only entered if truename mode
    jc .exit   ;If a bad path (somehow I dont see this happening often)
    test al, al ;Exit if this pathspec was a file
    jz .exitGood
    ;Here I have to take the cluster data from the found directory entry
    ; and setup the search for the next pathspec portion
    ;Copy necessary data from the current directory copy
    cmp byte [skipDisk], 0  ;If we are skipping disk activity, skip this too!
    jz .mainlp
    movzx eax, word [curDirCopy + fatDirEntry.fstClusHi]
    shl eax, 10h    ;Shift it high
    mov ax, word [curDirCopy + fatDirEntry.fstClusLo]
    ;eax now has the cluster number of the search
    mov dword [dirClustA], eax
    mov dword [dirClustPar], eax
    xor eax, eax    ;Start searching at the start of this dir
    mov dword [dirEntry], eax
    mov word [dirSect], ax
    jmp short .mainlp  ;Else, it was a found directory name, keep looping
.badDriveExit:
    mov eax, errBadDrv ;Bad drive letter found
    return
.exitGood:
    mov byte [fileExist], -1 ;If the file exists, set to -1
.exit:
    return

prepareDir:
;Used to transfer the current directory if it is necessary.
;Always necessary if the user specified a subst drive. Else only if 
; a relative path is specified.
;Input: al = 1-based drive letter
;Output: rdi = Pointing at where to place chars from source string
;        rbx -> Pointing at the point before which chars cannot be placed
;   If CF=CY => Drive invalid or drive letter too great
    push rsi    ;Push ptr to source string
    call dosCrit1Enter ;CDS/DPB cannot be touched whilst we read the pathstring
    call getCDSNotJoin   ;Set internal variables, working CDS etc etc
    jnc .notJoin ;Very valid disk
    test byte [skipDisk], -1    ;Are we a join drive in truename?
    jnz .okJoin                 ;If not, proceed. If so, fail.    
    stc
    jmp short .critExit    ;If the drive number in al is too great or a join drive specified.
.getDPB: 
    mov rdi, qword [workingCDS] 
    call getDiskDPB  ;Update working DPB and drv before searching
    return
.okJoin:
    call .getDPB
    jmp short .joinEp
.notJoin:
    call .getDPB
    ;rbp = DPB ptr now
.critExit:
    call dosCrit1Exit
    jc .badDriveExit 
    mov rdi, qword [fname1Ptr] ;Get the ptr to the filename buffer we will use
    ;If this CDS is a subst drive, copy the current path to backslashOffset
    ;If this CDS is a join drive... it can't be unless we truenamed our path!
    ;If the path is to be spliced, then we copy the whole CDS current path
    ;If the CDS is not subst drive, nor to be spliced, we copy first two chars.
    ;Before we begin, we init rbx to point at the backslash offset of the path
    mov rsi, qword [workingCDS] ;Now get the CDS ptr into rsi
    mov rbx, rdi
    movzx eax, word [rsi + cds.wBackslashOffset]
    add rbx, rax    ;Move rbx to the backslash offset for this drive
    ;Now check if we have a subst to handle
    test word [rsi + cds.wFlags], cdsSubstDrive
    jnz .prepDirSubst
    test byte [spliceFlag], -1
    jz .prepLoop ;If this flag is zero, we loop
    ;Else we copy the first two chars only (X:)
    movsw  
    mov al, "\"
    stosb   ;Store the path separator in internal buffer and increment rdi
.joinEp:
    xor eax, eax    ;Get cluster 0
    jmp short .prepDirExitSkip
.prepLoop:
    lodsb
    test al, al ;If al was null, then we stop
    jz .prepDirExit
    stosb
    jmp short .prepLoop
.prepDirExit:
    cmp byte [rdi - 1], "\" ;Was the previous char a pathsep?
    je .prepDirExitSkipPathsep
    mov al, "\"
    stosb   ;Store the path separator and increment rdi
.prepDirExitSkipPathsep:
    mov rsi, qword [workingCDS] ;Get the CDS ptr ONLY IF CDS Relative
    mov eax, dword [rsi + cds.dStartCluster]    ;... and start at given cluster
.prepDirExitSkip:
    call prepSetupDirSearchVars
    clc ;Clear carry before exiting
.badDriveExit:
    pop rsi
    return
.prepDirSubst:
    push rcx
    movzx ecx, word [rsi + cds.wBackslashOffset]
.prepDirCopy1:
    rep movsb   ;Copy the string over
    pop rcx
    test byte [spliceFlag], -1
    jnz .prepDirExit    ;If not relative, exit as we put the "root dir" marker
    jmp short .prepLoop ;Else, need to copy CDS now too as part of path
prepSetupDirSearchVars:
;Input: eax = Starting Cluster of search on disk (0=Root dir)
;       rbp -> Working DPB
    push rcx
    xor ecx, ecx
    mov word [dirSect], cx  ;Always start searching at sector 0 of dir cluster
    mov dword [dirEntry], ecx ;Always start at entry 0 of the sector in cluster
    call getFATtype ;Get type of fat
    cmp ecx, 2  ;2 = FAT32
    jne .psdsvExit      ;FAT 12/16 jump and store 0 if at root
    ;FAT 32 here
    test eax, eax   ;Are we looking for root dir of FAT32 drive?
    jnz .psdsvExit  ;If not, store the cluster number unchanged
    mov eax, dword [rbp + dpb.dFirstUnitOfRootDir]  ;Else get cluster number
.psdsvExit:
    mov dword [dirClustA], eax  ;Store directory cluster (or 0 if \ on FAT12/16)
    mov dword [dirClustPar], eax    ;Store parent cluster number
    pop rcx
    return 

copyPathspec:
;1) Copies a path portion from the source buffer to the destination
;2) Advances rsi to the next null, \ or /
;3) Expands all * to ?'s
;4) Understands \. means "this directory" and can be ignored with rsi moving to
;    next path separator and rdi pointing to the previous pathsep
;5) Understands \.. means "parent directory" and rdi should be changed to rbx
;    with rsi moving to path separator
;6) Each name in destination is at most 12 chars long, to account for the dot
;
;INPUT:     rsi = First char of pathspec to qualify
;           rdi = Points to where to store it
;
;RETURN:    rsi = First char of next pathspec or past terminating null
;           rdi = First char of next space to store next pathspec
;           al = Last char stored (either \ or NULL)
;           CF=NC = OK path
;           CF=CY = PATH OR FILE NOT FOUND
;               IF A WILDCARD FOUND IN A SUBDIR NAME, RETURN PNF.
    push rdi    ;Save the pointer into the user buffer
    mov byte [fcbSpaceOk], -1    ;Set to be ok to have space in the name
    lea rdi, fcbName
    push rdi
    mov ecx, 3
    mov eax, "    " ;Four spaces
    rep stosd   ;Store 12 spaces
    pop rdi ;Point rdi back to fcb name head

    lea rbx, fcbName + 11   ;Use rbx as the end of fcb name marker address
    lodsb   ;Get first char from user path in al
    cmp al, "."   ;Handle starting dot separately
    je .cpsDots
    dec rsi ;Else move rsi to point back to starting char
;First char is not a dot, so now check if starts with E5h? 
;If so, store 05h in its place!
    cmp al, 0E5h
    jne .cpsMainLoop
    inc rsi ;Push rsi to point to next char
    mov al, 05h
    stosb   ;Store the char, rsi is pointing at next char
.cpsMainLoop:
    lodsb   ;Get the char in al and advance rsi
    test al, al ;Is it the null char?
    jz .cpsStore  ;If so, terminate immediately
    call swapPathSeparator  ;And if it is a pathsep, skip any bunched pathseps
    jz .cpsSkipPathseps ; and then exit with the final converted pathsep in al
    cmp al, "." ;Filename extension separator
    je .cpsExtension
    ;If we have space in the filename, we check to see if the next char is *
    cmp al, "*" ;Wildcard?
    je .cpsWildcard
    cmp al, "?" ;Good wildcard?
    je .store
    call uppercaseChar  ;Uppercase the char if it needs to be...
    call checkCharValid ; and check it is a valid char
    je .cpsExitError
.store:
    cmp rdi, rbx
    je .cpsProcessName ;Skip any non-terminating chars
    stosb   ;And store the converted char in al and inc rdi
    jmp short .cpsMainLoop
.cpsInvalidChar:
    xor al, al
    jmp short .cpsProcessName
.cpsExtension:
;rsi has been incremented past the extension field. Discard the . in al
    mov ecx, 3 
    lea rdi, qword [fcbName + filename.fExt]    ;Goto the extension field
    push rdi
    mov al, " "
    rep stosb
    pop rdi
    jmp short .cpsMainLoop
.cpsWildcard:
;Fill the entire FCB filespec with ?'s.
;We lookahead to the first char past the final "*" to prevent multiple
; *'s from crashing DOS :)
    lodsb   ;Get char, inc rsi
    cmp al, "*"
    je short .cpsWildcard
    dec rsi ;Go back to the first non-star char
    mov al, "?"
.cpsWildcardLp:
    stosb
    cmp rdi, rbx    ;We filled the FCB name field yet?
    jne short .cpsWildcardLp
    jmp short .cpsMainLoop

.cpsDots:
    stosb   ;Store the first dot
    mov al, byte [rsi]
    cmp al, "."    ;Check now if we have a second dot
    jne .cpsStore
    movsb   ;Now advance rsi and rdi by copying the second dot over directly
    mov al, byte [rsi]  ;Ensure that the .. is not followed by any chars
    test al, al
    jz .cpsProcessName
    call swapPathSeparator  ;If the char following the .. is not null or pathsep, error
    jnz .cpsExitError
.cpsProcessName:
;Store the final char in the 12 space in the FCB name field, if it valid
    test al, al 
    jz .cpsStore
    call swapPathSeparator
    jz .cpsStore
.cpsFindTerminator:
    lodsb
    test al, al 
    jz .cpsStore
    call swapPathSeparator
    jz .cpsStore
    call checkCharValid ;If this is ZF=ZE => Terminator
    jnz short .cpsFindTerminator   ;Ensure we skip any extra chars
    ;If we encounter a terminator, convert to 0
    xor al, al
.cpsStore:
    lea rdi, fcbName+11
    stosb   ;Store the terminator in this slot. 0 for End of Path, \ for subdir
    pop rdi
    clc
    return
.cpsExitError:
    pop rdi
    stc
    return
.cpsSkipPathseps:
    lodsb
    call swapPathSeparator
    jz .cpsSkipPathseps
    dec rsi ;Go back to the first char in next section of the pathspec
    mov al, "\" ;Make sure to store a pathsep char
    jmp short .cpsStore

searchForPathspec:
    ;Now search the current directory for this filename
    ;Find first using SDA ffBlock
    ;If al = 0, we have final file name or directory name
    ;If al = \, we have subdirectory. NO WILDCARDS ALLOWED IF PATHSEP
    ;Output: CF=CY => Error occured, al has error code (Fnf or Pnf)
    ;        CF=NC => Disk File in fcbName found with selected attributes
    ;                 FF block somewhat setup
    ;Preserves rax, rbx, rsi,  rdi
    push rax
    push rbx
    push rsi    ;Save the current position of the pointer in the user buffer
    push rdi    ;Save current position to store filename in internal buffer
    movzx ebx, byte [fileDirFlag]   ;Save the old flag that was set on entry
    push rbx    ;and push it onto the stack
;Evaluate whether we are searching for a file for a directory
    test al, al
    jz .sfpPNfile
    ;Fall if subdir
    lea rdi, fcbName
    mov al, "?" ;Search for wildcard
    mov ecx, 11
    repne scasb
    je .sfpPnf  ;Path not found if a ? found in the name
    mov al, dirDirectory    ;We want a directory only search.
    mov byte [fileDirFlag], 0   ;Override setting to search exclusively for dir
    jmp short .sfpPNMain
.sfpPNfile:
    ;Here if we are searching for a file or directory as setup by search init
    movzx eax, byte [searchAttr]    ;Get the search attributes
.sfpPNMain:
    cmp byte [skipDisk], 0  ;If we are just qualifying a path, skip the disk hit
    je .sfpPNNoDisk
    call searchDir
    jc .sfpPNNoDisk
    cmp byte [curDirCopy], 05h 
    jne .noAdjust
    ;Readjust when the start of a dir entry name is 05h
    mov byte [curDirCopy], 0E5h
.noAdjust:
    clc ;Ensure the carry flag is clear here!
.sfpPNNoDisk:
    pop rbx
    mov byte [fileDirFlag], bl  ;Return the original flag
    pop rdi ;rdi points to free space in internal filename buffer
    pop rsi
    pop rbx
    pop rax ;Get back the original al value
    retnc   ;Return if file found. Else, report what was not found correctly!
    test al, al ;If we are on the last path comp, fail pnf!
    stc         ;Dont forget to set CF again!!
    mov eax, errFnf
    retz
    mov eax, errPnf ;Else it is path not found!
    return
.sfpPnf:
    mov eax, errPnf
.sfpErrExit:
    stc ;Set carry
    jmp short .sfpPNNoDisk

addPathspecToBuffer:
;Input: fcbName = Qualified pathname portion
;Output: CF=NC -> al = Last char in name (either Null or \) 
;        CF=CY -> Invalid path (i.e. tried to go too far backwards)
;rdi is advanced to the NEXT space for the next level of the filename
;rbx points to the "head of the path"
;rsi points to the first char of the next portion if al is pathsep
;Converts the special char if the filename starts with it!
    test byte [skipDisk], -1
    retnz   ;Only add if in truename mode (also clears CF)
    cmp byte [fcbName], "."   ;Handle destination pointer for  
    je .aptbDots
    ;Copy filename over to internal buffer
.aptbAddNull:
    push rsi    ;Save source pointer position
    lea rsi, fcbName
    cmp byte [rsi], 05h
    jne .notSpecialChar
    mov byte [rsi], 0E5h
.notSpecialChar:
    call FCBToAsciiz    ;Convert the filename in FCB format to asciiz
    dec rdi ;Go back to the in-situ null terminator char
    pop rsi ;Get back src ptr which points to first char in next pathspec
    call .aptbInterveneEnterJoin
    retc    ;Return bad if a disk error occured.
.aptbHandleTerminator:
    mov al, byte [fcbName + 11] ;Get the actual terminator for this portion.
    test al, al
    jz .aptbHandleNull
    call swapPathSeparator
    jnz .aptbErrorExit
;Handle path separator here directly
    push rax    ;Preserve the terminator char
    mov al, byte [rdi - 1]
    call swapPathSeparator  ;If the char before us is a pathsep, do nothing.
    pop rax
    jz .aptbExitOk 
    stosb   ;Now go past the pathsep we just placed
    jmp short .aptbExitOk
.aptbHandleNull:
    call .aptbAtHeadOfPath
    je .aptbHandleNullNoOverwritePrim
    push rax
    mov al, byte [rdi - 1]  ;Is previous char a pathsep?
    call swapPathSeparator
    pop rax
    jnz .aptbHandleNullNoOverwrite
    mov byte [rdi - 1], al  ;Overwrite it!
    jmp short .aptbExitOk
.aptbHandleNullNoOverwritePrim:
    cmp byte [rbx - 1], ":"
    je short .aptbHandleNullNoOverwrite
    ;This handles trailing slashes that are not right at the head of the path
    dec rdi
.aptbHandleNullNoOverwrite:
    mov byte [rdi], al  ;Write in a null
.aptbExitOk:
    clc
    return
.aptbDots:
;For one dot, we pull the terminating null to rdi
;For two dots, we search backwards for the previous "\"
    cmp byte [fcbName + 1], "." ;Was the second char also a dot?
    je .aptbMoreDots
    mov byte [fcbName], " "
    jmp short .aptbAddNull
.aptbMoreDots:
;Here we have two dots
    call .aptbAtHeadOfPath  ;Are we at the start of the path?
    je .aptbInterveneExitJoin ;If so, it must be join or fail
;Else, we are able to decrement.
    sub rdi, 2  ;Go past the pathsep infront of us.
.aptbDotsLp:
    call .aptbAtHeadOfPath  ;Are we safe to proceed?
    je .aptbHandleTerminator
    dec rdi ;Decrement the pointer by one
    mov al, byte [rdi]  ;Get the char
    call swapPathSeparator  ;Is this a valid pathsep?
    jnz .aptbDotsLp ;If not, keep searching
    inc rdi ;Now go just past the new pathsep
    jmp .aptbHandleTerminator
.aptbAtHeadOfPath:
;Returns ZF=ZE if at head of path.
    push rdi
    dec rdi
    cmp rbx, rdi    ;Are we right at the start of the path?
    pop rdi
    return
.aptbInterveneExitJoin:
;Here, if we are on a join CDS, go to the root of the original drive.
    mov rbp, qword [workingCDS]
    cmp word [rbp + cds.wFlags], cdsJoinDrive | cdsValidDrive
    jne .aptbErrorExit    ;If it is not, we error return (filenotfound)
    ;Now we change the drive letter and return
    mov al, byte [rbp]  ;Get the first char of the path 
    mov byte [rdi - 3], al  ;Replace the char in destination buffer
    sub al, "@" ;Convert to a 1 based drive number
    call getCDSNotJoin
    retc ;If this errors, something is really wrong. Propagate error.
    push rsi
    push rdi
    push rbx
    mov rdi, qword [workingCDS]
    call getDiskDPB 
    pop rbx
    pop rdi
    pop rsi
    jmp .aptbHandleTerminator
.aptbInterveneEnterJoin:
;Handles join paths.
    test byte [numJoinDrv], -1  ;Test if we have any join drives
    retz    ;Return if not 
    push rsi    ;rsi already points to the next pathspec
    mov rsi, qword [fname1Ptr]
    call handleJoin ;Enters crit section, changes the CDS
    pop rsi
    return
.aptbErrorExit:
    mov al, errFnf
    mov byte [errorAction], eActUsr
    mov byte [errorClass], eClsBadFmt
    mov byte [errorLocus], eLocUnk
    stc
    return

handleJoin:
;Intervenes if the subdirectory we are entering is joined.
;This path cannot be on a net redir drive, local redir is ok.
;Input:
; rsi = First char of potential JOIN'ed pathspec.
; rbx = Ptr to the pathsep behind which we cannot traverse.
; rax = 0
;Output:
;If no match, no effect.
;If a matched path is found, working CDS, DPB and drv are set for the
; join drive. rsi is advanced to the next path componant.
;If CF=CY => Disk detection error
    push rcx
    push rbp
    mov rbp, qword [workingCDS]
    movzx ecx, word [rbp + cds.wFlags]
    test ecx, cdsRedirDrive     ;Cannot join over networks.
    jz .okToGo
    test ecx, cdsRdirLocDrive   ;If not a local redir, exit (cannot be net).
    jz .exitNoCrit
.okToGo:
    call dosCrit1Enter
    mov rbp, qword [cdsHeadPtr]
    xor ecx, ecx    ;Use as a CDS counter
.checkCDS:
    cmp word [rbp + cds.wFlags], cdsValidDrive | cdsJoinDrive
    jne .gotoNextCDS
.scanCDSName:
;Get the length of the CDS path componant to check
    push rcx
    push rdi
    push rsi        ;Have rsi point to the user path buffer
    mov rdi, rbp    ;Have rdi point to the CDS path
    call strlen     ;Get length of the path componant in ecx
    dec ecx ;Dont wanna compare the terminator
    repe cmpsb      ;Ensure strings are equal
    jnz .notString
    ;Now ensure rsi is pointing at a pathsep/terminator char too.
    lodsb   ;Get this char and advance rsi to next path componant.
    call swapPathSeparator
    jz .goodString
    test al, al
    jnz .notString
    dec rsi ;If this is a null char, point rsi back to it
.goodString:
    ;Here we know we have the right string.
    pop rcx ;Trash original rsi
    pop rdi ;Get original rdi value (i.e. our internal built path).
    pop rcx 
    ;Now store the path in the original CDS before intervening
    ; in the path
    mov qword [workingCDS], rbp  ;Save the pointer here
    push rsi    ;Save ptr to the possible null char!
    push rdi
    mov rdi, rbp    ;Needs to be called with rdi = CDS ptr
    push rbx    ;Preserve the head of the path
    call getDiskDPB ;Rebuild DPB if needed. Sets working DPB and drive
    pop rbx
    pop rdi
    pop rsi
    jc .exit ;If return with CF=CY, this failed. Error exit
    mov al, byte [workingDrv]   ;Get 0 based number
    add al, "A" ;Turn into the letter to store in CDS path
    cmp byte [rbx - 1], ":"
    je .notNet
    stc     ;Net drives should be explicitly mounted on a drive first!
    jmp short .exit
.notNet:
; Join entry intervention occurs here.
    mov byte [rbx - 2], al
    lea rdi, qword [rbx + 1]    ;Go to first byte past pathsep
.pullLp:
    lodsb
    stosb
    test al, al
    jnz .pullLp
    dec rdi
    jmp short .exit
.notString:
    pop rsi
    pop rdi
    pop rcx
.gotoNextCDS:
    add rbp, cds_size
    inc ecx 
    cmp cl, byte [lastdrvNum]
    jnz .checkCDS
.exit:
    call dosCrit1Exit
.exitNoCrit:
    pop rbp
    pop rcx
    return

checkDevPath:
;Only works in truename and in the root dir of a disk. If the search pattern
; is a char device, replace the root pathsep with a UNIX pathsep.
;Else if we are searching for the DEV folder in the root of a disk
; and the next componant we are searching for is a char device, we 
; only copy over the char device to the destination buffer with a UNIX
; pathsep. Else, we do nothing. Clobbers rbx!
;Input: rsi = Pointer to the next path spec
;       rdi = Buffer where to add it
;Output: If \DEV\devicename then copy /devicename to buffer.
;        If devicename in the root dir, then replace pathsep
    test byte [skipDisk], -1    ;If set, return!
    retnz             
;First check we are searching in the root dir!
    cmp word [rdi - 2], ":\" 
    retne
;Now check to see if fcbname is DEV
    push rax
    mov rax, "DEV     "
    cmp qword [fcbName], rax    ;x64 cant handle cmp r\m64, imm64
    pop rax
    jne .checkDevice   ;Maybe this is a device if not DEV
    cmp dword [fcbName + 8], "   \"
    jne .checkDevice    ;Was not \DEV\, check if it was \CON or something
    ;So here we are in the DEV dir. Check if \DEV or just DEV
    test byte [spliceFlag], -1
    retz    ;Exit if relative!
    ;Else, don't add DEV to the name, replace "\" with a "/", move rsi past it
    push rsi    ;Save if not followed by dev!
    push rdi    ;If followed by dev, not a problem, we end the pathbuild!
    lea rdi, fcbName
    call asciiToFCB    ;Advances rsi
    ;Returns in al the terminating char of the source string
    pop rdi
    pop rsi
    ;If al is a pathsep, fail 
    call swapPathSeparator  ;Device names cannot be terminated with a "\" or "/"
    jz .repDev   ;Thus this is not a device, normal search!
    ;mov byte [fcbName + 11], 0 ;Store terminator in fcbName field
    push rbx
    call checkIfCharDevice
    pop rbx ;Don't need bh yet
    jnc .pathSepExit    ;If this is a char dev, jump down
.repDev:
;Componant after DEV was not a char dev, replace DEV back in the count!
    mov dword [fcbName], "DEV "
    mov dword [fcbName + 4], "    "
    mov dword [fcbName + 8], "   \" 
    mov al, "\" ;\DEV\ was a dir, report it was so!
    jmp short .exit
.pathSepExit:
;Here only if the device was determined to be a char device.
    xor al, al
    mov byte [fcbName + 11], al ;Store null pathsep here!
    mov byte [rdi - 1], "/" ;Store reverse pathsep here!
.exit:
    clc
    return
.checkDevice:
    call checkIfCharDevice  ;Check if what we already have is a chardev?
    jc .exit                ;If not a device, silently return
    cmp byte [fcbName + 11], 0  ;If null, its a char dev in the root!
    je .pathSepExit
    jmp short .exit         ;Else its gonna fail! Char dev as path comp... eek!

checkIfCharDevice:  ;Int 2Fh AX=1223h
;Compares the first 8 chars of the FCB field to each device name in the
; device driver chain. 
;Output: CF=CY if not found
;        CF=NC if found
;           BH = Low byte of the device attribute word
    push rax
    push rdi
    mov rax, qword [fcbName]    ;Get the 8 char name (space padded)
    lea rdi, nulDevHdr    ;Get a ptr to the start driver header
.checkName:
    mov bx, word [rdi + drvHdr.attrib]
    xchg bh, bl ;Swap lo and hi bytes
    test bl, 80h  ;Is the driver for disk drive?
    jz .walkList ;Jump to skip ANY and ALL Disk Drives
    or bh, 20h  ;RBIL says Bit 5 set and Bits 6-7 clear if Char dev
    and bh, ~(80h|40h) ;Clear the upper two bits
    cmp rax, qword [rdi + drvHdr.drvNam]
    je .exit    ;If equal, CF=NC is already cleared
.walkList:
    mov rdi, qword [rdi + drvHdr.nxtPtr]    ;Goto the next device
    cmp rdi, -1 ;Is rdi at End of Chain?
    jne .checkName  ;If no, rdi points to char device
    stc
.exit:
    pop rdi
    pop rax
    return

buildCharDir:
;Build a dummy directory entry for the char device in FCBName
; Unless we are in disk skip mode (just evaluating the name)
    push rax
    push rdi
    mov byte [fcbName+11], 0    ;Override and null terminate the fcbName field
    lea rdi, curDirCopy
    ;Zero the directory copy (32 bytes)
    push rax
    push rcx
    push rdi
    xor eax, eax    ;Zero rax
    mov ecx, 4
    rep stosq   ;Store 4 qwords of 0 to fill directory entry with zeros
    pop rdi
    pop rcx
    pop rax
    mov dword [dirEntry], -1    ;Indicate invalid offset into dir
    mov rax, qword [fcbName]
    mov qword [rdi + fatDirEntry.name], rax  ;Store filename
    mov eax, "    "    ;Four spaces, overwrite the attribute field
    mov dword [rdi + fatDirEntry.name + filename.fExt], eax
    mov byte [rdi + fatDirEntry.attribute], dirCharDev ;Mimic DOS, set to 40h
    ;Get date and time and set the write time in the directory entry
    call readDateTimeRecord ;Update DOS internal Time/Date variables
    call getDirDTwords  ;Get date time words packed in eax
    mov dword [rdi + fatDirEntry.wrtTime], eax      ;Write as a packed dword
    xor eax, eax
    pop rdi
    pop rax
    return

checkFailingComp:
;Returns if the failing part of the path was the LAST entry of the path,
; i.e. the part we may be creating or checking for the existance of.
;Returns: ZF=ZE => Last path componant
;         ZF=NZ => Not last path componant
    push rax
    mov al, byte [fcbName + 11]
    test al, al
    pop rax
    return