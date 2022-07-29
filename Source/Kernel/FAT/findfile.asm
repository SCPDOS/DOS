;Generic Find First and Find Next functions here

searchMoreDir:
;Called in a level 1 critical section. 
;The three directory variables are set up from the ffblock. 
; WorkingDPB is setup also (hence, level 1 critical section)
;Current DTA is also used to contain the ff block address
;All registers can be trashed
    mov rbp, qword [workingDPB]
    ;First setup dirClustA and dirSect vars
    mov rdi, qword [currentDTA]
    mov eax, dword [rdi + ffBlock.parDirClus]   ;Get the directory cluster
    mov dword [dirClustA], eax  ;... into dir var
    ;Get number of 32 byte entries in a sector
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    shr ecx, 5  ;Divide number of bytes per sector by 32
    xor edx, edx
    mov eax, dword [rdi + ffBlock.dirOffset]
    div ecx ;Divide 32 byte Cluster offset by # of 32 byte entries in a sector
    ;eax has sector rel cluster value, edx has 32 byte entry rel sector value
    mov word [dirSect], ax
    mov byte [dirEntry], dl
    mov eax, dword [dirClustA]
    test eax, eax
    jnz .clusters
;Old FAT 12/16 root dirs fall thru here only
    lea rax, searchDir.oldNextEP
    push rax    ;Push return address onto the stack
    movzx eax, word [dirSect]   ;Get the root directory sector offset
    add eax, dword [rbp + dpb.dFirstUnitOfRootDir] ;Add sector 0 of root dir
    jmp short .common
.clusters:
    lea rax, searchDir.nextEp
    push rax    ;Push the return address onto stack
    mov eax, dword [dirClustA]
    call getStartSectorOfCluster    ;Get Start Sector of cluster
    movzx ebx, word [dirSect]   ;Get sector offset into the cluster
    add rax, rbx    ;Add the sector offset into the cluster
.common:
    call getBufForDOS   ;Not quite a DOS buffer but we won't be making changes
    jc searchDir.hardError
    call searchDir.setupBuffer  ;rbx has the buffer ptr for this dir sector
    movzx eax, byte [dirEntry]
    sub ecx, eax    ;Subtract the offset to get the number of entries left
    shl eax, 5  ;Multiply by 32 to turn into bytes to add to rsi
    add rsi, rax    ;rsi points to current entry in the sector.
    ;We continue AS IF this entry was bad
    ;Now setup al and rdi as upon normal entry 
    mov rdi, qword [currentDTA] ;Get FFBlock buffer to use in rdi
    mov al, byte [rdi + ffBlock.attrib]  ;Get the search attrib
    jmp findInBuffer.nextEntry  ;Proceed from within findInBuffer
    ;The return address on the stack will return to the ep's pushed

searchDir:
;Called in a level 1 critical section. 
;The three directory variables are set and the ffblock is setup
; with template, drive number and attribute fields. 
; WorkingDPB is setup also (hence, level 1 critical section)
;Current DTA is also used to contain the ff block address
;All registers can be trashed
;Return with CF=CY if no entry found in directory with al = errNoFil
    mov rbp, qword [workingDPB] ;Get the working dpb for the transfer
    mov eax, dword [dirClustA]  ;Get the cluster number to start searching at
    test eax, eax
    jz .oldRoot
    call getStartSectorOfCluster    ;Else, get the start sector in rax
.sectorLoop:
    call getBufForDOS   ;Not quite a DOS buffer but we won't be making changes
    jc .hardError
    call .setupBuffer       ;rbx has the buffer pointer for this dir sector
    call findInBuffer
.nextEp:
    retnc   ;If CF=NC, then the dir has been found and the DTA has been setup
    ;Else, we now have to get the next sector of the cluster or next cluster
    ;IF however, the next cluster is -1, then we return fail
    push qword [currClustF] ;Push currClustF and D together
    push qword [currSectC]
    push qword [currSectD]
    mov eax, dword [dirClustA]  ;Get disk relative cluster
    mov dword [currClustD], eax
    mov ax, word [dirSect]
    mov byte [currSectC], al    ;Cluster Relative sector
    mov rax, qword [rbx + bufferHdr.bufferLBA]
    mov qword [currSectD], rax  
    mov dword [currClustF], 0 ;Use as flag to tell us if cluster has changed
    call getNextSectorOfFile
    pop qword [currSectD]
    pop qword [currSectC]
    pop qword [currClustF] ;Push currClustF and D together
    jc .hardError
    cmp rax, -1
    je .fnfError    ;We are at the end of the directory and didnt find the file
    inc word [dirSect]  ;Goto next sector
    mov eax, dword [dirClustA]  ;Get disk relative cluster
    cmp eax, dword [currClustD] ;Did it change?
    je .sectorLoop  ;If not, we advanced sectors only
    mov word [dirSect], 0   ;If we did, reset this counter
    jmp .sectorLoop 

.oldRoot:
;Different search for FAT 12/16 root directories. We assume we have 
; one large contiguous cluster.
;   ecx = Number of entries per sector
.oldSectorLp:
    movzx eax, word [dirSect]    ;Move the sector number into eax
    add eax, dword [rbp + dpb.dFirstUnitOfRootDir] ;Get sector 0 of root dir
    call getBufForDOS
    jc .hardError
    call .setupBuffer       ;rbx has the buffer pointer for this dir sector
    call findInBuffer
.oldNextEP:
    retnc   ;If CF=NC, then the dir has been found and the DTA has been setup 
    inc word [dirSect]  ;Goto next sector in directory
    mov eax, dword [rbp + dpb.wNumberRootDirSectors]
    cmp word [dirSect], ax
    jb .oldSectorLp    ;If equal, no more sectors to search. Game over!
.fnfError:
    mov al, errNoFil
    stc
    return
.hardError:
    mov al, -1
    return
.setupBuffer:
    mov byte [rbx + bufferHdr.bufferFlags], dirBuffer   ;Change to dir buffer
    lea rsi, qword [rbx + bufferHdr.dataarea]   ;Set rsi to buffer data area
    movzx ecx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    shr ecx, 5  ;Divide by 32 to get # of entries in sector buffer
    return

findInBuffer:
;Input: ecx = Number of entries in sector buffer to look for
;       rsi = Sector buffer data area
;Output: CF=CY => No entries found
;        CF=NC => Entry found, directory data copied to SDA
    mov rdi, qword [currentDTA] ;Get FFBlock buffer to use in rdi
    mov al, byte [rdi + ffBlock.attrib]  ;Get the search attrib
.searchMainLp:
    mov ah, byte [rsi + fatDirEntry.attribute]  ;ah = File attributes
    and ah, ~(dirReadOnly | dirArchive) ;Avoid these two bits in search
    cmp byte [fileDirFlag], 0   ;Are we in dir only mode?
    je .exclusiveDir
    cmp al, dirVolumeID
    je .volFile
    cmp ah, al  ;If file attr <= user selected attribs, scan name for match
    ja .nextEntry
    ;rsi points to the start of the fatDirEntry in the Sector Buffer (fname)
    ;rdi points to the ffBlock to use
.scanName:
    push rsi
    push rdi
    lea rdi, qword [rdi + ffBlock.template] ;Goto name to search for
    call .nameCompare
    pop rdi
    pop rsi
    je .searchEntryFound
.nextEntry:
;Go to next entry
    add rsi, fatDirEntry_size    ;Goto next entry
    inc byte [dirEntry] ;And denote that in variable
    dec ecx
    jnz .searchMainLp
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
    push rdi
    lea rdi, curDirCopy
    mov ecx, 32/8
    rep movsq   ;Copy the directory to SDA
    pop rdi
    ;Now fill in ffBlock. rdi points to ffblock start
    mov al, byte [dirEntry]
    mov byte [rdi + ffBlock.dirOffset], al
    mov eax, dword [dirClustA]
    mov dword [rdi + ffBlock.parDirClus], eax
    mov al, byte [curDirCopy + fatDirEntry.attribute]
    mov byte [rdi + ffBlock.attribFnd], al
    mov eax, dword [curDirCopy + fatDirEntry.wrtTime] ;Get time/date together
    mov dword [rdi + ffBlock.fileTime], eax
    mov eax, dword [curDirCopy + fatDirEntry.fileSize]
    mov dword [rdi + ffBlock.fileSize], eax
    lea rdi, qword [rdi + ffBlock.asciizName]   ;Goto the name field
    lea rsi, curDirCopy
    call FCBToAsciiz    ;Convert the filename in FCB format to asciiz
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

asciizToFCB:
;Converts a filename in the form FILENAME.EXT,0 to FILENAMEEXT
;Will uppercase any lowercase chars as this could be used with user buffers.
;Names such as SYS.COM get converted to "SYS     COM"
;Name is space padded.
;Input: rsi = ASCIIZ string buffer
;       rdi = FCB name buffer
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
    cmp word [rsi + 2], " " ;Is the final char a space?
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
;Uses currentDrv and fcbName
;Input: al = Search attributes
    push rax
    push rbx
    push rsi
    push rdi
    mov rbx, qword [currentDTA]
    mov byte [rbx + ffBlock.attrib], al 
    movzx eax, byte [currentDrv]  ;Get the 0 based current drive number
    mov byte [rbx + ffBlock.driveNum], al
    lea rsi, fcbName
    lea rdi, qword [rbx + ffBlock.template]
    movsq   ;Move 8 chars
    movsw   ;Move 2 chars
    movsb   ;Move the final char
    pop rdi
    pop rsi
    pop rbx
    pop rax
    return

getDrvLetterFromPath:
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
;Determines whether the path is spliced or not and transfers the chars
; from the user buffer into an internal buffer, normalising them.
; Single and double dot entries are left as is, but the wildcard * is converted
; to ?. Wildcards can only be present in the LAST portion of the given path.
; If the portion with wildcards does not end with an ASCII null, we fail the 
; request with pnf. If there is a redirector which needs to normalise the path, 
; we let it do its thing and return.
;If the user requests data from a remote server (i.e. a pathspec starting with
; \\) then wildcards, the colon and dots are forbidden.
;If a remote user requests data (dosInvoke = -1), then the pathspec must be an
; absolute path (no wildcards or dots) and must begin with a drive letter 
; (converted from using machine name by the net client program).
;We check if we are a net invoke to ensure that the pathspec that was recieved
; was good.
;Called with:
; rdi = SDA Buffer for filename
; rsi = Potentially unqualified filename
; al = 0 => Search for Dir only. al != 0 => Search for File (or dir)
    mov byte [fileDirFlag], al  
    mov al, -1
    mov byte [spliceFlag], al   ;Set splice for Full path by default
    mov byte [filspcExist], al  ;We are searching for a file that exists
    mov qword [fname1Ptr], rdi  ;Save the SDA buffer we are using for this file
    cbw  ;make ah = -1
    mov word [lastPartOff], ax  ;Store -1 for the last part of the path
    test byte [dosInvoke], -1   ;Was it invoked via server? -1 = Server
    jz .notServer
    ;In this case, the client network program will have correctly
    ; substituted the drive letter for the path before making the request.
    ;Thus we can immediately assume the existance of a drive letter in the path 
    call getDrvLetterFromPath
    call getCDS ;Get the cds for the drive letter on the path
    ;Do nothing for now
    mov rdi, qword [workingCDS]
    call dosCrit1Enter
    call ensureDiskValid
    call dosCrit1Exit
.serverExit:
    mov al, errPnf  ;If CF=CY, use this error code
    return
.notServer:
    ;Make Redir request to qualify the filename if NOT invoked by server call
    mov qword [workingCDS], -1  ;Set workingCDS to unknown
    mov eax, 1123h
    int 4fh ;CF=CY if not resolved. CF=NC if resolved
    retnc  ;Return if resolved
    call getDrvLetterFromPath ;Get the drive letter in al (or -1)
    pushfq  ;Save the flag state on stack
    push rax    ;Save whether rsi is incremented by 2
    mov ax, word [rsi]   ;Get the word pointed to by rsi
    call swapPathSeparator  ;Convert al if it is a path separator
    xchg ah, al ;Now swap al into ah to check if we on a network path (i.e. \\)
    call swapPathSeparator  ;Returns ZF=ZE if al = "/" or "\"
    jnz .notNet
    cmp ah, al  ;If they are equal, we have a net path
    jne .notNet
    pop rax ;We are in a net situation, so rsi is pointing at "\\"
    popfq
    movsw   ;Tfr the two chars rsi, rdi + 2
.moveNetChars:
    lodsb   ;Get the third char into al and inc rsi
    call uppercaseChar  ;Make char in al uppercase
    test al, al
    jz .netEnd
    call swapPathSeparator  ;If path sep, swap it
    mov rbx, rdi    ;Store current buffer offset in rbx
    stosb
    jnz .moveNetChars  ;If not a path separating char in al, keep looking
    call .mainlp    ;Now expand the pathspec portion
    return
.netEnd:
    stosb
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
    call dosCrit1Enter
    call prepareDirCrit    ;Prepare the dir if the drive is subst/join drive
    jc .badDriveExit
.mainlp:    ;Now we transfer each directory portion
    call copyPathspecCrit  ;Now setup the filename in the FCB name field
    call searchForPathspecCrit  ;and search the directory
    jc .driveExit    ;Return if the carry flag is set, error code in al
    test al, al ;Fallthru if this pathspec was a file
    jnz .mainlp  ;Else, it was a directory name, keep looping
.driveExit:
    call dosCrit1Exit
    return
.badDriveExit:
    mov eax, errNoFil ;No file for that spec found
    jmp short .driveExit


prepareDirCrit:
;Used to transfer the current directory if it is necessary.
;Always necessary if the user specified a subst/join drive. Else only if 
; relative
;Input: al = 1-based drive letter
;Output: rdi = Pointing at where to place chars from source string
;   If CF=CY => Drive invalid or drive letter too great
    push rsi
    call setDrive   ;Set internal variables, working CDS etc etc
    jc .badDriveExit    ;If the drive number in al is too great or drive invalid
    mov rdi, qword [workingCDS] 
    push rdi    ;Push CDS pointer on stack...
    call getDiskDPB  ;Update the working DPB ptr before searching, dpbptr in rbp
    pop rsi     ; ...and get CDS pointer in rsi
    mov rdi, qword [fname1Ptr] ;Get the ptr to the filename buffer we will use
    ;If this CDS is a subst drive, copy the current path to backslashOffset
    ;If this CDS is a join drive, copy the first 3 chars and up to the next 
    ;   terminating char (\, / or Null)
    ;If the path is to be spliced, then we copy the whole CDS current path
    ;If the CDS is not subst drive, nor to be spliced, we copy first two chars.
    test word [rsi + cds.wFlags], cdsJoinDrive
    jnz .prepDirJoin
    test word [rsi + cds.wFlags], cdsSubstDrive
    jnz .prepDirSubst
.prepMain:
;Ok so now preliminary copy complete, now we check if path spliced
    test byte [spliceFlag], -1
    jz .prepLoop ;If this flag is zero, we loop
    ;Else we copy the first two chars only (X:)
    movsw  
    mov al, "\"
    stosb   ;Store the path separator and increment rdi
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
    call .prepSetupDirSearchVars
    clc ;Clear carry before exiting
.badDriveExit:
    pop rsi
    return
.prepDirJoin:
    push rcx
    push rsi
    add rsi, 2  ;Goto the backslash for the dir
    mov ecx, 2  ;Instantiate ecx to copy X:
.prepDirJoin1:
    lodsb   ;Get the char
    test al, al ;Null char?
    jz .prepDirJoin2
    call swapPathSeparator
    jz .prepDirJoin2
    inc ecx ;Accrue length to copy
    jmp short .prepDirJoin1
.prepDirJoin2:
    pop rsi ;Return rsi to the start of the buffer
    jmp short .prepDirCopy1
.prepDirSubst:
    push rcx
    movzx ecx, word [rdi + cds.wBackslashOffset]
.prepDirCopy1:
    rep movsb   ;Copy the string over
    pop rcx
    test byte [spliceFlag], -1
    jnz .prepDirExit    ;If not relative, exit as we put the "root dir" marker
    jmp short .prepLoop ;Else, need to copy CDS now too as part of path
.prepSetupDirSearchVars:
;Input: eax = Starting Cluster of search on disk (0=Root dir)
;       rbp = DPB pointer for the device which will do transaction
    push rcx
    xor ecx, ecx
    mov word [dirSect], cx  ;Always start searching at sector 0 of dir cluster
    mov byte [dirEntry], cl ;Always start at entry 0 of the sector in cluster
    call getFATtype ;Get type of fat
    cmp ecx, 2  ;2 = FAT32
    jne .psdsvExit      ;FAT 12/16 jump and store 0 if at root
    ;FAT 32 here
    test eax, eax   ;Are we looking for root dir of FAT32 drive?
    jnz .psdsvExit  ;If not, store the cluster number unchanged
    mov eax, dword [rbp + dpb.dFirstUnitOfRootDir]  ;Else get cluster number
.psdsvExit:
    mov dword [dirClustA], eax  ;Store directory cluster (or 0 if \ on FAT12/16)
    pop rcx
    return 

copyPathspecCrit:
;1) Copies a path portion from the source buffer to the destination
;2) Advances rsi to the next null, \ or /
;3) Expands all * to ?'s
;4) Understands \. means "this directory" and can be ignored with rsi moving to
;    next path separator
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

    mov ecx, 8 ;8 chars to move over, when ecx = 0, the char must be . or term
    mov ch, 1   ;Set that we are in name field
    lodsb   ;Get first char from user path in al
    cmp al, "."   ;Handle starting dot separately
    je .cpsDots
    dec rsi ;Else move rsi to point back to starting char
;First char is not a dot, so now check if starts with E5h? 
;If so, store 05h in its place
    cmp al, 0E5h
    jne .cpsMainLoop
    inc rsi ;Push rsi to point to next char
    mov al, 05h
    stosb   ;Store the char, rsi is pointing at next char
    dec cl  ;One less char to tfr
.cpsMainLoop:
    lodsb   ;Get the char in al and advance rsi
    test al, al ;Is it the null char?
    jz .cpsProcessName  ;If so, terminate
    call swapPathSeparator  ;And if it is a pathsep, convert it before exiting
    jz .cpsProcessName
    cmp al, "." ;Filename extension separator
    je .cpsExtension
    cmp ecx, 0100h  ;If ch = 1 and cl = 0, then look for either . or terminator
    je .cpsMainLoop
    jecxz .cpsCharSkip ;If ch = 0 and cl = 0, scan for next terminator
    ;If we have space in the filename, we check to see if the next char is *
    cmp al, "*" ;Wildcard?
    je .cpsWildcard
    call uppercaseChar  ;Uppercase the char if it needs to be...
    call checkCharValid ; and check it is a valid char
    je .cpsInvalidChar  ;If it is not valid, replace with 0 and exit
    stosb   ;And store the converted char in al and inc rdi
    dec cl  ;One less char left to tfr
    jmp short .cpsMainLoop
.cpsInvalidChar:
    xor al, al
    jmp short .cpsProcessName
.cpsExtension:
;rsi has been incremented past the extension field. Discard the . in al
    mov ecx, 3  ;Set to 3 chars left, in extension (ch = 0)
    lea rdi, qword [fcbName + filename.fExt]    ;Goto the extension field
    jmp short .cpsMainLoop
.cpsDots:
    stosb   ;Store the first dot
    lodsb   ;Check now if we have a second dot
    cmp al, "."
    jne .cpsCharSkip
    stosb   ;Store the second dot
.cpsCharSkip:
    call .cpsPtrSkip    ;Now we are skipping any chars that arent terminators
    jmp short .cpsProcessName
.cpsWildcard:
    ;cl has the number of chars of ? to store 
    mov al, "?"
    push rcx
    movzx ecx, cl   ;Temporarily extend cl to ecx
    rep stosb   ;Store that many ? in buffer and return cl to 0
    pop rcx
    test ch, 1  ;Is this bit set? If so, we jump to .cpsExtension
    jnz .cpsExtension   ;Now fill the extension field
    ;Else, we process filename
    jmp short .cpsCharSkip
.cpsPtrSkip:
;Now advance rsi past the next pathsep or null char
;If an invalid char is detected, it is considered to be a terminator
;Output: al = Terminator char (either \ or null)
;        rsi -> First char of next pathspec (if al = \)
    lodsb
    call checkCharOk
    je .cpsBadChar
    test al, al ;Is this null?
    retz
    call swapPathSeparator
    retz
    jmp short .cpsPtrSkip
.cpsBadChar:
    xor al, al  ;Convert the char to a terminator
    return
.cpsProcessName:
;Store the final char in the 12 space in the FCB name field
    lea rdi, fcbName+11
    stosb   ;Store the terminator in this slot. 0 for End of Path, \ for subdir
    pop rdi
    return

searchForPathspecCrit:
    ;Now search the current directory for this filename
    ;Find first using SDA ffBlock
    ;If al = 0, we have a file name
    ;If al = \, we have subdirectory. NO WILDCARDS ALLOWED IF PATHSEP
    push rsi    ;Save the current position of the pointer in the user buffer
    push rdi    ;Save current position to store filename in internal buffer
;Evaluate whether we are searching for a file for a directory
    test al, al
    jz .sfpPNfile
    ;Fall if subdir
    lea rdi, fcbName
    mov al, "?" ;Search for wildcard
    mov ecx, 12
    repne scasb
    je .sfpPnf  ;Path not found if a ? found in the name
    mov al, dirDirectory    ;We want a directory only search.
    mov byte [fileDirFlag], 0   ;Set to search exclusively for a dir
    jmp short .sfpPNMain
.sfpPNfile:
    ;Here if we are searching for a file
    mov byte [fileDirFlag], -1  ;Search for file or dir according to attribs
    movzx eax, byte [searchAttr]    ;Get the search attributes
.sfpPNMain:
    call adjustSearchAttr   ;Edit the search attributes as needed
    call setupFFBlock   ;Sets up the internal ff block, attribs in al
    ;Now the internal ff block is setup, conduct search.
    call searchDir
    pop rdi ;rdi points to free space in internal filename buffer
    pop rsi
    jc .sfpSearchError
;Here if a file was found. First check the FCB pattern to see if it was . or ..
    cmp byte [fcbName], "."   ;Handle destination pointer for  
    je .sfpPNDots
    ;Copy filename over to internal buffer
    push rsi    ;Save source pointer position
    mov rsi, qword [currentDTA]
    lea rsi, qword [rsi + ffBlock.asciizName]    ;Get asciiz name ptr
.sfpPNtfrName:
    lodsb
    test al, al
    jz .sfpPNlastchar   ;Keep copying until null found, DONT PRINT IT
    stosb
    jmp short .sfpPNtfrName ;Keep looping
.sfpPNlastchar:
    pop rsi ;Get back src ptr which points to first char in next pathspec
    mov al, byte [rsi - 1]  ;Get the prev pathspec term char in al
    test al, al ;If null, return it
    jz short .sfpPNexit
    mov al, "\" ;Else, return path
.sfpPNexit:
    stosb   ;Store this final char (either \ or NULL) and return
    return
.sfpPNDots:
;For one dot, we leave rdi where it is
;For two dots, we search backwards for the previous "\"
    cmp byte [fcbName + 1], "." ;Was the second char also a dot?
    retne   ;Return with rdi untouched and rsi advanced.
    ;Here we have two dots
    ;Walk rdi backwards until a \ is found
    dec rdi  ;rdi points to current char. Preceeding it is a \. Skip that
.sfpPNDotsLp:
    dec rdi
    cmp byte [rdi], "\"
    jne .sfpPNDots  ;Keep looping around until it is a "\"
    inc rdi ;Go past that pathsep
    return
.sfpSearchError:
    mov eax, errNoFil
    jmp short .sfpErrExit
.sfpPnf:
    mov eax, errPnf
    jmp short .sfpErrExit
.sfpFnf:
    mov eax, errFnf
.sfpErrExit:
    stc ;Set carry
    return

checkDevPath:
;Called only if the file/directory was not found on disk.
;Checks if the current fcbname field is "DEV        \" (for the DEV 
; pseudo-directory). If it is, then we parse the next filename in to fcbName
; and check to see if it is a char device. If it is, build an ffblock.
; If it is not, proceed with the request fail.
;
;Input: rsi = Pointer to the next path spec
    cmp byte [fcbName + 11], 0  ;If the fcbname is a file name, exit
    rete                        
    ;Now check to see if fcbname is the DEV directory (could be real...)
    push rax
    mov rax, "DEV     "
    cmp qword [fcbName], rax    ;x64 cant handle cmp r\m64, imm64
    pop rax
    jne checkDeviceName
    cmp dword [fcbName + 8], "   \"
    jne checkDeviceName
    ;So the failed directory was DEV, now we search to see if we are
    ; looking for a device driver
checkDeviceName:
;Compares the first 8 chars of the FCB field to each device name in the
; device driver chain. Creates a dummy FFblock for them.
;Input: Nothing
;Output: ZF=ZE => fcbname a Character Device
;        ZF=NZ => fcbname NOT a device name
;If returns with CF=CY, fail request as date/time driver failed.
    push rax
    push rdi
    mov rax, qword [fcbName]    ;Get the 8 char name (space padded)
.altEp:
    lea rdi, nulDevHdr    ;Get a ptr to the start driver header
.checkName:
    test qword [rdi + drvHdr.attrib], devDrvChar  ;Is the driver for disk drive?
    jz .walkList ;Jump to skip ANY and ALL Disk Drives
    cmp rax, qword [rdi + drvHdr.drvNam]
    je .deviceFound
.walkList:
    mov rdi, qword [rdi + drvHdr.nxtPtr]    ;Goto the next device
    cmp rdi, -1 ;Is rdi at End of Chain?
    jne .checkName  ;If no, rdi points to char device
    dec rdi ;Clear ZF and CF before returning (-1 -> -2)
.exit:
    pop rdi
    pop rax
    return
.deviceFound:
;Now build a dummy directory entry for the char device
    lea rdi, curDirCopy
    ;Zero the directory copy (32 bytes)
    push rax
    push rcx
    push rdi
    xor eax, eax    ;Zero rax
    mov ecx, 4
    rep movsq   ;Store 4 qwords of 0 to fill directory entry with zeros
    pop rdi
    pop rcx
    pop rax

    mov qword [rdi + fatDirEntry.name], rax  ;Store filename
    mov eax, "    "    ;Four spaces, overwrite the attribute field
    mov dword [rdi + fatDirEntry.name + filename.fExt], eax
    mov byte [rdi + fatDirEntry.attribute], 40h ;Mimic DOS, set attr to 40h
    ;Get date and time and set the write time in the directory entry
    call readDateTimeRecord ;Update DOS internal Time/Date variables
    jc .exit  ;If we fail to get time/date, fail the request with CF=CY
    ;Build date and time words
    call getDirDTwords  ;Get date time words packed in eax
    mov word [rdi + fatDirEntry.wrtTime], ax
    shr eax, 16 ;Eject the time, get the date in eax
    mov word [rdi + fatDirEntry.wrtDate], ax
    xor eax, eax    ;Clear CF and set ZF
    jmp short .exit