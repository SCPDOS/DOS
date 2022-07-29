;DOS utility functions (Will be made available through Int 4F ah=12xx eventually)

;Basic Drive related Utilities
;Any function which takes args in rax (or any subpart of it), has that 
; argument provided on the stack when called from Int 4Fh interface (when 
; that gets set up)

setWorkingDPB:
;Gets dpb in rbp and saves to workingDPB
    mov qword [workingDPB], rbp
    return

testCDSNet:
;Checks if the workingCDS is a redirector drive
;Return: rdi = workingCDS
;        CF=NC => Net
;        CF=CY => Not net or invalid CDS
    mov rdi, qword [workingCDS]
    cmp rdi, -1
    je .notNet
    test word [rdi + cds.wFlags], cdsRedirDrive
    jnz .notNet ;Carry flag will always be clear
    return
.notNet:
    stc
    return

getDiskData:
;This function returns:
;al = sectors per cluster
;ah = media ID byte
;ebx = total clusters
;cx = bytes per sector
;edx = number of available clusters
;
;If CF=CY on exit, al contains the error code
    call testCDSNet ;Test if its a netCDS and puts CDS ptr in rdi
    jnc .physical
    ;Beep a redir request out
    mov eax, 110Ch 
    int 4Fh
    return
.physical:
;Now we must lock the structures
    mov byte [errorLocus], eLocDsk
    call dosCrit1Enter  ;Enter class 1 critical section
    call getDiskDPB ;Get disk dpb pointer in rbp for CDS in rdi
    call dosCrit1Exit
    jc .exit
    call findFreeClusterData    ;Get Free Cluster data in DPB
    jc .exit
    mov al, byte [rbp + dpb.bMaxSectorInCluster]
    inc al  ;Since bMaxSectorInCluster is one less than the number of sec/clus
    mov ah, byte [rbp + dpb.bMediaDescriptor]
    mov ebx, dword [rbp + dpb.dClusterCount]
    movzx ecx, word [rbp + dpb.wBytesPerSector] ;Save the value in ecx
    mov edx, dword [rbp + dpb.dNumberOfFreeClusters]    ;Get # free clusters
    clc
.exit:
    return

muxGetIntVector:    ;Int 4Fh AX=1202h
;Input: al = Interrupt number
;Output: rbx = Interrupt Vector
    push rax    ;Preserve rax, segment selector returned by call
    push rdx    ;Preserve rdx, Attribute Word returned by call
    mov bl, al  ;Get the interrupt vector number into bl
    mov eax, 0F007h
    int 35h
    pop rdx
    pop rax
    return

getUserRegs:   ;Int 4Fh AX=1218h
;Returns ptr to user regs in rsi
    mov rsi, qword [oldRSP]
    return

walkDPBchain:
;Called with al = 0 based drive number
;Returns in rsi a pointer to the DPB or if CF=CY, invalid drive number
    mov rsi, qword [sftHeadPtr]  ;Get variable pointing to first DPB
.walk:
    cmp rsi, -1
    je .exitBad
    cmp byte [rsi + dpb.bDriveNumber], al
    je .exit    ;Drive found
    mov rsi, qword [rsi + dpb.qNextDPBPtr]  ;Go to next drive 
    jmp short .walk
.exitBad:
    stc
.exit:
    return
setDrive:   
;Gets a drive CDS, sets it as working and checks it is a valid physical drive
;Input: al = 1-based drive number
;Output: al = 0-based drive number
;   CF=NC => Drive can be set as Current Drive (i.e. Not Network or Join)
;   CF=CY => 0-based drive number invalid OR CDS returned with Net or Join flags
;            set.
    call getCDS ;Setup working CDS DOS variable for this drive
    jc .exit    ;Carry the CF flag if not Physical or if al was too large
    push rsi
    mov rsi, qword [workingCDS] ;Get CDS
    test word [rsi + cds.wFlags], cdsJoinDrive  ;Check if Join
    pop rsi
    jz .exit
    stc
.exit:
    return

buildNewCDS:   ;Int 4Fh AX=121Fh
;Allows a redirector or subst/join to build a CDS
;Input drive letter must be above the reserved CDS entries for the system 
; volumes, that are made at system boot.
;Input: al = Drive Letter for drive
;       workingCDS = Set to the CDS array slot for the drive
;Output: rdi = newly filled in workingCDS
;CF=NC => CDS valid and has a DPB
;CF=CY => Either drive letter not ok OR No DPB for drive
    push rax
    sub al, "A"-1
    cmp al, byte [numPhysVol]    ;al must be bigger than # of block drives
    mov rdi, qword [workingCDS] ;Get CDS pointer
    mov word [rdi + cds.wFlags], 0  ;Nullify CDS (mark as invalid)
    pop rax
    jb .exit    ;Exit with CF=CY
    push rax
    or eax, 005C3A00h   ;Add path componants to eax, 5Ch=\, 3Ah=:
    mov dword [rdi + cds.sCurrentPath], eax  ;Since al has valid drive letter
    pop rax
    or word [rdi + cds.wFlags], cdsValidDrive    ;Config bit set
    mov dword [rdi + cds.dStartCluster], 0  ;Root dir
    mov qword [rdi + cds.qReserved], 0   ;Optional redir signature field
    mov word [rdi + cds.wBackslashOffset], 2    ;Skip letter and :
    ;Search for a DPB for the CDS if it is based on a physical device
    push rax
    push rsi
    sub al, "A" ;Get 0 based drive letter
    call walkDPBchain
    jb .skipSettingDPB
    mov qword [rdi + cds.qDPBPtr], rsi  ;Save DPB pointer for drive
.skipSettingDPB:
    pop rsi
    pop rax
.exit:
    return

getCDS:     ;Int 4Fh AX=1219h
;Gets the device DPB and saves it in the DOS variable
;This can be called to get CDS for network drives too!
;Input: al = 1 based drive number
;Sets workingCDS var with the CDS for the device. 
;   If device on a network, sets CF
;Returns al with 0-based drive number
    test al, al
    jnz .skip
    mov al, byte [currentDrv]   ;Get current drive
    inc al
.skip:
    dec al  ;Convert to 0 based (0=A: ...)
    push rsi
    mov byte [errorLocus], eLocDsk  ;Set the locus
    test byte [dosInvoke], -1   ;If non-zero, invalid
    jz .physDrive
    ;Invokation via 21/5D00, not yet fully supported
    ;If returned with CF=CY, consider it an error for now
    push rax
    push rdi
    lea rdi, tmpCDS ;Get the temporary CDS buffer
    mov qword [workingCDS], rdi ;Make it current
    add al, "A" ;Convert to a drive letter
    call buildNewCDS    ;Build a new CDS
    test word [rdi + cds.wFlags], cdsValidDrive  ;Is the CDS valid?
    pop rdi
    pop rax
    jz .exitBad    ;If the valid flag not set, fail!
    jmp short .exitOk   ;All oki
.physDrive:
    call getCDSforDrive ;Get CDS pointer in RSI and in curCDSPtr
    jc .exitBad
    test word [rsi + cds.wFlags], cdsValidDrive
    jnz .exitOk ;Exit with flag cleared
    ;Else Return to unknown error locus
.exitBad:
    mov byte [errorLocus], eLocUnk
.exitBad1:
    stc
.exitOk:
    pop rsi
    return

getCDSforDrive:     ;Int 4Fh AX=1217h
    ;Gets the CDS for the current drive in al
    ;Input: al = Drive number, 0 = A ...
    ;Output: CF=NC => rsi = Pointer to CDS for drive in al (and workingCDS var)
    ;        CF=CY => al not valid
    cmp al, byte [lastdrvNum]
    jb .ctn
    stc 
    return
.ctn:
    push rax
    push rdx
    movzx eax, al
    mov rsi, cds_size
    xor edx, edx
    mul esi ;Get the multiples of CDS's to skip in rax
    mov rsi, qword [cdsHeadPtr] ;Get the first CDS ptr
    add rsi, rax    ;Add the CDS array to the offset into it
    mov qword [workingCDS], rsi  ;Save in data area
    pop rdx
    pop rax
    clc
    return


swapPathSeparator:  ;Int 4Fh, AX=1204h, Normalise Path Separator
;Swap / to \ in a path. Leave all other chars alone.
;Input: AL = Char to normalise.
;Output: AL = Normalised Char (if / swap to \. Leave all other chars alone)
;If path separator, set ZF=ZE
    cmp al, "\"
    je .exit
    cmp al, "/" ;Will set ZF=ZE if / (aka, path separator)
    jne .exit
    mov al, "\" ;Set char in al to normal path separator
.exit:
    return

uppercaseChar:      ;Int 4Fh, AX=1213h, Uppercase Char
;Convert a lowercase char to uppercase
; Leave alone uppercase chars and invalid chars
;Input: al = Char to convert to uppercase
;Output: al = Processed char
    push rbx
    cmp al, "a"
    jb .exit
    cmp al, "z"
    ja .checkExt
    sub al, "a" - "A"   ;If lc char, convert to uc, then check ext status
.checkExt:
    cmp al, 80h ;Extended ASCII first char
    jb .exit
    sub al, 80h ;Turn into table offset
    lea rbx, extAsciiTbl
    xlatb   ;Get converted extended byte into al
.exit:
    push rax    ;Save al temporarily
    lea rbx, extAsciiSig
    xlatb   ;Get the signature in al
    test al, 1 ;test bit 0. Set ZF as appropriate
    pop rax
    pop rbx
    return
checkPathspecOK:
;Input:
;rdx -> points to a path to verify if it is ok.
;Output:
;CF=NC => The path is totally clean and ok to use.
;CF=CY => the path is malformed and may be used ONLY if ZF=ZE. 
; If ZF=ZE then the only malformed chars are path separators and may be used.
;Path separators are \, / and : for drive separation (not counted in ZF setting)

;Full paths may start with \\<15-char machine name>\...
; or <Drive Letter>:\...
;CWD relative paths start with <Drive Letter>:...
;Current Drive Root Relative paths may start with \...
;Current Drive CWD relative paths start with any other char
;FOR NOW, we allow redir but no servers. Too much hassle for now

;We accept lc and uc characters in this check function.
    push rax
    push rcx
    push rsi
    push rdi
    pushfq
    pop rcx ;Get flags into ecx
    and ecx, ~41h  ;Clear CF and ZF to begin
    ;First we verify that the first two chars are ok (either X: or \\ or chars)
    mov ax, word [rdx]  ;Get the first two chars
    cmp ax, "\\"    ;UNC network start
    je .okToScanNet
    cmp ax, "//"    ;Also acceptable UNC network start
    je .okToScanNet
    cmp ah, ":" ;Is this a full or CWD of drive letter relative disk path?
    je .diskPath    ;Need to check if the char preceeding is an ASCII drive char
    ;Is this a current drive CWD relative or current drive root relative path?
    ;If so the pathspec is not different to normal, goto scanLoop
    jmp short .scanLoop
.okToScanNet:
    or ecx, 41h  ;Set ZF and CF to indicate "technically" a bad char
    jmp short .okToScan
.badExit:
    and ecx, ~40h    ;Clear ZF if ZF was set
    or ecx, 1   ;Set carry flag
    jmp short .exit
.diskPath:
;Disk Letter must be A-Z (or a-z)
    cmp al, "A"
    jb .badExit
    cmp al, "z"
    ja .badExit
    cmp al, "a"
    jae .okToScan
    cmp al, "Z"
    ja .badExit
.okToScan:
    add rsi, 2  ;Skip first two chars now
.scanLoop:
    lodsb   ;Get char, inc rsi
    test al, al  ;Is al=0, i.e string terminator?
    je .exit
    call checkCharValid ;Validity check
    ;If ZF=ZE, check if it is a path separator
    jnz .scanLoop   ;Char was ok if ZF=NZ, loop around
    cmp al, "\"
    je .pathSepFnd
    cmp al, "/"
    je .pathSepFnd
    call checkCharOk    ;Check if it is ok, i.e. a period or wildcard
    jne .scanLoop    ;Pretend they are alright!
    jmp short .badExit
.pathSepFnd:
    or ecx, 41h  ;Set ZF and CF to indicate "technically" a bad char    
    jmp short .scanLoop
.exit:
    push rcx    ;Push flags back on
    popfq   ;And pop them into the flags register
    pop rdi
    pop rsi
    pop rcx
    pop rax
    return

checkCharValid:
;If ZF=ZE => Invalid Char
;If ZF=NZ => Valid Char
    push rcx
    push rdi
    mov ecx, badDirNameCharL    ;Get table length
    lea rdi, badDirNameChar ;Point to bad char table
    repne scasb ;Scan. Stop when equal
    pop rdi
    pop rcx
    return
checkCharOk:
;Same as checkCharValid except DOES not return error on * ? \ / .
    cmp al, "."
    je .exitOk
    cmp al, "*"
    je .exitOk
    cmp al, "?"
    je .exitOk
    cmp al, "\"
    je .exitOk
    cmp al, "/"
    je .exitOk
.ok:
    jmp short checkCharValid
.exitOk:
    push rax
    or al, 1    ;Always clears the ZF
    pop rax
    return