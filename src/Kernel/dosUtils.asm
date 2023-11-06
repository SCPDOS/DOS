;DOS utility functions 

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
;Returns: CF=NC => Not net
;         CF=CY => Network redirector
;         ZF=ZE => Net without CDS (\\ paths only)
;         ZF=NZ => Net with CDS (disk paths ok)
;         rdi = workingCDS ptr
    mov rdi, qword [workingCDS]
    cmp rdi, -1 ;Net without CDS
    je .net
    test word [rdi + cds.wFlags], cdsRedirDrive
    jnz .net ;Net with CDS
    return  ;CF=NC => Not net
.net:
    stc ;Set Net bit
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
    jc .exit
    call findFreeClusterData    ;Get Free Cluster data in DPB
    jc .exit
    mov al, byte [rbp + dpb.bMaxSectorInCluster]
    inc al  ;Since bMaxSectorInCluster is one less than the number of sec/clus
    mov ah, byte [rbp + dpb.bMediaDescriptor]
    mov ebx, dword [rbp + dpb.dClusterCount]
    dec ebx ;This is a count of clusters + 1 so subtract 1
    movzx ecx, word [rbp + dpb.wBytesPerSector] ;Save the value in ecx
    mov edx, dword [rbp + dpb.dNumberOfFreeClusters]    ;Get # free clusters
    clc
.exit:
    call dosCrit1Exit
    return

muxGetIntVector:    ;Int 4Fh AX=1202h
;Input: al = Interrupt number
;Output: rbx = Interrupt Vector
    push rax    ;Preserve rax
    cli ;Halt interrupts
    sidt [dosIdtPtr]    ;Get the current IDT base pointer
    movzx eax, al
    shl rax, 4h     ;Multiply IDT entry number by 16 (Size of IDT entry)
    add rax, qword [dosIdtPtr.base]    
    xor ebx, ebx
    mov ebx, dword [rax + 8]    ;Get bits 63...32
    shl rbx, 10h    ;Push the high dword high
    mov bx, word [rax + 6]      ;Get bits 31...16
    shl rbx, 10h    ;Push word 2 into posiiton
    mov bx, word [rax]          ;Get bits 15...0
    sti
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
;Returns: al with 0-based drive number and CF=CY if things not oki
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
    ;Invokation via 21/5D00
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

uppercaseCharAtPtr:
;Get the char pointed to by rsi and then fall
    lodsb
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
    lea rbx, fileUCTbl    ;Get ptr to ucFilenameTable
    xlatb   ;Get converted extended byte into al
.exit:
    push rax    ;Save al temporarily
    lea rbx, asciiCharProperties
    xlatb   ;Get the signature in al
    test al, 1 ;test bit 0. Set ZF as appropriate
    pop rax
    pop rbx
    return

strlen2:    ;Int 4Fh, AX=1212h
;Entry point for Multiplex
    push rdi
    mov rdi, rsi
    call strlen
    pop rdi
    return
strlen: 
;Gets the length of a ASCIIZ string
;Input: rdi = Source buffer
;Output: ecx = Length of string
    push rax
    push rdi
    xor al, al
    xor ecx, ecx    ;ONLY USE ECX!!!
    dec ecx ;rcx = -1
    repne scasb
    not ecx
    pop rdi
    pop rax
    return

strcpy:
;Copies a null terminated string from rsi to rdi
    lodsb
    stosb
    test al, al
    retz
    jmp short strcpy

strcmp:
;Compares two ASCIIZ strings for equality.
;Input: rsi = First string
;       rdi = Second string
;       ecx = Number of bytes to compare
;Output: ZF=ZE => Equal
;        ZF=NZ => Not equal
    push rsi
    push rdi
    repe cmpsb
    pop rdi
    pop rsi
    return


normaliseFileName:  ;Int 4Fh, AX=1211h
;Converts lowercase to uppercase and / to "\"
;Input: rsi = Source buffer
;       rdi = Buffer to place normalised path
    push rax
    push rsi
    push rdi
.scan:
    lodsb
    test al, al
    jz .exit
    call swapPathSeparator  ;If it is a pathsep, swap it
    call uppercaseChar  ;Uppercase the char if it to be uppercased
    stosb
    jmp short .scan
.exit:
    pop rdi
    pop rsi
    pop rax
    return

compareFileNames:   ;Int 4Fh, AX=121Eh
;Compares two filenames char by char. Accepts invalid chars too.
;Input: rsi = One ASCIIZ pathname
;       rdi = Second ASCIIZ pathname
;Return:
    ;ZF=ZE if equal, ZF=NZ if not
    push rax
    push rsi
    push rdi
.scan:
    mov al, byte [rsi]
    test al, al
    jz .endOfString
    mov ah, byte [rdi]
    call swapPathSeparator  ;Convert al to \ if pathsep
    jz .pathseps
    or ax, 2020h    ;Convert both chars to lower case
    cmp al, ah
    jnz .exit
.nextChar:
    inc rsi
    inc rdi
    jmp short .scan
.pathseps:
    xchg ah, al
    call swapPathSeparator  ;If ah is not a pathsep, then exit ZF=NZ
    jnz .exit
    jmp short .nextChar ;Else get the next chars
.endOfString:
    test ah, ah ;If ah is also the end of the path, then ZF=ZE else ZF=NZ
.exit:
    pop rdi
    pop rsi
    pop rax
    return
checkPathspecOK:
;Input:
;rsi -> points to a path to verify if it is ok.
;Output:
;CF=NC => The path is totally clean and ok to use.
;CF=CY => the path is malformed or longer than 64 chars.
; If CF=CY and ZF=ZE then wildcards were found in the last portion
; and depending on method of invokation and caller, may be permissable.
;We accept lc and uc characters in this check function.
    push rax
    push rbx    ;Use rbx as the return flag status
    push rsi
    xor ebx, ebx    ;Clear the return status flags

    ;Start by getting the length of the ASCIIZ string.
    push rcx
    push rdi
    mov rdi, rsi
    call strlen
    cmp ecx, 64    ;Check
    pop rdi
    pop rcx
    ja .badExit ;Above 64 only as the count includes the terminating null
    ;First we verify if our path is relative or canonical (absolute)
    mov ax, word [rsi]  ;Get the first two chars
    cmp ax, "\\"    ;UNC network start
    je .netName
    cmp ax, "//"    ;Also acceptable UNC network start
    je .netName
    cmp ah, ":" ;Is this a full or CWD of drive letter relative disk path?
    je .diskPath    ;Need to check if the char preceeding is an ASCII drive char
    ;Here if relative
    test byte [dosInvoke], -1
    jnz .badExit    ;If this is -1, server invoke.
    jmp short .okToScan
.netName:
    add rsi, 2  ;Goto the first char after the \\
    jmp short .okToScan
.diskPath:
    add rsi, 2  ;Go past the X:
    test byte [dosInvoke], -1    ;If this is minus 1, this is a server invoke
    jz .okToScan
    lodsb   ;Get the third byte. It MUST be a pathsep if server invokation.
    call swapPathSeparator
    jnz .badExit    ;If ZF=NZ => Not a pathsep, bad path
.okToScan:
    lodsb   
    test al, al ;End of path char?
    jz .exit
    call swapPathSeparator
    jz .wcCheck ;If it was a pathsep, ensure no WC's have been detected
    cmp al, "*" ;Was al a big wildcard?
    je .wcFound
    cmp al, "?" ;Was al a small wildcard?
    je .wcFound ;If al was a wildcard, proceed as for wildcard
    ;Else we check that the char in al is an acceptable char
    cmp al, "." ;Ensure that dots are allowed through this part check
    je .okToScan
    call checkCharValid
    jz .badExit ;If the char is invalid, exit immediately badly
    jmp short .okToScan
.wcFound:
    mov ebx, 41h    ;Set bit 6 and bit 0
    jmp short .okToScan
.wcCheck:
;This is to check we havent had any WC's upon hitting a pathsep
    test ebx, 40h
    jz .okToScan    ;Clearly al is not a WC, so goto next char now
    ;Else fall through in error
.badExit:
    mov ebx, 1  ;Totally clear ZF and set CF
.exit:
    push rbx    ;Set bit 0 for CF and bit 6 for ZF
    popfq
    pop rsi
    pop rbx
    pop rax
    return

scanPathWC:
;Scans a path for wildcards. Used in cases where wildcards cannot be permitted
; even in the final path componant.
;Input: rsi = Pointer to the ASCIIZ filename
;Output: CF=NC => No wildcards present
;        CF=CY => Wildcards found
    push rax
    push rsi
.scan:
    lodsb
    test al, al
    jz .exit
    cmp al, "?"
    je .wcFnd
    cmp al, "*"
    jne .scan
.wcFnd:
    stc
.exit:
    pop rsi
    pop rax
    return

checkCharValid:
;If ZF=ZE => Invalid Char
;If ZF=NZ => Valid Char
    push rcx
    push rdi
    cmp al, byte [fileTermTblExt.startBadRange]
    jb .setZeroFlag
    cmp al, byte [fileTermTblExt.endBadRange] 
    jbe .setZeroFlag
    movzx ecx, byte [fileTermTbl]
    lea rdi, fileTermTbl + 1
    repne scasb
.exit:
    pop rdi
    pop rcx
    return
.setZeroFlag:
    xor ecx, ecx    ;Clear CF too
    jmp short .exit

;checkCharValid:
;If ZF=ZE => Invalid Char
;If ZF=NZ => Valid Char
    ;push rcx
    ;push rdi
    ;mov ecx, badDirNameCharL    ;Get table length
    ;lea rdi, badDirNameChar ;Point to bad char table
    ;repne scasb ;Scan. Stop when equal
    ;pop rdi
    ;pop rcx
    ;return


skipSpacesAndTabs:
;Input: rsi -> String 
;Output: rsi -> First non Space or Tab type char
    lodsb
    call isCharSpaceType
    jz skipSpacesAndTabs
    dec rsi
    return

isCharDelimType:
;Input: al = Char to check properties of
;Output:    ZF=NZ => Char not name delimiter
;           ZF=ZE => Char delimiter
    push rax
    push rbx
    lea rbx, asciiCharProperties
    xlatb
    test al, 2
    pop rbx
    pop rax
    return

isCharSpaceType:
;Input: al = Char to check properties of
;Output:    ZF=NZ => Char not Space or Tab
;           ZF=ZE => Char Space or Tab
    push rax
    push rbx
    lea rbx, asciiCharProperties
    xlatb
    test al, 4
    pop rbx
    pop rax
    return

compareFarPointers: ;Int 4Fh, AX = 1214h
;Compare if two pointers are equal. A layover from the era of far pointers.
;Input: rsi = One pointer
;       rdi = Second pointer
;Output: ZF=ZE if equal
;        ZF=NZ if not
    cmp rsi, rdi
    return

checkPathNet:
;Input: rsi = Start of path to check if it starts with two slashes
;Output: ZF=ZE => Path is a net path
;        ZF=NZ => Path is not a net path
    push rax
    movzx eax, word [rsi]    ;Get the first two chars of the path
    call swapPathSeparator
    jnz .exit
    call swapPathSeparator  ;ZF=ZE if net path
.exit:
    pop rax
    return

getCharDevDriverPtr:
;Gets a pointer to the char device driver header with the 8 char name in rax
;Input: rax = Device Driver name (space padded)
;Output: rdi = Ptr to the header, -1 => Invalid filename and CF=CY
    lea rdi, nulDevHdr  ;Point to the start of the chain
.lp:
    cmp qword [rdi + drvHdr.drvNam], rax
    rete    ;Exit if equal
    mov rdi, qword [rdi + drvHdr.nxtPtr]    ;Goto next header
    cmp rdi, -1 ;End of chain?
    jne .lp ;If not loop
    stc ;Else bad exit
    return

