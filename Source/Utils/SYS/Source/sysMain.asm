startSys:
    jmp short .cVersion
.vNum:          db 1
.sectorSize:    dw 200h
.cVersion:
    push rax
    mov ah, 30h
    int 41h
    cmp al, byte [.vNum] ;Version 1
    jbe .okVersion
    pop rax
    lea rdx, badVerStr
    jmp badPrint ;Exit to caller or DOS to print bad command interpreter line
.okVersion:
;Check the passed argument is ok (flag in al)
    pop rax
    cmp al, -1
    jnz .driveOk
.badDrive:
    lea rdx, badDrvLtr
    jmp badPrint
.driveOk:
;Now fetch the drive we are working on
    mov dl, byte [r8 + psp.fcb1] ;Get the fcb 1 based drvNum
    dec dl  ;Turn it into a 0 based number
    mov byte [sysDrive], dl
    add dl, "A"
    mov byte [rootDir], dl
    mov byte [biosDest], dl
    mov byte [dosDest], dl
    ;Now get current drive
    mov eax, 1900h  ;Get current drive as the source of the copy
    int 41h
    add al, "A" ;Convert to an ASCII char
    mov byte [biosFile], al
    mov byte [dosFile], al
;Now we check that the drive specified is a physical device
    mov ah, 52h
    int 41h ;Get in rbx a ptr to list of lists
    add rbx, 2Ah    ;Point rbx to cdsHeadPtr
    mov rsi, qword [rbx]    ;Get the ptr to the CDS array
    movzx ecx, byte [sysDrive]
    jecxz .atCurrentCDS
.walkCDSArray:
    add rsi, cds_size
    dec ecx
    jnz .walkCDSArray
.atCurrentCDS: 
    test word [rsi + cds.wFlags], cdsJoinDrive | cdsSubstDrive | cdsRedirDrive
    jnz .badDrive
    mov rsi, qword [rsi + cds.qDPBPtr]  ;Get the DPB ptr 
    mov qword [dpbPtr], rsi ;Save the DPB ptr
;Now step 1, search root dir, if a file is found
    lea rdx, rootDir
    mov ecx, dirInclusive
    mov eax, 4E00h
    int 41h
    jnc .fileFound
    cmp al, errFnf
    je .rootDirOk
.badRoot:
    lea rdx, badRootDir
    jmp badPrint
.fileFound:
;A File was found, if it is a Volume Label, do a find next
    lea rdi, qword [r8 + psp.dta]   ;Get ptr to the find first block
    cmp byte [rdi + ffBlock.attrib], dirVolumeID
    jne .badRoot
    mov eax, 4F00h  ;Find Next
    int 41h
    jnc .badRoot    ;Another file found, exit bad
    cmp al, errNoFil
    jne .badRoot
.rootDirOk:
    clc ;Clear CF
    mov eax, 1C00h  ;Get FAT information
    mov dl, byte [sysDrive]
    inc dl  ;Convert to 1 based number
    int 41h
    mov word [sectorSize], cx   ;Move the sector size in, use for buffer size
    mov byte [secPerClus], al   ;Save the number of sectors per cluster too
    mov dword [clustCnt], edx   ;Save the count of clusters
    lea rdx, badSecSize
    cmp cx, word [startSys.sectorSize]    ;Temporary restriction for DOS 1.0.
    jne badPrint
;Now open two handles to the two files
    call dosCrit1Enter

    lea rsi, biosNamePair
    lea rdi, biosPair
    mov ecx, biosNameL
    call openFiles
    jc badHandle

    lea rsi, dosNamePair
    lea rdi, dosPair
    mov ecx, dosNameL
    call openFiles
    jc badHandle

    lea rsi, biosPair
    call copyFile
    jc badXfer

    lea rsi, dosPair
    call copyFile
    jc badXfer

;We close these handles here to update the DIR entries
    mov eax, 3E00h
    mov bx, word [biosHdlDst]
    int 41h
    mov eax, 3E00h
    mov bx, word [dosHdlDst]
    int 41h

;Now we must do the fun part, setting the Boot Sector details
;Start by searching sysDrive for new files. 
;FOR SCPBIOS.SYS, need the start sector of the file and the number of sectors
; it is.
;FOR SCPDOS.SYS, need only the start sector of the file.

;We do this by:
; - Reading the DPB, getting the first sector of the Root Directory
; - Read the necessary data from the Root Directory Copy
; - Read the BPB again
; - Set the variables in the BPB.
; - Set the bootable flag in the BPB
; - Write it back out to disk.
;
;3 Direct IO's needed. 
;   1) Read Sector 1 of Root Dir
;   2) Read BPB
;   3) Write back BPB 
    mov rbp, qword [dpbPtr]
    xor eax, eax    ;Request first sector of root directory
    call getStartSectorOfCluster    ;Thanks DOS!!
    mov qword [xfrSector], rax
    ;rax has the first sector of the root directory, read it in
    call readWrapper
    jc badPrint
    ;First entry could be volume id so double check
    mov rsi, qword [memoryBlock]
    test byte [rsi + fatDirEntry.attribute], dirVolumeID
    jz .notVolID
    add rsi, fatDirEntry_size   ;Goto next entry. This must be it
.notVolID:
    movzx edx, word [rsi + fatDirEntry.fstClusHi]   ;Get hi bits of cluster num
    movzx eax, word [rsi + fatDirEntry.fstClusLo]
    shl edx, 10h    ;Move into high word
    or eax, edx     ;eax has the first cluster
    call getStartSectorOfCluster    ;Get the first sector to read
    mov qword [biosSector], rax
    xor edx, edx
    mov eax, dword [rsi + fatDirEntry.fileSize] ;Get file size in bytes
    ;Divide by sector size
    movzx ebx, word [sectorSize]    ;Get the sector size
    div ebx
    test edx, edx   ;If no remainder, eax has number of sectors for file
    jnz .skipPlusSector1
    inc eax
.skipPlusSector1:
    mov word [biosSize], ax
    add rsi, fatDirEntry_size   ;Goto next entry
    movzx edx, word [rsi + fatDirEntry.fstClusHi]   ;Get hi bits of cluster num
    movzx eax, word [rsi + fatDirEntry.fstClusLo]
    shl edx, 10h    ;Move into high word
    or eax, edx     ;eax has the first cluster
    call getStartSectorOfCluster    ;Get the first sector to read
    mov qword [dosSector], rax

    ;Now we read the BPB in
    xor eax, eax
    mov qword [xfrSector], rax  ;Read sector 0
    call readWrapper
    jc badPrint
    ;Now we have the BPB in, set the variables and we are done.
    ;First set the variables that are past the end of the Boot Sector
    ;Then set the bootable flag and write back
    mov rbp, qword [dpbPtr]
    call getFATtype ;If ecx = 2, we have a 90 byte BPB, else 62 bytes
    mov eax, 62
    mov ebx, 90
    cmp ecx, 2
    cmove eax, ebx 
    ;eax has the number of bytes the BIOS file vars are
    mov rbx, qword [memoryBlock]
    lea rsi, qword [rbx + rax]
    mov rax, qword [biosSector]
    mov ecx, dword [rbx + bpb.hiddSec]  ;Must add hidden sectors too
    add rax, rcx
    mov qword [rsi], rax
    movzx eax, word [biosSize]
    mov word [rsi + 8], ax
    ;Now to the table at the end of the boot sector
    movzx eax, word [sectorSize]
    dec eax ;Last byte in the sector is byte sectorSize-1
    lea rsi, qword [rbx + rax - 14] ;Point to the start of the packet
    mov rax, qword [dosSector]
    add rax, rcx
    mov qword [rsi + sysInitTableStruc.firstLba], rax
    mov byte [rsi + sysInitTableStruc.bootable], -1
    call writeWrapper
    jc badPrint
exit:
    call dosCrit1Exit
    call freeResources
    lea rdx, okMsg
    mov eax, 0900h
    int 41h
    mov eax, 4C00h
    int 41h

badXfer:
    lea rbx, badMem
    lea rdx, badCopy
    cmp ecx, -1 ;Little flag to indicate memory issue
    cmove rdx, rbx
    jmp short badPrint
badHandle:
    lea rbx, badCreate
    lea rdx, badOpen
    test esi, esi
    cmovz rdx, rbx
badPrint:
;Generic Print entry point
    mov eax, 0900h
    int 41h
badExit:
;DOS will close the handles for us if needed
    call freeResources
    mov eax, 4CFFh
    int 41h

;Utility functions
freeResources:
    test byte [inCrit], -1
    jz .noCrit
    call dosCrit1Exit   ;Exit critical section
.noCrit:
    movsx ebx, word [biosHdlSrc]
    cmp ebx, -1
    call freeHandle
    movsx ebx, word [biosHdlDst]
    cmp ebx, -1
    call freeHandle
    movsx ebx, word [dosHdlSrc]
    cmp ebx, -1
    call freeHandle
    movsx ebx, word [dosHdlDst]
    cmp ebx, -1
    call freeHandle
    mov r8, qword [memoryBlock]
    test r8, r8
    retz
    mov eax, 4900h  ;Free block
    int 41h
    return
freeHandle:
    retz    ;Return if equal
    mov eax, 3E00h
    int 41h
    return

openFiles:
;Input: rsi = Name Pair ptr
;       rcx = Name Pair length
;       rdi = Handle pair ptr
;Return:
;   If CF=CY and esi = 0, error on create, else error on open
    mov rdx, rsi
    mov eax, 3D00h  ;Open in Read-Only mode
    int 41h
    retc
    mov word [rdi], ax  ;Place it in the source handle

    add rsi, rcx    ;Move the name forwards by its length
    mov rdx, rsi
    xor esi, esi    ;Indicate we are in the create phase now
    mov ecx, dirReadOnly | dirHidden | dirSystem    ;Attribute
    mov eax, 3C00h
    int 41h
    retc
    mov word [rdi + 2], ax  ;Place it in the destination handle
    return

copyFile:
;Copies in sector sized chunks
;Input: 
;rsi points to the word pair to use for handles
;Exit:
;If a CF=CY error is returned, something bad happened. If ecx = -1, error
; happened in the memory allocation portion of the copy
    mov rdx, qword [memoryBlock]
    test rdx, rdx
    jnz .skipRealloc    ;If there is a block, dont allocate one again
    movzx ebx, word [sectorSize]
    shr ebx, 4  ;Convert to paragraphs
    mov eax, 4800h
    int 41h
    mov ecx, -1
    retc    ;Return if error
    mov qword [memoryBlock], rax    ;Save the ptr here
    mov rdx, rax
.skipRealloc:
;rdx points to buffer to do IO through
    mov bx, word [rsi]  ;Get source file read
    movzx ecx, word [sectorSize]   
    mov eax, 3F00h  ;Read handle
    int 41h
    retc    ;Return if error
    mov bx, word [rsi + 2]
    mov ecx, eax    ;Move the number of bytes to write into ecx
    mov edi, eax    ;Keep a copy for later
    mov eax, 4000h  ;Write handle
    int 41h
    retc
    cmp ecx, edi    ;Did we write the same as we read?
    jne .badExit
    cmp di, word [sectorSize]   ;Did we read less than sector size?
    je .skipRealloc
    clc
    return  ;If not equal, exit
.badExit:
    stc
    return

getFATtype:
;Gets a pointer to a DPB and returns the FAT type on the drive
;Entry: rbp = DPB to ascertain FAT
;Exit: ecx = 0 => FAT 12, ecx = 1 => FAT 16, ecx = 2 => FAT 32
    push rbx
    mov ebx, dword [rbp + dpb.dClusterCount]
    mov ecx, 2  ;FAT 32 marker
    cmp ebx, fat16MaxClustCnt
    jae .exit
    dec ecx     ;FAT 16 marker
    cmp ebx, fat12MaxClustCnt
    jae .exit
    dec ecx     ;FAT 12 marker
.exit:
    pop rbx
    return

getStartSectorOfCluster:
;Input: eax = Cluster Number
;       rbp = dpb pointer
;Output: rax = Starting Sector number for cluster
;Gives the data sector we are at in the current cluster
;Start Sector = (ClusterNumber - 2)*SecPerClust + DataAreaStartSector
    push rcx
    or eax, eax ;Zero upper dword
    jz .rootDir ;If eax is zero, that is an alias for Root Directory
.fat32Root:
    sub rax, 2
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shl rax, cl
    xor ecx, ecx
    mov ecx, dword [rbp + dpb.dClusterHeapOffset]
    add rax, rcx
    ;rax now has the first sector of the current cluster
    pop rcx
    return
.rootDir:
    mov eax, dword [rbp + dpb.dFirstUnitOfRootDir]
    call getFATtype
    cmp ecx, 2
    je .fat32Root   ;If FAT32, eax now has zero extended 1st cluster of Root Dir
    ;Else rax has the first sector of the Root Dir
    pop rcx
    return

readWrapper:
;Reads the sector specified in xfrSector
;If returned with CF=CY, exit to badPrint
    mov esi, 3
.lp:
    mov rdx, qword [xfrSector]
    mov ecx, 1
    push rsi
    call readSector ;Read the sector into the buffer
    pop rsi
    retnc
    dec esi
    jnz .lp
    lea rdx, badDirectI
    return

writeWrapper:
;Writes to the sector specified in xfrSector
;If returned with CF=CY, exit to badPrint
    mov esi, 3
.lp:
    mov rdx, qword [xfrSector]
    mov ecx, 1
    push rsi
    call writeSector ;Write the sector from the buffer
    pop rsi
    retnc
    dec esi
    jnz .lp
    lea rdx, badDirectI
    return

readSector:
;Input:
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    mov al, byte [sysDrive]     ; Always read from sysDrive
    mov rbx, qword [memoryBlock] ; Memory Buffer address to read from
    int 45h
    pop rax ;Pop old flags into rax
    return
writeSector:
;Input:
;al = Drive number
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    mov al, byte [sysDrive]     ; Always write to sysDrive
    mov rbx, qword [memoryBlock] ; Memory Buffer address to read from
    int 46h
    pop rax ;Pop old flags into rax
    return

dosCrit1Enter:
    mov byte [inCrit], -1
    push rax 
    mov eax, 8001h
    int 4ah
    pop rax
    return

dosCrit1Exit:
    push rax 
    mov eax, 8101h
    int 4ah
    pop rax
    mov byte [inCrit], 0
    return