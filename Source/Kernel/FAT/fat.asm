;---------------------------------------------------:
;                   KERNEL FUNCTIONS                :
;---------------------------------------------------:
makeDIR:           ;ah = 39h
removeDIR:         ;ah = 3Ah
setCurrentDIR:     ;ah = 3Bh, set dir for current drive (or drive in path)
getCurrentDIR:     ;ah = 47h
getSetFileDateTime:;ah = 57h
trueName:          ;ah = 60h, get fully qualified name
    ret


;-----------------------------------:
;       File System routines        :
;-----------------------------------:
name2Clust:
;Converts a file name to a first cluster number
;Entry : rbx = Points to ASCIIZ string to parse for a Cluster number
;Exit : rax = Cluster number or -1 if file not found
;Three cases:
;1) Start with a letter and a : => Full path and Drive specified
;2) Start with \ or / => Current Drive and relative path from root
;3) Else => File name in Current Dir or a subdir from current dir
    push rsi
    push rdi
    push rcx
    push rdx
    mov rdi, rbx    ;Save string ptr in rdi
    cmp byte [rdi + 1], ":" ;Check it is a colon for full path
    je .fullPath
    cmp byte [rdi], "\"
    je .relPath
    cmp byte [rdi], "/"
    je .relPath ;Both CPM and UNIX are considered acceptible path separators
    ;Else search the current dir for an entry

.localFileNoRoot:
.relPath:
.fullPath:
.exit:
    pop rdx
    pop rcx
    pop rdi
    pop rsi
    ret

searchDirectorySectorForEntry:
;Proc that searches the sector for the string 
; UP UNTIL the NULL char or the path separator
;If a . is found in file name, skip it
;If a sector entry is found to start with 0, return fail
;Entry: rdx = Path Section ptr (point to first A/N char)
;       rbx = Sector buffer pointer
;       rsi = DPB pointer
;Exit: CF=NC => Found, CF=CY => Not found
;       If CF=CY read char pointed to by rbx.
;       If this char is 0, then end of directory reached!
    push rax
    push rcx
    ;cl has number of entries per sector
    mov cl, byte [rsi + dpb.bBytesPerSectorShift]
    sub cl, 5   ;5 is the number of bytes per dir entry shift
    mov eax, 1
    shl eax, cl ;eax has number of directory entries in sector
    ;Now search each entry for name
    ;Use ecx as counter for each entry
    mov ecx, eax
.searchDir:
    cmp byte [rbx], 0 ;Check if dir empty before proceeding
    jz .exitNotOK
    ;Do string compare here, search for / or \ or 0 to exit
    push rdx    ;Push the name pointer 
    push rbx    ;Push sector pointer
.searchLoop:
    mov al, byte [rdx] ;Get char in ASCIIZ buffer
    cmp al, "." ;Verify if name separator or directory entry
    je .dotCase
    cmp al, 05h ;Special Case
    je .specialCase
    cmp al, "/" ;Name Found
    je .nameFound
    cmp al, "\" ;Name Found
    je .nameFound
    cmp al, 0   ;End of ASCIIZ string!
    je .nameFound
.specRet:
    cmp al, byte [rbx]  ;Compare to char in sector buffer
    jne .nameNotFound
    inc rbx
.skipChar:
    inc rdx ;Go to Filename extension
    pop rbx ;Get start of directory entry into rbx
    push rbx
    add rbx, 8  ;Go to the extension portion of the Filename
    jmp .searchLoop
.specialCase:
    mov al, 0E5h
    jmp short .specRet
.dotCase:
;Check if next char is geq than 'A'. If yes, path separator
    cmp byte [rdx + 1], 'A'
    jnge .specRet   ;Not Path Separator
    jmp short .skipChar
.nameNotFound:
    pop rbx
    pop rdx
    add rbx, 20h    ;Goto next sector entry
    cmp byte [rbx], 0   ;Are we at the end of the Directory?
    jz .exitNotOK   ;Exit early, end of directory
    dec ecx
    jnz .searchDir
.exitNotOK:
    stc
.exitOk:
    pop rcx
    pop rax
    ret
.nameFound:
    pop rbx
    pop rdx
    clc
    jmp short .exitOk

getFATtype:
;Gets a pointer to a DPB and returns the FAT type on the drive
;Entry: rsi = DPB to ascertain FAT
;Exit: ecx = 0 => FAT 12, ecx = 1 => FAT 16, ecx = 2 => FAT 32
    push rbx
    mov ebx, dword [rsi + dpb.dClusterCount]
    mov ecx, 1  ;FAT 16 marker
    cmp ebx, fat16MaxClustCnt
    jae .exit
    dec ecx     ;FAT 12 marker
    cmp ebx, fat12MaxClustCnt
    jb .exit
    mov ecx, 2  ;Must be FAT 32 otherwise
.exit:
    pop rbx
    ret

clust2FATEntry:
;Converts a cluster number to a offset in the FAT
;Entry:  rbp points to the DPB for the transacting device
;        eax = Cluster number to look for
;Exit:   eax = Sector on disk of FAT 
;        ecx = 0 => FAT12, 1 => FAT16, 2 => FAT32
;        edx = 1.5Byte/Word/DWord in sector of entry
    push rbx
    mov ebx, dword [rbp + dpb.dClusterCount]
    cmp ebx, fat16MaxClustCnt
    jae .fat32
    cmp ebx, fat12MaxClustCnt
    jb .fat12
;FAT16
    shl eax, 1  ;Multiply cluster number by 2
    push qword 1
    jmp short .common
.fat12:
    mov ecx, eax    ;ecx = eax
    shr ecx, 1      ;ecx = ecx / 2
    add eax, ecx    ;eax = eax + ecx    (eax * 1.5)
    push qword 0
    jmp short .common
.fat32:
    push qword 2
    shl eax, 2  ;Multiply cluster number by 4
.common:
;eax has the FAToffset
    mov cl, byte [rbp + dpb.bBytesPerSectorShift]
    mov edx, 1
    shl edx, cl    ;Turn edx to number of bytes per sector
    mov ecx, edx
    xor edx, edx    ;edx = 0
    div ecx         ;Divide by bytes per sector (0:eax / ecx)
    movzx ebx, word [rbp + dpb.wFAToffset]   ;Add the offset to the first FAT
    add eax, ebx
    pop rcx ;Pop the FAT type back into rcx
    pop rbx
    ret

getStartSectorOfCluster:
;Input: eax = Cluster Number
;       rbp = dpb pointer
;Output: rax = Starting Sector number for cluster
;Gives the data sector we are at in the current cluster
;Start Sector = (ClusterNumber - 2)*SecPerClust + DataAreaStartSector
    push rcx
    or eax, eax ;Zero upper dword
    sub rax, 2
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shl rax, cl
    xor ecx, ecx
    mov ecx, dword [rbp + dpb.dClusterHeapOffset]
    add rax, rcx
    ;rax now has the first sector of the current cluster
    pop rcx
    ret


getNextSectorOfFileBROKEN:
;This function will read the next sector for a file into a buffer.
;If the next sector to be read lives in the next cluster, it will update
; the file handle of the file being read/written to the new cluster
;
;Input: qword [currentSFT] = sft pointer
;Output:
;       rbx = Pointer to buffer data
;       CF = NC, buffer OK to read
;       CF = CY, buffer not ok, something went wrong
;           ZF = ZE(1), Data not flushed to disk
;           ZF = NZ(0), Data no read from disk
    ;Read next sector. If at last sector in cluster, walk map, get
    ; next cluster and read first sector 
    push rax
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    ;Check if we need to go to next cluster
    mov rsi, qword [currentSFT] ;Get the current SFT
    mov rbp, qword [rsi + sft.qPtr] ;Get DPB pointer for file
    mov qword [workingDPB], rbp ;Make the DPB the working DPB
    ;mov ax, word [rsi + sft.wRelSect]    ;Upper byte is ALWAYS 0
    cmp al, byte [rbp + dpb.bMaxSectorInCluster]
    je .gotoNextCluster
    ;Goto next sector
    ;inc word [rsi + sft.wRelSect]    ;Goto next sector in cluster
.getSector:
    mov eax, dword [rsi + sft.dAbsClusr] ;Get cluster number
    call getStartSectorOfCluster
    ;movzx ebx, word [rsi + sft.wRelSect] ;Get relative sector number
    ;eax now has the correct sector in the cluster
    add eax, ebx    
    ;Read the sector into a buffer
    ;The sector read here is either DATA or DOS
    lea rsi, qword [rsi + sft.sFileName]
    lea rdi, dosBIOSName    ;Check if the file being read is the BIOS
    mov ecx, 11             ;File name length
    repe cmpsb
    je .OSFile
    lea rsi, qword [rsi + sft.sFileName]
    lea rdi, dosKernName
    mov ecx, 11             ;File name length
    repe cmpsb
    je .OSFile
    ;Not an OS file, dataBuffer
    mov cl, dataBuffer
.getSectorRead:
    call getBuffer  ;Get ptr to buffer header in rbx
    jc .getSectorFailed
    add rbx, bufferHdr.dataarea ;Goto data area
.getSectorExit:
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret
.OSFile:
    mov cl, dosBuffer
    jmp short .getSectorRead
.getSectorFailed:
    ;CF = CY => Something went wrong!
    ;   Set the Zero flag for data not flushed to disk
    ;   Clear Zero flag for data not read from disk
    test ch, ch ;This sets the zero flag correctly, but mangles CF
    stc ;Set the carry flag!
    jmp short .getSectorExit
.gotoNextCluster:
    ;Read FAT, find next cluster in cluster map, update SFT entries
    mov eax, dword [rsi + sft.dAbsClusr] ;Get the current cluster
    call clust2FATEntry ;Returns sector in FAT in eax, offset in sector in edx
    movzx ebx, word [rbp + dpb.wFAToffset]
    add eax, ebx    ;Add the FAT offset to the sector
    mov cl, fatBuffer
    ;call readBuffer ;Buffer Header in ebx
    jc .getSectorFailed
    ;Check if FAT 12, 16, 32
    test rdi, rdi
    jz .gotoNextClusterFat12    ;Handle FAT 12 separately
    test rdi, 1
    jz .goToNextClusterCommonFat32
    ;Here we handle FAT16
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]
    jmp short .goToNextClusterCommon
.goToNextClusterCommonFat32:
    mov eax, dword [rbx + bufferHdr.dataarea + rdx]
    and eax, 0FFFFFFFh  ;Zero upper nybble
.goToNextClusterCommon:
    mov dword [rsi + sft.dAbsClusr], eax ;Save new cluster number
    ;mov word [rsi + sft.wRelSect], 0 ;First sector in next cluster
    jmp .getSector
.gotoNextClusterFat12:
;FAT12 might need two FAT sectors read so we always read two sectors
;eax has the sector of the FAT, offset into the sector is in edx
    mov rdi, rbx    ;Save previous buffer header in rdi
    inc eax ;Get next sector
    ;call readBuffer ;Buffer Header in ebx
    jc .getSectorFailed
    ;rdi has first buffer header, rbx has second buffer header
    ;rdx has offset into first header for entry
    test dword [r8 + sft.dAbsClusr], 1  ;Check if cluster is odd
    jz .gotoNextClusterFat12Even
    ;Here the cluster is ODD, and might cross sector boundary
    mov eax, 1
    mov cl, byte [r8 + dpb.bBytesPerSectorShift]
    shl eax, cl
    sub eax, edx
    dec eax ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12NoCross
    ;Boundary cross, build entry properly
    xor eax, eax
    mov al, byte [rdi + bufferHdr.dataarea + rdx]
    mov ah, byte [rbx + bufferHdr.dataarea]  ;Read first entry of next sector
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .goToNextClusterCommon
.gotoNextClusterFat12NoCross:
    movzx eax, word [rdi + bufferHdr.dataarea + rdx]    ;Read the entry
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .goToNextClusterCommon
.gotoNextClusterFat12Even:
    ;Here the cluster is even and can't cross a sector boundary
    movzx eax, word [rdi + bufferHdr.dataarea + rdx]    ;Read the entry
    and eax, 0FFFh   ;Save lower three nybbles, eax has cluster num
    jmp short .goToNextClusterCommon