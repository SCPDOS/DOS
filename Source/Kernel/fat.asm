;-----------------------------------:
;       File System routines        :
;-----------------------------------:
name2Clust:
;Converts a file name to a first cluster number
;Converts the whole path subdirectory by subdirectory
;On entry: rbx = ptr to ASCIIZ string of file path, maxlen = 67 + 1 (for the 0)
;On return: ebx = First cluster number for the file
;   On error: CF=CY, ax = Error code

;First ascertain path is valid path and get drive letter
    push rsi
    push rdi
    push rcx
    mov rdi, rbx
    mov rsi, rbx
    cmp byte [rdi + 1], ":" ;Check it is a colon for full path
    jne .cdsPath    ;Use CDS for current dir
.getSubDir:
    mov al, "\"
    scasb   ;inc 
    je .psfnd   ;Path separator found

.psfnd:
    dec rdi
    mov rsi, rdi

.cdsPath:
    pop rcx
    pop rdi
    pop rsi
    ret

clust2FATEntry:
;Converts a cluster number to a FAT entry
;Entry:  rsi points to the DPB for the transacting device
;        eax = Cluster number to look for
;Exit:   eax = Sector on disk of FAT 
;        ecx = 0 => FAT12, 1 => FAT16, 2 => FAT32
;        edx = 1.5Byte/Word/DWord in sector of entry
    push rbx
    mov ebx, dword [rsi + dpb.dClusterCount]
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
    mov cl, byte [rsi + dpb.bBytesPerSectorShift]
    mov edx, 1
    shl edx, cl    ;Turn edx to number of bytes per sector
    mov ecx, edx
    xor edx, edx    ;edx = 0
    div ecx         ;Divide by bytes per sector (0:eax / ecx)
    movzx ebx, word [rsi + dpb.wFAToffset]   ;Add the offset to the first FAT
    add eax, ebx
    pop rcx ;Pop the FAT type back into rcx
    pop rbx
    ret
getStartSectorOfCluster:
;Input: eax = Cluster Number
;       r9 = dpb pointer
;Output: eax = Starting Sector number for cluster
;Gives the data sector we are at in the current cluster
;Start Sector = (ClusterNumber - 2)*SecPerClust + DataAreaStartSector
    push rcx
    sub eax, 2
    mov cl, byte [r9 + dpb.bSectorsPerClusterShift]
    shl eax, cl
    add eax, [r9 + dpb.dClusterHeapOffset]
    ;eax now has the first sector of the current cluster
    pop rcx
    ret
getNextSectorOfFile:
;This function will read the next sector for a file into a buffer.
;If the next sector to be read lives in the next cluster, it will update
; the file handle of the file being read/written to the new cluster
;
;Input: r8 = sft pointer
;       r9 = dpb pointer
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
    ;Check if we need to go to next cluster
    mov ax, word [r8 + sft.wRelSect]    ;Upper byte is ALWAYS 0
    cmp al, byte [r9 + dpb.bMaxSectorInCluster]
    je .gotoNextCluster
    ;Goto next sector
    inc word [r8 + sft.wRelSect]    ;Goto next sector in cluster
.getSector:
    mov eax, dword [r8 + sft.dAbsClusr] ;Get cluster number
    call getStartSectorOfCluster
    movzx ebx, word [r8 + sft.wRelSect] ;Get relative sector number
    ;eax now has the correct sector in the cluster
    add eax, ebx    
    ;Read the sector into a buffer
    ;The sector read here is either DATA or DOS
    lea rsi, qword [r8 + sft.sFileName]
    lea rdi, dosBIOSName    ;Check if the file being read is the BIOS
    mov ecx, 11             ;File name length
    repe cmpsb
    je .OSFile
    lea rsi, qword [r8 + sft.sFileName]
    lea rdi, dosKernName
    mov ecx, 11             ;File name length
    repe cmpsb
    je .OSFile
    ;Not an OS file, dataBuffer
    mov cl, dataBuffer
.getSectorRead:
    mov rsi, r9
    call readBuffer
    jc .getSectorFailed
    add rbx, bufferHdr.dataarea ;Goto data area
.getSectorExit:
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
    mov eax, dword [r8 + sft.dAbsClusr] ;Get the current cluster
    mov rsi, r9 ;Move dpb pointer into rsi, eax has cluster number
    call clust2FATEntry ;Returns sector in FAT in eax, offset in sector in edx
    movzx ebx, word [r9 + dpb.wFAToffset]
    add eax, ebx    ;Add the FAT offset to the sector
    mov cl, fatBuffer
    call readBuffer ;Buffer Header in ebx
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
    mov dword [r8 + sft.dAbsClusr], eax ;Save new cluster number
    mov word [r8 + sft.wRelSect], 0 ;First sector in next cluster
    jmp .getSector
.gotoNextClusterFat12:
;FAT12 might need two FAT sectors read so we always read two sectors
;eax has the sector of the FAT, offset into the sector is in edx
    mov rdi, rbx    ;Save previous buffer header in rdi
    inc eax ;Get next sector
    call readBuffer ;Buffer Header in ebx
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
;---------------------------------------------------:
;                   KERNEL FUNCTIONS                :
;---------------------------------------------------:
makeDIR:           ;ah = 39h
removeDIR:         ;ah = 3Ah
changeCurrentDIR:  ;ah = 3Bh, changes directory for current drive
getCurrentDIR:     ;ah = 47h
getSetFileDateTime:;ah = 57h
trueName:          ;ah = 60h, get fully qualified name
    ret