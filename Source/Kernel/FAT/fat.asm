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
getFATtype:
;Gets a pointer to a DPB and returns the FAT type on the drive
;Entry: rbp = DPB to ascertain FAT
;Exit: ecx = 0 => FAT 12, ecx = 1 => FAT 16, ecx = 2 => FAT 32
    push rbx
    mov ebx, dword [rbp + dpb.dClusterCount]
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
;Entry:  Uses the workingDPB to convert cluster number
;        eax = Cluster number to look for
;Exit:   eax = Sector on disk of FAT 
;        ecx = 0 => FAT12, 1 => FAT16, 2 => FAT32
;        edx = 1.5Byte/Word/DWord in sector of entry
    push rbx
    push rbp
    mov rbp, qword [workingDPB]
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
    pop rbp
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

findFreeCluster:
;Walks the FAT to find a free cluster and returns the 
;   zero extended cluster number in eax (-1 means no free cluster)
;Works on the workingDPB
;If returns with CF=CY => Fail set, return immediately to caller
    push rbx
    push rcx
    push rdx
    push rdi
    push rbp
    mov rbp, qword [workingDPB]
    movzx eax, word [rbp + dpb.wFAToffset]  ;Get first FAT sector
    mov qword [tempSect], rax   ;Save the sector number temporarily
;Use ebp as sector counter
    mov edx, dword [rbp + dpb.dFATlength]
;Get Sector Size in bytes in ebx
    mov ebx, 1
    mov cl, byte [rbp + dpb.bBytesPerSectorShift]
    shl ebx, cl ;Get sector size in ebx
;Get FAT type
    call getFATtype ;Gets FAT type (for number of elements in sector)
    jz .fat12
    test cl, 1
    jnz .fat16
;FAT32 proceeds here
    shr ebx, 2  ;Divide by 4 to get number of FAT entries in a sector buffer
    mov word [entries], bx
.fat32Search:
    mov cl, fatBuffer
    mov rax, qword [tempSect]
    call getBuffer ;Buffer Header in ebx
    jc .exitFail
    lea rdi, qword [rbx + bufferHdr.dataarea]
    xor eax, eax
    movzx ecx, word [entries]   ;Get entries per FAT sector in ecx
    repne scasd ;Look for the zero dword 
    je .fat32Found  ;If found, report cluster number (offset into FAT)
    dec edx ;Dec number of sectors left to search
    jz .noFreeClusters
    inc qword [tempSect]    ;Go to the next FAT sector
    jmp short .fat32Search
.fat32Found:
    sub edi, 4  ;edi is one dword past the entry
.computeEntry:
    movzx rcx, word [rbp + dpb.wFAToffset] ;Get start sector number of FAT 
    mov rax, qword [tempSect]   ;Get disk sector number of FAT into rax
    sub rax, rcx   ;Get Offset into FAT in rax
    movzx ecx, word [entries] ;Get number of entries in a FAT sector
    push rdx
    mul rcx ;Multiply rax with rcx (technically eax with ecx)
    pop rdx
;rbx points to current buffer header
    lea rdx, qword [rbx + bufferHdr.dataarea]
    sub rdi, rdx
    add rax, rdi    ;Add the offset into the sector to rax to get cluster number
.exit:
    clc
.exitFail:      ;Keep carry flag
    pop rbp
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    ret
.noFreeClusters:
    mov eax, -1 ;No free cluster marker
    jmp short .exit
.fat16:
    shr ebx, 1  ;Divide by 2 to get number of FAT entries in a sector buffer
    mov word [entries], bx
.fat16Search:
    mov cl, fatBuffer
    mov rax, qword [tempSect]
    call getBuffer ;Buffer Header in ebx
    jc .exitFail
    lea rdi, qword [rbx + bufferHdr.dataarea]
    xor eax, eax
    movzx ecx, word [entries]   ;Get entries per FAT sector in ecx
    repne scasw ;Look for the zero word 
    je .fat16Found  ;If found, report cluster number (offset into FAT)
    dec edx ;Dec number of sectors left to search
    jz .noFreeClusters
    inc qword [tempSect]    ;Go to the next FAT sector
    jmp short .fat16Search
.fat16Found:
    sub edi, 2  ;edi is one word past the entry
    jmp short .computeEntry
.fat12:
    mov eax, ebx    ;Get sectorsize in ax
    shl eax, 1  ;Multiply by 2
    mov ecx, 3  ;1.5 bytes per FAT entry *2
    push rdx    ;Preserve number of sectors in FAT counter
    xor edx, edx
    div ecx
    pop rdx
    mov word [entries], ax ;Get quotient (number of whole entries in sector) 
    ;The value is rounded down so we can read the next sector for the 
    ;last entry manually (thus buffering it if it not already buffered)
    mov cl, fatBuffer
    mov rax, qword [tempSect]
    call getBuffer ;Buffer Header in ebx
    jc .exitFail
    lea rdi, qword [rbx + bufferHdr.dataarea]
.fat12SearchNewSector:
    movzx ecx, word [entries]   ;This is total entries in Sector rounded down
.fat12Search:
    movzx eax, word [rdi]   ;Get first word (EVEN ENTRY)
    and eax, 0FFFh   ;Clear upper nybble
    jz .fat12EntryFound
    inc rdi ;Goto next byte
    dec ecx ;Dec the number of entries to search in sector
    movzx eax, word [rdi]  ;Get second word (ODD ENTRY)
    shr eax, 4  ;Shift down by 4
    jz .fat12EntryFound
    inc rdi ;Goto next entry
    dec ecx ;Dec the number of entries to search in sector
    jnz .fat12Search
;We arrive here when we are at the last entry in the sector
    inc qword [tempSect]    ;Get next Sector
    mov rax, qword [tempSect]   ;Get this sector in rax
    mov cl, fatBuffer
    call getBuffer ;Buffer Header in ebx
    jc .exitFail
    movzx eax, byte [rdi]  ;Get last byte in old buffer (rdi still points there)
    lea rcx, qword [rbx + bufferHdr.dataarea]   ;Go to data area (preserve rdi)
    mov ah, byte [rcx]  ;Get first byte in new sector
    shr eax, 4  ;Clear out bottom nybble
    jz .fat12EntryFound ;Found a sector!
    ;Empty cluster not found in sector
    dec edx ;Decrement sector count
    jz .noFreeClusters
    mov rdi, rcx    ;Set rdi to point at start of next sector
    jmp short .fat12SearchNewSector ;Reload the number of entries and search
.fat12EntryFound:
    jmp .computeEntry   ;Unnecessary redirection

getNextSectorOfFile:
;This function will read the next sector for a file into a buffer.
;If the next sector to be read lives in the next cluster, it will update
; the file handle of the file being read/written to the new cluster
;
;Input: qword [currentSFT] = sft pointer
;Output:
;       qword [currBuff] = ptr to buffer data (if rbx = -1, end of file reached)
;       CF = NC, buffer OK to read
;       CF = CY, Fail request
    ;Read next sector. If at last sector in cluster, walk map, get
    ; next cluster and read first sector 
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    ;Check if we need to go to next cluster
    mov rsi, qword [currentSFT] ;Get the current SFT
    mov rbp, qword [workingDPB] ;Get DPB pointer for file
    mov al, byte [currSect]    ;Get current sector rel Cluster
    cmp al, byte [rbp + dpb.bMaxSectorInCluster]
    je .gotoNextCluster
    ;Goto next sector in same cluster
    inc byte [currSect]    ;Goto next sector in cluster
    inc qword [currSectA]  ;Goto next sector on Disk
.getSector:
    mov rax, qword [currSectA]  ;Get the disk sector number to read
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
    call getBuffer  ;Get ptr to buffer header in [currBuff]
    jc .exitFail
.getSectorExit:
    clc
.exitFail:
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.OSFile:
    mov cl, dosBuffer
    jmp short .getSectorRead
.gotoNextCluster:
    mov eax, dword [currClustA] ;Get absolute cluster number
    call walkFAT
    ;eax now has the next cluster number to read (or -1 if EOF)
    cmp eax, -1
    jne .getSector
;Else, we are at the last sector, we return -1 ,and the caller decides 
; based on the call what to do!
    mov qword [currBuff], -1    ;Set current buffer to -1
    jmp short .getSectorExit

walkFAT:
;Given a cluster number, it gives us the next cluster in the cluster chain
; or -1 to indicate end of cluster chain on the device with workingDPB
;Input: eax = Cluster number (zero extended to 32 bits)
;Output: eax = Next Cluster number (-1 indicates end of chain)
;If carry set, getBuffer failed!
    push rbx
    push rcx
    push rdx
    push rdi
    push rbp
    mov edi, eax    ;Save cluster number in edi
    call clust2FATEntry ;Returns sector in FAT in eax, offset in sector in edx
    ;and FAT type in ecx
    movzx ebx, word [rbp + dpb.wFAToffset]
    add eax, ebx    ;Add the FAT offset to the sector
    push rcx    ;Move FAT signature onto stack
    mov cl, fatBuffer
    call getBuffer ;Buffer Header in ebx
    pop rcx
    jc .exitFail
    ;Check if FAT 12, 16, 32
    test ecx, ecx
    jz .gotoNextClusterFat12    ;Handle FAT 12 separately
    test ecx, 1
    jz .goToNextClusterFat32
    ;Here we handle FAT16
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]
    cmp ax, -1
    jne .exit
    movsx eax, al
    jmp short .exit
.goToNextClusterFat32:
    mov eax, dword [rbx + bufferHdr.dataarea + rdx]
    cmp eax, -1
    je .exit   ;If EOC, skip zeroing nybble
    and eax, 0FFFFFFFh  ;Zero upper nybble
.exit:
    clc
.exitFail:
    pop rbp
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    ret
.gotoNextClusterFat12:
;FAT12 might need two FAT sectors read so we always read two sectors
;eax has the sector of the FAT, offset into the sector is in edx

    push rdi    ;Save the cluster number on the stack
    mov rdi, rbx    ;Save previous buffer header in rdi
    inc eax ;Get next sector
    mov cl, fatBuffer
    call getBuffer ;Buffer Header in ebx
    pop rcx ;Return the cluster number in rcx
    jc .exitFail
    ;rdi has first buffer header, rbx has second buffer header
    ;rdx has offset into first header for entry
    test ecx, 1  ;Check if cluster is odd
    jz .gotoNextClusterFat12Even
    ;Here the cluster is ODD, and might cross sector boundary
    mov eax, 1
    mov cl, byte [rbp + dpb.bBytesPerSectorShift]
    shl eax, cl
    sub eax, edx
    dec eax ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12NoCross
    ;Boundary cross, build entry properly
    xor eax, eax
    mov al, byte [rdi + bufferHdr.dataarea + rdx]
    mov ah, byte [rbx + bufferHdr.dataarea]  ;Read first entry of next sector
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .checkIfLastFAT12Cluster
.gotoNextClusterFat12NoCross:
    movzx eax, word [rdi + bufferHdr.dataarea + rdx]    ;Read the entry
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .checkIfLastFAT12Cluster
.gotoNextClusterFat12Even:
    ;Here the cluster is even and can't cross a sector boundary
    movzx eax, word [rdi + bufferHdr.dataarea + rdx]    ;Read the entry
    and eax, 0FFFh   ;Save lower three nybbles, eax has cluster num
.checkIfLastFAT12Cluster:
    cmp ax, 0FFFh
    jne .exit
    mov eax, -1
    jmp .exit