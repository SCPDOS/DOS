;-----------------------------------:
;       File System routines        :
;-----------------------------------:
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
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    xor edx, edx    ;edx = 0
    div ecx         ;Divide by bytes per sector (0:eax / ecx)
    movzx ebx, word [rbp + dpb.wFAToffset]   ;Add the offset to the first FAT
    add eax, ebx
    pop rcx ;Pop the FAT type back into rcx
    pop rbp
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


getLastClusterInChain:
;Given a cluster value in eax, returns in eax the last cluster in the chain
;Input: eax = Cluster to start searching at
;Output: eax = Last cluster in chain
;If input eax = 0, output eax = 0
    test eax, eax   ;If eax = 0, then just exit
    retz
    push rbx
.lp:
    mov ebx, eax
    call readFAT
    jc .exit
    cmp eax, -1 ;Once this is EOC, we add a new cluster.
    jne .lp
    mov eax, ebx    ;Get the last cluster value in ebx
.exit: 
    pop rbx
    return

getNumberOfClustersInChain:
;Given a cluster value in eax, returns in eax the number of clusters in chain
;Input: eax = Cluster to start searching at
;Output: eax = Number of clusters in the chain
;If input eax = 0, output eax = 0
    test eax, eax   ;If eax = 0, then just exit
    retz
    push rcx
    xor ecx, ecx
.lp:
    inc ecx
    call readFAT
    jc .exit
    cmp eax, -1 ;Once this is EOC, we add a new cluster.
    jne .lp
    mov eax, ecx    ;Get the count
.exit: 
    pop rcx
    return

getClusterInChain:
;Given a starting cluster, walk forwards by a number of clusters.
;If an EOC is encountered, then ecx will not be 
;Input: eax = Start Cluster to start searching from
;       ecx = Number of clusters to go forwards by;
;Ouput: eax = Value of the cluster ecx number of clusters forwards
;       ecx = # of clusters left to walk forwards by (0 EOC was not encountered)
;Also usual CF babble.
    test eax, eax   ;If eax = 0, then just exit
    retz
    push rbx
    jecxz .exit
.lp:
    mov ebx, eax
    call readFAT
    jc .exit
    dec ecx
    jnz .lp
    mov eax, ebx    ;Get the value of the cluster in eax
.exit:
    pop rbx
    return

startNewChain:
;Working dpb must be set
;Returns: eax = First cluster new chain or -1=> Disk full
;If CF=CY, hard error
    push rbx
    push rsi
    call findFreeCluster    ;Get a free cluster in eax
    jc .exit    ;Disk read error?
    cmp eax, -1 ;Disk full?
    je .exit
    mov esi, -1 ;Value to write at eax is EOF
    mov ebx, eax
    call writeFAT   ;Propagate the CF 
    mov eax, ebx
    call decrementFreeClusterCount
.exit:
    pop rsi
    pop rbx
    return

allocateClusters:
;Working dpb must be set. 
;Input: ecx = Number of clusters to allocate in a chain
;       ebx = Cluster to link to the new cluster
;Output:    
;   CF=NC => Complete.
;   If eax = -1, then no more free clusters. 
;   CF=CY => Hard error, exit
; ecx always has number of allocated clusters
    clc ;Always clear the flags before starting
    push rbx
    push rsi
    push rcx    ;Save tfr count on stack
    jecxz .exit ;Allocating nothing? Exit
.allocateLoop:
    call findFreeCluster
    jc .exit
    cmp eax, -1 ;No more free clusters?
    je .exit    ;If the cluster number is -1, return immediately
    ;eax has first free cluster value to use
    ;First we link the previous cluster to this cluster
    mov esi, -1 ;EOC marker
    push rax
    call writeFAT   ;Allocate this cluster first
    pop rax
    jc .exit    ;Errors don't get flushed to disk so this is safe
    call decrementFreeClusterCount
    ;eax points to this allocated cluster
    ;ebx points to the previous last cluster
    mov esi, eax    ;New cluster as "value"
    mov eax, ebx    ;Previous cluster to allocate at
    call writeFAT   ;Now link previous EOC to this new EOC
    jc .exit
    mov ebx, esi    ;Move new cluster as the value to be linked to a new cluster
    dec ecx ;One less cluster allocated
    jnz .allocateLoop
.exit:
    pop rsi ;Pop the tfr count into esi
    pushfq
    neg ecx
    add ecx, esi
    popfq
    pop rsi
    pop rbx
    return

findFreeCluster:
;Walks the FAT to find a free cluster and returns the 
;   zero extended cluster number in eax (-1 means no free cluster)
;Works on the workingDPB
;If returns with CF=CY => Fail set, return immediately to caller
    push rbx
    push rdx
    push rbp
    mov rbp, qword [workingDPB]
    ;Use readFAT
    ;Starting with cluster number 2, goto to the MAX cluster
    ;If readFAT returns 0 then its a free cluster
    mov eax, 2  ;Start with cluster 2
.fatLoop:
    mov ebx, eax    ;Save the current cluster number in ebx
    call readFAT    ;Read the value at eax, if it is 0, then eax is free
    jc .exitFail    ;If something goes wrong, just return
    test eax, eax   ;Is this cluster free?
    jz .exit    ;If yes, exit
    lea eax, dword [ebx + 1]    ;Add one to ebx and save in eax
    cmp eax, dword [rbp + dpb.dClusterCount]
    jbe .fatLoop
.exit:
    mov eax, ebx
    clc
.exitFail:      ;Keep carry flag
    pop rbp
    pop rdx
    pop rbx
    return

findFreeClusterData:
;Walks the FAT to find a free cluster and returns the 
;   zero extended cluster number in eax (-1 means no free cluster)
; Also finds NumberOfFreeCLusters, if it is unknown. 
; Both fields get filled in the workingDPB
;Works on the workingDPB
;If returns with CF=CY => Fail set, return immediately to caller
    push rbx
    push rdx
    push rbp
    mov rbp, qword [workingDPB]
    movzx eax, word [rbp + dpb.wFAToffset]  ;Get first FAT sector
    ;Mark dFirstFreeCluster as -1, unknown
    mov dword [rbp + dpb.dFirstFreeCluster], -1
    ;Use readFAT
    ;Starting with cluster number 2, goto to the MAX cluster
    ;If readFAT returns 0 then its a free cluster
    mov eax, 2  ;Start with cluster 2
    mov edx, dword [rbp + dpb.dClusterCount]
.fatLoop:
    mov ebx, eax    ;Save the current cluster number in ebx
    call readFAT
    jc .exitFail   ;If something goes wrong, just return
    test eax, eax   ;Is this cluster free?
    jne .fatProceed
    cmp dword [rbp + dpb.dFirstFreeCluster], -1 ;Have we found the first clust?
    je .fatFirst
    inc dword [rbp + dpb.dNumberOfFreeClusters] ;Add 1 to # of free clusters
.fatProceed:
    lea eax, dword [ebx + 1]    ;Add one to ebx and save in eax
    cmp eax, edx
    jbe .fatLoop
.exit:
    mov eax, dword [rbp + dpb.dFirstFreeCluster]  ;Get first free cluster in eax
    clc
.exitFail:      ;Keep carry flag
    pop rbp
    pop rdx
    pop rbx
    return
.fatFirst:
;Set the first free cluster value in the DPB and check if we need to get a count
    mov dword [rbp + dpb.dFirstFreeCluster], ebx
    cmp dword [rbp + dpb.dNumberOfFreeClusters], -1 ;If unknown, get count
    je .fatProceed
    jmp short .exit ;Else, we have valid count so just exit

getNextSectorOfFile:
;This function will read the next sector for a file into a buffer.
;If the next sector to be read lives in the next cluster, it will update
; the vars appropriately
;
;Input: rbp = dpb pointer
;Output:
;       CF=NC => rax = Next sector to read into a memory buffer
; If rax = -1 => [currClustF] = Last Cluster of File. Also ZF=ZE in this case
;       CF=CY => Critical error occurred and was FAILed
;Read next sector. If at last sector in cluster, walk map, get
; next cluster and read first sector 
    ;Check if we need to go to next cluster
    mov al, byte [currSectC]    ;Get current sector rel Cluster
    cmp al, byte [rbp + dpb.bMaxSectorInCluster]
    je .gotoNextCluster
    ;Goto next sector in same cluster
    inc byte [currSectC]    ;Goto next sector in cluster
    inc qword [currSectD]  ;Goto next sector on Disk, clears ZF
    mov rax, qword [currSectD]
.exitOK:
    clc
.exitFail:
    return
.gotoNextCluster:
    mov eax, dword [currClustD] ;Get absolute cluster number
    call readFAT
    jc .exitFail
    ;eax now has the next cluster number to read (or -1 if EOF)
    cmp eax, -1
    je .exitOK
;Update the new cluster and sector information
    mov dword [currClustD], eax ;Update disk location of next cluster
    inc dword [currClustF]   ;Goto next file cluster
    call getStartSectorOfCluster    ;Get start sector of Cluster, clears ZF
    mov qword [currSectD], rax  ;Save it
    mov byte [currSectC], 0      ;We are at sector 0 rel Clust
    jmp short .exitOK

unlinkFAT:
;Given a cluster number, will free the cluster and walk the FAT until the first
; cluster number considered EOC is found. The given cluster number MUST be
; the start cluster of a chain, or at least the cluster linking to the given
; cluster must be set to EOC before this proc is called.
;Stops at first invalid cluster number.
;
;Input: eax = Cluster to start unlinking at (zero extended to 32 bits)
;       rbp = Current DPB to use for disk
;Output: CF = NC => All ok. CF = CY => Hard Error, exit

    push rax    ;Save the cluster number to start unlinking at
    push rsi
    call truncateFAT    ;Preserved eax
    jc .exit
    xor esi, esi  ;Free first cluster too
    call writeFAT
    jc .exit
    call incrementFreeClusterCount  ;One more cluster freed
.exit:
    pop rsi
    pop rax
    return

truncateFAT:
;Given a cluster number, will set that cluster to EOC and walk the FAT freeing 
; each cluster until the firstcluster number considered EOC is found. The given 
; cluster number MUST be the start cluster of a chain, or at least the cluster 
; linking to the given cluster must be set to EOC before this proc is called.
;Stops at first invalid cluster number.
;
;Input: eax = Cluster to start unlinking at (zero extended to 32 bits)
;       rbp = Current DPB to use for disk
;Output: CF = NC => All ok. CF = CY => Hard Error, exit
    push rax
    push rbx
    push rcx
    push rsi
    mov ebx, eax    ;Store the current cluster we are at in ebx
.lp:
    call readFAT    ;Get the value of the cluster at this location in eax
    jc .exit    ;Error exit
    mov ecx, eax   ;Move chain marker to ecx
    xchg eax, ebx  ;Move clust. to write at in eax and save next cluster in ebx
    xor esi, esi   ;Free cluster at eax (write a 0)
    call writeFAT
    jc .exit    ;Error exit
    call incrementFreeClusterCount  ;Successfully freed the cluster
    cmp ecx, -1 ;End of chain?
    je .exit
    mov eax, ebx    ;Move next cluster into eax
    jmp short .lp
.exit:
    pop rsi
    pop rcx
    pop rbx
    pop rax
    return
readFAT:
;Given a cluster number, it gives us the next cluster in the cluster chain
; or -1 to indicate end of cluster chain on the device with workingDPB
;Input: eax = Cluster number (zero extended to 32 bits)
;       rbp = DPB
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
    call getBufForFat ;Buffer Header in ebx, first buffer being requested
    jc .exitFail
    ;Check if FAT 12, 16, 32
    test ecx, ecx
    jz .gotoNextClusterFat12    ;Handle FAT 12 separately
    test ecx, 1
    jz .goToNextClusterFat32
    ;Here we handle FAT16
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]
    cmp eax, 0FFF6h  ;Valid cluster number?
    jb .exit
    mov eax, -1 ;If not, set to -1
    jmp short .exit
.goToNextClusterFat32:
    mov eax, dword [rbx + bufferHdr.dataarea + rdx]
    cmp eax, 0FFFFFF6h ;First reserved value. Any Reserved number = EOC
    jb .validCluster32   
    mov eax, -1 ;Always translate it to -1 and skip zeroing upper nybble
    jmp short .exit
.validCluster32:
    and eax, 0FFFFFFFh  ;Zero upper nybble
.exit:
    clc
.exitFail:
    pop rbp
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    return
.gotoNextClusterFat12:
;FAT12 might need two FAT sectors read so we always read two sectors
;eax has the sector number of the FAT
;edx has byte offset into the sector
;edi has current cluster number
;rbx has ptr to buffer header
    test edi, 1  ;Check if cluster is odd
    jz .gotoNextClusterFat12Even
    ;Here the cluster is ODD, and might cross sector boundary
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    sub ecx, edx
    dec ecx ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12NoCross
    ;Boundary cross, build entry properly
    movzx ebx, byte [rbx + bufferHdr.dataarea + rdx] ;Use ebx as it is free
    inc eax ;Get next FAT sector
    push rbx
    call getBufForFat ;Get buffer Header in ebx
    pop rcx ;Get bl in ecx, the last entry from the previous buffer
    jc .exitFail
    mov eax, ecx    ;Move the entry if all ok
    mov ah, byte [rbx + bufferHdr.dataarea]  ;Read first entry of next sector
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .checkIfLastFAT12Cluster
.gotoNextClusterFat12NoCross:
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]    ;Read the entry
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .checkIfLastFAT12Cluster
.gotoNextClusterFat12Even:
    ;Here the cluster is even and can't cross a sector boundary
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]    ;Read the entry
    and eax, 0FFFh   ;Save lower three nybbles, eax has cluster num
.checkIfLastFAT12Cluster:
    cmp eax, 0FEFh   ;Is it below the first invalid cluster number?
    jb .exit         ;If so, exit with it in eax (and clear CF)
    mov eax, -1 ;Else, replace with -1, EOC
    jmp .exit

writeFAT:
;Given a cluster number to edit in eax and a number in ebx to store in 
; that FAT entry, this program will write ebx for cluster eax in the FAT.
;If the FAT is FAT 12, only the low 12 bits of eax and ebx will be used.
;If the FAT is FAT 16, only the low 16 bits of eax and ebx will be used.
;Input: esi = Cluster value, eax = Cluster to write at
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    mov edi, eax    ;Save cluster number in edi
    call clust2FATEntry ;Returns sector in FAT in eax, offset in sector in edx
    ;and FAT type in ecx
    call getBufForFat ;Buffer Header in ebx, first buffer being requested
    jc .exitFail
    ;Check if FAT 12, 16, 32
    test ecx, ecx
    jz .gotoNextClusterFat12    ;Handle FAT 12 separately
    test ecx, 1
    jz .goToNextClusterFat32
    ;Here we handle FAT16
    mov word [rbx + bufferHdr.dataarea + rdx], si ;Store the value
    jmp short .exit
.goToNextClusterFat32:
    and esi, 0FFFFFFFh  ;Zero upper nybble
    mov dword [rbx + bufferHdr.dataarea + rdx], esi
.exit:
    call markBufferDirty
    clc
.exitFail:
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    return
.gotoNextClusterFat12:
;FAT12 might need two FAT sectors read so we always read two sectors
;eax has the sector number of the FAT
;edx has byte offset into the sector
;edi has current cluster number
;rbx has ptr to buffer header
    and esi, 0FFFh  ;Clear the upper bits. Save only low 12 bits
    test edi, 1  ;Check if cluster is odd
    jz .gotoNextClusterFat12Even
    ;Here the cluster is ODD, and might cross sector boundary
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    sub ecx, edx
    dec ecx ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12NoCross
    ;Boundary cross, build entry properly
    ;Replace the high nybble of the low byte 
    movzx ecx, byte [rbx + bufferHdr.dataarea + rdx] ;Use ecx as it is free
    and ecx, 0Fh    ;Clear the high nybble
    shl esi, 4  ;Shift value up by 4 to insert the low nybble in the right place
    or ecx, esi ;Add low nybble of esi to upper nybble of ecx
    shr esi, 8  ;Move upper byte to lower byte of esi
    mov byte [rbx + bufferHdr.dataarea + rdx], cl
    call markBufferDirty
    inc eax ;Get next FAT sector
    call getBufForFat ;Get buffer Header in ebx
    jc .exitFail
    mov ecx, esi    ;Get the high byte of the entry into cl
    mov byte [rbx + bufferHdr.dataarea], cl  ;Write entry
    jmp short .exit
.gotoNextClusterFat12NoCross:
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]    ;Read the entry
    and eax, 0Fh    ;Clear the upper three nybbles of entry (the entry)
    shl esi, 4  ;Shift entry up by 4
    jmp short .fat12common
.gotoNextClusterFat12Even:
    ;Here the cluster is even and can't cross a sector boundary
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]    ;Read the entry
    and eax, 0F000h ;Clear the lower three nybbles of entry (the entry)
.fat12common:
    or eax, esi     ;Add the new entry bits
    mov word [rbx + bufferHdr.dataarea + rdx], ax   ;Replace the entry
    jmp .exit

incrementFreeClusterCount:
;Cluster Deallocated Function
    pushfq
    cmp dword [rbp + dpb.dNumberOfFreeClusters], -1
    je .exit
    inc dword [rbp + dpb.dNumberOfFreeClusters]
.exit:
    popfq
    return
decrementFreeClusterCount:
;Cluster Allocated Function
    pushfq
    cmp dword [rbp + dpb.dNumberOfFreeClusters], -1
    je .exit
    dec dword [rbp + dpb.dNumberOfFreeClusters]
.exit:
    popfq
    return