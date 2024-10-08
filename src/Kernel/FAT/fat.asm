;-----------------------------------:
;       File System routines        :
;-----------------------------------:
getFATtype:
;Gets a pointer to a DPB and returns the FAT type on the drive
;Entry: rbp = DPB to ascertain FAT
;Exit: ecx = 0 => FAT 12, ecx = 1 => FAT 16, ecx = 2 => FAT 32
    push rbx
    mov ebx, dword [rbp + dpb.dMaxClusterAddr]
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
    mov ebx, dword [rbp + dpb.dMaxClusterAddr]
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
    mov cl, byte [rbp + dpb.bSecPerClustShift]
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
    je .exit2   ;Exit synching disk full status
    mov esi, -1 ;Value to write at eax is EOF
    mov ebx, eax
    call writeFAT
    jc .exit 
    mov eax, ebx
    call decrementFreeClusterCount
.exit2:
    push rax
    call writeFSInfoSector
    pop rax
.exit:
    pop rsi
    pop rbx
    return

allocateClusters:
;Working dpb must be set. 
;Input: ecx = Number of clusters to allocate in a chain
;       ebx = Cluster to link to the new cluster
;Output:    
;   ecx = Number of allocated clusters
;   CF=NC => Complete.
;   If eax = -1, then no more free clusters. 
;   CF=CY => Hard error, exit
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
    push rax
    push rcx
    call writeFSInfoSector
    pop rcx
    pop rax
    popfq
    pop rsi
    pop rbx
    return

findFreeCluster:
;Walks the FAT to find a free cluster and returns the 
;   zero extended cluster number in eax (-1 means no free cluster)
;Starts from the pervious first free cluster and wraps around if 
; we hit the max disk size and keeps searching until we get back
; to the starting cluster value. 
;If the first free cluster is unknown, we start from 2. If we hit the 
; max disk size in that case, we fail.
;Works on the workingDPB.
;If returns with CF=CY => Fail set, return immediately to caller.
    push rbx
    push rdx
    push rbp
    mov rbp, qword [workingDPB]
    mov eax, dword [rbp + dpb.dNextFreeClst]
    cmp eax, -1     ;Do we have a good starting free cluster? 
    jne .lp
    ;Check if the count of free clusters is 0. If so, we are full!
    cmp dword [rbp + dpb.dFreeClustCnt], 0
    je .diskFull
    ;Else, we just have unreliable metadata. Start search from 2!
    mov eax, 2      ;If not, start searching from 2.
    mov dword [rbp + dpb.dNextFreeClst], eax    ;Start searching here!
.lp:
    mov ebx, eax
    call readFAT    ;Deref the cluster value of ebx into eax
    jc .exitFast
    test eax, eax   ;Now eax = DATA [ebx]. If 0, this is free!
    jz .freeFnd
    lea eax, dword [ebx + 1]    ;Else, get sequentially the next cluster
    ;If these are equal, we've gone through the whole FAT. Disk full!
    cmp eax, dword [rbp + dpb.dNextFreeClst]
    je .diskFull    
    ;Are we past the max cluster address?
    cmp eax, dword [rbp + dpb.dMaxClusterAddr]
    jbe .lp ;If not, use this next cluster value!
;Else, we are past end of the disk. Start from 2 again or disk full?
    mov eax, 2  ;Default to search from 2 again
    ;If the firstfreecluster = 2, then we've searched everything. Fail.
    cmp dword [rbp + dpb.dNextFreeClst], eax    
    jne .lp
.diskFull:
;Set free cluster count to 0 and first free cluster to -1
    mov dword [rbp + dpb.dFreeClustCnt], 0  ;Set the free clusters
    mov ebx, -1     ;Set cluster value to -1
.freeFnd:
    mov eax, ebx    ;Get the free cluster value into eax
    mov dword [rbp + dpb.dNextFreeClst], eax    ;Save first free cluster!
    clc
.exitFast:
    pop rbp
    pop rdx
    pop rbx
    return

findFreeClusterData:
;Walks the FAT to find a free cluster and returns the 
;   zero extended cluster number in eax (-1 means no free cluster)
; Also finds NumberOfFreeClusters, if it is unknown. 
; Both fields get filled in the workingDPB
;Works on the workingDPB
;If returns with CF=CY => Fail set, return immediately to caller
    push rbx
    push rcx
    push rdx
    push rbp
    mov rbp, qword [workingDPB]
    call getFATtype
    cmp ecx, 2  ;If not FAT32, always get afresh
    jne .getNew
    ;Else, if either entry is unknown, get both!
    cmp dword [rbp + dpb.dFreeClustCnt], -1
    je .getNew
    cmp dword [rbp + dpb.dNextFreeClst], -1
    jne .exit
.getNew:
    ;Reset this data. Get first free cluster
    mov dword [rbp + dpb.dNextFreeClst], -1
    mov dword [rbp + dpb.dFreeClustCnt], -1
    call findFreeCluster
    jc .exitFail
    mov dword [rbp + dpb.dFreeClustCnt], 0  ;Set to 0 for new count
    cmp eax, -1 ;First free cluster = -1 means disk full!
    je .exit
    ;Else, eax has the first free cluster. Keep reading FAT
    call incrementFreeClusterCount  ;Increment the count for the first clust!
.lp:
    mov ebx, eax    ;Save the cluster number
    call readFAT    ;Get dereferenced value in eax
    jc .exitFail
    test eax, eax
    jnz .notFree
    call incrementFreeClusterCount  ;Increment the count!
.notFree:
    lea eax, dword [ebx + 1]    ;Get the next consecutive cluster
    cmp eax, dword [rbp + dpb.dMaxClusterAddr]
    jbe .lp
.exit:
    mov eax, dword [rbp + dpb.dNextFreeClst]  ;Get first free cluster in eax
    call writeFSInfoSector
    clc
.exitFail:      ;Keep carry flag
    pop rbp
    pop rdx
    pop rcx
    pop rbx
    return

getNextSectorOfFile:
;This function will return the next sector of a file in rax
;If the next sector to be read lives in the next cluster, it will update
; the vars appropriately
;
;Input: rbp = dpb pointer
;Output:
;       CF=NC => rax = Next sector to read into a memory buffer
;           If ZF=ZE => [currClustF] = Last Cluster of File.
;       CF=CY => Critical error occurred and was FAILed
;Read next sector. If at last sector in cluster, walk map, get
; next cluster and read first sector 
    ;Check if we need to go to next cluster
    mov al, byte [currSectC]    ;Get current sector rel Cluster
    cmp al, byte [rbp + dpb.bMaxSectorInCluster]
    je .gotoNextCluster
    ;Goto next sector in same cluster
    inc byte [currSectC]    ;Goto next sector in cluster
    inc qword [currSectD]  ;Goto next sector on Disk
    mov rax, qword [currSectD]
.exit:
    push rax
    xor eax, eax
    inc eax ;Ensure ZF=NZ
    pop rax
    clc     ;And CF=NC
    return
.gotoNextCluster:
    mov eax, dword [currClustD] ;Get absolute cluster number
    call readFAT
    retc
    ;eax now has the next cluster number to read (or -1 if EOF)
    cmp eax, -1 ;ZF=ZE and CF=NC if they are equal
    rete
;Update the new cluster and sector information
    mov dword [currClustD], eax ;Update disk location of next cluster
    inc dword [currClustF]   ;Goto next file cluster
    call getStartSectorOfCluster    ;Get start sector of Cluster
    mov qword [currSectD], rax  ;Save it
    mov byte [currSectC], 0      ;We are at sector 0 rel Clust
    jmp short .exit


truncateFAT:
;Same as below but always sets the cluster we are unlinking at to EOC.
;Input: eax = Cluster to start unlinking at (zero extended to 32 bits)
;       rbp = Current DPB to use for disk
;Output: CF = NC => All ok. CF = CY => Hard Error, exit
    push rax    ;Save the cluster number to start unlinking at
    push rsi
    call freeChainFAT   ;Preserves eax, frees the full chain.
    jc .exit
    mov esi, -1         ;Realloc the cluster we are freeing from as EOC
    push rax            ;Save the cluster we are writing at
    call writeFAT
    pop rax
    jc .exit
    call decrementFreeClusterCount  ;Remove it from the free cluster count!
.setFree:
;Do this stuff here, because we want to update the free count too!
    mov dword [rbp + dpb.dNextFreeClst], eax ;And set this to search from
    call writeFSInfoSector  ;New first free cluster values
.exit:
    pop rsi
    pop rax
    return
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
;The below call decrements the cluster count for the full chain freed including
; the starting cluster address.
    call freeChainFAT    ;Preserved eax. May set eax to first free cluster.
    jnc truncateFAT.setFree ;Set the first free sector to eax
    jmp short truncateFAT.exit  ;If an error, skip updating the FSInfo sector

freeChainFAT:
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
    cmp eax, 0FFF7h  ;Valid cluster number?
    jb .exit
    jmp short .eocExit
.goToNextClusterFat32:
    mov eax, dword [rbx + bufferHdr.dataarea + rdx]
    cmp eax, 0FFFFFF7h ;First reserved value. Any Reserved number = EOC
    jb .validCluster32   
.eocExit:
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
;eax has the sector number of the FAT
;edx has byte offset into the sector
;edi has current cluster number
;rbx has ptr to buffer header
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    test edi, 1  ;Check if cluster is odd
    jz .gotoNextClusterFat12Even
    ;Here the cluster is ODD, and might cross sector boundary
    sub ecx, edx
    dec ecx ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12OddNoX
    call .xBndry        ;Boundary cross, build entry properly
    jc .exitFail    
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .checkIfLastFAT12Cluster
.gotoNextClusterFat12OddNoX:
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]    ;Read the entry
    shr eax, 4   ;Save upper three nybbles of loword, eax has cluster num
    jmp short .checkIfLastFAT12Cluster
.gotoNextClusterFat12Even:
    sub ecx, edx
    dec ecx ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12EvenNoX
    call .xBndry    ;Save the lower three nybbles
    jc .exitFail
    jmp short .evenCmn
.gotoNextClusterFat12EvenNoX:
    ;Here the cluster is even and can't cross a sector boundary
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]    ;Read the entry
.evenCmn:
    and eax, 0FFFh   ;Save lower three nybbles, eax has cluster num
.checkIfLastFAT12Cluster:
    cmp eax, 0FF7h   ;Is it below the first invalid cluster number?
    jb .exit         ;If so, exit with it in eax (and clear CF)
    jmp short .eocExit
.xBndry:
;Gets a word that goes across a boundary in ax. It is left to the caller it 
; do what they will with it. If CF=CY on return, something went wrong.
    movzx ebx, byte [rbx + bufferHdr.dataarea + rdx] ;Use ebx as it is free
    inc eax ;Get next FAT sector
    push rbx
    call getBufForFat ;Get buffer Header in ebx
    pop rcx ;Get bl in ecx, the last entry from the previous buffer
    retc
    mov eax, ecx    ;Move the entry if all ok
    mov ah, byte [rbx + bufferHdr.dataarea]  ;Read first entry of next sector
    return

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
    movzx ecx, word [rbp + dpb.wBytesPerSector]
    and esi, 0FFFh  ;Clear the upper bits. Save only low 12 bits
    test edi, 1  ;Check if cluster is odd
    jz .gotoNextClusterFat12Even
    ;Here the cluster is ODD, and might cross sector boundary
    sub ecx, edx
    dec ecx ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12OddNoX
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
.gotoNextClusterFat12Even:
    sub ecx, edx
    dec ecx ;If edx = BytesPerSector - 1 then it crosses, else no
    jnz .gotoNextClusterFat12EvenNoX
    mov ecx, esi
    and ch, 0Fh ;Save only the lower nybble of ch
    mov byte [rbx + bufferHdr.dataarea + rdx], cl   ;Store the first byte
    call markBufferDirty
    inc eax ;Get next FAT sector
    call getBufForFat
    jc .exitFail
    mov cl, byte [rbx + bufferHdr.dataarea] ;Get the first data byte from buffer
    and cl, 0F0h    ;Clear the lower nybble of this entry
    or cl, ch       ;Add our entry in
    mov byte [rbx + bufferHdr.dataarea], cl ;Write it back
    jmp short .exit    
.gotoNextClusterFat12OddNoX:
    movzx eax, word [rbx + bufferHdr.dataarea + rdx]    ;Read the entry
    and eax, 0Fh    ;Clear the upper three nybbles of entry (the entry)
    shl esi, 4  ;Shift entry up by 4
    jmp short .fat12common
.gotoNextClusterFat12EvenNoX:
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
    cmp dword [rbp + dpb.dFreeClustCnt], -1
    je .exit
    inc dword [rbp + dpb.dFreeClustCnt]
.exit:
    popfq
    return
decrementFreeClusterCount:
;Cluster Allocated Function
    pushfq
    cmp dword [rbp + dpb.dFreeClustCnt], -1
    je .exit
    dec dword [rbp + dpb.dFreeClustCnt]
.exit:
    popfq
    return

getBytesPerCluster:
;Gets the bytes per cluster
;Input: rbp -> Current DPB
;Output: ecx = Total bytes per cluster
    push rax
    push rdx
    movzx eax, word [rbp + dpb.wBytesPerSector]
    movzx ecx, byte [rbp + dpb.bMaxSectorInCluster]
    inc ecx
    mul ecx
    mov ecx, eax
    pop rdx
    pop rax
    return

readFSInfoSector:
;Given a DPB, will attempt to read the FS Info sector. Destroys all regs.
;Input: rbp -> DPB pointer for FAT32 volume. Silently returns ok
;               if the DPB is not FAT32.
;       rsi -> BPB for the FAT32 volume.
;Output: rbp -> DPB dNextFreeClst and dNumberOfFreeCluster fields
;               correctly filled in from the FSInfo sector, if appropriate.
    push rcx
    call getFATtype
    cmp ecx, 2  ;Is the DPB a FAT32 DPB?
    pop rcx
    jne .exit
;Only read the FSinfo sector if the drive has never been accessed before!
    test byte [rbp + dpb.bAccessFlag], -1
    jz .exit
    movzx eax, word [rsi + bpb32.FSinfo]    ;Get the FSInfo sector number
    test eax, eax   ;0 is an invalid value for this 
    retz
    cmp eax, 0FFFFh ;If this is unknown, return
    rete
    mov qword [workingDPB], rbp ;Set this because it isnt set yet!
    call getBufForDOS   ;Now get the sector number
    retc
    ;Now sanity check the sector data itself
    cmp dword [rbx + bufferHdr.dataarea + FSInfo.leadSig], fsInfoSig1
    jne .exit
    cmp dword [rbx + bufferHdr.dataarea + FSInfo.strucSig], fsInfoSig2
    jne .exit
    cmp dword [rbx + bufferHdr.dataarea + FSInfo.trailSig], fsInfoSig3
    jne .exit
;Here we can assume the struct is accessible. 
;We sanity check the struct values to ensure they are possible.
;We reset with some sane defaults in the event of bad data
    mov dword [rbp + dpb.dNextFreeClst], -1  ;Start is default (clust 2)
    mov dword [rbp + dpb.dFreeClustCnt], -1  ;Unknown
    ;Start with next free check
    mov eax, dword [rbx + bufferHdr.dataarea + FSInfo.nextFree] 
    cmp eax, dword [rbp + dpb.dMaxClusterAddr]
    ja .skipFirstFree
    cmp eax, 2
    jb .skipFirstFree
    mov dword [rbp + dpb.dNextFreeClst], eax
.skipFirstFree:
    ;Now we do free count check
    mov eax, dword [rbx + bufferHdr.dataarea + FSInfo.freeCount]
    cmp eax, dword [rbp + dpb.dMaxClusterAddr]
    ja .exit
    mov dword [rbp + dpb.dFreeClustCnt], eax
.exit:
    clc
    return

writeFSInfoSector:
;Will write an FS Info sector back to the disk.
;Input: rbp -> DPB of the disk we are writing the FSInfo date for.
;Output: CF=NC: Updated FSInfo fields if FAT32. CF=CY: Error reading disk

    push rcx
    call getFATtype
    cmp ecx, 2  ;Is the DPB a FAT32 DPB?
    pop rcx
    clc         ;Clear the CF flag if not FAT32
    retne

    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi

    xor eax, eax        ;Get sector 0
    call getBufForDOS   ;Get a buffer Sector 0 pointed to be rbx
    jc .exit
    movzx eax, word [rbx + bufferHdr.dataarea + bpb32.FSinfo]
    test eax, eax
    jz .exit
    cmp eax, 0FFFFh ;If this is unknown, return
    je .exit
;First compute where the backup FSInfo is
    mov ecx, eax    ;(e)ax has FSInfo sector number
    add cx, word [rbx + bufferHdr.dataarea + bpb32.BkBootSec]    
    ;cx now has the backup sector
;If they are equal, because backup boot sector is 0, set ecx to -1
    cmp ecx, eax
    mov edx, -1
    cmove ecx, edx  ;Set ecx to -1 to avoid writing the same sector twice
    
    push rcx            ;Save the backup FSInfo sector value
    call getBufForDOS   ;Now get the primary FSInfo sector
    pop rdi             ;Return the backup FSInfo sector value in rdi
    jc .exit

    xor esi, esi    ;Use as a dirty marker
    mov ecx, dword [rbp + dpb.dFreeClustCnt]
    cmp dword [rbx + bufferHdr.dataarea + FSInfo.freeCount], ecx
    je .checkFirst
    ;Update the value
    mov dword [rbx + bufferHdr.dataarea + FSInfo.freeCount], ecx 
    inc esi
.checkFirst:
    mov ecx, dword [rbp + dpb.dNextFreeClst]
    cmp dword [rbx + bufferHdr.dataarea + FSInfo.nextFree], ecx
    je .checkFlush
    mov dword [rbx + bufferHdr.dataarea + FSInfo.nextFree], ecx
    inc esi
.checkFlush:
    test esi, esi   ;If this is zero, we didn't write new values. Return
    jz .exit
    call markBufferDirty    ;Else, the sector is dirty. Mark for flushing!
    ;Now since we have a dirty sector, we need to sync the backup sector too!
    cmp edi, -1 ;Do we have a backup sector? If not, simply return
    je .exit
    mov eax, edi    ;Else get the backup FSInfo sector value in eax
    call getBufForDOS   ;Now get the sector pointer   
    jc .exit            ;Fail to sync if this fails
    ;Now we know there is something to write so write it!
    ;Also CF=NC here
    mov eax, dword [rbp + dpb.dFreeClustCnt]
    mov dword [rbx + bufferHdr.dataarea + FSInfo.freeCount], eax 
    mov eax, dword [rbp + dpb.dNextFreeClst]
    mov dword [rbx + bufferHdr.dataarea + FSInfo.nextFree], eax
    call markBufferDirty    ;Mark this buffer as dirty too
.exit:
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return