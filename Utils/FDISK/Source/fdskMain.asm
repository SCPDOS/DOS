;Main FDISK file
startFdisk:
    jmp short .cVersion
.vNum:          db 1
.cVersion:
    cld
    xor eax, eax
    int 4Ah
    test eax, eax
    jz notMultitasking
    lea rdx, multiMsg
    mov eax, 0900h
    int 41h
.inLoop:
    mov eax, 0800h  ;Console input no echo
    int 41h
    cmp al, CR
    je notMultitasking
    cmp al, ESC 
    jne .inLoop
    jmp badExit
notMultitasking:
;Check Version Number
    mov ah, 30h
    int 41h
    cmp al, byte [startFdisk.vNum] ;Version 1
    jbe .okVersion
    lea rdx, badVerStr
    jmp badPrint ;Exit to caller or DOS to print bad version
.okVersion:
    call biosGetNumberOfFixedDisks
    cmp byte [numDisks], 0
    jnz .nonZeroDisks   ;Jump if we have any Fixed Disks
    lea rdx, noDisks
    jmp badPrint
.nonZeroDisks:
;Set CtrlC hook
    lea rdx, exit.ctrlCHandler
    mov eax, 2543h
    int 41h
;Print Start message
    lea rdx, strtMsg
    call print
    call printVersion
    lea rdx, cpyrtMsg
    call print
    ;Allocate 512 bytes now
    mov eax, 4800h
    mov ebx, fddSectorSize >> 4 
    int 41h
    jc badMemoryExit
    mov qword [xferBuffer], rax

    mov byte [currentDisk], 1
    call biosGetHardDiskParameters  ;Start by getting the FDD params for disk 1
    cmp word [sectorSize], fddSectorSize
    jne badSectorExit
    call getPartitionFlags  ;Get the partition flags

mainLoop:
    call printcrlf
    call printcrlf
    lea rdx, mainPageMsg
    call print
    cmp byte [numDisks], 1
    je .singlefdisk
    lea rdx, mpOptionalMsg
    call print
.singlefdisk:
    lea rdx, exitOptionMsg
    call print
    call printPrompt    ;Now print the prompt
;Now we input loop
    mov bl, 2
    call takeInput
    cmp byte [charsTyped], 1
    jne mainLoop    ;If the user didnt type 1 char, reprompt the user
    call printcrlf
    mov al, byte [inputString]  ;Get the char typed
    cmp al, "1"
    je createPtnMain
    cmp al, "2"
    je changeActivePtnMain
    cmp al, "3"
    je deletePtnMain
    cmp al, "4"
    je displayPtnInfoMain
    cmp byte [numDisks], 1
    je .singleDiskOptions
    cmp al, "5"
    je selectDiskMain
.singleDiskOptions:
    cmp al, "X"
    je exit
    cmp al, "x"
    je exit
    ;If we didnt get any valid options, go back again
    ;Prompt the user to make a valid selection
    lea rdx, selectValidOption
    call print
    jmp mainLoop


createPtnMain:
;Read the MBR for the selected Fixed Disk
;If there is a discernable MBR, error and return to mainLoop
;Else, go through the createPage
    xor edx, edx    ;Read sector 0
    call sectorRead
    jc badReadExit
    ;Sector in buffer, now we check
    mov rbx, qword [xferBuffer]
    movzx eax, word [rbx + mbr.mbrSig]  ;Get this sig
    cmp ax, 0AA55h  ;Usual
    je .mbrFound
    cmp ax, 055AAh  ;Unusual but we accept it
    jne .createNewMBR
.mbrFound:
;An MBR was found here. 
;DOS can currently only be installed on the first partition. Thus we 
; only check the first partition entry for a DOS signature. If one is 
; found, we refuse to proceed.
;We also refuse to use CHS. Set those entries to 0
    push rbx
    call getPartitionFlags  ;Now lets get some stats on the partition
    pop rbx
    cmp byte [numValidPtn], 0   ;If no valid partitions, act like new MBR
    je .createNewMBR
    mov al, byte [rbx + mbr.mbrEntry1 + mbrEntry.ptnType]
    cmp al, 01h
    je .fatPtnDetected
    cmp al, 04
    je .fatPtnDetected
    cmp al, 06
    je .fatPtnDetected
    cmp al, 0Bh
    je .fatPtnDetected
    cmp al, 0Ch
    je .fatPtnDetected
    cmp al, 0Eh
    je .fatPtnDetected
    ;Here we have space available, take the data from the table
    ; put it into the variables and jump
    ;Since the ptn size isnt changing, CHS values can remain in situ
    mov byte [ptnType], al
    mov eax, dword [rbx + mbr.mbrEntry1 + mbrEntry.lbaStart]
    mov dword [ptnStart], eax
    mov eax, dword [rbx + mbr.mbrEntry1 + mbrEntry.numSectors]
    mov dword [ptnSize], eax
    jmp .installPartition
.fatPtnDetected:
    call printcrlf
    call printcrlf
    lea rdx, createPageBadMsg
    call print
    jmp mainLoop
.createNewMBR:
;Here we lay down a fresh MBR.
;Prompt the user for their ptn size
    lea rdx, createPageMsg
    call getYNresponse  ;ZF=ZE => Y response, ZF=NZ => N response
    ;If the user responds Y, then we allocate the whole disk
    jnz .partialAllocation
    ;If here, start from "cylinder 1"
    mov dword [ptnStart], 40h
    mov eax, dword [curDiskSize]
    sub eax, dword [ptnStart]    ;Remove the sectors from the start of the disk
    mov dword [ptnSize], eax
    ;Always produce ptn types 0Ch - FAT with LBA (technically FAT32)
    call getPtnType
    jmp .copyMBR
.partialAllocation:
    lea rdx, createPage2Msg
    call print
.paLp:
    lea rdx, createPromptMsg
    call print
    mov bl, 3
    call takeInput
    cmp byte [charsTyped], 0    ;Repeat prompt if no chars typed (somehow)
    je .paLp 
    cmp byte [inputString], "x"
    je mainLoop
    cmp byte [inputString], "X"
    je mainLoop
    mov ax,"00"
    cmp byte [charsTyped], 1
    ja .twoDigits
    mov al, byte [inputString]
    jmp short .gotDigits
.twoDigits:
    mov ah, byte [inputString]
    mov al, byte [inputString + 1]
.gotDigits:
;Now check the digits are ok
;ah contains high digit, al contains low digit
    call getValue
    jc .paLp
    ;eax has the percentage of the disk to use
    mov ebx, dword [curDiskSize]    ;Get the disk size
    mul ebx
    mov ebx, 100    ;Divide by 100
    xor edx, edx
    div ebx ;Get in eax the number of sectors to allocate rounded down
    mov dword [ptnStart], 64
    sub eax, dword [ptnStart]
    mov dword [ptnSize], eax
    call getPtnType
.copyMBR:
;Now we copy the MBR into the xferBuffer
    lea rsi, freshMBRcopy   ;Has Zeroed CHS fields
    mov rdi, qword [xferBuffer]
    mov ecx, 200h/8
    rep movsq   ;Copy it over
.installPartition:
    mov rbx, qword [xferBuffer]
    mov eax, dword [ptnStart]
    mov dword [rbx + mbr.mbrEntry1 + mbrEntry.lbaStart], eax
    mov eax, dword [ptnSize]
    mov dword [rbx + mbr.mbrEntry1 + mbrEntry.numSectors], eax
    movzx eax, byte [ptnType]
    mov byte [rbx + mbr.mbrEntry1 + mbrEntry.ptnType], al
    ;Finish by marking this partition as active, and clearing
    ; active flag from other partitions
    mov byte [rbx + mbr.mbrEntry1 + mbrEntry.ptnAtrib], 80h
    mov byte [rbx + mbr.mbrEntry2 + mbrEntry.ptnAtrib], 00h
    mov byte [rbx + mbr.mbrEntry3 + mbrEntry.ptnAtrib], 00h
    mov byte [rbx + mbr.mbrEntry4 + mbrEntry.ptnAtrib], 00h

    xor edx, edx    ;Write sector 0 on the selected disk 
    call sectorWrite
    jc badWriteExit
    ;Now setup the partial VBR to the first sector of the partition
    ;Start by sanitising the Buffer
    mov rdi, qword [xferBuffer]
    xor eax, eax
    mov ecx, 200h/8
    push rdi
    rep stosq
    pop rdi
    lea rsi, partialVBR
    mov ecx, partialVBRL
    rep movsb
    ;xferBuffer has the partial vbr in it now
    ;rbx points to it
    mov eax, dword [ptnStart]
    mov dword [rbx + bpb.hiddSec], eax
    mov eax, dword [ptnSize]
    test eax, 0FFFF0000h ;Check for high bits
    jnz .fat32Var
    mov word [rbx + bpb.totSec16], ax
    mov dword [rbx + bpb.totSec32], 0
    jmp short .writeVBR
.fat32Var:
    mov word [rbx + bpb.totSec16], 0
    mov dword [rbx + bpb.totSec32], eax
.writeVBR:
    mov edx, dword [ptnStart]
    call sectorWrite
    jc badWriteExit

    lea rdx, createDoneMsg
    call print
    jmp mainLoop

changeActivePtnMain:
    ;Now we check which partitions are online. 
    ; If no partitions are OK, prompt the bad MBR string and return to mainLoop
    ; If 1 partition is OK and Active, say Partition already active and ret.
    ; If 1 partition is OK and set active and say Partition already...
    ; If more than 1 ptn is OK, prompt which partition to set active
    call getMBRandCheckValid
    jc badReadExit
    jnz mainLoop
    call printPartitionStatusTable
    cmp byte [numValidPtn], 0  ;If no partitions are valid, prompt to create
    jnz .validPtn
    call printBadMBR
    jmp mainLoop 
.validPtn:
;At least 1 partition is ok.
;If it is exactly 1 partition that is ok, check it is active too
    cmp byte [numValidPtn], 1
    jne .manyPartitions
    ;Now we check if that one partition is active. If it is, write message.
    ;If it is not, set it to active and write message.
    ;First we find partition
    xor eax, eax  ;Start from partition 1
.ptnSearchLp:
    inc eax
    call checkPartitionValid
    jz .ptnSearchLp ;This can never infinite loop
;eax has the partition number to check for being active
;mbr_size is 16 bytes so shift eax left by 4
    mov ecx, eax    ;Save the number in ecx
    dec eax ;Turn eax into an offset
    shl eax, 4  ;Multiply by 16
    mov ebx, eax
    mov rsi, qword [xferBuffer]
    lea rsi, qword [rsi + mbr.mbrEntry1 + rbx]
    ;rsi now points to the 1 partition
    test byte [rsi + mbrEntry.ptnAtrib], 80h    ;Is active bit set?
    jnz .singleActive   ;If yes, skip setting
    or byte [rsi + mbrEntry.ptnAtrib], 80h  ;Else set the bit
    xor edx, edx
    call sectorWrite    ;Write that sector back to the disk
    jc badWriteExit
.singleActive:
;Use the partition number in ecx to fix message
    or cl, "0"
    mov byte [activeSinglePtnMsg.number], cl
    lea rdx, activeSinglePtnMsg
    call print
    jmp mainLoop

.manyPartitions:
;If multiple partitions are marked as active, claim invalid MBR
    mov al, byte [ptnFlags]
    shr al, 4   ;Bring the high nybble bitfield into low nybble
    cmp al, 1
    je .manyValid
    cmp al, 2
    je .manyValid
    cmp al, 4
    je .manyValid
    cmp al, 8
    je .manyValid
    ;Multiple active partitions is a bad MBR. Exit and rebuild MBR
    call printBadMBR
    jmp mainLoop
.manyValid:
    lea rdx, activePromptMsg
    call print
    mov bl, 2
    call takeInput
    cmp byte [charsTyped], 1
    je .manyValid
    movzx eax, byte [inputString]  ;Get the first byte
    cmp al, "1"
    jb .manyValid
    cmp al, "4"
    ja .manyValid
    mov byte [activePartitionSetMsg.number], al ;Set this as the new active ptn
    sub al, "1"
    ;al is now a 0 based partition number
    shl eax, 4  ;Multiply by 16 to get number of bytes into the table
    mov rsi, qword [xferBuffer]
    lea rsi, qword [rsi + mbr.mbrEntry1]    ;Point to the first table entry
    lea rdi, qword [rsi + rax]  ;Get the pointer to mark as active
    mov ecx, 4
.setActiveBytes:
    call .editActiveByte
    dec ecx
    jnz .setActiveBytes
    
    lea rdx, activePartitionSetMsg
    call print
    ;Print the status table again to confirm what has happened
    call printPartitionStatusTable
    xor edx, edx
    call sectorWrite    ;Write the sector to disk with updated 
    jc badWriteExit
    jmp mainLoop
    
.editActiveByte:
;rsi -> current partition entry
;rdi -> Entry to mark as active
    mov byte [rsi + mbrEntry.ptnAtrib], 00h ;Always clear
    cmp rsi, rdi
    retz
    mov byte [rsi + mbrEntry.ptnAtrib], 80h ;Else, set the bit
    return

deletePtnMain:
;The user cannot delete an active partition UNLESS it is the last
; partition left on the MBR
    call getMBRandCheckValid    ;Gets up to date info on our MBR
    jc badReadExit
    jnz mainLoop
    call printPartitionStatusTable
    cmp byte [numValidPtn], 0
    jne .notNewMBR
    call printBadMBR
    jmp mainLoop 
.notNewMBR:
    cmp byte [numValidPtn], 1
    jne .multiplePartitions
    ;Here we have a single partition, do we want to delete
.nukeMBR:
    lea rdx, deleteNukeMsg
    call getYNresponse  ;ZF = ZE => Y, ZF = NZ => N
    jnz mainLoop
    ;Nuke the whole partition table
    mov rdi, qword [xferBuffer]
    lea rdi, qword [rdi + mbr.mbrEntry1]
    mov ecx, 8
    xor eax, eax
    rep stosq
    xor edx, edx
    call sectorWrite
    jc badWriteExit
    stosw   ;Clear the bootable signature too
    lea rdx, deleteNukeCompleteMsg
    call print
    jmp mainLoop
.multiplePartitions: 
;Now before we proceed, check if we have multiple active partitions.
;If so, we jump to the nuke subroutine
    mov al, byte [ptnFlags]
    shr al, 4
    cmp al, 1
    je .multipleOk
    cmp al, 2
    je .multipleOk
    cmp al, 4
    je .multipleOk
    cmp al, 8
    je .multipleOk
    lea rdx, deleteBadFoundMsg
    call print
    jmp short .nukeMBR
.multipleOk:
    lea rdx, deleteSelectMsg
    call print
    mov bl, 2
    call takeInput
    cmp byte [charsTyped], 1
    je .multiplePartitions
    movzx eax, byte [inputString]  ;Get the first byte
    cmp al, "1"
    jb .multiplePartitions
    cmp al, "4"
    ja .multiplePartitions
    mov bl, al
    sub al, "1"
    ;al is now a 0 based partition number
    shl eax, 4  ;Multiply by 16 to get number of bytes into the table
    mov rsi, qword [xferBuffer]
    lea rsi, qword [rsi + mbr.mbrEntry1 + rax]    ;Point to the entry we need
    test byte [rsi + mbrEntry.ptnAtrib], 80h
    jz .okToDelete
    lea rdx, deleteCannotMsg
    call print
    jmp mainLoop
.okToDelete:
    mov byte [deleteOkMsg.number], bl
    xor eax, eax
    stosq
    stosq
    xor edx, edx
    call sectorWrite
    jc badWriteExit
    lea rdx, deleteOkMsg
    jmp mainLoop

displayPtnInfoMain:
;This is done
    call getMBRandCheckValid
    jc badReadExit
    jnz mainLoop
    call printPartitionStatusTable
    jmp mainLoop

selectDiskMain:
;We select the disk and also read its size and sector size into the variables
;1) Say how many disks there are
;2) Prompt the user to select a disk
;3) Ensure it is a valid number
;4) Set the disk number variable
;5) Get the disk parameters
;6) Exit
;If al is greater than 8, then FDISK (for now), can only handle 8 hard disks
    mov al, byte [numDisks] ;Get the number of disks
    cmp al, 8
    jbe .proceed
    mov al, 8
.proceed:
    add al, "0" ;Convert to ASCII
    mov byte [selectNumber.number], al
    mov byte [selectPrompt.number], al
    lea rdx, selectNumber
    call print
.promptLoop:
    lea rdx, selectPrompt
    call print
    mov bl, 2
    call takeInput
    cmp byte [charsTyped], 1
    je .promptLoop
    movzx eax, byte [inputString]  ;Get the first byte
    cmp al, "1"
    jb .promptLoop
    cmp al, "8"
    ja .promptLoop
    sub al, "0" ;Get 1 based number 
    mov byte [currentDisk], al  ;Change the current Disk value
    call biosGetHardDiskParameters  ;Now we get the FDD parameters.
    jmp mainLoop

exit:
;In this case exit is done by ways of triple fault
;To do this, we hook int43h to prevent the user from stopping this 
; process and to link it directly to this the triple fault
    test byte [reboot], -1
    jnz .reboot
    call freeResources
    mov eax, 4C00h
    int 41h
.reboot:
    lea rdx, exitMsg
    mov eax, 0900h
    int 41h
    lea rdx, .badInstruction
    mov eax, 2543h
    int 41h
    mov eax, 0800h  ;Input no echo
    int 41h
.badInstruction:
    lidt [.resetIDT] ;Triple fault the machine
    lgdt [.resetIDT]
    xor eax, eax
    mov cs, ax
    int 00h ;Call div by 0 to trigger reboot if not somehow failed yet
.resetIDT:
    dw 0
    dq 0
.ctrlCHandler:
;If the reboot flag is set, kill the computer
    test byte [reboot], -1
    jnz .reboot
    call freeResources
    stc ;Continue with the exit
    ret ;Return with CF set kills the task

badWriteExit:
    lea rdx, badWriteExit
    jmp short badPrint
badReadExit:
    lea rdx, badReadMsg
    jmp short badPrint
badMemoryExit:
    lea rdx, noMemoryMsg
    jmp short badPrint
badSectorExit:
    lea rdx, badSectorMsg
badPrint:
    mov ah, 09h
    int 41h
badExit:
;If we need to reboot, reboot even after an error.
    test byte [reboot], -1
    jnz exit.reboot
    call freeResources
    mov eax, 4CFFh  ;Exit bad
    int 41h

freeResources:
;Any memory taken from DOS, free it here
    cmp qword [xferBuffer], 0
    retz
    push rax
    push r8
    mov r8, qword [xferBuffer]
    mov eax, 4900h
    int 41h
    pop r8
    pop rax
    return