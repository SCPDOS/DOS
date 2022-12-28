
;General Utility functions
getValue:
;Input: ah = Upper ASCII digit
;       al = Lower ASCII digit
;Output: eax = Number typed in (between 0 and 99)
;           If CF=CY, error
    cmp al, "9"
    ja .bad
    cmp al, "0"
    jb .bad
    cmp ah, "9"
    ja .bad
    cmp ah, "0"
    jb .bad
;Digits are valid, now proceed
    sub ah, "0"
    sub al, "0"
    movzx ecx, al
    movzx eax, ah
    mov bl, 10
    mul bl  ;Multiply al with bl, result in al
    add al, cl
    movzx eax, al
    return
.bad:
    stc
    return

getPartitionFlags:
;Gets the partition flags ptnFlags and numValidPtn
    mov rsi, qword [xferBuffer]
    lea rsi, qword [rsi + mbr.mbrEntry1]
    mov byte [ptnFlags], 0  ;Clear the bitfield
    mov byte [numValidPtn], 0   ;And this variable
    mov bl, 1
    mov ecx, 4  ;Scan 4 MBR table entries
.ptnScanLoop:
    call .ptnSigSet
    dec ecx
    jnz .ptnScanLoop
    return

.ptnSigSet:
;Input: bl = On first call, with inital value of mask
    mov al, byte [rsi + mbrEntry.ptnType]
    test al, -1
    jz .pssNo
    or byte [ptnFlags], bl
    inc byte [numValidPtn]
    test byte byte [rsi + mbrEntry.ptnAtrib], 80h
    jz .pssNo
    shl bl, 4   ;Move bit to high nybble
    or byte [ptnFlags], bl
    shr bl, 4   ;Bring it back low
.pssNo:
    add rsi, mbrEntry_size
    shl bl, 1
    return

checkPartitionValid:
;Checks if the given partition number is valid or invalid
;Input: eax = Partition Number 1-4 
;Output: ZF = ZE if value in eax not valid
;        ZF = NZ if value in eax is valid
    push rax
    push rcx
    dec al  ;Turn into a zero based number (for shift factor)
    mov cl, al  
    mov al, 1   ;Set mask bit
    shl al, cl  ;Shift the mask bit into the right place
    mov cl, byte [ptnFlags] ;Get the flags
    and cl, 0Fh ;Keep only the low nybble
    and cl, al  ;Set the Status Flags. If this is zero, ptn not active
    pop rcx
    pop rax
    return

getPtnType:
;Based on ptnStart and ptnSize, puts the right type of partition
;Since we can only make these partitions (for now) at sector 64
; of the disk, we can always assume that ptnStart is at 64
;If size < 4MB, FAT12, use 01h
;If size < 65,536 sectors, use 04h
;If size < 1048576 sectors (512Mb), use 06h
;Else, use 0Ch
    mov eax, dword [ptnSize]
    cmp eax, 4085*2
    jb .fat12
    cmp eax, 65536
    jb .fat16
    cmp eax, 1048576
    jb .fat16b
    mov byte [ptnType], 0Ch
    return
.fat12:
    mov byte [ptnType], 01h
    return
.fat16:
    mov byte [ptnType], 04h
    return
.fat16b:
    mov byte [ptnType], 06h
    return


getYNresponse:
;Prints a message, and waits for input and verfies it is Y or N.
;Input: rdx = String to print
;Return: ZF = ZE => Y
;        ZF = NZ => N
    push rdx    ;Save the input string ptr
.ep:
    call print
    mov bl, 2
    call takeInput
    cmp byte [charsTyped], 1
    jne .ep
    movzx eax, byte [inputString]
    cmp al, "y"
    je .yResponse
    cmp al, "Y"
    je .yResponse
    cmp al, "n"
    je .nResponse
    cmp al, "N"
    je .nResponse
    pop rdx
    jmp short getYNresponse
.yResponse:
    pop rdx
    return
.nResponse:
    pop rdx
    cmp rax, rdx    ;These cannot be equal at all
    return


sectorRead:
;Input: rdx = Sector to read from disk into the buffer
    mov qword [sectorNum], rdx
    push rcx
    mov ecx, 0303h  ;ch=number of "resets", cl = Number of retrys
.tryAgain:
    call biosReadSector
    jnc .exit
    dec cl  ;One less retry
    jnz .tryAgain
    call biosResetHardDisk
    jc .exit    ;If this fails, just fail automatically
    mov cl, 3   ;Reset retry count
    dec ch      ;One less reset check
    jnz .tryAgain
    stc
.exit:
    pop rcx
    return

sectorWrite:
;Input: rdx = Sector to write to disk from the buffer
    mov qword [sectorNum], rdx
    push rcx
    mov ecx, 0303h  ;ch=number of "resets", cl = Number of retrys
.tryAgain:
    call biosWriteSector
    jnc .exitGood
    dec cl  ;One less retry
    jnz .tryAgain
    call biosResetHardDisk
    jc .exit    ;If this fails, just fail automatically
    mov cl, 3   ;Reset retry count
    dec ch      ;One less reset check
    jnz .tryAgain
    stc
.exit:
    pop rcx
    return
.exitGood:
    mov byte [reboot], -1   ;Successful write => Need to reboot now
    jmp short .exit

getMBRandCheckValid:
;If CF=CY, error, exit. Error reading disk
;If CF=NC and ZF=ZE, Valid MBR
;If CF=CY and ZF=NZ, Invalid or missing MBR, return to mainLoop
;Updates the partition info if all ok!
    xor edx, edx    ;Read sector 0
    call sectorRead
    retc
    mov rsi, qword [xferBuffer]
    movzx eax, word [rsi + mbr.mbrSig]  ;Get this sig
    cmp ax, 0AA55h  ;Usual
    je .greatMbr
    cmp ax, 055AAh  ;Unusual but we accept it
    je .greatMbr
    ;Bad Disk error, return to main menu
    call printBadMBR
    xor eax, eax
    inc eax
    return
.greatMbr:
    pushf
    call getPartitionFlags  ;Now update the partition detail flags
    popf
    return
    
takeInput:
;Input: bl = Number of chars to input
;Simply wraps the call to the buffered input function and sanitises the cmdline
    xor eax, eax
    mov dword [cmdLine], eax
    mov byte [cmdLine + 4], al
    lea rdx, cmdLine
    mov byte [rdx], bl   ;Read 1 char (plus CR)
    mov eax, 0C0Ah  ;Flush Input buffer and do Buffered Input
    int 41h
    return


;Print Util Functions
printBadMBR:
    lea rdx, invalidMBRMsg
    jmp short print
printExit:
    lea rdx, exitMsg
    jmp short print
printcrlf:
    lea rdx, crlf
print:
    mov eax, 0900h
    int 41h
    return

printPrompt:
    call printcrlf
    mov al, byte [currentDisk]
    add al, "0" ;Convert to ASCII digit
    mov byte [drvNum], al
    lea rdx, prompt
    jmp short print

printVersion:
    mov ah, 30h ;Get version numbers, al = Major, ah = Minor
    int 41h
    push rax
    movzx eax, al
    call printDecimalWordAtCursor
    mov dl, "."
    mov ah, 02h
    int 41h
    pop rax
    movzx eax, ah
    call printDecimalWordAtCursor
    return

printPartitionStatusTable:
;xferBuffer must have the MBR we are acting on loaded
;Start by cleaning the string
    call printcrlf
    call printcrlf
    lea rdx, partTitle
    call print

    mov rsi, qword [xferBuffer]
    lea rsi, qword [rsi + mbr.mbrEntry1]    ;Goto first table entry
    cmp byte [rsi + mbrEntry.ptnType], 00
    je .second
    mov bl, 1
    call .fillString
.second:
    add rsi, mbrEntry_size  ;Goto next entry
    cmp byte [rsi + mbrEntry.ptnType], 00
    je .third
    mov bl, 2
    call .fillString
.third:
    add rsi, mbrEntry_size  ;Goto next entry
    cmp byte [rsi + mbrEntry.ptnType], 00
    je .fourth
    mov bl, 3
    call .fillString
.fourth:
    add rsi, mbrEntry_size  ;Goto next entry
    cmp byte [rsi + mbrEntry.ptnType], 00
    je .exit
    mov bl, 4
    call .fillString
.exit:
    lea rdi, totalSpaceMsg.sizeCount
    mov rax, "        "
    push rdi
    stosq
    pop rdi
    mov eax, dword [curDiskSize]    ;Get the current disk size
    call printDecimalWord

    lea rdi, totalSpaceMsg.sectorCount
    mov eax, "    "
    push rdi
    stosd
    pop rdi
    movzx eax, word [sectorSize]
    call printDecimalWord

    lea rdx, totalSpaceMsg
    call print
    call printcrlf
    return

.fillString:
;Input:
; bl = Partition number (number 1-4)
; rsi -> mbr entry to write the string for
    lea rdi, partString
    mov al, SPC ;Fill the string with space characters
    mov ecx, partStringL
    rep stosb   ;Cleanse the string
    
    add bl, "0"
    mov byte [partString.ptnNum], bl
    test byte [rsi + mbrEntry.ptnAtrib], 80h
    jz .notActive
    mov byte [partString.ptnSts], "A"
.notActive:
;01h is FAT12
;04h, 06h and 0Eh are FAT16
;0Bh and 0Ch are FAT32 
    lea rdi, partString.ptnFS   ;Point to the FS part
    mov al, byte [rsi + mbrEntry.ptnType]   ;Get the type
    cmp al, 01
    je .fat12
    cmp al, 04h
    je .fat16
    cmp al, 06h
    je .fat16
    cmp al, 0Eh
    je .fat16
    cmp al, 0Bh
    je .fat32
    cmp al, 0Ch
    je .fat32
    ;Else we print the number
    call getHexTwoBytes
    xchg al, ah
    stosb
    mov al, ah
    stosb
    mov ah, "h"
    stosb
    jmp short .fatCont
.fat12:
    push rsi
    lea rsi, fat12String
    jmp short .fatCmn
.fat16:
    push rsi
    lea rsi, fat16String
    jmp short .fatCmn
.fat32:
    push rsi
    lea rsi, fat32String
.fatCmn:
    movsd
    movsb
    pop rsi
.fatCont:
    mov eax, dword [rsi + mbrEntry.lbaStart]
    lea rdi, partString.ptnStart
    push rax
    call printDecimalWord
    pop rax
    mov ebx, dword [rsi + mbrEntry.numSectors]
    add eax, ebx
    dec eax ;Get the address of the last sector (one less than extant)
    lea rdi, partString.ptnEnd
    push rbx
    call printDecimalWord
    pop rbx
    mov eax, ebx
    lea rdi, partString.ptnSize
    call printDecimalWord

    lea rdx, partString
    call print
    return



printDecimalWordAtCursor:
;Takes the qword in eax and prints its decimal representation
    xor ecx, ecx
    xor ebx, ebx    ;Store upper 8 nybbles here
    test eax, eax
    jnz .notZero
    mov ecx, "0"
    mov ebp, 1  ;Print one digit
    jmp short .dpfb2
.notZero:
    xor ebp, ebp  ;Use bp as #of digits counter
    mov esi, 0Ah  ;Divide by 10
.dpfb0:
    inc ebp
    cmp ebp, 8
    jb .dpfb00
    shl rbx, 8    ;Space for next nybble
    jmp short .dpfb01
.dpfb00:
    shl rcx, 8    ;Space for next nybble
.dpfb01:
    xor edx, edx
    div rsi
    add dl, '0'
    cmp dl, '9'
    jbe .dpfb1
    add dl, 'A'-'0'-10
.dpfb1:
    cmp ebp, 8
    jb .dpfb10
    mov bl, dl ;Add the bottom bits
    jmp short .dpfb11
.dpfb10:
    mov cl, dl    ;Save remainder byte
.dpfb11:
    test rax, rax
    jnz .dpfb0
.dpfb2:
    cmp ebp, 8
    jb .dpfb20
    mov dl, bl
    shr rbx, 8
    jmp short .dpfb21
.dpfb20:
    mov dl, cl    ;Get most sig digit into al
    shr rcx, 8    ;Get next digit down
.dpfb21:
    mov ah, 02h
    int 41h
    dec ebp
    jnz .dpfb2
    return

printDecimalWord:
;Takes the qword in eax and prints its decimal representation
;Result placed at [rdi]
    push rsi
    xor ecx, ecx
    xor ebx, ebx    ;Store upper 8 nybbles here
    test eax, eax
    jnz .notZero
    mov ecx, "0"
    mov ebp, 1  ;Print one digit
    jmp short .dpfb2
.notZero:
    xor ebp, ebp  ;Use bp as #of digits counter
    mov esi, 0Ah  ;Divide by 10
.dpfb0:
    inc ebp
    cmp ebp, 8
    jb .dpfb00
    shl rbx, 8    ;Space for next nybble
    jmp short .dpfb01
.dpfb00:
    shl rcx, 8    ;Space for next nybble
.dpfb01:
    xor edx, edx
    div rsi
    add dl, '0'
    cmp dl, '9'
    jbe .dpfb1
    add dl, 'A'-'0'-10
.dpfb1:
    cmp ebp, 8
    jb .dpfb10
    mov bl, dl ;Add the bottom bits
    jmp short .dpfb11
.dpfb10:
    mov cl, dl    ;Save remainder byte
.dpfb11:
    test rax, rax
    jnz .dpfb0
.dpfb2:
    cmp ebp, 8
    jb .dpfb20
    mov dl, bl
    shr rbx, 8
    jmp short .dpfb21
.dpfb20:
    mov al, cl    ;Get most sig digit into al
    shr rcx, 8    ;Get next digit down
.dpfb21:
    stosb
    dec ebp
    jnz .dpfb2
    pop rsi
    return

getHexTwoBytes:
;Given a number in al, get the HEX digits in ASCII in ax
;Input: al = 8 bit number 
;Output: ax = ASCII representation of the number
    push rbx
    push rdx
    lea rbx, .ascii
    movzx eax, al
    movzx edx, al
    and edx, 0Fh    ;Isolate low nybble
    shr eax, 4      ;Isolate high nybble and bring it low
    xlatb  ;Get high digit first
    mov dh, al  ;Save the digit in dh
    mov al, dl
    xlatb
    mov ah, dh  ;Get the high digit in ah to form ax
    pop rdx
    pop rbx
    return
.ascii: db "0123456789ABCEDF"

getDecimalWord:
;Works on MAX A dword in eax
;Gets the decimalised DWORD to print in rcx (at most 8 digits)
    xor ecx, ecx
    xor ebp, ebp  ;Use bp as #of digits counter
    mov ebx, 0Ah  ;Divide by 10
.dpfb0:
    inc ebp
    shl rcx, 8    ;Space for next nybble
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .dpfb1
    add dl, 'A'-'0'-10
.dpfb1:
    mov cl, dl    ;Save remainder byte
    test rax, rax
    jnz .dpfb0
    return