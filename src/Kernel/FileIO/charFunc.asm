;-----------------------------------------------------------------------:
;                  DOS default char functions live here                 :
;                                                                       :
; All input Char functions wait for input. Only directConIO doesnt wait :
;                                                                       :
;Rules for the naming of the DOS functions                              :
;If the name is <name>     => Has NO break checking and no echo         :
;If the name is <name>_B   => Has Break checking and no echo            :
;If the name is <name>_E   => Has No Break checking AND Echo to STDOUT  :
;If the name is <name>_BE  => Has Break checking AND Echo to STDOUT     :
;                                                                       :
;-----------------------------------------------------------------------:

charIn_BE:     ;ah = 01h
;Return char that has been read and echoed in al
    call charIn_B
    push rax
    call charOut_B.in    ;Output it to screen
    pop rax
    return

charOut_B:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    mov al, dl
.in:  ;Internal function Entry Point, with char in al, also Int 2Fh, AX=1205h
    cmp al, asciiCaret
    jb .control
    cmp al, DEL ;DEL char?
    je .skipCurs
    inc byte [vConCursPos]  ;Increment Cursor pos
.skipCurs:
    inc byte [vConErr]   ;Increment 2 char error checker
    and byte [vConErr], 3
    push rsi
    jnz .skipErrorCheck
    push rax
    call vConCtrlCheck
    pop rax
.skipErrorCheck:
    call outputOnStdout
    pop rsi
    test byte [printEcho], -1   ;Do we echo this char?
    retz    ;If zero, no echo
    push rbx
    push rsi
    mov ebx, 1  ;STDOUT handle 
    call getCharDevSFT  ;Get SFT handle in rsi or exit if CF=CY
    jc auxOutCmn.exit
    ;Ensure we only echo if STDOUT is a char device!!
    movzx ebx, word [rsi + sft.wDeviceInfo]
    test ebx, devRedirDev
    jnz auxOutCmn.exit  ;Exit if STDOUT is redir
    test ebx, devCharDev
    jz auxOutCmn.exit
    mov ebx, 4  ;STDPRN handle
    call getCharDevSFT  ;Get printer sft in rsi
    jc auxOutCmn.exit   ;Exit if handle closed
    test word [rsi + sft.wDeviceInfo], charDevNetSpool  ;Network printer?
    jz auxOutCmn.diskFileEP
    mov byte [printEcho], 0 ;Stop echoing
    jmp auxOutCmn.netFileEP
.control:
    cmp al, CR
    je .newline
    cmp al, BSP
    je .back
    cmp al, TAB
    jne .skipCurs   ;Treat as normal
    ;TAB key here
.tab:
    push rcx
    movzx ecx, byte [vConCursPos]
    or cl, ~7
    neg cl
    jecxz .stopTab ;If this and was 0, skip printing spaces
.tabloop:
    mov al, SPC
    call charOut_B.in   ;Output the char
    dec cl
    jnz .tabloop
.stopTab:
    pop rcx
    return ;Exit
.newline:
    mov byte [vConCursPos], 0   ;Start of the line
    jmp .skipCurs   ;And echo 
.back:
    dec byte [vConCursPos]
    jmp .skipCurs   ;And echo

auxIn_B:        ;ah = 03h
    call vConCtrlCheck  ;Check if STDIN has a ^C pending regardless
    mov ebx, 3
    call getCharDevSFT
    retc    ;Return if CF set (handle 3 is closed)
.auxloop:
    mov ah, 01h ;Do a non-destructive read of rsi (AUX SFT ptr)
    call mainCharIO
    jz .signalLoop
    xor ah, ah  ;Read the char in
    call mainCharIO
    return
.signalLoop:
    call callInt28h
    jmp short .auxloop

auxOut_B:       ;ah = 04h
    push rbx
    mov ebx, 3  ;STDAUX handle
    jmp short auxOutCmn
prnOut_B:       ;ah = 05h
    push rbx
    mov ebx, 4  ;STDPRN handle
auxOutCmn: ;Auxilliary output device common 
    mov al, dl  ;Get the char into al
    push rax
    call vConCtrlCheck  ;Check if STDIN has a ^C pending
    pop rax
    push rsi
.diskFileEP:
    call outputToHandle ;bx has handle, convert to sft ptr and output char!
.netFileEP:
.exit:
    pop rsi
    pop rbx
    return

directConIO:    ;ah = 06h
    mov al, dl  ;Move the char to print/subfunction into al
    cmp al, -1  ;Anything other than -1 means output the char
    jne outputOnStdout  ;So output on stdout and return via output function
;Here is the read char direct function
    xor ebx, ebx    
    call getCharDevSFT  ;Get the sft pointer in rsi
    retc    ;Return error if ebx closed
    mov rbp, qword [oldRSP] ;Get pointer to stack frame
    mov ah, 01h ;ND read from rsi sft ptr
    call mainCharIO
    call callInt28h ;This preserves flags so call here!
    jnz .readChar
    or byte [rbp + callerFrame.flags], 40h  ;Set Zero Flag
    xor al, al  ;Set caller return code to 0
    return
.readChar:
    and byte [rbp + callerFrame.flags], ~40h    ;Clear Zero Flag
    ;Fallthrough here to get the char at STDIN
charIn:         ;ah = 07h
;Return char in al from STDIN without waiting
    xor ebx, ebx
    call getCharDevSFT
    retc
    mov ah, 01  ;ND read for char
    call mainCharIO
    jnz .getChar
    mov ah, 84h ;Multitasking keyboard loop
    int 2Ah
    call callInt28h
    jmp short charIn    ;Loop again awaiting the char
.getChar:
    ;Get the char in al and exit
    xor ah, ah
    call mainCharIO
    return
charIn_B:       ;ah = 08h
;Return char in al from STDIN
    push rsi
.readAgain:
    call vConCtrlCheck  ;Check if the char at STDIN is ctrl and handle it
    ;Also sets rsi to point to the device SFT for 
    jnz .pullChar
    cmp byte [lookahead], 00    ;0 => Lookahead
    jne .skiplookahead
    mov ah, 05h ;Fake the lookahead for device in rsi
    call mainCharIO
.skiplookahead:
    mov ah, 84h
    int 2ah ;Multitasking keyboard loop
    cmp word [keybTicks], -1    ;We reached -1 yet?
    jne .skipClockRead
    call dosPushRegs
    clc ;Clear CF, write primary header to backup
    call swapPrimaryHeader
    call readDateTimeRecord
    stc ;Set CF, write backup to primary header
    call swapPrimaryHeader
    call dosPopRegs
.skipClockRead:
    inc word [keybTicks]
    jmp short .readAgain
.pullChar:
    xor ah, ah
    call mainCharIO ;Pull char from device buffer
    pop rsi
    cmp al, NUL ;Is this the null char?
    jne .exit
    ;We are mid extended ascii, prevent printer toggling
    mov byte [inExtASCII], 1    ;Set to be in the Extended ASCII
.exit:
    return

printString_B:      ;ah = 09h
    mov rsi, rdx    ;Set up for scasb
.ps0:
    lodsb   ;Get char in al and inc rsi
    cmp al, "$" ;End of string char?
    rete    ;Return if equal
    call charOut_B.in
    jmp short .ps0

checkStdinStatus:  ;ah = 0Bh
;Returns the status of the driver controlling vCon
    call vConCtrlCheck  ;Get status (handling special case chars)
    mov al, 00  ;Set return code to 0 without affecting flags
    retz    ;If BSY set (no chars available), return with al=00
    dec al  ;Set al to -1 if char available
    return  ;Exit
clearbuffDoFunc:   ;ah = 0Ch
;Clears any buffers and issues a console read command (the command in al)
;If al neq 01, 06 (dl neq -1), 07, 08, 0A, then set al = 0 and return
    push rax
    push rdx
    xor ebx, ebx    ;Handle 0, STDIN
    call getCharDevSFT  ;Get sft ptr for device
    jc .skipFlush   ;If the handle is closed, attempt a read regardless
    mov ah, 04h ;Flush input buffers
    call mainCharIO ;Remember sft ptr in rsi
.skipFlush:
    pop rdx
    pop rax
    mov ah, al  ;Move function number into ah
    cmp al, 06h ;Special case (check if dl == FFh)
    jne .others
    cmp dl, 0FFh ;Is the char invalid?
    jz .bad ;Yes, exit
    jmp short .callFunction ;Else, call function in ah
.others:
    cmp al, 01h
    je .callFunction
    cmp al, 07h
    je .callFunction
    cmp al, 08h
    je .callFunction
    cmp al, 0Ah
    je .callFunction
.bad:
    xor al, al
    return
.callFunction:
    cli ;Prepare to swap stack pointer
    ;The below address avoids "properly" reentering DOS
    ;We simply reuse the function dispatch aspect. 
    ;this means we dont trash the caller's register frame 
    jmp functionDispatch.charFun0CEP    ;Go to the entry point
;------------------------
;  Primitive functions  :
;------------------------
swapPrimaryHeader:
;Will swap the primary header to the backup or vice-versa, depending on CF. 
; CF = NC, write to backup, CF=CY, read from backup
    push rsi
    push rdi
    push rcx
    mov rcx, ioReqPkt_size
    lea rsi, primReqHdr
    lea rdi, bkupReqHdr
    jnc .read
    xchg rsi, rdi   ;If carry flag set, swap pointers
.read:
    rep movsb   ;Swap
    pop rcx
    pop rdi
    pop rsi
    return
outputOnStdout:
;Input: al = Char to output
;Output: CF=CY, all good
    push rbx
    mov ebx, 1  ;STDOUT handle
    call getCharDevSFT  ;Get pointer in rsi
    jc .exit
    test word [rsi + sft.wDeviceInfo], devCharDev
    jz .notCharDevOrFast  ;If disk or redir device, skip fast check
    mov rbx, qword [rsi + sft.qPtr] ;Get driver pointer in rbx
    test word [rbx + drvHdr.attrib], devDrvFastOut  ;Can we use Int 29?
    jz .notCharDevOrFast
    int 29h ;Fast output
.exitOk:
    clc
.exit:
    pop rbx
    return
.notCharDevOrFast:
    call outputOnSFT
    pop rbx
    return

outputToHandle:
;Char to output must be in al
    call getCharDevSFT  ;Get SFT pointer in rsi and fall into output on SFT
    retc    ;Return if carry flag set (bx has invalid pointer)
outputOnSFT:
;Output char in al to SFT in rsi
;Waits until device is not busy to send char.
;Calls int 28h if device busy
    push rax
    mov ah, 03h ;Get output Status (ready to recieve?)
    call mainCharIO
    pop rax
    jz .signalLoop  ;If device not ready, signal waiting 
    mov ah, 02h ;Output char in al
    call mainCharIO
    clc
    return
.signalLoop:
    call callInt28h
    jmp short outputOnSFT

callInt28h:
;Preserve full state, including "safetocallint28" flag and flags
    pushfq
    test byte [int28Flag], -1
    jz .exit    ;If zero, not safe
    test byte [critErrFlag], -1 ;Are we in a critical error situation?
    jnz .exit
;Preserve stack alignment!!! Push Qword including and after int28Flag 
    push qword [int28Flag] 
    int 28h
    pop qword [int28Flag]    ;Return original value
.exit:
    popfq
    return


vConCtrlCheck:
;Checks if the char at the vConsole needs special processing and enacts
; the processing. 
;Note, unless it is a special processing char, it only CHECKS the char.
; The char then needs to be pulled out of the buffer if it is not special.
;
;^C will terminate application (either directly on hardware console or vCon)
;^S will pause screen processing until another key is processed by vCon input
;^P will toggle the printer echo feature of the vConsole
;
;This is usually STDIN (Handle 0), but can be any file handle 
; in ASCII mode when called using 21h/3Fh (Handle Read)
;
;Output: 
;   CF=CY : Error
;   CF=NC : ...
;   ZF=NZ, AL = Char that was typed, NOT pulled from buffer (i.e not ctrl char)
;   ZF=ZY, No char to read, device busy (nothing buffered)
    call checkBreak ;Check if there is a ^C on CON
    push rbx
    xor ebx, ebx    ;Get STDIN handle in rsi
    call getCharDevSFT
    pop rbx
    retc    ;Return if CF=CY
    mov ah, 01  ;Non destructively read CON
    call mainCharIO
    jz callInt28h   ;If ZF=ZE, BUSY set, no char in al, return thru Int 28h
    ;Check if we have a ^C, ^S or a ^P to process as needed
    cmp al, DC3 ;^S ?
    jne .checkPrintOrExit    ;Nope, check ^P or ^C?
    xor ah, ah  ;Pull ^S out of the device buffer
    call mainCharIO
    jmp .waitNextChar  ;Pause processing until char pressed again!
.checkPrintOrExit:
    cmp al, DLE ;Do we have ^P?
    je .readCharNoWait    ;Yes, pull it from the buffer
    cmp al, ETX ;Do we have ^C?
    je .readCharNoWait    ;Yes, pull it from the buffer
;We dont have ^S, ^P or ^C, no need for extra processing. Return with ZF=NZ!
    return 
.printToggle:
    not byte [printEcho]    ;Compliment the flag.
    push rbx
    mov ebx, 4  ;PRN handle
    call getCharDevSFT  ;Get device SFT in rsi here
    pop rbx
    retc    ;If CF=CY, exit
    push rdi
    mov rdi, rsi    ;Move SFT pointer into rdi
    test word [rdi + sft.wDeviceInfo], charDevNetSpool  ;Check if net spooler
    jz .notNet
    push rax
    mov eax, 1126h  ;Network redirector! Toggle Remote Printer Echo!
    int 2Fh
    pop rax
    jnc .notNet  ;If returned not Carry, all ok, now echo char as needed 
    ;If something went wrong, turn off echo
    mov byte [printEcho], 0 ;Turn off local echo byte
    push rax
    mov eax, 1124h  ;Net redir! Turn off Remote Printer!
    int 2Fh
    pop rax
    jmp short .printExit    ;Skip the following for local printers
.notNet:
;Here depending on whether the print Echo flag is on, we transmit either 
; 0Dh or 0Eh
    cmp byte [printEcho], 00
    jnz .echoOn
    call closeSFT   ;Reduce open count for SFT in rdi
    jmp short .printExit
.echoOn:
    call openSFT    ;Increase open count for SFT in rdi
.printExit:
    pop rdi
    return
.sigNextChar:   ;Signal Int 28h before waiting for the next char
    call callInt28h
.waitNextChar:   ;Here get next char
    mov ah, 01h ;ND read
    call mainCharIO
    jz .sigNextChar    ;IF device busy, Int 28h and keep waiting
.readCharNoWait:    ;Pull the non ^S control char from the buffer
    push rbx
    xor ebx, ebx
    call getCharDevSFT
    pop rbx
    retc    ;Return if STDIN closed
    xor ah, ah  ;Pull char out of buffer
    call mainCharIO
    cmp al, DLE ;Was char ^P, printer echo?
    jnz .checkBreak2
    cmp byte [noPrintTog], 00   ;Should we toggle?
    jz .printToggle ;If 0, toggle!
    mov byte [noPrintTog], 00   ;Else, end extended char read!
.checkBreak2:
    cmp al, ETX
    retne   ;Return if not equal, al has char and ZF=NZ
    jmp ctrlBreakHdlr   ;If it is ^C, error exit!


getCharDevSFT:
;Gets the appropriate SFT pointer in rsi for the device in bx
;Input: bx = zero extended handle number
;Output: CF=NC => rsi = SFT pointer for device
;        CF=CY => al = Error code, abort operation
    test byte [vConDrvSwp], -1  ;Has this device been swapped?
    jnz .swap ;If any bits are set, assume swapped (thus working SFT set)
.getSFT:
    push rdi
    call derefSFTPtr   ;Get device ptr in rdi (or error in al)
    mov rsi, rdi
    pop rdi
    return  ;Return with CF set
.swap:
;workingSFT is only set for CON calls
    cmp ebx, 1  ;bx is zero extended anyway
    ja .getSFT  ;vConAlt is only for bx=0 (STDIN)
    mov rsi, qword [vConAltSFTPtr]  ;Get the alternate CON device SFT pointer
    clc
    return

;------------------------
;   Utility functions   :
;------------------------
printCaretASCII:
;Input: al = Char to print with a caret
;Output: On STDOUT, print char with caret IF valid caret char
;First check if the char should be careted, and then print normally if so
    cmp al, asciiCaret  ;Is this char to be printed normally?
    jae charOut_B.in
    cmp al, TAB
    je charOut_B.in
    cmp al, NAK
    je charOut_B.in
    cmp al, DC4
    je charOut_B.in
    push rax
    mov al, "^" ;Get caret in place
    call charOut_B.in
    pop rax
    add al, "@" ;Turn into an ASCII Char
    jmp charOut_B.in  ;Now print the char in al and return
printCRLF:
    mov al, CR
    call charOut_B.in
    mov al, LF
    jmp charOut_B.in

checkBreak:
;Reads bytes from CON if there are any bytes to read and 
; if it is a ^C or CTRL+BREAK, then exit via Int 23h
    cmp byte [inDOS], 1
    retne    ;Return if not inDOS only once
;Returns in al the keystroke that is available IF one is available
; or al=0 if no keystroke available
    push rbx
    push rsi
    mov rsi, qword [vConPtr] ;Get pointer to Console device driver
    ;Place command code and a zero status word at the same time
    mov dword [critReqHdr + ndInNoWaitPkt.cmdcde], drvNONDESTREAD
    ;Place the packet size in the hdrlen field
    mov byte [critReqHdr + ndInNoWaitPkt.hdrlen], ndInNoWaitPkt_size
    lea rbx, critReqHdr
    call goDriver   ;Called with rsi and rbx with appropriate pointers
    ;Check if the busy bit is set (No keystroke available)
    test word [critReqHdr + ndInNoWaitPkt.status], drvBsyStatus
    jz .charFound
.exit:
    xor al, al
    pop rsi
    pop rbx
    return
.charFound:
;Keystroke available, proceed
    mov al, byte [critReqHdr + ndInNoWaitPkt.retbyt]    ;Get char
    cmp al, ETX ;BREAK/^C =ASCII 03h
    jne .exit   ;If not equal exit
;Now we pull the char out of the buffer
    mov dword [critReqHdr + ioReqPkt.cmdcde], drvREAD ;Read command
    mov byte [critReqHdr + ioReqPkt.hdrlen], ioReqPkt_size  ;Place packet size
    ;Place pointers and number of chars
    mov dword [critReqHdr + ioReqPkt.tfrlen], 1 ;One char to be read
    ;Use media byte space as the char buffer (to avoid issues & save a byte)
    lea rax, qword [critReqHdr + ioReqPkt.medesc]
    mov qword [critReqHdr + ioReqPkt.bufptr], rax
    call goDriver   ;RSI and RBX as before
    jmp ctrlBreakHdlr   ;Read the char and jump to ^C handler

vConSwapDriver:
;Sets up the vCon to use the alternative SFT pointer
    push rdi
    mov byte [vConDrvSwp], 1    ;Set to use alternative driver
    mov rdi, qword [currentSFT] ;Get current SFT pointer
    mov qword [vConAltSFTPtr], rdi ;Save the SFT ptr in var
    pop rdi
    return

vConRetDriver:
;Returns the original driver (sets flag down)
    mov byte [vConDrvSwp], 0
    return

;--------------------------------------------------
;      Main Keyboard Buffered Input Function      :
;--------------------------------------------------
buffCharInput_BE:  ;ah = 0Ah
;Buffer pointer in rdx
; byte [rdx + 0], buffer length
; byte [rdx + 1], number of chars in buffer
; byte [rdx + 2], buffer start
; If [rdx+2 + [rdx + 1]] == CR => Enable Function Edit keys
;Register use
; dh = Char offset in internal buffer
; dl = Buffer length
; bh = Char offset in user buffer
; bl = Number of chars already in the buffer
; rdi = Internal buffer
; rsi = User buffer
    mov rsi, rdx
    lodsw   ;Get buffer metadata in ax
    test al, al
    retz    ;If buffer length zero, return
    movzx ebx, ah   ;Move buffer number of chars in buffer into ebx
    cmp al, bl  ;Compare the buffer length to the number of chars in the buffer
    jbe .avoidcheck
    cmp byte [rsi + rbx], CR     ;zero extended through rbx
    je .functionKeyOk
.avoidcheck:
    xor bl, bl  ;Reset number of chars in buffer to 0
.functionKeyOk:
    movzx edx, al  ;Move the buffer length to dl to use as buffer counter
    dec dl  ;One less char to make space for terminating 0Dh
.breakEP:
    mov al, byte [vConCursPos]  ;Set the current buffer cnt to curs. pos.
    mov byte [vConCurCnt], al
    push rsi    ;Push user buffer address
    lea rdi, vConBuffer
    mov byte [vConInsert], 0    ;Set insert mode off by default
.mainLoop:
    call charIn_B   ;Get a char in AL from 21/08h
    cmp al, LF
    jne .checkControlChars
.mainLoop2:
    call charIn_B
.checkControlChars:
    cmp al, ACK
    je .mainLoop2   ;Get another char
    cmp al, byte [extESC]   ;Is it our ESC key?
    je .escape
    cmp al, DEL
    je .delete
    cmp al, BSP
    je .delete
    cmp al, ETB
    ;Add space for patched jump instructions
    db 5 dup (90h)  ;NOP
    cmp al, NAK
    db 5 dup (90h)
    cmp al, CR
    je .carriageReturn
    cmp al, LF
    je .lineFeed
    cmp al, byte [extBreak] ;Is this our Break key?
    je .break
.checkIfCanInsert:
    cmp dh, dl
    jae .bufOflw
    stosb   ;Store the byte and increment rdi!
    inc dh  ;Inc the count of bytes in the buffer
    call printCaretASCII    ;Print the char with a caret if needed or as is!
    cmp byte [vConInsert], 00h  ;Are we in insert mode? 0 = No, 1 = Yes
    jne .mainLoop2
    ;IF not in insert mode, we fall here
    ;Here we follow the chars in the user buffer so we can overwrite 
    ; or insert chars if needed.
    cmp bh, bl  ;IS the number of chars in the buffer equal to the number placed
    jae .mainLoop2  ;If geq dont follow in user buffer
    inc rsi ;Otherwise, goto the next char in the user buffer
    inc bh  ;Incrememnt the counter of the char in user buffer we now point at
    jmp short .mainLoop2
.bufOflw:
;Buffer overflow
    mov al, BEL ;Sound the bell
    call charOut_B.in   ;Call this with char in al
    jmp short .mainLoop2
.break:
;Break, Place a "\", and do a CRLF
    mov al, "\"
    call charOut_B.in
    pop rsi ;Realign stack
.breakAlt:  ;Enter with stack aligned, print tab aligned CRLF
    call printCRLF
    ;Align to next tabstop
    call charOut_B.tab
    jmp .breakEP
.carriageReturn:
    stosb
    call charOut_B.in
    pop rdi ;User buffer address was pushed 
    mov byte [rdi - 1], dh  ;Save count of chars stored
    inc dh  ;Inc count of chars by one (add the terminating CR)
.carriageReturnAlt: ;EP without affecting buffer counts
    lea rsi, vConBuffer
    movzx ecx, dh   ;Move chars between buffers now
    repz movsb  ;If the inc dh cause an overflow, dont copy! 
    return
.lineFeed:
    call printCRLF
    jmp .mainLoop2
.delete:
    call .removeChar
    jmp .mainLoop2
.removeChar:
    test dh, dh ;Is char count 0?
    jz .normalChar   ;If so, skip going back!
    call .vConErase
    mov al, byte [rdi]  ;Get the byte that was just erased
    cmp al, SPC
    jae .normalChar
    cmp al, TAB
    je .eraTab
    cmp al, NAK
    je .normalChar
    cmp al, DC4
    je .normalChar
    call .vConEraseNoDec    ;Else, was not a normal char. Remove caret prefix
.normalChar:
    cmp byte [vConInsert], 00   ;We in insert mode?
    retne ;Yes, return
    test bh, bh ;Beginning of user buffer?
    retz    ;Yes, return
    dec rsi ;Else, go back a space in the user buffer
    dec bh
    return
.eraTab:
;Remember, the tab char is placed in the buffer but the vCon has 
; up to a tab stop worth of space chars printed
    push rdi
    dec rdi
    std ;Go backwards
    movzx ecx, dh    ;Use as counter
    mov al, SPC
    push rbx
    mov ebx, 7
    jecxz .onTabstop
.scanString:
    scasb   ;Is rdi pointing to a space or ctrl char? (also dec scasb)
    jbe .notChar    ;No, skip handling
    cmp byte [rdi + 1], TAB ;Was the char a tab?
    je .tabChar
    dec bl
.notChar:
    loop .scanString
.onTabstop:
    sub bl, [vConCurCnt]    ;Subtract the current count in internal from bl
.tabChar:
    sub bl, dh  ;Same on tabstop
    add cl, bl
    and cl, 7
    pop rbx
    pop rdi
    cld
    jz .normalChar
    ;Now erase all the spaces placed on the vCon
.vConDelTab:
    call .vConEraseNoDec
    loop .vConDelTab
    jmp .normalChar
.vConErase:
;Erase the char on the vCon
;Return through the output function
    dec rdi ;Go back a space in the 
    dec dh  ;Decrement char count in the buffer
.vConEraseNoDec:
    mov al, BSP ;Move cursor back
    call charOut_B.in
    mov al, SPC ;Replace with a space
    call charOut_B.in
    mov al, BSP ;Move cursor back again
    jmp charOut_B.in    ;Return to caller through charOut_B return
.escape:
;ESCAPE, meaning null here. This technique allows a user to install
; a custom handler to handle the extended ascii keys if they wish, 
; including the function keys.
    jmp [extKeyFunc]    ;Jmp to user customisable extended key handler here
.f2:
    call .fCommon2
    jmp short .fCommon
.f3:
    movzx ecx, bl  ;Get chars in user buffer
    sub cl, bh  ;Sub our current position
    jmp short .fCommon
.f1:
    mov ecx, 1  ;Get one char
.fCommon:
    mov byte [vConInsert], 0    ;Turn off insert if on
    cmp dh, dl  ;Are we already at the end of internal buffer?
    je .mainLoop2
    cmp bh, bl  ;Are we already at the end of user stored string?
    je .mainLoop2
    ;Else, copy byte by byte, and retain char in al
    lodsb
    stosb
    call printCaretASCII    ;Print caret if necessary, else print normal
    inc bh
    inc dh
    loop .fCommon   ;Keep loading until end of string or buffers
    jmp .mainLoop2
.f4:
    call .fCommon2
    add rsi, rcx
    add bh, cl
    jmp .mainLoop2
.fCommon2:
    call charIn_B   ;Get a char in al
    cmp al, byte [extESC]   ;IS this the escape char?
    jne .fnotEscape
    ;Get another char if they typed escape and force it in the buffer
    ; Do not return to caller
    call charIn_B
.fforceExit:
    pop rcx ;Get original return address from stack
    jmp .mainLoop2
.fnotEscape:
    movzx ecx, bl   ;Zero extend to rcx
    sub cl, bh
    jz .fforceExit
    dec ecx
    jz .fforceExit
    push rdi
    mov rdi, rsi
    inc rdi
    repne scasb ;Search for the char to start printing from
    pop rdi
    jne .fforceExit ;If char not found, return
    not cl
    add cl, bl
    sub cl, bh
    return
.f5:
    mov al, "@"
    call charOut_B.in   ;Print the char
    pop rdi ;Get old rsi into rdi and push it anew
    push rdi
    call .carriageReturnAlt ;Enter with og user buffer ptr in rdi
    pop rsi ;Pop the old user buffer back into rsi
    mov bl, dh
    jmp .breakAlt
.f6:
;If the user wants to insert a EOF, they can use F6
    mov al, EOF
    jmp .checkIfCanInsert
.f7:
;If the user wants to insert a readl ESC char, they can use F7
    mov al, byte [extESC]
    jmp .checkIfCanInsert
.toggleIns:
    not byte [vConInsert]   ;Toggle
    return
.eDel:
    cmp bh, bl
    je .mainLoop2
    inc bh
    inc rsi
    jmp .mainLoop2

editKeys:
;Our Default Extended keys handler
    call charIn_B   ;Get the next char in al
    mov ecx, extKeyTbl_len  ;Get number of entries in table
    push rdi    ;Preserve rdi
    lea rdi, extKeyTbl
    push rdi
    ;Each entry is 3 bytes. 1st byte is char, 2nd word is
    ; offset of function from extKeyTbl
.lp:
    scasb   ;Compare byte 1 to al, inc rdi to point to word offset
    je .charFound
    dec ecx ;If this goes to zero, reenter count.
    jz .notInTable
    add rdi, 2  ;Skip next two bytes
    jmp short .lp
.charFound:
    pop rcx ;Pop back the effective address of the table
    movzx rdi, word [rdi]   ;Get high word into rdi zero extended
    add rcx, rdi    ;Add offset from table to table address to get jump addr
    pop rdi
    jmp rcx
.notInTable:
    pop rcx ;Realign stack
    pop rcx
    jmp buffCharInput_BE.mainLoop2