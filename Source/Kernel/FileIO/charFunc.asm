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
.in:  ;Internal function Entry Point, with char in al
    cmp al, asciiCaret
    jb .control
    cmp al, DEL ;DEL char?
    je .skipCurs
    inc byte [vConCursPos]  ;Increment Cursor pos
.skipCurs:
    inc byte [vConErr]   ;Increment 4 char error checker
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
    jc .exitPrintEcho
    ;Ensure we only echo if STDOUT is a char device!!
    movzx ebx, word [rsi + sft.wDeviceInfo]
    test ebx, devRedirDev
    jnz .exitPrintEcho  ;Exit if STDOUT is redir
    test ebx, devCharDev
    jz .exitPrintEcho
    mov ebx, 4  ;STDPRN handle
    call getCharDevSFT  ;Get printer sft in rsi
    jc .exitPrintEcho   ;Exit if handle closed
    test word [rsi + sft.wDeviceInfo], charDevNetSpool  ;Network printer?
    jz .netSpool
    call outputOnSFT
    jmp short .exitPrintEcho
.netSpool:
    mov byte [printEcho], 0 ;Stop echoing
.exitPrintEcho:
    pop rsi
    pop rbx
    return
.control:
    cmp al, CR
    je .newline
    cmp al, BSP
    je .back
    cmp al, TAB
    jne .skipCurs   ;Treat as normal
    ;TAB key here
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
auxOut_B:       ;ah = 04h
prnOut_B:       ;ah = 05h
directConIO:    ;ah = 06h
;Only special thing about this function is that it doesn't wait for input.
charIn:         ;ah = 07h
;Return char in al from STDIN without waiting
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
    int 4ah ;Multitasking keyboard loop
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
buffCharInput_BE:  ;ah = 0Ah
;Works as the main input function for the vCon keyboard buffer
checkStdinStatus:  ;ah = 0Bh
;Returns the status of the driver controlling vCon
clearbuffDoFunc:   ;ah = 0Ch
;Clears any buffers and issues a console command
;------------------------
;  Primitive functions  :
;------------------------
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
    test word [rbx + drvHdr.attrib], devDrvFastOut  ;Can we use Int 49?
    jz .notCharDevOrFast
    int 49h ;Fast output
.exitOk:
    clc
.exit:
    pop rbx
    return
.notCharDevOrFast:
    call outputOnSFT
    pop rbx
    return

outputOnSFT:
;Output char in al to SFT in rsi
;Waits until device is not busy to send char.
;Calls int 48h if device busy
    push rax
    mov ah, 03h ;Get output Status (ready to recieve?)
    call mainCharIO
    pop rax
    jz .signalLoop  ;If device not ready, signal waiting 
    mov ah, 02h ;Output char in al
    call mainCharIO
    clc
    ret
.signalLoop:
    call callInt48h
    jmp short outputOnSFT



callInt48h:
;Preserve full state, including "safetocallint48" flag
    pushfq
    test byte [int48Flag], -1
    jz .exit    ;If zero, not safe
    test byte [critErrFlag], -1 ;Are we in a critical error situation?
    jnz .exit
;Preserve stack alignment!!! Push Qword including and after int48Flag 
    push qword [int48Flag] 
    int 48h
    pop qword [int48Flag]    ;Return original value
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
; in ASCII mode when called using 41h/3Fh (Handle Read)
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
    jz callInt48h   ;If ZF=ZE, BUSY set, no char in al, return thru Int 48h
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
    int 4Fh
    pop rax
    jnc .notNet  ;If returned not Carry, all ok, now echo char as needed 
    ;If something went wrong, turn off echo
    mov byte [printEcho], 0 ;Turn off local echo byte
    push rax
    mov eax, 1124h  ;Net redir! Turn off Remote Printer!
    int 4Fh
    pop rax
    jmp short .printExit    ;Skip the following for local printers
.notNet:
;Here depending on whether the print Echo flag is on, we transmit either 
; 0Dh or 0Eh
    cmp byte [printEcho], 00
    jnz .echoOn
    ;Here do something with echo off!
    jmp short .printExit
.echoOn:
    ;Here do something with echo on!
.printExit:
    pop rdi
    return
.sigNextChar:   ;Signal Int 48h before waiting for the next char
    call callInt48h
.waitNextChar:   ;Here get next char
    mov ah, 01h ;ND read
    call mainCharIO
    jz .sigNextChar    ;IF device busy, Int 48h and keep waiting
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
    ja charOut_B.in
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
; if it is a ^C or CTRL+BREAK, then exit via INT 43h
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