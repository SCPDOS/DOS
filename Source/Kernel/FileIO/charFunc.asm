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
    test al, al
    jz .stdireexit
    call charOut_B.in    ;Output it to screen
.stdireexit:
    return

charOut_B:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    mov al, dl
.in:  ;Internal function Entry Point, with char in al
    push rsi
    mov byte [singleIObyt], al
    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call wByteSetup ;Puts in rbx the request block
    call goDriver
    pop rsi
    return
auxIn_B:        ;ah = 03h
auxOut_B:       ;ah = 04h
prnOut_B:       ;ah = 05h
directConIO:    ;ah = 06h
;Only special thing about this function is that it doesn't wait for input.
charIn:         ;ah = 07h
;Return char in al from STDIN
charIn_B:       ;ah = 08h
;Return char in al from STDIN
    call checkBreak ;First check if buffer has ^C in it already
    call rByteSetup
    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call goDriver
    mov al, byte [singleIObyt]  ;Get byte in al to return as return value
    cmp al, ETX
    je ctrlBreakHdlr
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
wByteSetup:
;Preserve all registers EXCEPT RBX= Request header pointer
    push rax
    mov ah, drvWRITE
    jmp short rByteSetup.ep
rByteSetup:
;Preserve all registers EXCEPT RBX= Request header pointer
    push rax
    mov ah, drvREAD
.ep:
    push rcx
    push rdi
    mov ecx, 1
    lea rdi, singleIObyt    ;Get address of symbol
    call secdReqCharIOReq   ;Make request, return rbx = Request header
    pop rdi
    pop rcx
    pop rax
    return

callInt48h:
;Preserve full state, including "safetocallint48" flag
    pushfq
    test byte [int48Flag], -1
    jz .exit    ;If zero, not safe
    test byte [critErrFlag], -1 ;Are we in a critical error situation?
    jnz .exit
;Preserve stack alignment!!! Zero extended int48Flag to rax and push it
    push rax
    movzx eax, byte [int48Flag] 
    push rax
    int 48h
    pop rax
    mov byte [int48Flag], al    ;Return original value
    pop rax
.exit:
    popfq
    return


asciiSTDIN:
;Gets chars in ASCII mode from STDIN.
;
;^C will terminate application
;^S will pause screen processing until another key is struck on STDIN
;^P will toggle the printer echo feature of the vConsole
;
;Output: CF=NC : AL = Char that was typed
;        CF=CY : Error
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
    jmp .getNextChar  ;Pause processing until char pressed again!
.checkPrintOrExit:
    cmp al, DLE ;Do we have ^P?
    jmp short .printToggle    ;Yes, jmp to toggle print echo
    cmp al, ETX ;Do we have ^C?
    jmp short .printToggle    ;Yes, toggle echo and proceed with ^C exit
    return  ;We dont have ^S, ^P or ^C, no need for extra processing. Return!
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
    mov eax, 1124h  ;Network redirector! Toggle Remote Printer Echo!
    int 4Fh
    pop rax
    jmp short .printExit    ;Skip the following for local printers
.notNet:
    cmp byte [printEcho], 00
    ;Do some printer magic here
.printExit:
    pop rdi
    return
.sigNextChar:   ;Signal Int 48h before next char
    call callInt48h
.getNextChar:   ;Here get next char
    mov ah, 01h ;ND read
    call mainCharIO
    jz .sigNextChar    ;IF device busy, Int 48h and keep waiting
    push rbx
    xor ebx, ebx
    call getCharDevSFT
    pop rbx
    retc    ;Return if STDIN closed
    xor ah, ah  ;Pull char out of buffer
    call mainCharIO
    cmp al, DLE ;Was char ^P, printer echo?
    jnz .checkBreak2
    ;Here a Printing flag must be checked! This flag, DS:0F83h is unknown 
    ; as of yet. 
.checkBreak2:
    cmp al, ETX
    retne   ;Return if not equal
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
    ja .getSFT
    mov rsi, qword [vConAltSFTPtr]  ;Get the alternate CON SFT pointer
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