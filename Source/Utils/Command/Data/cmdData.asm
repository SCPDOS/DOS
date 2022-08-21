;Static Data Area for COMMAND.COM    
startLbl:
    jmp cmdLdr
crlf        db CR,LF,"$"
basicPrompt db 5,"$n$g"   ;Default Prompt String, length 5
returnCode  dw 0    ;Return Code from a child process
currentDrv  db 0    ;Current Drive    
realParent  dq -1   ;Only the first Copy of COMMAND.COM sets itself here
sysVars     dq 0    ;Ptr to DOS sysvars
numHdls     dw 20   ;Get number of handles permitted, hardcoded in this version
promptPtr   dw -1   ;Offset From Start Label to Pointer String
pathSep     db "\"  ;Default path sep
switchChar  db "/"  ;Default switch char
;Structs
cmdLine     db 80h dup (0)
fcb1        db fcb_size dup (0) ;Reserve space for two FCB's
fcb2        db fcb_size dup (0) 
fileName    db 16 dup (0)   ;Reserve 16 bytes for parsing the filename itself
promptBuf:  ;Alternate symbol for building the prompt
strBuf      db 80h dup (0)  ;This is the main buffer to build command strings


functionTable:
;Use Pascal strings with each row of hte table having three columns:
; Col 1, BYTE, Length of command
; Col 2, String, String representing the user input
; Col 3, WORD, Offset from the startLbl into COMMAND.COM of the function
    db 3, "DIR"
    dw dir - startLbl

    db 2, "CD"
    dw chdir - startLbl

    db 5, "CHDIR"
    dw chdir - startLbl

    db 2, "MD"
    dw mkdir - startLbl

    db 5, "MKDIR"
    dw mkdir - startLbl

    db 2, "RD"
    dw rmdir - startLbl

    db 5, "RMDIR"
    dw rmdir - startLbl

    db 3, "DEL"
    dw erase - startLbl

    db 5, "ERASE"
    dw erase - startLbl

    db 4, "DATE"
    dw date - startLbl

    db 4, "TIME"
    dw time - startLbl

    db 4, "COPY"
    dw copy - startLbl

    db 4, "CTTY"
    dw ctty - startLbl

    db 3, "CLS"
    dw cls - startLbl

    db 5, "BREAK"
    dw break - startLbl

    db 7, "RENAME"
    dw rename - startLbl

dosName  db "Scientific Computer Research(R) SCP/DOS(R) Version "
dosNameL equ $ - dosName