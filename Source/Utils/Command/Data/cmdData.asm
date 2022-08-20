;Static Data Area for COMMAND.COM    
startLbl:
    breakpoint
    jmp cmdLdr
basicPrompt: db "_>$"
currentDrv  db 0  ;Current Drive    
cmdLinePtr  dq -1   ;Set to use the PSP tail as the command line
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