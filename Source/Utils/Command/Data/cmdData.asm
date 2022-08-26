;Static Data Area for COMMAND.COM    
startLbl:   ;Start symbol, this is the entry point
    jmp cmdLdr
stackBottom dq 0    ;Pointer to the top of the stack as allocated by DOS
returnCode  dw 0    ;Return Code from a child process
realParent  dq -1   ;Only the first Copy of COMMAND.COM sets itself here
sysVars     dq 0    ;Ptr to DOS sysvars
numHdls     dw 20   ;Get number of handles permitted, hardcoded in this version
promptPtr   dw -1   ;Offset From Environemnt Start to prompt String. -1 => dflt 
pathSep     db "\"  ;Default path sep
switchChar  db "/"  ;Default switch char

cmdLineState:   ;Symbol to use for cmdLineState variable block
;Offsets are computed from dta + 2
cmdStart    db 0    ;Offset into input buffer to the start of the current cmd
cmdEnd      db 0    ;Offset into input buffer to the end of the current cmd
searchDrv   db 0    ;Search drive for the operation
;Points to terminating char for the current cmd which is either a 0Dh OR pipe
pipeFlag    db 0    ;If set, we are piping across a command
pipeHndl    dw 0    ;If pipeFlag set, this var has the handle to the pipe file
pipeName    db 11 dup (" ") ;Name of the pipe file we created
redirIn     db 0    ;If set, we are redirecting input from a file
redirOut    db 0    ;If set, we are redirecting output to a file, destructive
redirOut2   db 0    ;If set, we are appending output to a file, not destructive
cmdLineStateL equ $ - cmdLineState

;Structs and strings
ctryData    db countryStruc_size dup (0)  ;Length of the country table
cmdFcb      db 10h dup (0) ;Internal "fcb" for parsing the command name
;Reuse byte 0 to optionally store the count of chars in the filename too
cmdFFBlock  db ffBlock_size ;Internal Find First Block to use as default DTA

inBuffer    db cmdBufferL dup (0)  ;Add one to add space for terminating CR
cmdBuffer   db cmdBufferL dup (0)  ;This is the to copy input to when processing
currDirStr  db fullDirPathZL dup (0) ;Current Directory String
cmdPathSpec db fileSpecZL dup (0)    ;Full path to a external command
