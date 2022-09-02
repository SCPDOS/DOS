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
;Static strings, not used in command line parsing
ctryData    db countryStruc_size dup (0)  ;Length of the country table
currDirStr  db fullDirPathZL dup (0) ;Current Directory String


cmdLineStatePtr:
cmdStartOff db 0    ;Offset to the first char for this command (may be a space)
cmdEndOff   db 0    ;Offset to the terminating char for this command (0Dh or |)
pipeFlag    db 0    ;If set, we are piping across a command
pipeHndl    dw 0    ;If pipeFlag set, this var has the handle to the pipe file
pipeName    db 11 dup (" ") ;Name of the pipe file we created

cmdStatePtr:   ;Symbol to use for clearing command state variables
;These variables are valid for a SINGLE command in a command line
cmdDrvSpec  dw 0    ;Read the first word in to see if the pathspec has drivespec
redirIn     db 0    ;If set, we are redirecting input from a file
redirOut    db 0    ;If 1, we are redirecting output to a file, destructively
;                    If 2, we are redirecting output to a file, by appending
;FLG and SWCH are read as a word when checking if argX is a switch
arg1Flg     db 0    ;Set if there was a first argument
arg1Off     db 0    ;Offset into cmdBuffer to the argument
arg1FCBret  db 0    ;AL on return from parse filename for argument 1

arg2Flg     db 0    ;Set if there was a second argument
arg2Off     db 0    ;Offset into cmdBuffer to the argument
arg2FCBret  db 0    ;AL on return from parse filename for argument 2

cmdStateL equ $ - cmdStatePtr
cmdLineStateL equ $ - cmdLineStatePtr

;Structs and strings

cmdFcb      db 10h dup (0) ;Internal "fcb" for parsing the command name
cmdFFBlock  db ffBlock_size ;Internal Find First Block to use as default DTA

inBuffer    db cmdBufferL dup (0)  ;Add one to add space for terminating CR
cmdBuffer   db cmdBufferL dup (0)  ;This is the to copy input to when processing
cmdPathSpec db fileSpecZL dup (0)  ;Space for full path to a external command

fcbCmdSpec  db fcbNameL dup (0) ;Used to make a FCB style name for the file
cmdSpec     db fileNameZL dup (0)   ;ASCIIZ command spec for the command name
cmdName     db cmdNameL dup (0) ;Command name string prefixed by length of word

rdrInFilespec   db fileSpecZL dup (0)   ;Space for the redir in filespec
rdrOutFilespec  db fileSpecZL dup (0)   ;Space for the redir out filespec

searchSpec  db cmdBufferL dup (0)   ;Contains the pathspec for the search file
;The above is larger than is needed/supported by DOS to allow for 
; users overtyping

;Internal Function vars
dirPrnType  db 0    ;Print type.    Bit[0] set => /W or /w specified
;                                   Bit[1] set => /P or /p specified
dirLineCtr  db 0    ;Counter to keep track of which line we printed (0-23)
dirFileCtr  db 0    ;Used in /W mode, rollover after 5
dirPathOff  db 0
dirVolLbl   db 13 dup (0)
dirVolPathBuf db 8 dup (0)  ;Used to build X:\*.*,0 for volume label search
dirDrv      db 0    ;0 based drive number to use

td1 db 0    ;Minutes/Year
td2 db 0    ;Hours/Zero
td3 db 0    ;Hundredths/Day
td4 db 0    ;Seconds/Month