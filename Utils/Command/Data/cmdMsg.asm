;COMMAND.COM Messages and strings
crlf    db  CR,LF,"$"
crlf2   db  CR,LF,CR,LF,"$"
badBat  db  CR,LF,"Batch file missing",CR,LF,"$"    ;Used in BAT
needBat db  CR,LF,"Insert disk with batch file"     ;Used in BAT
        db  CR,LF,"and press any key when ready",CR,LF,"$"
badCmd  db  "Bad command or file name",CR,LF,"$"
dupName db  "Duplicate file name or "
fnfMsg  db  "File not found",CR,LF,"$"
noSpace db  "Insufficient disk space",CR,LF,"$"
noEnvSpace  db  "Out of environment space",CR,LF,"$"
fulRootDir  db  "File creation error",CR,LF,"$"
noSelfCopy  db  "File cannot be copied onto itself",CR,LF,"$"
filLostErr  db  "Content of destination lost before copy",CR,LF,"$"
;Copy end message. First write # files copied then this message 
copyOk  db  " File(s) copied",CR,LF, "$"
;Dir end1 msg. First write # files in dir then this
dirOk   db  " File(s) $"
;Dir end2 msg. First write # bytes free then this message
bytesOk db  " bytes free",CR,LF, "$"
dirLbl  db  " <DIR>  $"

badDrv  db  "Invalid drive specification",CR,LF,"$"
pauseMes     db  "Strike a key when ready . . . $"
badParm db  "Invalid parameter",CR,LF,"$"

dayName db  "SunMonTueWedThuFriSat"
badDate db  CR,LF,"Invalid date$"
curDate db  "Current date is $"
newDate db  CR,LF,"Enter new date $"
usDate  db  "(mm-dd-yy): $"
ukDate  db  "(dd-mm-yy): $"
jpDate  db  "(yy-mm-dd): $"
badTime db  CR,LF,"Invalid time$"
curTime db  "Current time is $"
newTime db  CR,LF,"Enter new time: $"

ynMes   db  "Are you sure (Y/N)? $"

dosVer  db " SCP/DOS Version $"

volMes  db " Volume in drive $"
volOk   db " is $"
volNo   db " has no label$"

badDir  db "Invalid Directory", CR,LF, "$"
badMD   db "Unable to create directory",CR,LF,"$"
badRD   db "Invalid path, not directory,",CR,LF
        db "or directory not empty",CR,LF,"$"
dirMain db " Directory of  $"
noPath  db "No Path $"
badDrvSrch  db "Invalid drive in search path",CR,LF,"$"
badDev  db "Invalid device",CR,LF,"$"
badLbl  db "Label not found",CR,LF,"$"  ;Used in BAT
syntaxErr   db "Syntax error",CR,LF,"$"
forNest db CR,"FOR cannot be nested",CR,LF,"$"    ;Used in BAT
pipeErr db "Intermediate file error during pipe",CR,LF,"$"
binDevErr   db "Cannot do binary reads from a device",CR,LF,"$"
offMes  db "off",CR,LF,"$"
onMes   db "on",CR,LF,"$"
breakIs db "BREAK is $"
verifyIs    db "VERIFY is $"
echoIs  db "ECHO is $"  ;Used in BAT
badSpec db "Invalid path or file name",CR,LF,"$"
badArgs db "Invalid number of parameters",CR,LF,"$"
devWriteErr db "Error writing to device"
backSpace   db BSP," ",BSP,NUL

memSys  db CR,LF,"              Total system memory: $"
memDOS  db CR,LF,"               Memory used by DOS: $"
memApp  db CR,LF,"      Memory used by applications: $"
memHole db CR,LF,"      Memory reserved by hardware: $"
memFree db CR,LF,"                      Memory free: $"
memByte db " bytes$"
memBad0 db CR,LF,"Could Not Assertain DOS Entry Point$"
memBad1 db CR,LF,"Memory Allocation Error$"
memBad2 db CR,LF,"Memory Error$"
memBad3 db CR,LF,"System halted$"

touchErr db "Unable to create file",CR,LF,"$"
pipeErrMsg  db "Unable to create pipe",CR,LF
pipeErrMsgL equ $ - pipeErrMsg
redirErrMsg db "Redirection error",CR,LF
redirErrMsgL   equ $ - redirErrMsg

ansiCls  db 01BH,"[2J" ;ANSI CLS sequence, 4 chars long
fourSpc  db "    $"
threeSpc db "   $"
twoSpc   db "  $"

badOnOff db "Must specify ON or OFF",CR,LF,"$"
pathEVar db "PATH="
promptEVar  db "PROMPT="

;If anything goes wrong with piping or redirecting just close first two 
; handles and reopen CON
conName db "CON",0      