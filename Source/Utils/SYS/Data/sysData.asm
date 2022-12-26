sysDrive    db -1
memoryBlock dq -1   

biosHdlSrc  dw -1   ;File handles
biosHdlDst  dw -1

dosHdlSrc   dw -1
dosHdlDst   dw -1

biosFile    db "A:\SCPBIOS.SYS",0   ;A should be overwritten with current drive
biosDest    db "A:\SCPBIOS.SYS",0
dosFile     db "A:\SCPDOS.SYS",0
dosDest     db "A:\SCPDOS.SYS",0

rootDir  db "A:\*.*",0  ;A should be overwritten with the letter given

;Messages
badVerStr   db "Invalid DOS Version",0Ah,0Dh,"$"
badDrvLtr   db "Invalid Drive Specified",0Ah,0Dh,"$"
badRootDir  db "Root Directory Not Empty", 0Ah, 0Dh,"$"
badSearch   db "Error Finding System Files",0Ah,0Dh,"$"
badOpen     db "Cannot Open System Files.",0Ah,0Dh,"$"
badCreate   db "Cannot Create System Files.",0Ah,0Dh,"$"