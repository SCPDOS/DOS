sysDrive    db -1   ;Drive we want to install the system files on
memoryBlock dq 0    ;Null ptr is unused
inCrit      db 0    ;If -1, in critical section
sectorSize  dw 0
secPerClus  db 0
clustCnt    dd 0    ;Used for figuring out the FAT type
dpbPtr      dq 0    ;Used for finding the first sector of the Root Dir
xfrSector   dq 0    ;Used to temporarily store the sector number

biosPair:
biosHdlSrc  dw -1   ;File handles
biosHdlDst  dw -1

dosPair:
dosHdlSrc   dw -1
dosHdlDst   dw -1

cmdPair:
cmdHdlSrc   dw -1
cmdHdlDst   dw -1

biosNamePair:
biosFile    db "A:\SCPBIOS.SYS",0   ;A should be overwritten with current drive
biosNameL   equ $ - biosFile
biosDest    db "A:\SCPBIOS.SYS",0
dosNamePair:
dosFile     db "A:\SCPDOS.SYS",0
dosNameL   equ $ - dosFile
dosDest     db "A:\SCPDOS.SYS",0
cmdNamePair:
cmdFile     db "A:\COMMAND.COM",0
cmdNameL   equ $ - cmdFile
cmdDest     db "A:\COMMAND.COM",0
;Data for Boot Sector
biosSector  dq 0
biosSize    dw 0
dosSector   dq 0

rootDir  db "A:\*.*",0  ;A should be overwritten with the letter given

;Messages
badVerStr   db "Invalid DOS Version",0Ah,0Dh,"$"
badDrvLtr   db "Invalid Drive Specified",0Ah,0Dh,"$"
badRootDir  db "Root Directory Not Empty", 0Ah, 0Dh,"$"
badSearch   db "Error Finding System Files",0Ah,0Dh,"$"
badOpen     db "Cannot Open System Files.",0Ah,0Dh,"$"
badCreate   db "Cannot Create System Files.",0Ah,0Dh,"$"
badCopy     db "Error Transferring System Files",0Ah,0Dh,"$"
badMem      db "Not Enough Memory to Transfer System Files",0Ah,0Dh,"$"
badSecSize  db "Invalid Medium Sector Size",0Ah,0Dh,"$"
badDirectI  db "Unable to Read File",0Ah,0Dh,"$"
badDirectO  db "Unable to Write File",0Ah,0Dh,"$"
okMsg       db "System Transfer Complete",0Ah,0Dh,"$"