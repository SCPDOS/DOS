sysDrive    db -1

biosFile    db "A:\SCPBIOS.SYS",0
dosFile     db "A:\SCPDOS.SYS",0

rootDir  db "A:\*.*",0

;Messages
badVerStr   db "Invalid DOS Version",0Ah,0Dh,"$"
badDrvLtr   db "Invalid Drive Specified",0Ah,0Dh,"$"
badRootDir  db "Root Directory Not Empty", 0Ah, 0Dh,"$"
badSearch   db "Error Finding System Files",0Ah,0Dh,"$"