;Messages go here
badVerStr   db "Invalid DOS Version",0Ah,0Dh,"$"
badDrvLtr   db "Invalid Drive Specified",0Ah,0Dh,"$"
badRedir    db "Cannot Format Redir, Subst or Join Drives",0Ah,0Dh,"$"
badGeneric  db "Cannot Format Drive",0Ah,0Dh,"$"
cancel      db "Are you sure you wish to abort formatting drive "
driveLetter db "A?", 0Ah,0Dh
            db "Doing so will result in an unusable volume. Y/N?",0Ah,0Dh,"$"
badVolBig   db "Volume too large to format",0Ah,0Dh,"$"
badSecSize  db "Invalid Medium Sector Size",0Ah,0Dh,"$"
okFormat    db "Format complete",0Ah,0Dh,"$"
currentFmt  db "Cannot format current drive",0Ah,0Dh,"$"