;Messages go here
badVerStr   db 0Ah,0Dh,"Invalid DOS Version",0Ah,0Dh,"$"
badDrvLtr   db 0Ah,0Dh,"Invalid Drive Specified",0Ah,0Dh,"$"
badRedir    db 0Ah,0Dh,"Cannot Format Redir, Subst or Join Drives",0Ah,0Dh,"$"
badGeneric  db 0Ah,0Dh,"Cannot Format Drive",0Ah,0Dh,"$"
cancel      db 0Ah,0Dh,"Are you sure you wish to abort formatting drive "
driveLetter db "A?", 0Ah,0Dh
            db "Doing so will result in an unusable volume. Y/N?",0Ah,0Dh,"$"
badVolBig   db 0Ah,0Dh,"Volume too large to format",0Ah,0Dh,"$"