    
masterEnv:  ;Yeet this is this is a child command processor
    db "_:COMMAND.COM",0
    db "PATH=",0,0
    db ($ - masterEnv) dup (" ")    ;Pad the environment with spaces
endOfAlloc: ;Symbol to find the start of where I can yeet 