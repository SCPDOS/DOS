    
masterEnv:  ;Yeet this is this is a child command processor
    db "_:COMMAND.COM",0
    db "PATH=",0,0  ;Terminate with two null bytes
    db ($ - masterEnv) dup (" ")    ;Pad the environment with spaces
