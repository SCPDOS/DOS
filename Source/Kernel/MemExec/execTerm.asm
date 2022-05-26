;EXEC and all default terminates are here

;========================
;   Interrupt handlers
;========================
terminateProcess:   ;Int 40h
    iretq
terminateRes:       ;Int 47h
    iretq
;========================
;    Int 21h functions
;========================
simpleTerminate:   ;ah = 00h
    ret
terminateStayRes:  ;ah = 31h
    ret
loadExecChild:     ;ah = 4Bh, EXEC
    ret
terminateClean:    ;ah = 4Ch, EXIT
    ret