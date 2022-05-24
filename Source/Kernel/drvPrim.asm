;Driver Primitives, functions for Disk IO and calling a device driver
; are placed here (Int 45h Int 46h and goDriver)

goDriver:
;rbx = Ptr to the request header for the driver call!
    push rsi
    mov rsi, qword [drvrPtr]    ;Get the driver pointer
    call [rsi + drvHdr.strPtr]
    call [rsi + drvHdr.intPtr]
    
    pop rsi
    ret