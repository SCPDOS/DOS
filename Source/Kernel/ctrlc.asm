criticalDOSError:
;Will swap stacks and enter int 44h safely and handle passing the right data 
; to the critical error handler.
; Called with ax, di and rsi set as required by Int 44h (caller decides)
;               AH = Critical Error Bitfield
;               Bit 7 = 0 - Disk Error, Bit 7 = 1 - Char Device Error
;               Bit 6 - Reserved
;               Bit 5 = 0 - IGNORE not allowed, Bit 5 = 1 - IGNORE allowed
;               Bit 4 = 0 - RETRY not allowed, Bit 4 = 1 - RETRY allowed
;               Bit 3 = 0 - FAIL not allowed, Bit 3 = 1 - FAIL allowed
;               Bits [2-1] = Affected Disk Error
;                     0 0   DOS area
;                     0 1   FAT area
;                     1 0   Directory area
;                     1 1   Data area
;               Bit 0 = 0 - Read Operation, Bit 0 = 1 - Write Operation
;               AL  = Failing drive number if AH[7] = 0
;               DIL = Error code for errorMsg
;               RSI = EA of Device Header for which device the error occured
;Return:
;               AL = 0 - Ignore the Error       (Ignore)
;                  = 1 - Retry the Operation    (Retry)
;                  = 2 - Terminate the Program  (Abort)
;                  = 3 - Fail the DOS call      (Fail)
; Return response from int 44h in al
    cli ;Disable Interrupts
    mov byte [critErrFlag], 1   ;Set flag for critical error
    mov qword [xInt44hRSP], rsp
    mov rsp, qword [oldRSP] ;Get the old RSP value
    int 44h ;Call critical error handler
    mov rsp, qword [xInt44hRSP] ;Return to the stack of the function that failed
    mov byte [critErrFlag], 0   ;Clear critical error flag
    sti ;Reenable Interrupts
    ret