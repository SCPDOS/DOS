
;-----------------------------------:
;       Static Data Variables       :
;-----------------------------------:
dosMajor    db 00h      ;Version 0
dosMinor    db 01h      ;.01
dosBIOSName db "SCPBIOS .SYS"
dosKernName db "SCPBDOS .SYS"

;-----------------------------------:
;        Static Data Tables         :
;-----------------------------------:

;IO Char table
;This table has Request header length, command code and error flags
; as a packed DWORD entry
ioRqCmdErrTbl:
;Request header length  Reserved byte     Command code  Error Flags
;          BYTE 0          BYTE 1            BYTE 2       BYTE 3   
    db ioReqPkt_size,       00h,            drvREAD,       86h  ;AH = 00h
    db ndInNoWaitPkt_size,  00h,         drvNONDESTREAD,   86h  ;AH = 01h
    db ioReqPkt_size,       00h,            drvWRITE,      87h  ;AH = 02h
    db statusReqPkt_size,   00h,          drvOUTSTATUS,    87h  ;AH = 03h
    db flushReqPkt_size,    00h,          drvFLUSHINBUF,   86h  ;AH = 04h
    db ndInNoWaitPkt_size,  00h,         drvNONDESTREAD,   86h  ;AH = 05h



;Keyboard vCon static data taht can be edited and replaced by a user if they
; wish to install their own custom logic.
extKeyFunc  dq 0 ;The editing keys can be replaced by replacing this ptr
extESC      db 00h  ;NULL char is our Escape char
extBreak    db 1Bh  ;1Bh is our Break Char
;Extended ASCII keys with special meanings
extKeyTbl   db eF1
            dw (buffCharInput_BE.f1 - extKeyTbl)
            db eF2
            dw (buffCharInput_BE.f2 - extKeyTbl)
            db eF3
            dw (buffCharInput_BE.f3 - extKeyTbl)
            db eF4
            dw (buffCharInput_BE.f4 - extKeyTbl)
            db eF5
            dw (buffCharInput_BE.f5 - extKeyTbl)
            db eF6
            dw (buffCharInput_BE.f6 - extKeyTbl)
            db eF7
            dw (buffCharInput_BE.f7 - extKeyTbl)
            db eCursL
            dw (buffCharInput_BE.delete - extKeyTbl)
            db eCursR
            dw (buffCharInput_BE.f1 - extKeyTbl)
            db eIns
            dw (buffCharInput_BE.toggleIns - extKeyTbl)
            db eDel
            dw (buffCharInput_BE.eDel - extKeyTbl)
extKeyTbl_len   equ ($ - extKeyTbl) / 3

;When counting the number of days, first compute the number of years since
; 1980 and your year. 
;Then, using the table below, find the number of leap years between 1980
; and (YourYear - 1). 
;Then do (YourYear - 1980) * 365 + numberOfLeapYears to get the number of 
; days since 01/01/1980 and 01/01/YourYear.
;
;Use the months table to get the number of days in a normal month as leap 
; years are added using the previous comment.

;This table is the number of days in the month
;The function which updates the days in Feb writes the number to this table
monthsTbl:  
    db 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31