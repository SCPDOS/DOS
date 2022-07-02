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
