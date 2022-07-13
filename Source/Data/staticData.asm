
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
ctryTbl:
;Country Table (defaulting to UK)
.dtfmt:     dw 1            ;Date time format (2 bytes)
                            ;0 = month day year hh:mm:ss
                            ;1 = day month year hh:mm:ss
                            ;2 = year month day hh:mm:ss
.curr:      db "£",0,0,0,0  ;ASCIIZ Currency symbol (5 chars)
.thouSep:   db ",",0        ;ASCIIZ Thousands separator
.dcmlSep:   db ".",0        ;ASCIIZ Decimal separator
.dateSep:   db "-",0        ;ASCIIZ Date separator
.timeSep:   db ":",0        ;ASCIIZ Time separator
.currFmt:   db 0            ;Currency Format
                ;0 = Symbol leads, without space
                ;1 = Symbol follows, without space
                ;2 = Symbol leads, one space
                ;3 = Symbol follows, one space
                ;4 = Symbol replace decimal separator
.digtdpt:   db 2 ;Number of digits after the decimal point
.timefmt:   db 0 ;Time format, Bit 0 = 0 => 12 hour clock, = 1 => 24 hour clock
.mapaddr:   dq 0 ;Case map address (0 is nulptr)
.dataSep:   db ",",0    ;Data list separator
.resv:      db 0,0,0,0,0,0,0,0,0,0  ;Reserve 10 bytes

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

;Error tables
errXlatTbl:
;Each entry is n bytes long, defined as
; Byte 0      : DOS function number for which translation will occur
; Byte 1      : Length of the table entry - 2
; Bytes n - 3 : Acceptable Error codes
; Byte n - 1  : Acceptable Error code and default value to translate to
;The table is terminated with a single -1 byte
    db 38h, 02h, errInvFnc, errFnf                      ;getsetCountryInfo
    db 39h, 03h, errPnf, errFnf, errAccDen              ;MKDIR
    db 3Ah, 04h, errBadEnv, errPnf, errFnf, errAccDen   ;RMDIR
    db 3Bh, 02h, errFnf, errPnf                         ;setCurrentDIR (CHDIR)
    db 3Ch, 04h, errPnf, errFnf, errNhl, errAccDen      ;Create File (Handle)
    db 3Dh, 05h, errPnf, errFnf, errAccCde, errNhl, errAccDen ;Open File Handle
    db 3Eh, 01h, errBadHdl                              ;Close File Handle
    db 3Fh, 02h, errBadHdl, errAccDen                   ;Read File Handle
    db 40h, 02h, errBadHdl, errAccDen                   ;Write File Handle
    db 41h, 03h, errPnf, errFnf, errAccDen              ;Delete File (Handle)
    db 42h, 02h, errBadHdl, errInvFnc                   ;LSEEK
    db 43h, 04h, errPnf, errFnf, errInvFnc, errAccDen   ;CHMOD
    db 44h, 05h, errBadDrv, errInvDat, errInvFnc, errBadHdl, errAccDen ;IOCTL
    db 45h, 02h, errBadHdl, errInvFnc                   ;DUP handle
    db 46h, 02h, errBadHdl, errInvFnc                   ;Force DUP handle
    db 47h, 01h, errBadDrv                              ;Get Current Dir
    db 48h, 02h, errMCBbad, errNoMem                    ;ALLOC
    db 49h, 02h, errMCBbad, errMemAddr                  ;FREE
    db 4Ah, 03h, errMCBbad, errMemAddr, errNoMem        ;REALLOC
    db 4Bh, 08h, errPnf, errInvFnc, errFnf, errNhl, errBadFmt, errBadEnv, 
    db errNoMem, errAccDen                              ;EXEC
    db 4Eh, 03h, errPnf, errFnf, errNoFil               ;Find First (Handle)
    db 4Fh, 01h, errNoFil                               ;Find Next (Handle)
    db 56h, 04h, errDevUnk, errPnf, errFnf, errAccDen   ;REN (Handle)
    db 57h, 02h, errBadHdl, errInvFnc                   ;Set Handle Time/Date
    db 58h, 01h, errInvFnc                              ;Get Alloc strat
    db 5Ah, 04h, errPnf, errFnf, errNhl, errAccDen      ;Create Unique File
    db 5Bh, 05h, errFilExist, errPnf, errFnf, errNhl, errAccDen ;Create New File
    db 5Ch, 04h, errBadHdl, errInvFnc, errShrFul, errLokVio ;Lock/Unlock File
    db 65h, 02h, errInvFnc, errFnf                      ;Get Ext. Count. Info
    db 66h, 02h, errInvFnc, errFnf                      ;Get/Set Global Codepage
    db 68h, 01h, errBadHdl                              ;Commit File
    db 67h, 03h, errNhl, errNoMem, errInvFnc            ;Set Handle Count
    db -1   ;End Of Table marker


extErrTbl:
;This table contains the default values for how to respond
; to particular errors. Fields with -1 need to be filled in before
; setting variables using this table.
;Each entry in the table is 4 bytes long, and are defined as follows:
; Byte 0: Extended Error Code as a byte
; Byte 1: Error Class
; Byte 2: Error Suggested Action
; Byte 3: Error Locus
;If a byte is -1, we dont set that variable (Thus allowing the caller to set)
;
;Error 01: Invalid function number
    db errInvFnc, eClsAppFlt, eActAbt, -1   ;Locus set before call
;Error 02: File not found
    db errFnf, eClsNotFnd, eActUsr, eLocDsk 
;Error 03: Path not found
    db errPnf, eClsNotFnd, eActUsr, eLocDsk
;Error 04: Too many open handles, and no handles are left
    db errNhl, eClsOoR, eActAbt, eLocUnk
;Error 05: Access being denied
    db errAccDen, eClsAuth, eActUsr, eLocUnk
;Error 06: Invalid File handle being provided
    db errBadHdl, eClsAppFlt, eActAbt, eLocUnk
;Error 07: MCB chain destroyed
    db errMCBbad, eClsAppFlt, eActKil, eLocMem
;Error 08: No Memory remaining
    db errNoMem, eClsOoR, eActAbt, eLocMem
;Error 09: Invalid MCB block Address
    db errMemAddr, eClsAppFlt, eActAbt, eLocMem
;Error 0A: Bad Environment block
    db errBadEnv, eClsAppFlt, eActAbt, eLocMem
;Error 0B: Data provided in a bad format
    db errBadFmt, eClsBadFmt, eActUsr, eLocUnk
;Error 0C: Access Code Invalid
    db errAccCde, eClsAppFlt, eActAbt, eLocUnk
;Error 0D: Error due to Invalid Data provided
    db errInvDat, eClsBadFmt, eActAbt, eLocUnk
;Error 0F: Error due to a bad drive letter being provided
    db errBadDrv, eClsNotFnd, eActUsr, eLocDsk
;Error 10: Error due to attempting to delete the CWD
    db errDelCD, eClsAuth, eActUsr, eLocDsk
;Error 11: Error due to a unknown device being used
    db errDevUnk, eClsUnk, eActUsr, eLocDsk
;Error 12: No more file handles available
    db errNoFil, eClsNotFnd, eActUsr, eLocDsk
;Error 50: Network request not supported
    db errNoNet, eClsClash, eActUsr, eLocDsk
;Error 20: Generic Share Violation, Sharing Resource cannot be shared
    db errShrVio, eClsLocked, eActDRet, eLocDsk
;Error 21: File Locking Violation
    db errLokVio, eClsLocked, eActDRet, eLocDsk
;Error 54: Too many levels of redirection error
    db errRedir, eClsOoR, eActAbt, -1
;Error 56: Bad resource password provided
    db errBadPass, eClsAuth, eActUsr, eLocUnk
;Error 52: Directory already exists
    db errDirExist, eClsOoR, eActAbt, eLocDsk
;Error 32: Network request not supported by DOS
    db errNoNet, eClsBadFmt, eActUsr, eLocNet
;Error 55: Trying to duplicate a redirection for a resource
    db errDupRedir, eClsClash, eActUsr, eLocNet
;Error 57: Bad parameter in request
    db errBadParam, eClsBadFmt, eActUsr, eLocUnk
;Error 53: Fail was returned from Int 44h
    db errFI44, eClsUnk, eActAbt, eLocUnk
;Error 24: Sharing Buffer Full
    db errShrFul, eClsOoR, eActAbt, eLocMem
    dd -1   ;End of table signature