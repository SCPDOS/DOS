
;-----------------------------------:
;       Static Data Variables       :
;-----------------------------------:
dosMajor    db 00h      ;Version 0
dosMinor    db 33      ;.33
dosBIOSName db "SCPBIOS .SYS"
dosKernName db "SCPBDOS .SYS"
maxHndls    dw 20    ;Initially hardcoded 20, will be made changable soon
;Use the idea of having a JFT valid byte in the PSP to reuse PSP as ptrToNewJFT
;At that point, maxHndls will be removed
;-----------------------------------:
;        Static Data Tables         :
;-----------------------------------:
ctryTbl:
;Country Table (defaulting to UK), refer to struct in dosStruc.inc
    dw 1            
    db 9Ch,0,0,0,0  ;9Ch = £ in British codepage
    db ",",0
    db ".",0
    db "-",0
    db ":",0
    db 0
    db 2 
    db 0 
    dq 0 
    db ",",0    
    db 0,0,0,0,0,0,0,0,0,0

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
;Note, all of these functions use the disk stack so it is important to save rax
; only on entry to a disk function
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

badDirNameChar: ;This table needs to be replaced in other Codepages (i.e. Kanji)
    db 00h,01h,02h,03h,04h,05h,06h,07h,08h,09h,0Ah,0Bh,0Ch,0Dh,0Eh,0Fh
    db 10h,11h,12h,13h,14h,15h,16h,17h,18h,19h,1Ah,1Bh,1Ch,1Dh,1Eh,1Fh
    db '"', "*", "+",",",".","/",":",";","<","=",">","?","[","\","]","|"
badDirNameCharL equ $ - badDirNameChar
;The chars * ? . \ / need to always be handled separately

extAsciiTbl:    ;This table needs to be replaced in other Codepages
    db 80h, 9Ah, 45h, 41h, 8Eh, 41h, 8Fh, 80h, 45h, 45h, 45h, 49h, 49h, 49h
    db 8Eh, 8Fh, 90h, 92h, 92h, 4Fh, 99h, 4Fh, 55h, 55h, 59h, 99h, 9Ah, 9Bh
    db 9Ch, 9Dh, 9Eh, 9Fh, 41h, 49h, 4Fh, 55h, 0A5h, 0A5h, 0A6h, 0A7h, 0A8h
    db 0A9h, 0AAh, 0ABh, 0ACh, 0ADh, 0AEh, 0AFh, 0B0h, 0B1h, 0B2h, 0B3h, 0B4h
    db 0B5h, 0B6h, 0B7h, 0B8h, 0B9h, 0BAh, 0BBh, 0BCh, 0BDh, 0BEh, 0BFh, 0C0h 
    db 0C1h, 0C2h, 0C3h, 0C4h, 0C5h, 0C6h, 0C7h, 0C8h, 0C9h, 0CAh, 0CBh, 0CCh
    db 0CDh, 0CEh, 0CFh, 0D0h, 0D1h, 0D2h, 0D3h, 0D4h, 0D5h, 0D6h, 0D7h, 0D8h 
    db 0D9h, 0DAh, 0DBh, 0DCh, 0DDh, 0DEh, 0DFh, 0E0h, 0E1h, 0E2h, 0E3h, 0E4h
    db 0E5h, 0E6h, 0E7h, 0E8h, 0E9h, 0EAh, 0EBh, 0ECh, 0EDh, 0EEh, 0EFh, 0F0h
    db 0F1h, 0F2h, 0F3h, 0F4h, 0F5h, 0F6h, 0F7h, 0F8h, 0F9h, 0FAh, 0FBh, 0FCh
    db 0FDh, 0FEh, 0FFh

asciiCharProperties:   ;This table needs to replaces in other Codepages
    db 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F0h, 0F6h, 0F6h 
    db 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h
    db 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F8h, 0FFh, 0F6h, 0FFh 
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F4h, 0F4h, 0FFh, 0FEh, 0F6h 
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F4h, 0F4h 
    db 0F4h, 0F4h, 0F4h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh 
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh 
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F6h, 0F6h, 0F6h, 0FFh, 0FFh 
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh 
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh 
    db 0FFh, 0FFh, 0FFh, 0FFh, 0F4h, 0FFh, 0FFh, 0FFh   