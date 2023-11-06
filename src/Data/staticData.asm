
;-----------------------------------:
;       Static Data Variables       :
;-----------------------------------:
dosMajor    db 00h      ;Version 0
dosMinor    db 95      ;.95
dosBIOSName db "SCPBIOS .SYS"
dosKernName db "SCPDOS  .SYS"
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



;Keyboard vCon static data that can be edited and replaced by a user if they
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
    db 67h, 03h, errNhl, errNoMem, errInvFnc            ;Set Handle Count
    db 68h, 01h, errBadHdl                              ;Commit File
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

;Nationalisation stuff
dosCodepage:   ;Symbol to point to 
leadingZeros:   db 8 dup (0)    ;Unknown why they are 0 in DOS
defltCtry:      db "\COUNTRY.SYS", (64-12) dup (0) ;FQ Path to COUNTRY.SYS file
defaultCP:      dw 437  ;Set to multilingual codepage
ctryFunctions:  dw 5    ;Support 5 extended functions: al=01,02,04,05,06
charTableArray: ;All the qwords need fixing up here
.ucTable:
    db 2
    dq ucTblExt
.filenameUCTable:
    db 4
    dq fileUCTblExt
.filenameTerminatingTable:
    db 5
    dq fileTermTblExt
.collatingTable:
    db 6
    dq collTblExt
;Extended country table
extCtryTbl:
    db 1    ;infoIDCode (always 1)
    dw 42   ;Total length of the structure
.countryCode:
    dw 044  ;Current (Active) Country ID (044 is UK)
.activeCP:
    dw 437  ;Current (Active) Code page (starts same as default)
;Regular country table
ctryTbl:
;Country Table (defaulting to UK), refer to struct in dosStruc.inc
    dw 1    ;Date format, UK style
    db 9Ch,0,0,0,0  ;9Ch = £ in British codepage
    db ",",0    ;Thousand separator
    db ".",0    ;Decimal separator
    db "-",0    ;Date separator
    db ":",0    ;Time separator
    db 0        ;Currency format, symbol leads with no space
    db 2        ;Number of digits after decimal point
    db 0        ;Time format, 12hr clock
    dq 0        ;Map to function that does UC conversions
    db ",",0    ;Data list separator
    db 10 dup (0)

ucTblExt:   ;External pointer to the uppercase table
    dw 80h
ucTbl:    ;Internal ptr, used by casemapfunc
    db 080h, 09Ah, 045h, 041h, 08Eh, 041h, 08Fh, 080h
    db 045h, 045h, 045h, 049h, 049h, 049h, 08Eh, 08Fh
    db 090h, 092h, 092h, 04Fh, 099h, 04Fh, 055h, 055h
    db 059h, 099h, 09Ah, 09Bh, 09Ch, 09Dh, 09Eh, 09Fh
    db 041h, 049h, 04Fh, 055h, 0A5h, 0A5h, 0A6h, 0A7h
    db 0A8h, 0A9h, 0AAh, 0ABh, 0ACh, 0ADh, 0AEh, 0AFh
    db 0B0h, 0B1h, 0B2h, 0B3h, 0B4h, 0B5h, 0B6h, 0B7h
    db 0B8h, 0B9h, 0BAh, 0BBh, 0BCh, 0BDh, 0BEh, 0BFh
    db 0C0h, 0C1h, 0C2h, 0C3h, 0C4h, 0C5h, 0C6h, 0C7h
    db 0C8h, 0C9h, 0CAh, 0CBh, 0CCh, 0CDh, 0CEh, 0CFh
    db 0D0h, 0D1h, 0D2h, 0D3h, 0D4h, 0D5h, 0D6h, 0D7h
    db 0D8h, 0D9h, 0DAh, 0DBh, 0DCh, 0DDh, 0DEh, 0DFh
    db 0E0h, 0E1h, 0E2h, 0E3h, 0E4h, 0E5h, 0E6h, 0E7h
    db 0E8h, 0E9h, 0EAh, 0EBh, 0ECh, 0EDh, 0EEh, 0EFh
    db 0F0h, 0F1h, 0F2h, 0F3h, 0F4h, 0F5h, 0F6h, 0F7h
    db 0F8h, 0F9h, 0FAh, 0FBh, 0FCh, 0FDh, 0FEh, 0FFh

fileUCTblExt:   ;External ptr to the uc table for filenames
    dw 80h
fileUCTbl:    ;Internal ptr, used to convert pathspecs correctly
    db 080h, 09Ah, 045h, 041h, 08Eh, 041h, 08Fh, 080h
    db 045h, 045h, 045h, 049h, 049h, 049h, 08Eh, 08Fh
    db 090h, 092h, 092h, 04Fh, 099h, 04Fh, 055h, 055h
    db 059h, 099h, 09Ah, 09Bh, 09Ch, 09Dh, 09Eh, 09Fh
    db 041h, 049h, 04Fh, 055h, 0A5h, 0A5h, 0A6h, 0A7h
    db 0A8h, 0A9h, 0AAh, 0ABh, 0ACh, 0ADh, 0AEh, 0AFh
    db 0B0h, 0B1h, 0B2h, 0B3h, 0B4h, 0B5h, 0B6h, 0B7h
    db 0B8h, 0B9h, 0BAh, 0BBh, 0BCh, 0BDh, 0BEh, 0BFh
    db 0C0h, 0C1h, 0C2h, 0C3h, 0C4h, 0C5h, 0C6h, 0C7h
    db 0C8h, 0C9h, 0CAh, 0CBh, 0CCh, 0CDh, 0CEh, 0CFh
    db 0D0h, 0D1h, 0D2h, 0D3h, 0D4h, 0D5h, 0D6h, 0D7h
    db 0D8h, 0D9h, 0DAh, 0DBh, 0DCh, 0DDh, 0DEh, 0DFh
    db 0E0h, 0E1h, 0E2h, 0E3h, 0E4h, 0E5h, 0E6h, 0E7h
    db 0E8h, 0E9h, 0EAh, 0EBh, 0ECh, 0EDh, 0EEh, 0EFh
    db 0F0h, 0F1h, 0F2h, 0F3h, 0F4h, 0F5h, 0F6h, 0F7h
    db 0F8h, 0F9h, 0FAh, 0FBh, 0FCh, 0FDh, 0FEh, 0FFh

fileTermTblExt:
    dw filenameTermTblExt_len - 2   ;Length not including this word
    db 1    ;Signature byte for the table (1)
    db 0    ;Lowest permissible char value for filename
    db -1   ;Highest permissible char value for filename
    db 0    ;Signature byte for DOS 3.3 (0)
.startBadRange:
    db 0    ;Start of the illegal range of filename chars
.endBadRange:
    db 20h  ;End of the illegal range of filename chars (inclusive)
    db 2    ;Signature byte for DOS 3.3 (2)
fileTermTbl:
    db fileTermTbl_len - 1 ;Length of the table below
    db ".", '"', "/", "\", "[", "]", 
    db ":", "|", "<",">","+","=",";",","
    fileTermTbl_len equ $ - fileTermTbl
    filenameTermTblExt_len equ $ - fileTermTblExt
    ;Buffer space
    db 24 dup (0) ;DOS 3.30 has this buffer present

collTblExt:  ;Collating sequence table, for sorting
    dw 0100h
collTbl:
    db 000h, 001h, 002h, 003h, 004h, 005h, 006h, 007h
    db 008h, 009h, 00Ah, 00Bh, 00Ch, 00Dh, 00Eh, 00Fh
    db 010h, 011h, 012h, 013h, 014h, 015h, 016h, 017h
    db 018h, 019h, 01Ah, 01Bh, 01Ch, 01Dh, 01Eh, 01Fh
    db 020h, 021h, 022h, 023h, 024h, 025h, 026h, 027h
    db 028h, 029h, 02Ah, 02Bh, 02Ch, 02Dh, 02Eh, 02Fh
    db 030h, 031h, 032h, 033h, 034h, 035h, 036h, 037h
    db 038h, 039h, 03Ah, 03Bh, 03Ch, 03Dh, 03Eh, 03Fh
    db 040h, 041h, 042h, 043h, 044h, 045h, 046h, 047h
    db 048h, 049h, 04Ah, 04Bh, 04Ch, 04Dh, 04Eh, 04Fh
    db 050h, 051h, 052h, 053h, 054h, 055h, 056h, 057h
    db 058h, 059h, 05Ah, 05Bh, 05Ch, 05Dh, 05Eh, 05Fh
    db 060h, 041h, 042h, 043h, 044h, 045h, 046h, 047h
    db 048h, 049h, 04Ah, 04Bh, 04Ch, 04Dh, 04Eh, 04Fh
    db 050h, 051h, 052h, 053h, 054h, 055h, 056h, 057h
    db 058h, 059h, 05Ah, 07Bh, 07Ch, 07Dh, 07Eh, 07Fh
    db 043h, 055h, 045h, 041h, 041h, 041h, 041h, 043h
    db 045h, 045h, 045h, 049h, 049h, 049h, 041h, 041h
    db 045h, 041h, 041h, 04Fh, 04Fh, 04Fh, 055h, 055h
    db 059h, 04Fh, 055h, 024h, 024h, 024h, 024h, 024h
    db 041h, 049h, 04Fh, 055h, 04Eh, 04Eh, 0A6h, 0A7h
    db 03Fh, 0A9h, 0AAh, 0ABh, 0ACh, 021h, 022h, 022h
    db 0B0h, 0B1h, 0B2h, 0B3h, 0B4h, 0B5h, 0B6h, 0B7h
    db 0B8h, 0B9h, 0BAh, 0BBh, 0BCh, 0BDh, 0BEh, 0BFh
    db 0C0h, 0C1h, 0C2h, 0C3h, 0C4h, 0C5h, 0C6h, 0C7h
    db 0C8h, 0C9h, 0CAh, 0CBh, 0CCh, 0CDh, 0CEh, 0CFh
    db 0D0h, 0D1h, 0D2h, 0D3h, 0D4h, 0D5h, 0D6h, 0D7h
    db 0D8h, 0D9h, 0DAh, 0DBh, 0DCh, 0DDh, 0DEh, 0DFh
    db 0E0h, 053h, 0E2h, 0E3h, 0E4h, 0E5h, 0E6h, 0E7h
    db 0E8h, 0E9h, 0EAh, 0EBh, 0ECh, 0EDh, 0EEh, 0EFh
    db 0F0h, 0F1h, 0F2h, 0F3h, 0F4h, 0F5h, 0F6h, 0F7h
    db 0F8h, 0F9h, 0FAh, 0FBh, 0FCh, 0FDh, 0FEh, 0FFh

asciiCharProperties:   ;This table is const. Gives "properties" of chars.
;Bit[0]=Clear if the char is an invalid filename character.
;Bit[1]=Clear if the char of a terminating type.
;Bit[2]=Clear if the char is of space/tab type.
;Bit[3]=Clear if the char is an invalid FCB name character.
;Bit[4-7]=Unused.    

    db 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h   ;Chars 00h-07h
    db 0F6h, 0F0h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h   ;Chars 08h-0Fh
    db 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h   ;Chars 10h-17h
    db 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h, 0F6h   ;Chars 18h-1Fh
    db 0F8h, 0FFh, 0F6h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 20h-27h
    db 0FFh, 0FFh, 0FFh, 0F4h, 0F4h, 0FFh, 0FEh, 0F6h   ;Chars 28h-2Fh
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 30h-37h
    db 0FFh, 0FFh, 0F4h, 0F4h, 0F4h, 0F4h, 0F4h, 0FFh   ;Chars 38h-3Fh
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 40h-47h
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 48h-4Fh
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 50h-57h
    db 0FFh, 0FFh, 0FFh, 0F6h, 0F6h, 0F6h, 0FFh, 0FFh   ;Chars 58h-5Fh
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 60h-67h
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 68h-6Fh
    db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh   ;Chars 70h-77h
    db 0FFh, 0FFh, 0FFh, 0FFh, 0F4h, 0FFh, 0FFh, 0FFh   ;Chars 78h-7Fh
    db 128 dup (0FFh)                                   ;Chars 80h-100h

;!!!NOTE!!!:
;This table is being commented out, and checkCharValid has been replaced
; with a version using the codepage table now, properly.
;This is being left commented in FOR NOW, in case of any instabilities.
;Should not be the case ever
;!!!NOTE!!!:
;badDirNameChar: ;This table needs to be replaced in other Codepages (i.e. Kanji)
;    db 00h,01h,02h,03h,04h,05h,06h,07h,08h,09h,0Ah,0Bh,0Ch,0Dh,0Eh,0Fh
;    db 10h,11h,12h,13h,14h,15h,16h,17h,18h,19h,1Ah,1Bh,1Ch,1Dh,1Eh,1Fh
;    db '"', "*", "+",",",".","/",":",";","<","=",">","?","[","\","]","|"
;badDirNameCharL equ $ - badDirNameChar
;The chars * ? . \ / need to always be handled separately


hardErrorStack:
    db errWpd
    db eClsMedia
    db eActRetUsr
    db eLocDsk