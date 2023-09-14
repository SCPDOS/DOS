;DOS Error codes

;Extended Error Codes
errInvFnc   equ 1   ;Invalid function number
errFnf      equ 2   ;File not found
errPnf      equ 3   ;Path not found
errNhl      equ 4   ;Too many open handles, no handles left
errAccDen   equ 5   ;Access denied (to resource)
errBadHdl   equ 6   ;Invalid handle
errMCBbad   equ 7   ;MCB's destroyed
errNoMem    equ 8   ;Insufficient memory
errMemAddr  equ 9   ;Invalid Memory Block Address
errBadEnv   equ 10  ;Invalid environment
errBadFmt   equ 11  ;Invalid format (path etc)
errAccCde   equ 12  ;Invalid Access Code    ?
errInvDat   equ 13  ;Invalid data
;           equ 14  ;Reserved
errBadDrv   equ 15  ;Invalid drive specified
errDelCD    equ 16  ;Attempt to delete the current directory
errDevUnk   equ 17  ;Not the same device (Device Unknown)
errNoFil    equ 18  ;No more files, no match found
;The following are Driver error codes + 19
drvErrShft  equ 19  ;Used to shift Driver error codes to extended error codes
errWpd      equ 19  ;Attempt to write on write protected disk
errUnkUnt   equ 20  ;Unknown Unit
errDrvNR    equ 21  ;Drive not ready
errUnkCmd   equ 22  ;Unknown Command
errCRCerr   equ 23  ;Data (CRC) error
errBadRLn   equ 24  ;Bad request structure length
errSekErr   equ 25  ;Seek error
errUnkMed   equ 26  ;Unknown media type
errSecNF    equ 27  ;Sector not Found
errNoPap    equ 28  ;Printer out of paper
errWF       equ 29  ;Write fault
errRF       equ 30  ;Read fault
errGF       equ 31  ;General fault
;----------------------------------------------
;The following two go inbetween the Disk errors
errShrVio   equ 32  ;Sharing violations
errLokVio   equ 33  ;File Lock violation
;----------------------------------------------
errIDC      equ 34  ;Driver code, Invalid Disk Change
errNoFCB    equ 35  ;FCB unavailable
errShrFul   equ 36  ;Sharing buffer full

errNoNet    equ 50  ;Network request not supported, generic net fail

errFilExist equ 80  ;File already exists
errDirExist equ 82  ;Directory already exists
errFI44     equ 83  ;Fail on INT 44h
errRedir    equ 84  ;Too many levels of redirection
errDupRedir equ 85  ;Trying to redirect a redirection
errBadPass  equ 86  ;Bad password
errBadParam equ 87  ;Bad parameter passed to request
errNetWrite equ 88  ;Network Write Fault
;-----------------------------------------------------------------------------
;Error Loci
eLocUnk     equ 1   ;Unknown locus, nonspecific or not appropriate
eLocDsk     equ 2   ;Disk, related to Random Access to a disk or block device
eLocNet     equ 3   ;Network related issues, shouldnt affect us (yet!)
eLocChr     equ 4   ;Serial/Character Device error
eLocMem     equ 5   ;Related to RAM
;-----------------------------------------------------------------------------
;Error Actions
eActRet     equ 1   ;Retry
eActDRet    equ 2   ;Delay then retry (Try again later)
eActUsr     equ 3   ;Ask user to re-input data, i.e. bad filename or something
eActAbt     equ 4   ;Abort with cleanup
eActKil     equ 5   ;Abort without cleanup
eActIgn     equ 6   ;Ignore error
eActRetUsr  equ 7   ;Retry after user intervention (i.e swap remdev)
;-----------------------------------------------------------------------------
;Error classes
eClsOoR     equ 1   ;Out of resources, no mem or handles
eClsTS      equ 2   ;Temp sitch, should go away
eClsAuth    equ 3   ;Authorisation, e.g. file permission error 
eClsInt     equ 4   ;Internal DOS error or bug
eClsHrdFlt  equ 5   ;Hardware Failure
eClsSysFlt  equ 6   ;System Failure, i.e. missing or bad cfg files
eClsAppFlt  equ 7   ;Application Program error, inconsistent requests etc...
eClsNotFnd  equ 8   ;Resource not found (such as free SFT entry etc...)
eClsBadFmt  equ 9   ;Bad format for resource
eClsLocked  equ 10  ;Resource Locked
eClsMedia   equ 11  ;Error is on medium, i.e. wrong or damaged remdev
eClsClash   equ 12  ;Resource already exists!
eClsUnk     equ 13  ;Unknown class