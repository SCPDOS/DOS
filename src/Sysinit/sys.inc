
;Sysinit globals
GLOBAL FINALDOSPTR
GLOBAL FILES
GLOBAL BUFFERS
GLOBAL DFLTDRIVE
GLOBAL LASTDRIVE
GLOBAL OEMBIOS
GLOBAL OEMDRVCHAIN
GLOBAL OEMPTR
GLOBAL OEMVERSION
GLOBAL MCBANCHOR
GLOBAL OEMMEMPTR
GLOBAL SYSENTRY

;OEM externs
EXTERN OEMMCBINIT
EXTERN OEMHALT
EXTERN OEMCALLBK

;Linker externs
EXTERN dosEnd   ;Pointer from linker script
EXTERN dosLen   ;From linker script
EXTERN dosFileS ;From linker script

;DOS externs
EXTERN dSegLen  ;From .bss of DOS

EXTERN nulDevHdr
EXTERN anchorMcb
EXTERN dayOfMonth
EXTERN monthOfYear
EXTERN vConPtr
EXTERN clockPtr
EXTERN currentPSP
EXTERN editKeys
EXTERN shareCount
EXTERN shareDelay
EXTERN caseMapFunc
EXTERN charTableArray
EXTERN charTableArray.ucTable
EXTERN charTableArray.filenameUCTable
EXTERN charTableArray.filenameTerminatingTable
EXTERN charTableArray.collatingTable
EXTERN serverDispTbl
EXTERN machineName
EXTERN critPtchTbl
EXTERN dosCrit1Enter
EXTERN dosCrit1Exit
EXTERN dosCrit2Enter
EXTERN dosCrit2Exit
EXTERN dosMgrHooks
EXTERN dllHooks
EXTERN shareHooks
EXTERN goodDfltShareHook
EXTERN badDfltShareHook
EXTERN dosBIOSName
EXTERN biosVers
EXTERN biosPtr
EXTERN bootDrive
EXTERN numFiles
EXTERN lastdrvNum
EXTERN firstSftHeader
EXTERN mcbChainPtr
EXTERN vConPtr
EXTERN clockPtr
EXTERN extKeyFunc
EXTERN errorDrv
EXTERN currentNdx
EXTERN switchChar
EXTERN ctryTbl
EXTERN serverDispTblPtr
EXTERN currentDTA
EXTERN cdsHeadPtr
EXTERN dpbHeadPtr
EXTERN maxBytesSec
EXTERN bufHeadPtr
EXTERN sftHeadPtr
EXTERN numBuffers
EXTERN fcbsHeadPtr
EXTERN numPhysVol


;DOS Externs (SYSDATA)
EXTERN i0
EXTERN i1
EXTERN i2
EXTERN i3
EXTERN i4
EXTERN i5
EXTERN i6
EXTERN i7
EXTERN i8
EXTERN i9
EXTERN i10
EXTERN i11
EXTERN i12
EXTERN i13
EXTERN i14
EXTERN i15
EXTERN i16
EXTERN i17
EXTERN i18
EXTERN i19
EXTERN i20
EXTERN i21

EXTERN terminateProcess 
EXTERN functionDispatch 
EXTERN defaultIretq
EXTERN dosDefCritErrHdlr    
EXTERN absDiskRead      
EXTERN absDiskWrite     
EXTERN terminateRes     
EXTERN multiplexHdlr
EXTERN nulStrat
EXTERN nulIntr