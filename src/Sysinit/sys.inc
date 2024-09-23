
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

;OEM externs
EXTERN OEMMCBINIT
EXTERN OEMHALT
EXTERN OEMCALLBK

;Linker externs
EXTERN dosEnd   ;Pointer from linker script
EXTERN dosLen   ;From linker script

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
