;DOS globals

; DOS variables for reloc computations in sysinit
GLOBAL dSegLen  ;Exported for calculations with dosSeg
GLOBAL dtextL   ;Exported for calculation of initialised code/data w/o sysinit

; DOS DATA AREA variables
GLOBAL nulDevHdr
GLOBAL anchorMcb
GLOBAL dayOfMonth
GLOBAL monthOfYear
GLOBAL vConPtr
GLOBAL clockPtr
GLOBAL currentPSP
GLOBAL editKeys
GLOBAL shareCount
GLOBAL shareDelay
GLOBAL caseMapFunc
GLOBAL charTableArray
GLOBAL serverDispTbl
GLOBAL machineName
GLOBAL critPtchTbl
GLOBAL dosCrit1Enter
GLOBAL dosCrit1Exit
GLOBAL dosCrit2Enter
GLOBAL dosCrit2Exit
GLOBAL dosMgrHooks
GLOBAL dllHooks
GLOBAL shareHooks
GLOBAL goodDfltShareHook
GLOBAL badDfltShareHook
GLOBAL dosBIOSName
GLOBAL biosVers
GLOBAL biosPtr
GLOBAL bootDrive
GLOBAL numFiles
GLOBAL lastdrvNum
GLOBAL firstSftHeader
GLOBAL mcbChainPtr
GLOBAL vConPtr
GLOBAL clockPtr
GLOBAL extKeyFunc
GLOBAL errorDrv
GLOBAL currentNdx
GLOBAL switchChar
GLOBAL ctryTbl
GLOBAL serverDispTblPtr
GLOBAL currentDTA
GLOBAL cdsHeadPtr
GLOBAL dpbHeadPtr
GLOBAL maxBytesSec
GLOBAL bufHeadPtr
GLOBAL sftHeadPtr
GLOBAL numBuffers
GLOBAL fcbsHeadPtr
GLOBAL numPhysVol