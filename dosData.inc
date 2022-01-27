; This file contains the structure of the DOS Data area
dSeg STRUC 
    dataSegPtr  dq ?    ;Pointer to the data Segment itself
    codeSegPtr  dq ?    ;Pointer to the start of the resident Code segment
    bootDrive   db ?    ;The Int 33h device we booted from
    requestHdr  drvReqHdr    <,,,,>   ;The device driver header
                db 20 dup (?)    ;Reserve xtra space for cmd data, with padding
    sysVarsPtr  dq ?    ;Pointer to dpbHeadPtr, head of Sys Vars structure below
    mcbChainPtr dq ?    ;Pointer to the MCB chain
    dpbHeadPtr  dq ?    ;Pointer to the first DPB in the DPB chain
    sftHeadPtr  dq ?    ;Pointer to the first SFT header in SFT chain
    clockPtr    dq ?    ;Pointer to the current active CLOCK$ device
    conPtr      dq ?    ;Pointer to the current active CON device
    maxBytesSec dw ?    ;Maximum number of bytes per sector (size of buffers)
    bufHeadPtr  dq ?    ;Pointer to the first disk buffer
    cdsHeadPtr  dq ?    ;Pointer to the head of the CDS array
    sfcbHeadPTr dq ?    ;Pointer to the head of the System FCB chain
    numSafeSFCB dw ?    ;Number of protected FCBs (y in FCBS=x,y)
    numInt33drv db ?    ;Number of Int 33h drives detected in system
    lastdrvNum  db ?    ;Value of LASTDRIVE (default = 5) [Size of CDS array]
    numJoinDrv  db ?    ;Number of Joined Drives
    nulDevHdr   drvHdr <,,,,>


    inDOS       db ?    ;Incremented on each DOS call, decremented when leaving
    breakFlag   db ?    ;If set, check for CTRL+C on all DOS calls
    defaultDrv  db ?    ;Default, last accessed drive

    critStack   dq 41 dup (?)
    critStakTop dq ?
    IOStack     dq 199 dup (?)
    IOStakTop   dq ?
    DiskStack   dq 199 dup (?)
    DiskStakTop dq ?
dSeg ENDS