;-----------------------------------:
;          Driver routines          :
;-----------------------------------:
driverChain:
conHdr:
    dq auxHdr
    dw devDrvChar | devDrvFastOut | devDrvConOut | devDrvConIn
    dq commonStrat
    dq conDriver
    db "CON     "
auxHdr:
    dq prnHdr
    dw devDrvChar
    dq commonStrat
    dq com1Intr
    db "AUX     "
prnHdr:
    dq clkHdr
    dw devDrvChar | devDrvOutToBsy | devDrvIOCTL    ;Dont really support IOCTL
    dq commonStrat
    dq prnDriver
    db "PRN     "
clkHdr:
    dq msdHdr
    dw devDrvChar | devDrvClockDev
    dq commonStrat
    dq clkDriver
    db "CLOCK$  "
msdHdr:
    dq com1Hdr
    dw devDrvOpClRem | devDrvIOCTLQ | devDrvIOCTL
    dq commonStrat
    dq msdDriver
    db 0,0,0,0,0,0,0,0
com1Hdr:
    dq com2Hdr
    dw devDrvChar
    dq commonStrat
    dq com1Intr
    db "COM1    "
com2Hdr:
    dq com3Hdr
    dw devDrvChar
    dq commonStrat
    dq com2Intr
    db "COM2    "
com3Hdr:
    dq com4Hdr
    dw devDrvChar
    dq commonStrat
    dq com3Intr
    db "COM3    "
com4Hdr:
    dq lpt1Hdr
    dw devDrvChar
    dq commonStrat
    dq com4Intr
    db "COM4    "
lpt1Hdr:
    dq lpt2Hdr
    dw devDrvChar | devDrvOutToBsy | devDrvIOCTL    ;Dont really support IOCTL
    dq commonStrat
    dq prnDriver
    db "LPT1    "
lpt2Hdr:
    dq lpt3Hdr
    dw devDrvChar | devDrvOutToBsy | devDrvIOCTL    ;Dont really support IOCTL
    dq commonStrat
    dq prnDriver
    db "LPT2    "
lpt3Hdr:
    dq -1
    dw devDrvChar | devDrvOutToBsy | devDrvIOCTL    ;Dont really support IOCTL
    dq commonStrat
    dq prnDriver
    dq "LPT3    "
reqPktPtr  dq 0    ;Where the default device drivers store the ReqPtr