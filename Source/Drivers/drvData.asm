;-----------------------------------:
;          Driver routines          :
;-----------------------------------:
drivers:
conHdr:
    dq auxHdr
    dw 08013h
    dq commonStrat
    dq conDriver
    db "CON     "
auxHdr:
    dq prnHdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "AUX     "
prnHdr:
    dq clkHdr
    dw 0A040h
    dq nulStrat
    dq nulIntr
    db "PRN     "
clkHdr:
    dq msdHdr
    dw 08008h
    dq commonStrat
    dq clkDriver
    db "CLOCK$  "
msdHdr:
    dq com1Hdr
    dw 00800h   ;Once Generic IO implemented, change to 00840h
    dq commonStrat
    dq msdDriver
    db 0,0,0,0,0,0,0,0
com1Hdr:
    dq com2Hdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "COM1    "
com2Hdr:
    dq com3Hdr
    dw 08000h
    dq commonStrat
    dq com2Intr
    db "COM2    "
com3Hdr:
    dq com4Hdr
    dw 08000h
    dq commonStrat
    dq com3Intr
    db "COM3    "
com4Hdr:
    dq lpt1Hdr
    dw 08000h
    dq commonStrat
    dq com4Intr
    db "COM4    "
lpt1Hdr:
    dq lpt2Hdr
    dw 0A040h
    dq nulStrat
    dq nulIntr
    db "LPT1    "
lpt2Hdr:
    dq lpt3Hdr
    dw 0A040h
    dq nulStrat
    dq nulIntr
    db "LPT2    "
lpt3Hdr:
    dq -1
    dw 0A040h
    dq nulStrat
    dq nulIntr
    dq "LPT3    "
reqHdrPtr  dq 0    ;Where the default device drivers store the ReqPtr