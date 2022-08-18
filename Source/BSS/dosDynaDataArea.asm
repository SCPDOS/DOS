;This is the scratch space allocated by DOS. This also forms the start of 
; the MCB chain!
ddaStart:
dosMCB          resb mcb_size

dosDynamicArea: ;Difference of this symbol with cfgFrame endPtr gives alloc size
;Create DPB chain of first 5 available DPB slots
firstDPB        resb dpb_size   ;First two are always present
secondDPB       resb dpb_size
thirdDPB        resb dpb_size
fourthDPB       resb dpb_size
fifthDPB        resb dpb_size 

;fcbsArray       resb sfth_size + 4*sft_size ;One header plus 4 SFT entries

cdsArray        resb 5*cds_size ;5 CDS's unless more are specified

tmpBufHdr       resb bufferHdr_size
msdTempBuffer   resb 4096    ;Reserve one (MAX) sectors' worth of space
;This buffer is used as a temp read buffer by the MSDdriver directly (no header)
cfgLoad:    ;This symbol is used to read config.sys here
dynamicDataAreaLength equ $ - ddaStart