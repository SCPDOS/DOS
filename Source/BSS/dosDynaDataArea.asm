;This is the scratch space allocated by DOS. This also forms the start of 
; the MCB chain!
ddaStart:
dosMCB          resb mcb_size

;Create DPB chain of first 5 available DPB slots
;The bottom three DPB's are jettisonable if the devices dont exist!
firstDPB        resb dpb_size
secondDPB       resb dpb_size
DOSENDPTR:   ;Points to the end of the initially reserved DOS area
;Always jettison this space
thirdDPB        resb dpb_size
fourthDPB       resb dpb_size
fifthDPB        resb dpb_size 

tmpBufHdr       resb bufferHdr_size
msdTempBuffer   resb 4096    ;Reserve one (MAX) sectors' worth of space
;This buffer is used as a temp read buffer by the MSDdriver directly (no header)
dynamicDataAreaLength equ $ - ddaStart