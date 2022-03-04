;Create DPB chain of first 5 available DPB slots
firstDPB        resb dpb_size
secondDPB       resb dpb_size
thirdDPB        resb dpb_size
fourthDPB       resb dpb_size
fifthDPB        resb dpb_size 

;Create SFT header and corresponding array of five default sft entries
firstSftHeader  resb sfth_size
firstSft        resb sft_size
secondSft       resb sft_size
thirdSft        resb sft_size
fourthSft       resb sft_size
fifthSft        resb sft_size

;Create a five drive CDS here so we can jettison it if config wants more
initCDS         resb 5*cds_size 
;Always jettison this space
DOSENDPTR:   ;Points to the end of the initially reserved DOS area
tmpBufHdr       resb bufferHdr_size
msdTempBuffer   resb 512    ;Reserve one sectors worth of space
dynamicDataAreaLength equ $