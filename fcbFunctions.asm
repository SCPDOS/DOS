openFileFCB:       ;ah = 0Fh
closeFileFCB:      ;ah = 10h
findFirstFileFCB:  ;ah = 11h
findNextFileFCB:   ;ah = 12h
deleteFileFCB:     ;ah = 13h
sequentialReadFCB: ;ah = 14h
sequentialWriteFCB:;ah = 15h
createFileFCB:     ;ah = 16h
renameFileFCB:     ;ah = 17h
    ret
setDTA:            ;ah = 1Ah
;Called with:
;   rdx = Pointer to the new default DTA
    mov rbx, qword [oldRSP]
    mov rdx, qword [rbx + callerFrame.rdx]
    mov qword [currentDTA], rdx
    ret
randomReadFCB:     ;ah = 21h
randomWriteFCB:    ;ah = 22h
getFileSizeFCB:    ;ah = 23h
setRelRecordFCB:   ;ah = 24h
randBlockReadFCB:  ;ah = 27h
randBlockWriteFCB: ;ah = 28h
parseFilenameFCB:  ;ah = 29h
    ret