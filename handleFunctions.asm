;-----------------------------------:
;    File Handle Kernel routines    :
;-----------------------------------:

createFileHdl:     ;ah = 3Ch, handle function
openFileHdl:       ;ah = 3Dh, handle function
closeFileHdl:      ;ah = 3Eh, handle function
readFileHdl:       ;ah = 3Fh, handle function
writeFileHdl:      ;ah = 40h, handle function
deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
movFileReadPtr:    ;ah = 42h, handle function, LSEEK
changeFileModeHdl: ;ah = 43h, handle function, CHMOD
ioctrl:            ;ah = 44h, handle function
duplicateHandle:   ;ah = 45h, handle function
forceDuplicateHdl: ;ah = 46h, handle function
findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
renameFile:        ;ah = 56h
createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
createNewFile:     ;ah = 5Bh
lockUnlockFile:    ;ah = 5Ch
setHandleCount:    ;ah = 67h
commitFile:        ;ah = 68h, flushes buffers for handle to disk 
    ret
;-----------------------------------:
;        File Handle routines       :
;-----------------------------------:
readBinaryByteFromFile:
;Reads a byte from a SFT entry, does not translate it. 
;Read or RW permissions are checked at the INT 41h level
;Entry: rbx = SFT entry pointer
;       rdx = Address of the data buffer to read to
;       ecx = Number of bytes to read
;Exit: If CF = NC : All ok!
;       rbx = SFT entry pointer
;       al = 8 bit binary value read from device/file
;      If CF = CY : Error!
;       rbx = SFT entry pointer
;       al = Error code to ret if user returns fail from int 44h or no int 44h
;
; !!! Use the disk request header for all file handle IO !!!
;
    test word [rbx + sft.wDeviceInfo], devCharDev
    jnz .readBinaryByteFromCharDevice
.readBinaryByteFromHardFile:
;Disk files are accessed from here
;Use the sector buffers if the data is already buffered,
; else use the dpb to fill a sector buffer


.readBinaryByteFromCharDevice:
;Devices are accessed from here
    mov rbp, qword [rbx + sft.qPtr] ;Get device driver header pointer
    push rbx
    lea rbx, charReqHdr
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], drvREAD
    mov word [rbx + ioReqPkt.status], 0
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], ecx

    call qword [rbp + drvHdr.strPtr]
    call qword [rbp + drvHdr.intPtr]
    mov eax, dword [rbx + ioReqPkt.tfrlen] ;Get number of bytes read
    test word [rbx + ioReqPkt.status], 8000h    ;Test the error bit is set
    pop rbx
    jz .readBinaryByteExitGood  ;Error bit not set, all good!
.readBinaryByteExitGood:
    ret