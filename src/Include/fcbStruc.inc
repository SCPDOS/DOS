;FCB related structures

;Note: If record size > 64, high byte of randRecrd is ignored.
;This field also used for random read/write ops to find the right record
struc fcb
    .driveNum   resb 1  ;0 based drive number
    .filename   resb 8
    .fileext    resb 3
    .curBlock   resb 2  ;Current block number relative to 0
;One block consists of 128 records, of size denoted in the below
    .recordSize resb 2  ;Record size, default 80h
    .fileSize   resb 4  ;File size, in bytes
    .fileDate   resb 2  ;Standard date format
    .fileTime   resb 2  ;Standard time format
    ;----------------------------------------
    ;Internal Records
    .sftNdx     resb 1  ;Internal, SFT ndx number
    .fcbMode    resb 1  ;Internal byte, to indicate format of special field
    .special    resb 6  ;For use with below internal structures
    ;----------------------------------------
    .recordNum  resb 1  ;Current record in block
    .randRecrd  resb 4  ;Relative record number, relative to start of file
endstruc

struc exFcb
    .extSig     resb 1  ;If the first byte is -1, signals an exFCB
    .reserved   resb 5  
    .attribute  resb 1  ;File attribute byte
    .driveNum   resb 1  ;0 based drive number
    .filename   resb 8
    .fileext    resb 3
    .curBlock   resb 2  ;Current block number relative to 0
;One block consists of 128 records, of size denoted in the below
    .recordSize resb 2  ;Record size, default 80h
    .fileSize   resb 4  ;File size, in bytes
    .fileDate   resb 2  ;Standard date format
    .fileTime   resb 2  ;Standard time format
    ;----------------------------------------
    ;Internal Records
    .sftNdx     resb 1  ;Internal, SFT ndx number
    .fcbMode    resb 1  ;Internal byte, to indicate format of special field
    .special    resb 6  ;For use with below internal structures
    ;----------------------------------------
    .recordNum  resb 1  ;Current record in block
    .randRecrd  resb 4  ;Relative record number, relative to start of file
endstruc

struc renameFcb
;Used for accessing the names and drive letter for renaming
;The fcb ptr must be pointing to the drivenumber even if the passed fcb was ext
    .driveNum   resb 1
    .filename   resb 8
    .fileext    resb 3
    .reserved   resb 4  ;Alignment bytes, cleared by AH=29h
    .resDrvNum  resb 1  ;Always zero, used by AH=29h for the drive letter
    .newName    resb 8
    .newExt     resb 3
    .reserved2  resb 4  ;Alignement bytes, cleared by AH=29h
endstruc

;FCBmode equates:
;Bit 7 = Set => Share loaded, Clear => Share not loaded
;Bit 6 = Set => Char device, Clear => Disk file
;Bits [5,0] = Low 6 bits of device attribute word 
fcbShareOn  equ 80h
fcbCharDev  equ 40h
fcbLocal    equ 40h

;Structures of the special 6 bytes

;Used when share is NOT loaded (fcbMode = 0xxxxxxxb)
struc noShare   
    .attribWord resb 1  ;(lowbyte DevAttrWord && 0Ch) || open mode
    .startClust resb 2  ;Starting cluster of file (too small for FAT32)
    .dirSect    resb 2  ;Sector containing dir entry (too small for FAT32)
    .dirEntry   resb 1  ;Number of 32 byte directory entry IN sector
endstruc

;Used when the volume is local and share is loaded (fcbMode = 11xxxxxxb)
struc shareLocal    
    .strtClust  resb 2  ;Starting cluster on disk of file (too small for FAT32)
    .shareSig   resb 2  ;Share record number for this file (to find record)
    .fileAttr   resb 2  ;File attribute (accessed as a byte)
endstruc

;Used when the volume is remote and share is loaded (fcbMode = 10xxxxxxb)
struc shareNet
;All three fields too small for FAT32
    .dirSect    resb 2  ;Sector number containing dir entry
    .relClust   resb 2  ;File relative cluster of last cluster accessed
    .absClust   resb 2  ;Disk relative cluster of last cluster accessed
endstruc