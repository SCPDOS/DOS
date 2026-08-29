;FCB related structures

;Note: If record size > 64, high byte of randRecrd is ignored.
;This field also used for random read/write ops to find the right record
struc fcb
    .driveNum   db ?  ;0 based drive number
    .filename   db 8 dup (?)
    .fileext    db 3 dup (?)
    .curBlock   db 2 dup (?)  ;Current block number relative to 0
;One block consists of 128 records, of size denoted in the below
    .recordSize db 2 dup (?)  ;Record size, default 80h
    .fileSize   db 4 dup (?)  ;File size, in bytes
    .fileDate   db 2 dup (?)  ;Standard date format
    .fileTime   db 2 dup (?)  ;Standard time format
    ;----------------------------------------
    ;Internal Records
    .sftNdx     db ?  ;Internal, SFT ndx number
    .fcbMode    db ?  ;Internal byte, to indicate format of special field
    .special    db 6 dup (?)  ;For use with below internal structures
    ;----------------------------------------
    .recordNum  db ?  ;Current record in block
    .randRecrd  db 4 dup (?)  ;Relative record number, relative to start of file
endstruc

struc exFcb
    .extSig     db ?  ;If the first byte is -1, signals an exFCB
    .reserved   db 5 dup (?)  
    .attribute  db ?  ;File attribute byte
    .driveNum   db ?  ;0 based drive number
    .filename   db 8 dup (?)
    .fileext    db 3 dup (?)
    .curBlock   db 2 dup (?)  ;Current block number relative to 0
;One block consists of 128 records, of size denoted in the below
    .recordSize db 2 dup (?)  ;Record size, default 80h
    .fileSize   db 4 dup (?)  ;File size, in bytes
    .fileDate   db 2 dup (?)  ;Standard date format
    .fileTime   db 2 dup (?)  ;Standard time format
    ;----------------------------------------
    ;Internal Records
    .sftNdx     db ?  ;Internal, SFT ndx number
    .fcbMode    db ?  ;Internal byte, to indicate format of special field
    .special    db 6 dup (?)  ;For use with below internal structures
    ;----------------------------------------
    .recordNum  db ?  ;Current record in block
    .randRecrd  db 4 dup (?)  ;Relative record number, relative to start of file
endstruc

struc renameFcb
;Used for accessing the names and drive letter for renaming
;The fcb ptr must be pointing to the drivenumber even if the passed fcb was ext
    .driveNum   db ?
    .filename   db 8 dup (?)
    .fileext    db 3 dup (?)
    .reserved   db 4 dup (?)  ;Alignment bytes, cleared by AH=29h
    .resDrvNum  db ?  ;Always zero, used by AH=29h for the drive letter
    .newName    db 8 dup (?)
    .newExt     db 3 dup (?)
    .reserved2  db 4 dup (?)  ;Alignement bytes, cleared by AH=29h
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
    .attribWord db ?  ;(lowbyte DevAttrWord && 0Ch) || open mode
    .startClust db 2 dup (?)  ;Starting cluster of file (too small for FAT32)
    .dirSect    db 2 dup (?)  ;Sector containing dir entry (too small for FAT32)
    .dirEntry   db ?  ;Number of 32 byte directory entry IN sector
endstruc

;Used when the volume is local and share is loaded (fcbMode = 11xxxxxxb)
struc shareLocal    
    .strtClust  db 2 dup (?)  ;Starting cluster on disk of file (too small for FAT32)
    .shareSig   db 2 dup (?)  ;Share record number for this file (to find record)
    .fileAttr   db 2 dup (?)  ;File attribute (accessed as a byte)
endstruc

;Used when the volume is remote and share is loaded (fcbMode = 10xxxxxxb)
struc shareNet
;All three fields too small for FAT32
    .dirSect    db 2 dup (?)  ;Sector number containing dir entry
    .relClust   db 2 dup (?)  ;File relative cluster of last cluster accessed
    .absClust   db 2 dup (?)  ;Disk relative cluster of last cluster accessed
endstruc