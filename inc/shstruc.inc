;The Master File Table has three types:
;Allocated -> All fields are meaningful
;Free -> Only the bSig and wLen fields are meaningful
;End -> Only .bSig is meaningful

mftAlloc equ 1       ;Indicates the space is allocated.
mftFree  equ 0       ;Free space in the allocation area
mftEnd   equ 0FFh    ;End of table marker.

struc mft
    .bSig       db ?    ;Struc status
    .dLen       dd ?    ;Length of struc
    .bCheckSum  db ?    ;Checksum of the full filename (helps with searches)
    .pLock      dq ?    ;Pointer to the first lock owned by file
    .pSFT       dq ?    ;Pointer to the first SFT in this file's chain
    .sName:             ;Symbol to the filename (variable size, as in wLen)
endstruc

;The symbol below is the size of the smallest allocatable MFT.
;This gives space for an MFT header plus a X:\XXXXXXXX.XXX<NUL>
MIN_MFT_SIZE    equ     mft_size + MAX_DRIVE + MAX_FILE

;Each MFT (MFT) points to an SFT and, possibly, a lock. If a file has multiple 
; locks, each lock is then linked into a linked list. If a file has multiple 
; SFTs, then they are also linked into a linked list.
;All SFTs point back to the same MFT.
;
;Share synchronises meta-data across all SFT entries. Once all SFT's allocated
; to an MFT have been freed the MFT is also freed. We then do garbage 
; collection.


;The file lock block has one type only
struc fileLock
    .pNext      dq ?    ;Ptr to the next flock. 0 if end
    .qStart     dq ?    ;Ptr to the first byte of the locked region
    .qEnd       dq ?    ;Ptr to the last byte of the locked region
    .pSFT       dq ?    ;Ptr to the SFT which owns this lock
    .qPID       dq ?    ;Process ID of the process which owns this lock
endstruc

MAX_LOCKS       equ 9999    
LOCKS_DFLT      equ 20

MAX_MFT_SIZE    equ 1048576 ;1Mb
MFT_SIZE_DFLT   equ 2048