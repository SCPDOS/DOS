;IOCTL request header
reqTable    db genioctlGetParamsTable_size dup (0)
;Static BPB data here
genericBPB:  defaultBPB



;Data area here
oldDrive    db -1       ;Old default drive (0 based)
fmtDrive    db -1       ;Drive we are operating on (0 based)
fmtDrvInv   db 0        ;If set to -1, the drive needs to be reactivated
cdsPtr      dq 0        ;CDS ptr here
bufferArea  dq 0        ;Ptr to the buffer area
;Format Data here
remDev      db 0        ;0 = Removable, -1 = Fixed
fatType     db -1       ;0 = FAT12, 1 = FAT16, 2 = FAT32, -1 = No FAT
sectorSize  dw 0        ;Sector size in bytes
numSectors  dq 0        ;Number of sectors in volume