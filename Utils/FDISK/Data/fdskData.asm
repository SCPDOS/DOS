;Disk Vars
reboot      db 0    ;If this flag is set, should reboot on exit
currentDisk db 1    ;1 based. For SCP/BIOS disk 1 = 80h, disk 2 = 81h etc...

curDiskSize dq 0    ;Number of usable sectors on device
sectorSize  dw 0    ;Size of a sector (should always be 512, error if not)

numDisks    db 0    ;Count of fixed disks

;Create Partition Variables
;We also refuse to use CHS. Set those entries to 0 generally
ptnStart    dd 0    ;LBA for start of partition
ptnSize     dd 0    ;Number of sectors in partition
ptnType     db 0    ;Partition signature, always 0Ch

;ActivePtnVariables
;Copy the partition status bytes here if the table is not free.
ptnFlags    db 0    ;Bits [3-0] give the valid partitions
                    ;Bits [7-4] give the active status of the partitions
numValidPtn db 0    ;Keeps a count of the number of valid partitions


cmdLine:    ;Users type at most 1 or 2 chars, add some padding
stringLen   db 0    ;Byte 0 = Max string len
charsTyped  db 0    ;Byte 1 = Num typed chars
inputString db 3 dup (0)    ;Where the chars are typed
;xfer Variables
xferBuffer  dq 0    ;A ptr to the memory block through which all xacts occur
sectorNum   dq 0    ;Number of the sector we wish to xact with
