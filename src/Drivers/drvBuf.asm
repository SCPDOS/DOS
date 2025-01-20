;This file is just to denote UNINITIALISED buffers and vars
; for the drivers that are to be used during INIT.
;They can (and should) be ejected after use
bpbArray:   ;Recycle this space for the BPBptr array.
physVol db ?    ;Count of volumes we report to DOS
fixPtn: ;Use this symbol to keep track of how many fixed partitions we have
fixDrv  db ?    ;Fixed disks
remDrv  db ?    ;Removable devices
biosDrv db ?    ;Current BIOS number we are playing with
dosDrv  db ?    ;Dos drive number we are setting up for
mbrEtry db ?    ;Indicates which entry we are analysing
mbrE    db 64 dup (?)   ;We copy the MBR we are analysing here
ebrE    db 32 dup (?)   ;We copy the EBR we are analysing here
msdTempBuffer   db 4096 dup (?)