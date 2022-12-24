;This is the disk formatting utility for SCP/DOS 1.0
;Uses the undocumented LBA based Generic IO interface

;Supports exactly one command line argument, the drive letter.
;Invoked as so: FORMAT x: where the colon is necessary.

;>>> 7 Steps to Disk Domination <<<
;1) Format begins by turning the drive offline by cleaning the 
;    cdsValidDrive bit in the device CDS. 
;2) Format begins by ascertaining how large the volume/device is.
;    -If the device is removable, format will get the device parameters to 
;      ascertain the size of the volume.
;    -If the device fixed, format will use the VBR to ascertain the size of the 
;      volume and gets device parameters to get the sector size.
;3) Format will then choose which FAT to use and build the BPB accordingly
;    and write it to disk.
;4) Format will then rebuild the disk DPB from the new BPB.
;5) Format will then create two fresh FAT tables.
;6) Format will then clean the root directory (FAT12/16) or allocate a 
;    cluster and sanitise it (FAT32)
;7) Finally, format re-enables cdsValidDrive and exits.

;If a ^C is invoked during the format procedure, we prompt the user 
; for the "are you sure you wish to abandon the format" and that "this may
; result in an unusable volume that will need reformatting" message.
;If they respond with Y, we re-enable the CDS and return to DOS to exit.

;Note Format does not format the full medium and uses Int 45h to read 
; sectors from the old format and Int 46h to write new sectors to the 
; volume. 
;Format also doesnt depend on any old BPB's or anything like so.
;Any old FAT (or other FS) data structures are considered nukable.

[map all ./Source/Utils/FORMAT/Listings/format.map]
[DEFAULT REL]
BITS 64
%include "./Source/Include/dosMacro.mac"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Include/fatStruc.inc"

struc genioctlGetParamsTable
    .size           resb 1
    .res            resb 7
    .sectorSize     resb 8  ;Only the lower dword is valid here
    .numSectors     resb 8
endstruc

%include "./Source/Utils/FORMAT/Source/fmtMain.asm"
%include "./Source/Utils/FORMAT/Data/fmtData.asm"
%include "./Source/Utils/FORMAT/Data/fmtMsg.asm"