;Installs DOS onto a target Disk. Copies system files and a command interpreter
;Also sets the byte in the VBR to bootable once everything is copied over.

;1) Check Root Dir has no entries in it (ignoring Volume Labels)
;2) Search for the boot files in the root dir of current drive.
;3) If the boot files are found, read them both into memory.
;4) Write them onto the target medium root dir and close the handles.
;5) Enter a critical section and direct IO read sector 0 of the volume
;6) Set the bootable flag to on and fix the SCP/BIOS struct data for 
;    SCP/BIOS.SYS and SCP/DOS.SYS.
;7) Write back to disk, Exit critical section and exit.

;SYS needs to set the startSector (QWORD) and numberOfSectors (WORD) as 
; well as the transfer packet.
;The first is to locate a copy of SCPBIOS somewhere on the disk (can be
; in another volume).
;The second is tho locate SCPDOS to load after SCPBIOS is complete and 
; resident.

;startSector and numberOfSectors are always placed in that order
; directly AFTER the BPB (or BPB32, please check which FAT type we are).
;The transfer packet is always located before the boot switch byte.
;The boot switch byte is always located before the boot signature.

[map all ./Source/Utils/SYS/Listings/sys.map]
[DEFAULT REL]
BITS 64
%include "./Source/Include/dosMacro.mac"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Include/fatStruc.inc"
%include "./Source/Include/dosError.inc"

%include "./Source/Utils/FORMAT/Source/sysMain.asm"
%include "./Source/Utils/FORMAT/Data/sysData.asm"