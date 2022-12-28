
;FDISK utility for SCP/DOS 1.0
;Bypases filesystem to "low-level format" the target fixed disk.
;Restarts the machine upon completion by triple faulting.

;Steps taken:
;0) Call Int 4Ah/EAX=0h. If it returns ah <> 0, then tell the user to
;    stop any other tasks and networking software before continuing.
;    Give them a "Press Enter to continue or ESC to return to DOS" prompt.
;1) Queries the BIOS implementation (in this case SCP/BIOS) for the number
;    of fixed disks.
; Enter the Menu.
;Main menu will have 4 main options and 1 optional option
;   1) Create 

;If the user wants to create a new partition, we ask them how much of the 
; disk they want their partioned to take up as a percentage from 1%-99%
;We then round to the nearest cylinder (multiple of 64 sectors), 
; rounding up (if 0) or down (if past max sector).

[map all ./Utils/FDISK/Listings/fdisk.map]
[DEFAULT REL]
BITS 64
%include "./Source/Include/dosMacro.mac"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Include/fatStruc.inc"
%include "./Source/Include/dosError.inc"

;Hard disks must have 512 byte sectors
fddSectorSize   equ 200h

%include "./Utils/FDISK/Source/fdskMain.asm"
%include "./Utils/FDISK/Source/fdskUtil.asm"
%include "./Utils/FDISK/Source/fdskExt.asm"
%include "./Utils/FDISK/Data/fdskData.asm"
%include "./Utils/FDISK/Data/fdskMsg.asm"

;Place the partial VBR here.
;All uninitialised values are set to 0
;If total sectors is bigger than 16 bits, set totSec16 to 0
;Else set totSec32 to 0
partialVBR:
    istruc bpb
    at bpb.jmpBoot,     db 0EBh, 00h, 90h   ;Needed start signtature
    at bpb.oemName,     db 'SCPDOSv1'
    at bpb.bytsPerSec,  dw 0200h        ;512 bytes per sector always
    at bpb.secPerClus,  db 0            ;Unset by FDISK
    at bpb.revdSecCnt,  dw 0            ;Unset by FDISK
    at bpb.numFATs,     db 0            ;Unset by FDISK
    at bpb.rootEntCnt,  dw 0            ;Unset by FDISK
    at bpb.totSec16,    dw -1           ;Total number of sectors on disk
    at bpb.media,       db 0F8h         ;Hard Disk Media byte
    at bpb.FATsz16,     dw 0            ;Unset by FDISK
    at bpb.secPerTrk,   dw 003Fh        ;Fake Hard disk geometry 64 sec/trk
    at bpb.numHeads,    dw 00FFh        ;255 Heads
    at bpb.hiddSec,     dd -1           ;Set this field
    at bpb.totSec32,    dd -1           ;Set total sec if bigger than 16 bits 
    iend
partialVBRL equ $ - partialVBR
freshMBRcopy:   ;Symbol pointing to the MBR copy appended