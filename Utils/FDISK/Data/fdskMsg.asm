;This file will contain the "pages" of the application

crlf    db LF,CR,"$"
;This message is only printed if a multitasker is detected as installed
multiMsg:   
    db "Please stop all other programs and/or any networking software",
    db " before proceeding.",LF,CR
    db "Press Enter to continue once this is done or ESC to return to",
    db "DOS",LF,CR,"$"
strtMsg:
    db CR,LF
    db "Scientific Control Program / Disk Operating System",LF,CR
    db "Fixed Disk Setup Program Version $"
cpyrtMsg:
    db LF,CR,"(C) Copyright Scientific Computer Research. 2022", LF,CR,"$"

exitMsg:
    db "System will now restart",LF,CR
    db "Insert bootable DOS medium in drive A:",LF,CR
    db "Press any key when ready...",LF,CR,"$"

prompt: db "Fixed Disk "
drvNum  db "1] $"

helpMsg:
    db "?) Display Available Options",LF,CR,"$"

exitOptionMsg:
    db "Press X to exit",LF,CR,"$"
retMsg:
    db "Press X to return to FDISK options",CR,LF,"$"

selectValidOption:
    db CR,LF,LF,LF,
    db "------------------------------------",CR,LF
    db "    Please select a valid choice    ",CR,LF,
    db "------------------------------------",CR,LF,LF,LF,"$"

;Pages
;MAIN PAGE
mainPageMsg:
    db "Choose one of the following:",LF,CR
    db "  1) Create DOS Partition",LF,CR
    db "  2) Change Active Partition",LF,CR
    db "  3) Delete DOS Partition",LF,CR
    db "  4) Display Partition Information",LF,CR,"$"
mpOptionalMsg:
    db "  5) Select Fixed Disk Drive",LF,CR,"$"

;CREATE PARTITION PAGE
createPageBadMsg:
    db "Primary DOS partition already exists",LF,CR,"$"
createPageMsg:
    db "Create DOS Partition",LF,LF,CR
    db "Do you wish to use the entire fixed disk for DOS? Y/N",CR,LF,"$"
createPage2Msg:
    db "Specify how much of the fixed disk you wish to use (1%-99%) or"
    db CR,LF,"$"    ;Followed by the retMsg
createPromptMsg: 
    db "Type a number between 1 and 99] $"
createDoneMsg:
    db "Primary partition created", CR,LF,"$"

;CHANGE ACTIVE PARTITION ON DISK PAGE
activeSinglePtnMsg:
    db CR,LF,"Partition"
.number:
    db "1 is already active",CR,LF,"$"

activePromptMsg:
    db CR,LF,"Please select a partition to mark as active (1-4): $"
activePartitionSetMsg:
    db CR,LF,"Partition"
.number:
    db "1 now set active",CR,LF,"$"

;DELETE PARTITION PAGE
deleteNukeMsg:
    db CR,LF,
    db "Delete the whole Partition Table? Y/N: $"
deleteNukeCompleteMsg:
    db CR,LF,"Partition Table Deleted",CR,LF,"$"
deleteSelectMsg:
    db CR,LF, "Please select a partition to delete (1-4): $"
deleteCannotMsg:
    db CR,LF, "Cannot delete an active partition",CR,LF,"$"
deleteBadFoundMsg:
    db CR,LF, "Bad Partition Table detected.",CR,LF,"$"
deleteOkMsg:
    db CR,LF, "Partition"
.number:
    db "1 deleted",CR,LF,"$"

;SELECT FIXED DISK PAGE 
selectNumber:
    db CR,LF,"FDISK has detected"
.number:
    db "0 Fixed Disk Drives",CR,LF,"$"
selectPrompt:
    db CR,LF,"Please select a Fixed Disk (1-"
.number:
    db "1): $"
;Partition status table
partTitle:
    db "-------------------------------------------------",CR,LF
    db "Partition Status Type  Start    End      Size    ",CR,LF,"$"
;Partition status table string, this gets edited... a lot
partString:
    db "    "
.ptnNum:
    db "1", "    ", " ", "   "
.ptnSts:
    db "A","  "," " 
.ptnFS:
    db "     "  ;This is 5 chars, either FAT12, FAT16, FAT32 or a 2 digit num 
    db " "
.ptnStart:
    db "        "   ;8 digits for the LBA of the first sector of the partition
    db " "
.ptnEnd:
    db "        "   ;8 digits for the LBA of the last sector of the partition
    db " "
.ptnSize:
    db "        "   ;8 digits for the number of sectors in the partition
partStringL equ $ - partString  ;Use this to clean the string with spaces
    db CR,LF,"$"

totalSpaceMsg:
    db CR,LF
    db "-------------------------------------------------",CR,LF
    db "Total Disk Space is ", 
.sizeCount:
    db "        ",
    db " Sectors",CR,LF
    db "Sector Size: "
.sectorCount:
    db "    "
    db " Bytes",CR,LF
    db "-------------------------------------------------",CR,LF,"$"


fat12String:    db "FAT12"
fat16String:    db "FAT16"
fat32String:    db "FAT32"

;Error Messages
badVerStr:
    db "Incorrect DOS version",CR,LF,"$"
noDisks:
    db "No fixed disks present",CR,LF,"$"
noMemoryMsg:
    db "Not enough free memory in system",CR,LF,"$"
invalidMBRMsg:
    db "Invalid or Missing Master Boot Record",CR,LF
    db "Please Create a new Master Boot Record",CR,LF,"$"
badSectorMsg:
    db "Bad Sector Size Detected",CR,LF,"$"
badReadMsg:
    db "Error reading Fixed Disk",CR,LF,"$"
badWriteMsg:
    db "Error writing Fixed Disk",CR,LF,"$"

