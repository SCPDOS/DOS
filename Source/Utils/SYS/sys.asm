;Installs DOS onto a target Disk. Copies system files and a command interpreter
;Also sets the byte in the VBR to bootable once everything is copied over.

;1) Check Root Dir has no entries in it (ignoring Volume Labels)
;2) Starting from drive A:, excluding the drive being formatted,
;    search for the boot files, first in X:\ then X:\DOS\
;3) If the boot files are found, read them both into memory.
;4) Write them onto the target medium root dir and close the handles.
;5) Enter a critical section and direct IO read sector 0 of the volume
;6) Set the bootable flag to on and fix the SCP/BIOS struct data for 
;    SCP/BIOS.SYS and SCP/DOS.SYS.
;7) Write back to disk, Exit critical section and exit.

