
;-----------------------------------:
;           Static Data             :
;-----------------------------------:
dosMajor    db 00h      ;Version 0
dosMinor    db 01h      ;.01
dosBIOSName db "SCPBIOS .SYS"
dosKernName db "SCPBDOS .SYS"
switchchar  db "/" ;Editable by the Int 41h/ah=37h