
;-----------------------------------:
;           Static Data             :
;-----------------------------------:
dosMajor    db 00h      ;Version 0
dosMinor    db 01h      ;.01
dosBIOSName db "SCPBIOS .SYS"
dosKernName db "SCPBDOS .SYS"

;Keyboard vCon static data taht can be edited and replaced by a user if they
; wish to install their own custom logic.
oemKeyFunc  dq 0 ;The editing keys can be replaced by replacing this ptr
oemESC      db 00h  ;NULL char is our Escape char
oemBreak    db 1Bh  ;1Bh is our Break Char
;Extended ASCII keys with special meanings
oemKeyTbl   db eF1, "T"
            dw (buffCharInput_BE.f1 - oemKeyTbl)
            db eF2, "I"
            dw (buffCharInput_BE.f2 - oemKeyTbl)
            db eF3, "N"
            dw (buffCharInput_BE.f3 - oemKeyTbl)
            db eF4, "A"
            dw (buffCharInput_BE.f4 - oemKeyTbl)
            db eF5, " "
            dw (buffCharInput_BE.f5 - oemKeyTbl)
            db eF6, "R"
            dw (buffCharInput_BE.f6 - oemKeyTbl)
            db eF7, "E"
            dw (buffCharInput_BE.f7 - oemKeyTbl)
            db eCursL, "M"
            dw (buffCharInput_BE.delete - oemKeyTbl)
            db eCursR, "E"
            dw (buffCharInput_BE.f1 - oemKeyTbl)
            db eIns, "C"
            dw (buffCharInput_BE.toggleIns - oemKeyTbl)
            db eDel, " "
            dw (buffCharInput_BE.eDel - oemKeyTbl)
oemKeyTbl_len   equ ($ - oemKeyTbl) / 4