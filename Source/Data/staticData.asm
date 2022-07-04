
;-----------------------------------:
;           Static Data             :
;-----------------------------------:
dosMajor    db 00h      ;Version 0
dosMinor    db 01h      ;.01
dosBIOSName db "SCPBIOS .SYS"
dosKernName db "SCPBDOS .SYS"

;Keyboard vCon static data taht can be edited and replaced by a user if they
; wish to install their own custom logic.
extKeyFunc  dq 0 ;The editing keys can be replaced by replacing this ptr
extESC      db 00h  ;NULL char is our Escape char
extBreak    db 1Bh  ;1Bh is our Break Char
;Extended ASCII keys with special meanings
extKeyTbl   db eF1
            dw (buffCharInput_BE.f1 - extKeyTbl)
            db eF2
            dw (buffCharInput_BE.f2 - extKeyTbl)
            db eF3
            dw (buffCharInput_BE.f3 - extKeyTbl)
            db eF4
            dw (buffCharInput_BE.f4 - extKeyTbl)
            db eF5
            dw (buffCharInput_BE.f5 - extKeyTbl)
            db eF6
            dw (buffCharInput_BE.f6 - extKeyTbl)
            db eF7
            dw (buffCharInput_BE.f7 - extKeyTbl)
            db eCursL
            dw (buffCharInput_BE.delete - extKeyTbl)
            db eCursR
            dw (buffCharInput_BE.f1 - extKeyTbl)
            db eIns
            dw (buffCharInput_BE.toggleIns - extKeyTbl)
            db eDel
            dw (buffCharInput_BE.eDel - extKeyTbl)
extKeyTbl_len   equ ($ - extKeyTbl) / 3