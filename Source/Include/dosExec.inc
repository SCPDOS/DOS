;This include file contains structures related to the EXE format
; and the exec frame used when setting up the EXE in memory by 41h/4Bh

struc execFrame
    .bSubFunc   resb 1  ;Subfunction number
    .wProgHdl   resb 2  ;File handle number for executable file
    .wEnvSize   resb 2  ;Size of the environment block
    .wNameLen   resb 2  ;Needs to be less than 64 but welp
                resb 1  ;Alignment byte
    .rbx:
    .pParam     resq 1  ;Parameter list pointer
    .rdx:
    .pProgname  resq 1  ;Ptr to ASCIIZ program name
    .dNewSize   resd 1  ;Size of the allocation for the new program
                resb 4  ;Alignment dword
    .pEnvBase   resq 1  ;Ptr to new Environment block 
    .pProgMCB   resq 1  ;Ptr to the program MCB
    .pProgBase  resq 1  ;Ptr to new program base address

endstruc
