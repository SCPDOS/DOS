    
    .x64p

    INCLUDE fatStruc.inc
    INCLUDE dosStruc.inc
    INCLUDE driverStruc.inc


loadCode SEGMENT USE64
    ASSUME ds:FLAT, es:FLAT

    dw 0AA55h           ;Initial signature
    xchg bx, bx
    lea rbp, startmsg   ;Get the RIP relative address of message
    mov eax, 1304h
    int 30h
    mov ebp, edx        ;Save the boot drive in bp
    mov ecx, 0C0000100h ;Read MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in
    lea rsi, dataSegPtr
    mov qword ptr [rsi], rdi    ;Save the pointer to the data segment 
    mov rcx, 1000h      ;Move 32Kb high
    push rdi
    rep movsq
    pop rdi
    add rdi, DiskStakTop + SIZEOF DiskStakTop  ;Reserve space for data area
    ;Add the size to the offset of the last element in dataSeg
    jmp rdi
    startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0
loadCode ENDS

resCode SEGMENT BYTE USE64
    ASSUME ds:FLAT, es:FLAT
    jmp short $
resCode ENDS

dataSeg SEGMENT BYTE
    dataSegPtr  dq ?    ;Pointer to the data Segment itself
    bootDrive   db ?    ;The Int 33h device we booted from
    requestHdr  drvReqHdr    <,,,,>   ;The device driver header
                db 20 dup (?)    ;Reserve xtra space for cmd data, with padding
    sysVarsPtr  dq ?    ;Pointer to the below structure
    mcbChainPtr dq ?    ;Pointer to the MCB chain
    dpbHeadPtr  dq ?    ;Pointer to the first DPB in the DPB chain
    sftHeadPtr  dq ?    ;Pointer to the first SFT header in SFT chain
    clockPtr    dq ?    ;Pointer to the current active CLOCK$ device
    conPtr      dq ?    ;Pointer to the current active CON device
    maxBytesSec dw ?    ;Maximum number of bytes per sector (size of buffers)
    bufHeadPtr  dq ?    ;Pointer to the first disk buffer
    cdsHeadPtr  dq ?    ;Pointer to the head of the CDS array
    sfcbHeadPTr dq ?    ;Pointer to the head of the System FCB chain
    numSafeSFCB dw ?    ;Number of protected FCBs (y in FCBS=x,y)
    numInt33drv db ?    ;Number of Int 33h drives detected in system
    lastdrvNum  db ?    ;Value of LASTDRIVE (default = 5) [Size of CDS array]
    numJoinDrv  db ?    ;Number of Joined Drives
    nulDevHdr   drvHdr <,,,,>


    inDOS       db ?    ;Incremented on each DOS call, decremented when leaving
    breakFlag   db ?    ;If set, check for CTRL+C on all DOS calls
    defaultDrv  db ?    ;Default, last accessed drive

    critStack   dq 40 dup (?)
    critStakTop LABEL QWORD 
    IOStack     dq 300 dup (?)
    IOStakTop   LABEL QWORD 
    DiskStack   dq 300 dup (?)
    DiskStakTop LABEL QWORD 
dataSeg ENDS

END