ioctrl:            ;ah = 44h, handle function
;Possible subfunctions
;al = 00h => Get Handle Information word in dx  x
;al = 01h => Set Handle Information word        x
;al = 02h => Read from Char device              
;al = 03h => Write to Char device               
;al = 04h => Read from Block device             
;al = 05h => Write to Block device              
;al = 06h => Get input status                   
;al = 07h => Get output status                  
;al = 08h => Is a block device removable?       
;al = 09h => Is a logical device local or remote?   
;al = 0Ah => Is a handle to a local or remote file? 
;al = 0Bh => Change sharing retry count 
;al = 0Ch => Generic IOCTL handle request (RESERVED) - Code page switching
;al = 0Dh => Block device Generic IOCTL request     
;al = 0Eh => Get logical device                 
;al = 0Fh => Set logical device  
    call derefSFTPtr
    jnc .handleOk
    mov eax, errBadHdl
    jmp extErrExit
.handleOk:
;Now rdi has ptr to SFT handle               
    test al, al
    jz .getDevWord
    cmp al, 1
    je .setDevWord
    cmp al, 2
    je .readCharDev
    cmp al, 3
    je .writeCharDev
    cmp al, 4
    je .readBlockDev
    cmp al, 5
    je .writeBlockDev
    cmp al, 6
    je .getInputStatus
    cmp al, 7
    je .getOutputStatus
    cmp al, 8
    je .isRemdev
    cmp al, 9
    je .isDevLocal
    cmp al, 0Ah
    je .isHdlLocal
    cmp al, 0Bh
    je .setSharingCount
    cmp al, 0Ch
    je .invalidFunction ;Change Codepage, reserved for now
    cmp al, 0Dh
    je .genericDevIOCTL
    cmp al, 0Eh
    je .getDrvLogicalDevice
    cmp al, 0Fh
    je .setDrvLogicalDevice
.invalidFunction:
    mov al, errInvFnc
    jmp extErrExit
.getDevWord:
;Input: bx = File Handle
    mov dx, word [rdi + sft.wDeviceInfo]
    xor al, al
    call getUserRegs
    mov word [rsi + callerFrame.rdx], dx
    return
.setDevWord:
;Input: bx = File Handle
;       dl = Low byte of Device information word 
    mov byte [rdi + sft.wDeviceInfo], dl
    return
.readCharDev:
.writeCharDev:
.readBlockDev:
.writeBlockDev:
.getInputStatus:
.getOutputStatus:
.isRemdev:
.isDevLocal:
.isHdlLocal:
.setSharingCount:
.genericDevIOCTL:
.getDrvLogicalDevice:
.setDrvLogicalDevice:
    jmp extErrExit