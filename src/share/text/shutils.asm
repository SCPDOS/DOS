;All utility functions go here

critEnter:
    mov eax, 8001h  ;Enter DOS Level 1 critical section
    int 2ah
    return
critExit:
    mov eax, 8101h  ;Exit DOS Level 1 critical section
    int 2ah
    return

