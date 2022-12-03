BITS 64
DEFAULT REL

segment .data
    msg db "Hello World", 0Ah, 0Dh, "$"

segment .code
    global main
main:
    lea rdx, msg
    mov eax, 0900h
    int 41h
    mov eax, 4C00h
    int 41h
