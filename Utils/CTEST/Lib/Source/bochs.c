void bochsBreak(){
    __asm__ __volatile__ (
    "xchg bx, bx"
    );
}