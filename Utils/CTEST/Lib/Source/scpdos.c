#include "../../Headers/scpdos.h"
#include "internal.c"
#include "dosfcb.c"
#include "dosfile.c"
#include "dosexec.c"
#include "dosmem.c"
#include "doscon.c"
#include "dosnet.c"

// Some generic functions go here too
BYTE GetDiskReadVerifyFlag(){
    __asm__ __volatile__(
        "mov eax, 0x5400\n\t"
        "int 0x41\n\t"
    );
}