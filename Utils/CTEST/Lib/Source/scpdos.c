#include "../../Headers/scpdos.h"
#include "doserror.c"
#include "internal.c"
#include "dosfcb.c"
#include "dosfile.c"
#include "dosexec.c"
#include "dosmem.c"
#include "doscon.c"
#include "dosnet.c"

VOID SetInterruptVector(DWORD dwInterruptVector, LPVOID lpInterruptHandler){
    __asm__ __volatile__(
        "movzx eax, cl\n\t"
        "mov ah, 0x25\n\t"
        "int 0x41"
    );
}

LPVOID GetInterruptVector(DWORD dwInterruptVector){
    __asm__ __volatile__(
        "push rbx\n\t"
        "movzx eax, cl\n\t"
        "mov ah, 0x35\n\t"
        "int 0x41\n\t"
        "mov rax, rbx\n\t"
        "pop rbx"
    );
}

// Gotta do char functions still
/*
Gotta do the following:
    INT 21,29  Parse filename for FCB
	INT 21,2A  Get date
	INT 21,2B  Set date
	INT 21,2C  Get time
	INT 21,2D  Set time
	INT 21,2E  Set/reset verify switch
	INT 21,2F  Get disk transfer address
	INT 21,30  Get DOS version number
	INT 21,31  Terminate process and remain resident
	INT 21,32  Get pointer to drive parameter table (undocumented)
	INT 21,33  Get/set Ctrl-Break check state & get boot drive
	INT 21,34  Get address to DOS critical flag (undocumented)
	INT 21,35  Get vector
	INT 21,36  Get disk free space
	INT 21,37  Get/set switch character (undocumented)
	INT 21,38  Get/set country dependent information

    and 

    INT 21,5C  Lock/unlock file access (3.x+)
	INT 21,5D  Critical error information (undocumented 3.x+)
	INT 21,5E  Network services (3.1+)
	INT 21,5F  Network redirection (3.1+)
*/