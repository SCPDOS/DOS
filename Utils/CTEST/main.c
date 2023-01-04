/*Uses SYS V-64 ABI
 * Arguments passed in:              rdi, rsi, rdx, rcx, r8, r9 then stack
 * Return value is passed in:        rax, rdx. Low qword in rax.
 * Registers that can be clobbered:  rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
 * Registers that need be preserved: rbx, rsp, rbp, r12, r13, r14, r15 
 */

 /* PLEASE USE THE MS STANDARD PLEASE PLEASE*/

#include "./Headers/scpdos.h"

void main(){
	char *outString = "\n\rThis is a test string\n\r$";
    writeString(outString);
    exit(0);    /*Return with a return code of zero, always*/
}

