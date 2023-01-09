/* A test C program using our own libraries for OS interaction */

#include "./Headers/scpdos.h"

void main(){
	LPCSTR outString = "\n\rThis is a test string\n\r$";
    WriteString(outString);
    ExitProcess(0);    /*Return with a return code of zero, always*/
}
