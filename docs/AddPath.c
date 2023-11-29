/*
 * This Program is designed to give a high level overview in C of how the addPathspecToBuffer()
 * function works. Whilst it is a proper C function it is not to be built (yet) as part of 
 * the kernel, but to be used as pseudocode to understand how the filename parsing works.
 *
 * This is part of a series of functions that I will be writing to explain how filename 
 * and path parsing works in SCP/DOS. At the same time, it will allow me to write some  
 * filesystem functions in C, which will be helpful eventually when I start writing C code 
 * in the kernel and when I start working on SCP/DOS 2 (OS/3).
 *
 * Note this function is written in "assembly syle", i.e. variables are named after registers
 * and GOTOs are used for flow control when  while semantics stray too far from the original
 * assembly source.
*/

void addPathspecToBuffer (char *rbx, char *rsi, char *rdi){
	if (!dotPath()){
		FCBtoAsciiz();
		iterveneEnterJoin(); /*Returns silently affecting (or not) the path as needed for JOIN*/
	}
	else{
		if(twoDots()){
			if(rbx == rdi - 1){
				if (!interveneExitJoin()) /*Intervenes and returns true if it is a JOIN drive*/){
					goto error; /*Cannot go back along a root directory if not on a JOIN drive*/
				}
			}
			else{
				rdi-=2;	/*This is guaranteed to be safe due to the format of a path :) */
				while (rbx > (rdi - 1)){
					rdi--;
					if(swapPathSeparator(*rdi)){
						rdi++;	/*Go past the pathsep char.*/
						goto handleTerminator; /*Exit pathsep handling*/
					}
				}
			}
		}
	}
handleTerminator:
	char al = *(fcbName + 11); /*Get the terminator char for this path portion*/
	if(al == 0){
		/*If this is a terminator, check if the previous char on FQ path is a SUBDIR pathsep.*/
		if (rbx > (rdi - 1) && swapPathSeparator(*(rdi - 1))){
			rdi--;
			*rdi = al; /*Overwrite the trailing pathsep char*/
			rdi++;
		}
		else{
			*rdi = al; /*Don't overwrite the trailing pathsep*/
		}
	}
	else if (swapPathSeparator(al)){
		rdi--;
		if (!swapPathSeparator(*rdi)){
			rdi++;
			*rdi = al;
			/*Use the alignment increment below to push us past the pathsep we just placed.*/
		}
		rdi++;		
	}
	else{
		goto error;
	}
exitOk:
	/*Clear CF*/
	return;
error:
	/*Set globals and CF*/
	return;
}