/*
    The start of a DOS low level library upon which stdlib (newlib) will be 
    built!
*/

#ifndef DOS_H
#define DOS_H
#pragma pack(1) //Ensure all structs are aligned to 1 byte boundaries

#include "basetsd.h"
#include "dosdrive.h"
#include "doserror.h"
#include "internal.h"
#include "dosdrvr.h"
#include "dosfcb.h"
#include "dosfile.h"
#include "dosexec.h"
#include "dosmem.h"
#include "doscon.h"
#include "dosnet.h"


#endif