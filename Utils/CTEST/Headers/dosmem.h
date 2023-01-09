#ifndef DOSMEM_H
#define DOSMEM_H
#include "basetsd.h"

#define MEMORY_FIRST_FIT    0
#define MEMORY_LAST_FIT     1
#define MEMORY_BEST_FIT     2
#define MEMORY_ERROR        0xFF
#endif
LPVOID VirtualAllocate(SIZE_T dwNumberOfParagraphs);

BOOL VirtualFree(LPVOID lpAddress);

BOOL VirtualReallocate(LPVOID lpAddress, SIZE_T dwNumberOfParagraphs);

BYTE GetMemoryAllocationStrategy();

BOOL SetMemoryAllocationStrategy(BYTE bAllocationStrategy);

//Undocumented but exposed function for now.
LPVOID PhysicalAllocate(SIZE_T dwNumberOfParagraphs);

//Undocumented but exposed function for now.
BOOL PhysicalFree(LPVOID lpAddress);

//Undocumented but exposed function for now.
BOOL PhysicalReallocate(LPVOID lpAddress, SIZE_T dwNumberOfParagraphs);