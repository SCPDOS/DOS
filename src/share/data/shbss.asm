;All BSS variables go here

pMftArena   dq ?
dMftArenaSz dd ?    ;Size of MFT arena in bytes (max 1048576 bytes i.e. 1 Mb)
pLockArena  dq ?
pFreeLock   dq ?    ;Ptr to head of Free lock chain. 0 means no free locks left
wNumLocks   dw ?    ;Store number of locks here (max 9999)
pOldI2Fh    dq ?    ;Ptr to old Int 2Fh for chaining
pDosseg     dq ?    ;Ptr to the DOSSEG
pPSP        dq ?    ;Ptr to share psp. Used as scratch space once resident.