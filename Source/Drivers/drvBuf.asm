;This file is just to denote UNINITIALISED buffers and vars
; for the drivers that are to be used during INIT.
;They can (and should) be ejected after use

physVol db ?
fixDrv  db ?
remDrv  db ?

msdTempBuffer   db 4096 dup (?) 
