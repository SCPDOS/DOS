/*Linker script for DOS*/

/* These symbols need to be at the bottom of the image:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dosLen = length of initialised sections of (.drv$ + .dos$)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*/

/*Order of module linkage: TOP -> OEMINIT -> SYSINIT -> DOS -> Drivers -> EOF*/

SECTIONS {
    .oem$: {

    }
    .sys$: {

    }
    .dos$: {

    }
    .drv$: {

    }
    dosEnd = .
}