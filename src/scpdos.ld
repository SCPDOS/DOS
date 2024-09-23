/*Linker script for DOS*/

/*Order of module linkage: TOP -> OEMINIT -> SYSINIT -> DOS -> Drivers -> EOF*/


SECTIONS {
    .text : {

    }

    oem$ 0x0: {
        ./bin/tmp/oem.obj
    }

    sys$ BLOCK(1): AT(ADDR(oem$) + SIZEOF(oem$)) {
        ./bin/tmp/sys.obj(stext)
        ./bin/tmp/sys.obj(sdata)
    }

    /*--------------------------------------------*/
    /* Addresses start afresh in the module below */
    /*--------------------------------------------*/

    dos$bss 0x0 (NOLOAD): AT(LOADADDR(sys$) + SIZEOF(sys$)) {
        ./bin/tmp/krn.obj(dBSS)   
    }

    dos$ BLOCK(1) : AT(LOADADDR(sys$) + SIZEOF(sys$)){
        ./bin/tmp/krn.obj(dtext)
    }

    drv$ BLOCK(1) : AT(LOADADDR(dos$) + SIZEOF(dos$)){
        /* Ensure the segments in the driver are in the right order*/
        ./bin/tmp/drv.obj(kDrvText)
        ./bin/tmp/drv.obj(kDrvDat)
        dosResLen = .;          /*End of resident code and data*/
    }

    drv$bss (NOLOAD): AT(LOADADDR(drv$) + SIZEOF(drv$)){
        ./bin/tmp/drv.obj(kDrvBSS)
    }

    dosResStart = LOADADDR(dos$);  /*Start of the DOS code in the file*/
    dosEnd = . ; /*Get the vpointer to the end of the allocation*/
}