/*Linker script for DOS*/

/*Order of module linkage: TOP -> OEMINIT -> SYSINIT -> DOS -> Drivers -> EOF*/

SECTIONS {
    oem$ 0x0: {
        ./bin/tmp/oem.obj
    }
    sys$ ALIGN(1): {
        ./bin/tmp/sys.obj
    }
    dos$ ALIGN(1): {
        ./bin/tmp/krn.obj
    }
    /* Ensure the segments in the driver are in the right order*/
    drv$ ALIGN(1): {
        ./bin/tmp/drv.obj(kDrvText)
        ./bin/tmp/drv.obj(kDrvDat)
        __drvEnd__ = .;
        ./bin/tmp/drv.obj(COMMON) /*All unallocated sections go here*/
    }
    /*Get the file length of resident DOS code*/
    /*dosLen = length of initialised sections of (.dos$ + .drv$)*/
    dosFileS = LOADADDR(dos$);  /*Start of the DOS code in the file*/
    dosLen = __drvEnd__ - dosFileS;
    dosEnd = .; /*Get the vpointer to the end of the allocation*/
}