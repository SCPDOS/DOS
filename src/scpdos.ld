/*Linker script for DOS*/

/*Order of module linkage: TOP -> OEMINIT -> SYSINIT -> DOS -> Drivers -> EOF*/


SECTIONS {
    .text : {

    }

    oem$ 0x0: {
        ./bin/tmp/oem.obj
    }

    sys$ BLOCK(1): {
        ./bin/tmp/sys.obj(stext)
        ./bin/tmp/sys.obj(sdata)
    }

    dosResStart = . ;
    
    /*--------------------------------------------*/
    /* Addresses start afresh in the module below */
    /*--------------------------------------------*/

    dos$bss 0x0 (NOLOAD): {
        ./bin/tmp/krn.obj(dBSS)   
    }

    dos$ BLOCK(1) : {
        ./bin/tmp/krn.obj(dtext)
    }

    drv$ BLOCK(1) : {
        /* Ensure the segments in the driver are in the right order*/
        drv$_start = .; /*Used by SYSINIT to get a ptr to the header*/
        ./bin/tmp/drv.obj(kDrvText)
        ./bin/tmp/drv.obj(kDrvDat)
        dosResLen = .;          /*End of resident code and data*/
    }

    drv$bss (NOLOAD):{
        ./bin/tmp/drv.obj(kDrvBSS)
    }

    dosEnd = ALIGN(16) ; /*Get the para aligned vptr to end of alloc*/
}