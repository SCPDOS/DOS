/*Linker script for DOS*/

/*Order of module linkage: TOP -> OEMINIT -> SYSINIT -> DOS -> Drivers -> EOF*/


SECTIONS {
    .text : {

    }

    oem$ 0x0: {
        ./bin/oem.obj
    }

    sys$ BLOCK(1): {
        ./bin/sys.obj(stext)
        ./bin/sys.obj(sdata)
    }

    dosResStart = . ;
    
    /*--------------------------------------------*/
    /* Addresses start afresh in the module below */
    /*--------------------------------------------*/

    dos$bss 0x0 (NOLOAD): {
        ./bin/krn.obj(dBSS)   
    }

    dos$ BLOCK(1) : {
        ./bin/krn.obj(dtext)
    }

    drv$ BLOCK(1) : {
        /* Ensure the segments in the driver are in the right order*/
        drv$_start = .; /*Used by SYSINIT to get a ptr to the header*/
        ./bin/drv.obj(kDrvText)
        ./bin/drv.obj(kDrvDat)
        dosResLen = .;          /*End of resident code and data*/
    }

    drv$bss (NOLOAD):{
        ./bin/drv.obj(kDrvBSS)
    }

    dosEnd = ALIGN(16) ; /*Get the para aligned vptr to end of alloc*/
}