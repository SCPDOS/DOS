/*Linker script for SHARE*/
/*Order of module linkage: DOSSEG -> SHAREBSS -> SHARECODE -> SHAREINIT*/

OUTPUT_ARCH(i386:x86-64)
OUTPUT_FORMAT("pei-x86-64")
ENTRY(ep)

SECTIONS {
    __resstart = .;
    bss$r 0x0 (NOLOAD): {
        ./bin/share.obj(bss$r)
    }

    code$r ALIGN(0x4) : {
        ./bin/share.obj(code$r)
    }
    __resend = .;

/*The sections below are all ejected at load time*/

    code$i ALIGN(0x4) : {
        ./bin/share.obj(code$i)
    }

    .text ALIGN(0x4) : {
        /**/
    }
}

/*Length of the resident section in paragraphs rounded up*/
resLenParas = (((__resend - __resstart) + 0x0F) >> 4) ;