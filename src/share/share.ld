/*Linker script for SHARE*/
/*Order of module linkage: DOSSEG -> SHAREBSS -> SHARECODE -> SHAREINIT*/

OUTPUT_ARCH(i386:x86-64)
OUTPUT_FORMAT("pei-x86-64")
ENTRY(ep)

alignment = __section_alignment__;
alignMask = alignment - 1;

loadAddress = (SIZEOF_HEADERS + alignMask) & ~alignMask;

SECTIONS {
    __resstart = .;
    bss$r loadAddress (NOLOAD): {
        ./bin/share.obj(bss$r)
    }

    code$r ALIGN(alignment) : {
        ./bin/share.obj(code$r)
    }
    __resend = .;

/*The sections below are all ejected at load time*/

    code$i ALIGN(alignment) : {
        ./bin/share.obj(code$i)
    }

    .text ALIGN(alignment) : {
        /**/
    }
}

/*Length of the resident section in paragraphs rounded up*/
resLenParas = (((__resend - __resstart) + 0x0F) >> 4) ;