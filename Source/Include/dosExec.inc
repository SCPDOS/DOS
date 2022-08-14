;This include file contains structures related to the EXE format
; and the exec frame used when setting up the EXE in memory by 41h/4Bh

;Int 41h/4Bh stack frame, used to store values whilst processing a EXE or COM
; file.
execLoadGo  equ 0
execLoad    equ 1
execInvld   equ 2
execOverlay equ 3

struc execFrame
    .bSubFunc   resb 1  ;Subfunction number
    .wProgHdl   resb 2  ;File handle number for executable file
    .wEnvSize   resb 2  ;Size of the environment block
    .wNameLen   resb 2  ;Needs to be less than 64 but welp
                resb 1  ;Alignment byte
    .rbx:
    .pParam     resq 1  ;Parameter list pointer
    .rdx:
    .pProgname  resq 1  ;Ptr to ASCIIZ program name
    .dNewSize   resd 1  ;Size of the allocation for the new program
                resd 1  ;Reserved 4 bytes
    .pEnvBase   resq 1  ;Ptr to new Environment block, NOT THE MCB FOR ENV BLK
    .pProgMCB   resq 1  ;Ptr to the program MCB, for convenience
    .pProgBase  resq 1  ;Ptr to new program base address

    .dCOFFhdr   resd 1  ;Offset in file to COFF header. Set to 0 for COM
    .dStackSz   resd 1  ;Stack size to allocate in bytes. Set to 0 for COM
    .dHeapSz    resd 1  ;Heap size in bytes in bytes. Set to 0 for COM 
    .wNumSecns  resw 1  ;Number of sections in the file, set to 0 for COM
    .wSzOptHdr  resw 1  ;Size of the optional header, set to 0 for COM

endstruc

;Possible blocks to be pointed to by rbx

struc execProg 
    .pEnv       resq 1  ;Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   resq 1  ;Ptr to the command line to be placed at PSP + 80h
    .pfcb1      resq 1  ;Ptr to the first FCB (parsed argument 1)
    .pfcb2      resq 1  ;Ptr to the second FCB  (parsed argument 2)
endstruc

struc loadProg
    execProg    resb execProg_size
    .initRSP    resq 1
    .initRIP    resq 1
endstruc

struc loadOvly
    .pLoadLoc   resq 1  ;Address in memory to load the overlay
    .dRelocFct  resd 1  ;Relocation factor for 
endstruc

;----------------------------
;   EXE header structures   :
;----------------------------

;DOS MZ .EXE header. Functionally useless except for e_magic and e_lfanew
dosMagicSignature   equ "MZ"
dosMagicSignature2  equ "ZM"
struc imageDosHdr
    .e_magic    resw 1  ;      // Magic number
    .e_cblp     resw 1  ;      // Bytes on last page of file
    .e_cp       resw 1  ;      // Pages in file
    .e_crlc     resw 1  ;      // Relocations
    .e_cparhdr  resw 1  ;      // Size of header in paragraphs
    .e_minalloc resw 1  ;      // Minimum extra paragraphs needed
    .e_maxalloc resw 1  ;      // Maximum extra paragraphs needed
    .e_ss       resw 1  ;      // Initial (relative) SS value
    .e_sp       resw 1  ;      // Initial SP value
    .e_csum     resw 1  ;      // Checksum
    .e_ip       resw 1  ;      // Initial IP value
    .e_cs       resw 1  ;      // Initial (relative) CS value
    .e_lfarlc   resw 1  ;      // File address of relocation table
    .e_ovno     resw 1  ;      // Overlay number
    .e_res      resw 4  ;      // Reserved words
    .e_oemid    resw 1  ;      // OEM identifier (for e_oeminfo)
    .e_oeminfo  resw 1  ;      // OEM information; e_oemid specific
    .e_res2     resw 10 ;      // Reserved words
    .e_lfanew   resd 1  ;      // File address of new exe header
endstruc

;Pssobile signatures pointed to by e_lfanew
imageDosSignature   equ 05A4Dh      ; MZ, REJECT
imageOS2Signature   equ 0454Eh      ; NE, REJECT
imageOS2SignatureLE equ 0454Ch      ; LE, REJECT
imagePESignature    equ 00004550h   ; PE00, ACCEPT

;COFF File header, immadiately follows the signature so @ [e_lfanew + 4]

struc imageFileHeader
    .wMachineType        resw 1  ;Machine Type, must be imageFileMachineAMD64
    .wNumberOfSections   resw 1  ;Number of sections in file
    .dTimeDateStamp      resd 1  ;Unix datestamp
    .dPtrToSymbolTbl     resd 1  ;Pointer to the symbol table,      NOT USED
    .dNumberOfSymbols    resd 1  ;Number of symbols in the table,   NOT USED
    .wSizeOfOptionalHdr  resw 1  ;Size of the optional header
    .wCharacteristics    resw 1  ;Bitfield characteristics
endstruc

;The only valid values in the .machineType field

imageFileMachineUnknown equ 0
imageFileMachineAMD64   equ 08664h 

;Optional header, immediately follows COFF header

struc imageFileOptionalHeader
	.wMagic                     resw 1 ;0x010b - PE32, 0x020b - PE32+ (64 bit)
	.bMajorLinkerVersion        resb 1
	.bMinorLinkerVersion        resb 1
	.dSizeOfCode                resd 1
	.dSizeOfInitializedData     resd 1
	.dSizeOfUninitializedData   resd 1
	.dAddressOfEntryPoint       resd 1
	.dBaseOfCode                resd 1
.optLen:    ;Use this symbol for the basic data from the header
    .qImageBase                 resq 1
	.dSectionAlignment          resd 1
	.dFileAlignment             resd 1
	.wMajOSVer                  resw 1
	.wMinOSVer                  resw 1
	.wMajorImageVersion         resw 1
	.wMinorImageVersion         resw 1
	.wMajorSubsystemVersion     resw 1
	.wMinorSubsystemVersion     resw 1
	.dWin32VersionValue         resd 1
	.dSizeOfImage               resd 1
	.dSizeOfHeaders             resd 1
	.dCheckSum                  resd 1
	.wSubsystem                 resw 1
	.wDllCharacteristics        resw 1
;The stuff between optLen and here are useless
	.qSizeOfStackReserve        resq 1
	.qSizeOfStackCommit         resq 1  ;Default allocation is 128Kb
	.qSizeOfHeapReserve         resq 1
	.qSizeOfHeapCommit          resq 1  ;Default allocation is 128Kb
;Last two entries similarly useless
	.dLoaderFlags               resd 1
	.dNumberOfRvaAndSizes       resd 1
endstruc

romMagicNum     equ 0107h
pe32MagicNum    equ 010Bh
pe64MagicNum    equ 020Bh

;Data directory header structure

struc imageDataDirectory
    .virtualAddress  resd 1
    .size    resd 1
endstruc

;   >>>> CAVEAT    CAVEAT    CAVEAT    CAVEAT    CAVEAT    CAVEAT <<<<
;We dont use data directory entries. The section header table begins at the end 
; of the optional header. Thus we add the size of the optional header to the 
; ptr to the start of the optional header to get the first section.
;Note if there is no optional header, then the sectionTable immediately follows
; COFF header
;   >>>> CAVEAT    CAVEAT    CAVEAT    CAVEAT    CAVEAT    CAVEAT <<<<

;Section headers that form the Section Table.
;There are wNumberOfSections many entries in this table.
;Each entry is one imageSectionHdr

struc imageSectionHdr       ;size 40 bytes
    .name                   resb 8
	.dVirtualSize           resd 1
	.dVirtualAddress        resd 1
	.dSizeOfRawData         resd 1
	.dPointerToRawData      resd 1
	.dPointerToRelocations  resd 1
	.dPointerToLinenumbers  resd 1
	.wNumberOfRelocations   resw 1
	.wNumberOfLinenumbers   resw 1
	.dCharacteristics       resd 1
endstruc

;Section Hdr characteristics flags 
imgScnCntCode   equ 20h   ;The section contains executable code.
imgScnCntData   equ 40h   ;The section contains initialized data.
imgScnCntBSS    equ 80h   ;The section contains uninitialized data. 