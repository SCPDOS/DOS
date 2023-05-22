;This include file contains structures related to the EXE format
; and the exec frame used when setting up the EXE in memory by 41h/4Bh

;Int 41h/4Bh stack frame, used to store values whilst processing a EXE or COM
; file.
execLoadGo  equ 0
execLoad    equ 1
execInvld   equ 2
execOverlay equ 3
execBkgrnd  equ 4

struc execFrame
    .oldRBP     resq 1  
    .bSubFunc   resq 1  ;Subfunction number
    .wProgHdl   resq 1  ;File handle number for executable file
    .wEnvSize   resq 1  ;Size of the environment block
    .wNameLen   resq 1  ;Needs to be less than 64 but welp
    .pParam     resq 1  ;Parameter list pointer
    .pProgname  resq 1  ;Ptr to ASCIIZ program name

    .pEnvBase   resq 1  ;Ptr to new Environment block, NOT THE MCB FOR ENV BLK
    .pPSPBase   resq 1  ;Ptr to new PSP base address
    .pProgBase  resq 1  ;Ptr to Prog. Base addr., ALIGNED FOR FIRST EXE SECTION
    .dProgSize  resq 1  ;Program size is saved here, PSP included.
    .dCOFFhdr   resq 1  ;Offset in file to COFF header. Set to 0 for COM
    .wCOFFChars resq 1  ;Save the COFF characteristics in here
.pProgEP:   ;Program EP, recycle this qword
    .wNumSeg    resq 1  ;Number of segments, consider reusing NameLen
    .dFilePtr   resq 1  ;Keep of track of where in file we are
.qRelocVal:  ;Used when computing relocations, Rebasing factor
endstruc

;Possible blocks to be pointed to by rbx

struc execProg 
    .pEnv       resq 1  ;Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   resq 1  ;Ptr to the command line to be placed at PSP + 80h
    .pfcb1      resq 1  ;Ptr to the first FCB (parsed argument 1)
    .pfcb2      resq 1  ;Ptr to the second FCB  (parsed argument 2)
endstruc

struc loadProg
    .pEnv       resq 1  ;Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   resq 1  ;Ptr to the command line to be placed at PSP + 80h
    .pfcb1      resq 1  ;Ptr to the first FCB (parsed argument 1)
    .pfcb2      resq 1  ;Ptr to the second FCB  (parsed argument 2)
    .initRSP    resq 1  ;Return the start value for rsp
    .initRIP    resq 1  ;Return the entry point for the task
endstruc

struc loadOvly
    .pLoadLoc   resq 1  ;Address in memory to load the overlay
    .qRelocFct  resq 1  ;Relocation address for relocatable items (0 for COM files)
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

;COFF File header @ [e_lfanew]

struc imageFileHeader
    .dPESignature        resd 1  ;PESignature, must be PE00
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

;Characteristics to check for
imageFileRelocsStripped equ 0001h   ;If set, base addr MUST equal load addr
imageFileExecutable     equ 0002h   ;Must always be set else badfmterr

;Optional header, immediately follows COFF header

struc imageFileOptionalHeader
	.wMagic                     resw 1 ;0x010b - PE32, 0x020b - PE32+ (64 bit)
	.bMajorLinkerVersion        resb 1
	.bMinorLinkerVersion        resb 1
	.dSizeOfCode                resd 1
	.dSizeOfInitializedData     resd 1
	.dSizeOfUninitializedData   resd 1
	.dAddressOfEntryPoint       resd 1  ;RVA of the EP for program
	.dBaseOfCode                resd 1
    .qImageBase                 resq 1  ;Preferred load address  
	.dSectionAlignment          resd 1
	.dFileAlignment             resd 1
	.wMajOSVer                  resw 1
	.wMinOSVer                  resw 1
	.wMajorImageVersion         resw 1
	.wMinorImageVersion         resw 1
	.wMajorSubsystemVersion     resw 1
	.wMinorSubsystemVersion     resw 1
	.dWin32VersionValue         resd 1
	.dSizeOfImage               resd 1  ;Size of ctg mem to hold program
	.dSizeOfHeaders             resd 1
	.dCheckSum                  resd 1
	.wSubsystem                 resw 1
	.wDllCharacteristics        resw 1
	.qSizeOfStackReserve        resq 1
	.qSizeOfStackCommit         resq 1  ;Default stack allocation is 256Kb
	.qSizeOfHeapReserve         resq 1
	.qSizeOfHeapCommit          resq 1
	.dLoaderFlags               resd 1
	.dNumberOfRvaAndSizes       resd 1  ;Number of data dirs entries in header
endstruc

romMagicNum     equ 0107h
pe32MagicNum    equ 010Bh
pe64MagicNum    equ 020Bh

;Data directory header structure

struc imageDataDirectory
    .virtualAddress  resd 1
    .size    resd 1
endstruc

struc baseRelocBlock
    .pageRVA    resd 1  ;Offset from the load address
    .size       resd 1  ;Number of bytes in this block
    ;Each entry is a word. High 4 bits are type of reloc (all must be 0Ah)
endstruc
;Section headers that form the Section Table.
;There are wNumberOfSections many entries in this table.
;Each entry is one imageSectionHdr

struc imageSectionHdr       ;size 40 bytes
    .qName                  resb 8
	.dVirtualSize           resd 1  ;If this is bigger than RawData, 0 pad
	.dVirtualAddress        resd 1  ;Offset from ImageBase
	.dSizeOfRawData         resd 1
	.dPointerToRawData      resd 1  ;File pointer location
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